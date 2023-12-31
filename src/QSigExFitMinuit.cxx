// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#include "QSigExFitMinuit.h"

ClassImp(QSigExFitMinuit)

QProcRandom3 QSigExFitMinuit::fRnd;

QSigExFitMinuit::~QSigExFitMinuit()
{
  TerminateFit();
  if(fMinimArgs) {
    delete fMinimArgs;
    fMinimArgs=NULL;
  }
}

Double_t QSigExFitMinuit::EvalFCN() const
{
  if(!fMinuit) {
    fprintf(stderr,"QSigExFitMinuit::EvalFCN: Error: InitFit has not been called\n");
    throw 1;
  }
  Double_t fval;

  fCurInstance=this;
  fMinuit->Eval(fMinuit->GetNumFreePars(),NULL,fval,fMinuit->fU,0);

  return fval;
}

Int_t QSigExFitMinuit::FindMinimArg(const char* name) const
{
  for(Int_t i=0; i<fMinimArgs->Count(); i++){
    if(!strdiffer((*fMinimArgs)[i].GetName(),name)) return i;
  }
  return -1;
}

Double_t QSigExFitMinuit::Fit(const Bool_t& fituncerts, const Int_t& niters, const Bool_t& stoponsuccess, const Bool_t& inititerseed)
{
  Int_t i,j;
  Double_t* minimargs=new Double_t[fMinimArgs->Count()];
  Int_t ierflg=0;
  Int_t numpar=fParams.Count();
  Double_t dbuf1,dbuf2,dbuf3,dbuf4; 
  Int_t ibuf1;
  TString strbuf;
  Int_t iter;
  TString minuitstatus;
  Double_t fcnmin;

  fCurInstance=this;

  if(!fMinuit) {
    fprintf(stderr,"QSigExFitMinuit::Fit: Error: InitFit has not been called\n");
    throw 1;
  }

  try{

    for(i=0; i<fMinimArgs->Count(); i++) minimargs[i]=(*fMinimArgs)[i];
    j=0;

    // Reinitialize floating parameters values
    for (i=0; i<numpar; i++){

      if(fParams[i].IsMaster()) {

	//If the parameter is not hidden to Minuit
	if(fParams[i].IsFixed()!=1) {

	  if(!fParams[i].IsFixed()) {
	    fMinuit->mnparm(j, fParams[i].GetName(), fParams[i].GetStartVal(), fParams[i].GetStepVal(), fParams[i].GetMinVal(), fParams[i].GetMaxVal(), ierflg);
	  }
	  ++j;
	}
      }
    }

    if(inititerseed) fRnd.InitProcObj();

    //Loop over fit iterations
    for(iter=0;;) {

      if(j) {
	dbuf1=fMinuitStrategy;
	fMinuit->mnexcm("SET STR",&dbuf1,1,ierflg);

	if(fMinuitAccuracy>0) {
	  dbuf1=fMinuitAccuracy;
	  fMinuit->mnexcm("SET EPSmachine",&dbuf1,1,ierflg);
	}

	//**** Call the minimizer ****
	fMinuit->mnexcm((TString&)fMinimName, minimargs, fMinimArgs->Count(), ierflg ); 

	if(fituncerts) {
	  j=0;

	  // set fit parameters to result from this run of the minimizer
	  for (i=0; i<numpar; i++){

	    //If the parameter is not hidden to Minuit
	    if(fParams[i].IsFixed()!=1 && fParams[i].IsMaster()) {

	      if(!fParams[i].IsFixed()) {
		fMinuit->mnpout(j,strbuf,dbuf4,dbuf1,dbuf2,dbuf3,ibuf1); //dbuf4 contains the fitted value
		fMinuit->mnparm(j, fParams[i].GetName(), dbuf4, fParams[i].GetStepVal(), fParams[i].GetMinVal(), fParams[i].GetMaxVal(), ierflg);
	      }
	      ++j;
	    }
	  }
	  fMinuit->mnexcm((TString&)fMinimName, minimargs, fMinimArgs->Count(), ierflg ); 

	  dbuf1=fMinosMaxCalls;
	  fMinuit->mnexcm("MINO",&dbuf1,(Int_t)(fMinosMaxCalls>=0),ierflg); 
	}
	minuitstatus=fMinuit->fCstatu;
	fcnmin=fMinuit->fAmin;

      } else {
	minuitstatus="SUCCESSFUL";
	fcnmin=EvalFCN();
      }

      if(!iter || (!(fcnmin>=fFCNMin) && (!strdiffer(minuitstatus,"SUCCESSFUL") || !strdiffer(minuitstatus,"CONVERGED ")))) {
	fFCNMin=fcnmin;
	fMinuitStatus=minuitstatus;
	ierflg = 0; 
	j=0;

	//Loop over the parameters
	for(i=0;i<numpar;i++){

	  //If the parameter is not hidden to Minuit
	  if(fParams[i].IsFixed()!=1) {

	    if(fParams[i].IsMaster()) {
	      fMinuit->mnpout(j,strbuf,ParamFitVal(i),dbuf1,dbuf2,dbuf3,ibuf1);

	      if(fituncerts) fMinuit->mnerrs(j,ParamPlusFitError(i),ParamMinusFitError(i),dbuf1,dbuf2);
	      else ParamPlusFitError(i)=ParamMinusFitError(i)=0;
	      ++j;

	    }

	  } else if(fQProcessor) ParamFitVal(i)=fQProcessor->GetParam(i);
	}

	if(fituncerts) {

	  if(fCovMatrix) {
	    delete fCovMatrix;
	    fCovMatrix=NULL;
	  }

	  if(fCorMatrix) {
	    delete fCorMatrix;
	    fCorMatrix=NULL;
	  }
	  Int_t numfpar=fMinuit->GetNumFreePars();        //Number of floating parameters
	  Double_t *covmat=new Double_t[numfpar*numfpar]; //covariance matrix array
	  //Get the covariance matrix from TMinuit
	  fMinuit->mnemat(covmat,numfpar);
	  //Store the matrix in a TMatrixDSym object
	  fCovMatrix=new TMatrixDSym(numfpar,covmat);
	  //Delete the covariance matrix array
	  delete[] covmat;

	  if(stoponsuccess && !strdiffer((TString&)fMinuitStatus,"SUCCESSFUL")) break;

	} else if(stoponsuccess && !strdiffer((TString&)fMinuitStatus,"CONVERGED ")) break;
      }

      ++iter;
      if(iter==niters) break;

      j=0;

      // Initialize floating parameters values using random values
      for (i=0; i<numpar; i++){

	if(fParams[i].IsMaster()) {

	  //If the parameter is not hidden to Minuit
	  if(fParams[i].IsFixed()!=1) {

	    if(!fParams[i].IsFixed()) {
	      fMinuit->mnparm(j, fParams[i].GetName(), fParams[i].GetMinVal()+fRnd.Rndm()*(fParams[i].GetMaxVal()-fParams[i].GetMinVal()), fParams[i].GetStepVal(), fParams[i].GetMinVal(), fParams[i].GetMaxVal(), ierflg);
	    }
	    ++j;
	  }
	}
      }
    }
    delete[] minimargs;

  } catch (Int_t e) {
    fprintf(stderr,"Exception handled by QSigExFit::Fit()\n");
    throw e;
  }

  return fFCNMin;
} 

void QSigExFitMinuit::InitFit()
{
  QSigExFit::InitFit();

  Int_t i,j;
  Double_t dbuf;
  Int_t numfpar=0;

  fMinuit=new TMinuit(fParams.Count());

  //Set parameters to use for Minuit SET PRINT command:
  Double_t level=fVerbose-1; // -1 tells minuit we want minimal printout.
  Int_t ierflg;
  //Execute the minuit SET PRINT command:
  fMinuit->mnexcm( "SET PRINT", &level, 1, ierflg );  

  //Execute the minuit SET WIDTH command: (changes the width of std output)
  Double_t width=200;
  fMinuit->mnexcm( "SET WIDTH", &width, 1, ierflg );  

  //Initialize Minuit with the type of fit function we are using.  
  //See http://root.cern.ch/root/roottalk/roottalk97/0407.html for more details on the following lines

  //Initialize Minuit with the type of fit function we are using.  
  //See http://root.cern.ch/root/roottalk/roottalk97/0407.html for more details on the following lines
  if(fCompiledFunc) { //If the function is compiled
    //If the function pointer is passed from CINT, call TMinuit::SetFCN(void*)
    /*if(G__p2f2funcname((void*)fCompiledFunc)) fMinuit->SetFCN((void*)fCompiledFunc);
    //else if it's passed from compiled code, call TMiuit::SetFCN(void (*)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t))
    else*/ fMinuit->SetFCN(fCompiledFunc);  //Sets name of fit function
  //}

  //else if(fInterpretedFunc) { //Else if the function is interpreted by CINT
  //  fMinuit->SetFCN(fInterpretedFunc);  //Sets name of fit function

  } else { //Else if no function has been set, throw an error
    fprintf(stderr,"QSigExFitMinuit::InitFit: Error: No fit function has been set\n");
    throw 1;
  }

  //set Minuit parameters to "undefined"
  fMinuit->mncler(); 

  //Execute the SET ERR command:
  fMinuit->mnexcm("SET ERR", &((Double_t&)fFCNError), 1, ierflg );

  ierflg=0;
  j=0;

  //Loop through master parameters, and initialize a variable in Minuit for each one.
  for (i=0; i<fParams.Count(); i++){

    //If the current parameter is not a slave of another param, add it to Minuit
    if(fParams[i].IsMaster()) {
      //printf("Param '%s' owns its value\n",fParams[i].GetName());

      //If IsFixed() is not equal to 1, add the parameter to Minuit
      if(fParams[i].IsFixed()!=1) {
	//mnparm implements a parameter definition with a parameter number,
	//name, starting value, step size, min and max values, and an error flag.
	fMinuit->mnparm(j, fParams[i].GetName(), fParams[i].GetStartVal(), fParams[i].GetStepVal(), fParams[i].GetMinVal(), fParams[i].GetMaxVal(), ierflg);

	//If the parameter is fixed, tell to TMinuit
	if(fParams[i].IsFixed()){
	  ParamFreeParamIndex(i)=-1;
	  dbuf=j+1;
	  fMinuit->mnexcm("FIX",&dbuf,1,ierflg);     //fix the parameter with that index

	  //Else if the parameter is free
	} else {
	  ParamFreeParamIndex(i)=numfpar++;
	}

	if(fQProcessor) fQProcessor->SetParamAddress(i,&(fMinuit->fU[j]));
	++j;

	//Else if the parameter is fixed but is not required to be passed to Minuit
      } else {

	if(fQProcessor) fQProcessor->SetParam(i,fParams[i].GetStartVal());
	ParamFreeParamIndex(i)=-1;
      }
    }
  }

  if(fQProcessor) {

    //Loop through slave parameters
    for (i=0; i<fParams.Count(); i++){

      //Update the buffer address
      if(!fParams[i].IsMaster()) fQProcessor->CopyParamAddress(fParams[i].GetTopMasterIndex(),i);
    }
  }
}

void QSigExFitMinuit::TerminateFit()
{
  if(fMinuit) {
    delete fMinuit;
    fMinuit=NULL;

    if(fQProcessor) {
      int j=0;

      //Loop through master parameters, and initialize a variable in Minuit for each one.
      for (int i=0; i<fParams.Count(); i++){

	//If the current parameter is not a slave of another param, add it to Minuit
	if(fParams[i].IsMaster()) {
	  //printf("Param '%s' owns its value\n",fParams[i].GetName());

	  //If IsFixed() is not equal to 1, add the parameter to Minuit
	  if(fParams[i].IsFixed()!=1) {
	    fQProcessor->SetParamAddress(i,NULL);
	    ++j;
	  }
	}
      }
    }
  }
}

void QSigExFitMinuit::SetFCN(void (*fcn)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t))
{
  //This overloaded version of QSigExFitMinuit::SetFCN() function is called when fcn is
  //a compiled function (fcn pointer is passed in compiled code or by the CINT
  //interpreter). It sets the parameters fitter function.

  fInterpretedFunc=NULL;
  fCompiledFunc=fcn;
}

void QSigExFitMinuit::SetFCN(void* fcn)
{
  //This overloaded version of QSigExFitMinuit::SetFCN() function is called when fcn
  //is an interpreted function by the CINT interpreter. It sets the parameters
  //fitter function.

  fCompiledFunc=NULL;
  fInterpretedFunc=fcn;
}

void QSigExFitMinuit::Browse(TBrowser *b)
{
  QSigExFit::Browse(b);
  b->Add(&fFCNError);
  b->Add(&fMinimName);
  b->Add(fMinimArgs,"Minimizer Arguments");
  b->Add(&fMinosMaxCalls);
  b->Add(&fMinuitAccuracy);
  b->Add(&fMinuitStrategy);
  b->Add(&fMinuitStatus);
}
