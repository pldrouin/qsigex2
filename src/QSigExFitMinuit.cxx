#include "QSigExFitMinuit.h"

ClassImp(QSigExFitMinuit)

QSigExFitMinuit::~QSigExFitMinuit()
{
  if(fMinuit) {
    delete fMinuit;
    fMinuit=NULL;
  }
  
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
    if(!strcmp((*fMinimArgs)[i].GetName(),name)) return i;
  }
  return -1;
}

Double_t QSigExFitMinuit::Fit(Bool_t fituncerts)
{
  Int_t i,j;
  Double_t* minimargs=new Double_t[fMinimArgs->Count()];
  Int_t ierflg=0;
  Int_t numpar=fParams.Count();
  Double_t dbuf1,dbuf2,dbuf3,dbuf4; 
  Int_t ibuf1;
  TString strbuf;

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

      //If the parameter is not hided to Minuit
      if(fParams[i].IsFixed()!=1) {

	if(!fParams[i].IsFixed()) {
	  fMinuit->mnparm(j, fParams[i].GetName(), fParams[i].GetStartVal(), fParams[i].GetStepVal(), fParams[i].GetMinVal(), fParams[i].GetMaxVal(), ierflg);
	}
	j++;

      //Else if the parameter is hided
      } else {
	if(fQProcessor) fQProcessor->SetParam(i,fParams[i].GetStartVal());
      }
    }

    //**** Call the minimizer ****
    fMinuit->mnexcm((TString&)fMinimName, minimargs, fMinimArgs->Count(), ierflg ); 

    if(fituncerts) {
      j=0;

      // set fit parameters to result from this run of the minimizer
      for (i=0; i<numpar; i++){

	//If the parameter is not hided to Minuit
	if(fParams[i].IsFixed()!=1) {

	  if(!fParams[i].IsFixed()) {
	    fMinuit->mnpout(j,strbuf,dbuf4,dbuf1,dbuf2,dbuf3,ibuf1); //dbuf4 contains the fitted value
	    fMinuit->mnparm(j, fParams[i].GetName(), dbuf4, fParams[i].GetStepVal(), fParams[i].GetMinVal(), fParams[i].GetMaxVal(), ierflg);
	  }
	  j++;
	}
      }
      // rerun minimizer
      fMinuit->mnexcm((TString&)fMinimName, minimargs, fMinimArgs->Count(), ierflg ); 

      //Calculate non-symmetric errors with MINOS:
      //Minuit calculates errors by finding the change in the parameter value 
      //required to change the function by fcnerror.
      //In the case of a Chi-2, 
      //     fcnerror =1.0 --> 1 sigma
      //     fcnerror =4.0 --> 2 sigma
      //     fcnerror =9.0 --> 3 sigma
      //When minosmaxcalls is positive, it sets the maximum number of function
      //calls per parameter to its values 
      dbuf1=fMinosMaxCalls;
      fMinuit->mnexcm("MINO",&dbuf1,(Int_t)(fMinosMaxCalls>=0),ierflg); 
    }

    delete[] minimargs;
    ierflg = 0; 

    fMinuitStatus=fMinuit->fCstatu;
    fFCNMin=fMinuit->fAmin;
    j=0;

    //Loop over the parameters
    for(Int_t i=0;i<numpar;i++){

      //If the parameter is not hided to Minuit
      if(fParams[i].IsFixed()!=1) {
	//mnpout takes in the index of the parameter we're asking about, and returns
	//it's name, fitted value, estimate of parameter uncertainty, lower limit
	//on the parameter value, upper limit on the parameter value, and the
	//internal parameter number (if the parameter is variable).  See Minuit
	//documentation for details.  We aren't actually interested in any of this
	//except the fit value.

	fMinuit->mnpout(j,strbuf,ParamFitVal(i),dbuf1,dbuf2,dbuf3,ibuf1);

	//mnerrs reports the errors calculated by MINOS.  It takes in the index of 
	//the parameter we're asking about, and returns the positive error, 
	//negative error (as a negative number), the parabolic parameter error 
	//and the global correlation coefficient for the parameter.  

	if(fituncerts) fMinuit->mnerrs(j,ParamPlusFitError(i),ParamMinusFitError(i),dbuf1,dbuf2);
	else ParamPlusFitError(i)=ParamMinusFitError(i)=0;
	j++;

      } else {
	ParamFitVal(i)=fParams[i].GetStartVal();
      }
    }

    if(fCovMatrix) {
      delete fCovMatrix;
      fCovMatrix=NULL;
    }

    if(fituncerts) {
      Int_t numfpar=fMinuit->GetNumFreePars();        //Number of floating parameters
      Double_t *covmat=new Double_t[numfpar*numfpar]; //covariance matrix array
      //Get the covariance matrix from TMinuit
      fMinuit->mnemat(covmat,numfpar);
      //Store the matrix in a TMatrixDSym object
      fCovMatrix=new TMatrixDSym(numfpar,covmat);
      //Delete the covariance matrix array
      delete[] covmat;
    }

  } catch (Int_t e) {
    fprintf(stderr,"Exception handled by QSigExFit::Fit()\n");
    throw e;
  }

  return fFCNMin;
} 

void QSigExFitMinuit::InitFit()
{
  Int_t i,j;
  Double_t dbuf;
  Int_t numfpar=0;

  if(fMinuit) delete fMinuit;
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
    if(G__p2f2funcname((void*)fCompiledFunc)) fMinuit->SetFCN((void*)fCompiledFunc);
    //else if it's passed from compiled code, call TMiuit::SetFCN(void (*)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t))
    else fMinuit->SetFCN(fCompiledFunc);  //Sets name of fit function
  }

  else if(fInterpretedFunc) { //Else if the function is interpreted by CINT
    fMinuit->SetFCN(fInterpretedFunc);  //Sets name of fit function

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

  //Loop through parameters, and initialize a variable in Minuit for each one.
  for (i=0; i<fParams.Count(); i++){

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

      j++;

      //Else if the parameter is fixed but is not required to be passed to Minuit
    } else {
      if(fQProcessor) fQProcessor->SetParam(i,fParams[i].GetStartVal());
      ParamFreeParamIndex(i)=-1;
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
  b->Add(&fMinuitStatus);
}
