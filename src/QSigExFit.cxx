#include "QSigExFit.h"

ClassImp(QSigExFit)

void QSigExFit::Init()
{
  if(fQTPL) {
    Int_t i,j,k,l;

    //For each combination of 2 different QProcessor objects
    for(i=0; i<fQTPL->GetNQProcs(); i++) {

      for(j=i+1; j<fQTPL->GetNQProcs(); j++) {

	//For each combination of parameters for the two QProcessor objects
	for(k=0; k<(*fQTPL)[i].GetNParams(); k++) {

	  for(l=0; l<(*fQTPL)[j].GetNParams(); l++) {

	    //If the parameters have the same name, throw an error
	    if(!strcmp((*fQTPL)[i].GetParamName(k),(*fQTPL)[j].GetParamName(l))) {
	      fprintf(stderr,"QSigExFit::Init(): Error: Duplicate parameter: '%s'\n",(*fQTPL)[i].GetParamName(k));
	      throw 1;
	    }
	  }
	}
      }
    }

    fParams.Clear();

    //Loop over all QProcessor objects
    for(i=0; i<fQTPL->GetNQProcs(); i++) {

      //Loop over all parameters for the current QProcessor object
      for(j=0; j<(*fQTPL)[i].GetNParams(); j++) {
	fParams.RedimList(fParams.Count()+1);
	fParams.GetLast().SetName((*fQTPL)[i].GetParamName(j));
      }
    }
  }
}

void QSigExFit::InitFit()
{
  Int_t i;
  Double_t dbuf;

  if(fMinuit) delete fMinuit;
  fMinuit=new TMinuit(fParams.Count());

  //Set parameters to use for Minuit SET PRINT command:
  Double_t level=0;                       // -1 tells minuit we want minimal printout.
  Int_t ierflg;
  //Execute the minuit SET PRINT command:
  fMinuit->mnexcm( "SET PRINT", &level, 1, ierflg );  

  //Execute the minuit SET WIDTH command: (changes the width of std output)
  Double_t width=200;
  fMinuit->mnexcm( "SET WIDTH", &width, 1, ierflg );  

  //Initialize Minuit with the type of fit function we are using.  
  //See http://root.cern.ch/root/roottalk/roottalk97/0407.html for more details on the following lines

  if(fCompiledFunc){ //If the function is compiled
    //If the function pointer is passed from CINT, call TMinuit::SetFCN(void*)
    if(G__p2f2funcname((void*)fCompiledFunc)) fMinuit->SetFCN((void*)fCompiledFunc);
    //else if it's passed from compiled code, call TMiuit::SetFCN(void (*)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t))
    else fMinuit->SetFCN(fCompiledFunc);  //Sets name of fit function

  } else if(fInterpretedFunc){ //Else if the function is interpreted by CINT
    fMinuit->SetFCN(fInterpretedFunc);  //Sets name of fit function

  }else{ //Else if no function has been set, throw an error
    fprintf(stderr,"QSigExFit::Fit(): Error: No fit function has been set\n");
    throw 1;
  }

  //set Minuit parameters to "undefined"
  fMinuit->mncler(); 

  //Execute the SET ERR command:
  fMinuit->mnexcm("SET ERR", &const_cast<Double_t&>(fFCNError.GetValue()), 1, ierflg );

  ierflg=0;

  //Loop through parameters, and initialize a variable in Minuit for each one.
  for (i=0; i<fParams.Count(); i++){

    //mnparm implements a parameter definition with a parameter number,
    //name, starting value, step size, min and max values, and an error flag.

    fMinuit->mnparm(i, fParams[i].GetName(), fParams[i].GetStartVal(), fParams[i].GetStepVal(), fParams[i].GetMinVal(), fParams[i].GetMaxVal(), ierflg);
    //If the parameter is fixed, tell to TMinuit
    if(fParams[i].IsFixed()){
      dbuf=i+1;
      fMinuit->mnexcm("FIX",&dbuf,1,ierflg);     //fix the parameter with that index
    }
  }

  gQProcList=fQTPL;
}

Int_t QSigExFit::FindMinimArg(const char* name)
{
  for(Int_t i=0; i<fMinimArgs->Count(); i++){
    if(!strcmp((*fMinimArgs)[i].GetName(),name)) return i;
  }
  return -1;
}

Double_t QSigExFit::Fit()
{
  Int_t i;
  Double_t* minimargs=new Double_t[fMinimArgs->Count()];
  Int_t ierflg=0;
  Int_t numpar=fParams.Count();
  Double_t dbuf1,dbuf2,dbuf3,dbuf4; 
  Int_t ibuf1;
  TString strbuf;
  Double_t dummyd=0;

  for(i=0; i<fMinimArgs->Count(); i++) minimargs[i]=(*fMinimArgs)[i];

  // Reinitialize floating parameters values
  for (i=0; i<numpar; i++){
    if(!fParams[i].IsFixed()) {
      fMinuit->mnparm(i, fParams[i].GetName(), dbuf4, fParams[i].GetStepVal(), fParams[i].GetMinVal(), fParams[i].GetMaxVal(), ierflg);
    }
  }

  //**** Call the minimizer ****
  fMinuit->mnexcm(fMinimName.GetValue(), minimargs, fMinimArgs->Count(), ierflg ); 

  // set fit parameters to result from this run of the minimizer
  for (i=0; i<numpar; i++){
    if(!fParams[i].IsFixed()) {
      fMinuit->mnpout(i,strbuf,dbuf4,dbuf1,dbuf2,dbuf3,ibuf1); //dbuf4 contains the fitted value
      fMinuit->mnparm(i, fParams[i].GetName(), dbuf4, fParams[i].GetStepVal(), fParams[i].GetMinVal(), fParams[i].GetMaxVal(), ierflg);
    }
  }
  // rerun minimizer
  fMinuit->mnexcm(fMinimName.GetValue(), minimargs, fMinimArgs->Count(), ierflg ); 

  delete[] minimargs;

  ierflg = 0; 

  //Print the covariance matrix
  cout << "\nParameter correlations calculated by MIGRAD:"<<"\n";
  fMinuit->mnexcm("SHOW COR",&dummyd,0,ierflg);

  //Calculate non-symmetric errors with MINOS:
  //Minuit calculates errors by finding the change in the parameter value 
  //required to change the function by fcnerror.
  //In the case of a Chi-2, 
  //     fcnerror =1.0 --> 1 sigma
  //     fcnerror =4.0 --> 2 sigma
  //     fcnerror =9.0 --> 3 sigma
  //When minosmaxcalls is positive, it sets the maximum number of function
  //calls per parameter to its values 
  dbuf1=fMinosMaxCalls.GetValue();
  fMinuit->mnexcm("MINO",&dbuf1,(Int_t)(fMinosMaxCalls.GetValue()>=0),ierflg); 

  fMinuitStatus=fMinuit->fCstatu;
  fFCNMin=fMinuit->fAmin;

  //Loop over the parameters
  for(Int_t i=0;i<numpar;i++){
    //mnpout takes in the index of the parameter we're asking about, and returns
    //it's name, fitted value, estimate of parameter uncertainty, lower limit
    //on the parameter value, upper limit on the parameter value, and the
    //internal parameter number (if the parameter is variable).  See Minuit
    //documentation for details.  We aren't actually interested in any of this
    //except the fit value.

    fMinuit->mnpout(i,strbuf,(Double_t&)fParams[i],dbuf1,dbuf2,dbuf3,ibuf1);

    //mnerrs reports the errors calculated by MINOS.  It takes in the index of 
    //the parameter we're asking about, and returns the positive error, 
    //negative error (as a negative number), the parabolic parameter error 
    //and the global correlation coefficient for the parameter.  

    fMinuit->mnerrs(i,(Double_t&)fParams[i].fPlusFitError,(Double_t&)fParams[i].fMinusFitError,dbuf1,dbuf2);
  }

  return fFCNMin;
} 

void QSigExFit::SetFCN(void (*fcn)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t))
{
  //This overloaded version of QSigExFit::SetFCN() function is called when fcn is
  //a compiled function (fcn pointer is passed in compiled code or by the CINT
  //interpreter). It sets the parameters fitter function.

  fInterpretedFunc=NULL;
  fCompiledFunc=fcn;
}
    
void QSigExFit::SetFCN(void* fcn)
{
  //This overloaded version of QSigExFit::SetFCN() function is called when fcn
  //is an interpreted function by the CINT interpreter. It sets the parameters
  //fitter function.

  fCompiledFunc=NULL;
  fInterpretedFunc=fcn;
}

void QSigExFit::Browse(TBrowser *b)
{
  b->Add(&fParams,"Fit Parameters");
  b->Add(&fFCNError);
  b->Add(&fMinimName);
  b->Add(fMinimArgs,"Minimizer Arguments");
}
