#include "QSigExFitMCMC.h"

ClassImp(QSigExFitMCMC)

QSigExFitMCMC::~QSigExFitMCMC()
{
  TerminateFit();
  if(fParVals) {
    delete[] fParVals;
    delete[] fParJumps;
  }
  delete fParOANames;
  delete fParOAIndices;
  delete fParOArrays;
}

void QSigExFitMCMC::AddParOutput(const Int_t index, const char *name, const char *title)
{
  QNamedVar<TString> qnvbuf(name,title);
  Int_t idx=fParOAIndices->AddUnique(index);

  if(idx==-1) fParOANames->Add(qnvbuf);
  else (*fParOANames)[idx]=qnvbuf;
}

void QSigExFitMCMC::AddParOutput(const char *paramname, const char *name, const char *title)
{
  Int_t i;
  if((i=FindParamIndex(paramname))==-1) {
    fprintf(stderr,"QSigExFitMCMC::AddParOutput: Error: Parameter '%s' does not exist\n",paramname);
    throw 1;
  }
  AddParOutput(i,name,title);
}

void QSigExFitMCMC::DelParOutput(const Int_t index)
{
  Int_t idx=fParOAIndices->FindFirst(index);

  if(idx!=-1) {
    fParOAIndices->Del(idx);
    fParOANames->Del(idx);

  } else {
    fprintf(stderr,"Error: QSigExFitMCMC::DelParOutput(): There is no parameter output entry for parameter index %i\n",index);
    throw 1;
  }
}

void QSigExFitMCMC::DelParOutput(const char *paramname)
{
  Int_t i;
  if((i=FindParamIndex(paramname))==-1) {
    fprintf(stderr,"QSigExFitMCMC::DelParOutput: Error: Parameter '%s' does not exist\n",paramname);
    throw 1;
  }
  DelParOutput(i);
}

Double_t QSigExFitMCMC::EvalFCN() const
{

  if(fCompiledFunc) {
    Double_t prob;
    Int_t numpar=fParams.Count();
    fCurInstance=this;
    fCompiledFunc(numpar,NULL,prob,fParVals,0);
    return prob;

  } else if(fQProcessor && fQPOutputs.Count() && dynamic_cast<QProcDouble*>(fQPOutputs[0])) {
    Double_t &prob=*dynamic_cast<QProcDouble*>(fQPOutputs[0]);
    fQProcessor->Exec();
    return prob;

  } else {
      fprintf(stderr,"Error: QSigExFitMCMC::EvalFCN(): Neither a C function nor a QProcessor with a QProcDouble output has been specified\n");
      throw 1;
  }
  return 0;
}

Double_t QSigExFitMCMC::Fit(Bool_t fituncerts)
{
  Int_t i,j;
  Int_t numpar=0;

  // Reinitialize floating parameters values
  for (i=0; i<fParams.Count(); i++){

    if(fParams[i].IsMaster()) {

      //If the parameter is not hided to Minuit
      if(!fParams[i].IsFixed()) {

	fParVals[numpar]=fParams[i].GetStartVal();
	fParJumps[numpar]=fParams[i].GetStepVal();
	numpar++;

	//Else if the parameter is hided
      } else {
	if(fQProcessor) fQProcessor->SetParam(i,fParams[i].GetStartVal());
      }
    }
  }

  Double_t *oldpars=new Double_t[numpar];
  TRandom3 rnd;

  for(j=0; j<fParOArrays->Count(); j++) (*fParOArrays)[j]->InitProcObj();

  if(fCompiledFunc) {
    Double_t prob,newprob;
    fCurInstance=this;
    fCompiledFunc(numpar,NULL,prob,fParVals,0);

    //Burn-In here
    for(i=0; i<fNBurnInIterations+fNParsingIterations; i++) {
      memcpy(oldpars,fParVals,numpar*sizeof(Double_t));

      for(j=0; j<numpar; j++) fParVals[j]=rnd.Gaus(fParVals[j],fParJumps[j]);
      fCompiledFunc(numpar,NULL,newprob,fParVals,0);

      if(newprob<prob && rnd.Rndm()*prob>newprob) {
	memcpy(fParVals,oldpars,numpar*sizeof(Double_t));

      } else {
	prob=newprob;
      }

      if(i>=fNBurnInIterations) {

	for(j=0; j<fParOArrays->Count(); j++) (*fParOArrays)[j]->Fill();
      }
    }

  } else if(fQProcessor && fQPOutputs.Count() && dynamic_cast<QProcDouble*>(fQPOutputs[0])) {
    Double_t &prob=*dynamic_cast<QProcDouble*>(fQPOutputs[0]);
    Double_t oldprob;
    fQProcessor->Exec();

    //Burn-In here
    for(i=0; i<fNBurnInIterations+fNParsingIterations; i++) {
      memcpy(oldpars,fParVals,numpar*sizeof(Double_t));
      oldprob=prob;

      for(j=0; j<numpar; j++) fParVals[j]=rnd.Gaus(fParVals[j],fParJumps[j]);
      fQProcessor->Exec();

      if(prob<oldprob && rnd.Rndm()*oldprob>prob) {
	memcpy(fParVals,oldpars,numpar*sizeof(Double_t));
	prob=oldprob;
      }

      if(i>=fNBurnInIterations) {

	for(j=0; j<fParOArrays->Count(); j++) (*fParOArrays)[j]->Fill();
      }
    }

  } else {
      fprintf(stderr,"Error: QSigExFitMCMC::Fit(): Neither a C function nor a QProcessor with a QProcDouble output has been specified\n");
      throw 1;
  }

  for(j=0; j<fParOArrays->Count(); j++) (*fParOArrays)[j]->TerminateProcObj();

  delete[] oldpars;

  return fFCNMin;
} 

void QSigExFitMCMC::InitFit()
{
  TerminateFit();
  TDirectory *curdir=gDirectory;
  TDirectory *dbuf;
  Int_t i,j;
  TString proto;

  fParOArrays->Clear();

  fParVals=new Double_t[GetNVarParams()];
  fParJumps=new Double_t[GetNVarParams()];

  QSigExFit::InitFit();

  j=0;

  //Loop through the master parameters, and initialize parameter value for each one
  for (i=0; i<fParams.Count(); i++){

    if(fParams[i].IsMaster()) {
      //printf("Param '%s' owns its value\n",fParams[i].GetName());

      //If parameter is variable
      if(!fParams[i].IsFixed()) {
	fParVals[j]=fParams[i].GetStartVal();
	fParJumps[i]=fParams[i].GetStepVal();

	if(fQProcessor) fQProcessor->SetParamAddress(i,fParVals+j);
	j++;

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

  //Loop over the parameter output arrays
  for(i=0; i<fParOANames->Count(); i++) {
    dbuf=curdir;
    dbuf->cd();
    j=(*fParOANames)[i].GetValue().Index("://");

    if(j==-1) {
      fprintf(stderr,"QSigExFitMCMC::InitFit(): Error: Array type is not specified\n");
      throw 1;
    }
    proto=(*fParOANames)[i].GetValue()(0,j);

    if(!strcmp(proto,"tree")) {
      //Create the output array and store the pointer
      (*fParOArrays).Add(QProcBranchHandler::LoadBranch((TString)(*fParOANames)[i].GetValue()(j+3,(*fParOANames)[i].GetValue().Length()-j-3),(*fParOANames)[i].GetName(), kTRUE));

    } else if(!strcmp(proto,"qoa")) {
      //Create the output array and store the pointer
      (*fParOArrays).Add(QProcQOAHandler::LoadQOA((TString)(*fParOANames)[i].GetValue()(j+3,(*fParOANames)[i].GetValue().Length()-j-3),(*fParOANames)[i].GetName(), kTRUE));

    } else {
      fprintf(stderr,"QSigExFitMCMC::InitFit(): Error: Array type '%s' is unknown\n",proto.Data());
      throw 1;
    }
    (*fParOArrays)[i]->SetBuffer(fParVals+(*fParOAIndices)[i]);
  }
  curdir->cd();
}

const QSigExFitMCMC& QSigExFitMCMC::operator=(const QSigExFitMCMC &rhs)
{
  QSigExFit::operator=(rhs);
  fNBurnInIterations=rhs.fNBurnInIterations;
  fNParsingIterations=rhs.fNParsingIterations;
  *fParOANames=*rhs.fParOANames;
  *fParOAIndices=*rhs.fParOAIndices;
  *fParOArrays=*rhs.fParOArrays;
  return *this;
}

void QSigExFitMCMC::SetFCN(void (*fcn)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t))
{
  //This overloaded version of QSigExFitMCMC::SetFCN() function is called when fcn is
  //a compiled function (fcn pointer is passed in compiled code or by the CINT
  //interpreter).
  //
  //The function should correspond to a probability density function.
  //Note that it is optional to use a C function to use QSigExFitMCMC. If this function
  //is not set, the first output from the QProcessor is used instead.

  fCompiledFunc=fcn;
}

void QSigExFitMCMC::TerminateFit()
{
  Int_t i;

  for(i=0; i<fParOArrays->Count(); i++) (*fParOArrays)[i]->UnloadArray();
}

void QSigExFitMCMC::Browse(TBrowser *b)
{
  QSigExFit::Browse(b);
  b->Add(&fFCNError);
  b->Add(&fNBurnInIterations);
}
