#include "QSigExFit.h"

ClassImp(QSigExFit)

const QSigExFit* QSigExFit::fCurInstance;

QSigExFit::~QSigExFit()
{
  if(fCovMatrix) {
    delete fCovMatrix;
    fCovMatrix=NULL;
  }
}

Int_t QSigExFit::FindFreeParamIndex(const char *paramname) const
{
  Int_t ret=FindParamIndex(paramname);
  if(ret != -1) {
    ret=fParams[ret].GetFreeParamIndex();
    if(ret == -1) {
      fprintf(stderr,"QSigExFit::FindFreeParamIndex: Error: parameter '%s' is not free\n",paramname);
      throw 1;
    }
  }

  return ret;
}

Int_t QSigExFit::FindParamIndex(const char *paramname) const
{
  Int_t ret=-1;
  for(Int_t i=0; i<fParams.Count(); i++) {

    if(!strcmp(fParams[i].GetName(),paramname)) ret=i;
  }

  if(ret == -1) {
    fprintf(stderr,"QSigExFit::FindParamIndex: Error: parameter '%s' not found\n",paramname);
    throw 1;
  }
  return ret;
}

void QSigExFit::Init()
{
  if(fQProcessor) {
    Int_t i;

    fParams.Clear();
    fParams.RedimList(fQProcessor->GetNParams());

    //Loop over all parameters for the QProcessor object
    for(i=0; i<fQProcessor->GetNParams(); i++) {

      fParams[i].SetName(fQProcessor->GetParamName(i));
      (Double_t&)fParams[i]=0.;
    }
  }
}

Int_t QSigExFit::GetNVarParams() const
{
  Int_t i,n=0;

  for(i=0; i<fParams.Count(); i++) if(!fParams.GetArray()[i].IsFixed()) n++;
  return n;
}

QSigExFitParam& QSigExFit::Param(const char* paramname) const
{
  Int_t i;
  if((i=FindParamIndex(paramname))==-1) {
    fprintf(stderr,"QSigExFit::Param: Error: Parameter '%s' does not exist\n",paramname);
    throw 1;
  }

  return Param(i);
}

void QSigExFit::PrintParams() const
{
  for(Int_t i=0; i<fParams.Count(); i++) fParams[i].Print();
}

void QSigExFit::Browse(TBrowser *b)
{
  b->Add(&fParams,"Fit Parameters");
  if(fCovMatrix) b->Add(fCovMatrix,"Covariance Matrix");
  b->Add(&fFCNMin);
}
