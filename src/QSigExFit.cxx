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

void QSigExFit::InitFit()
{
  Int_t i,j;

  for (i=0; i<fParams.Count(); i++){
    ParamMasterIndex(i)=-1;

    if(fQProcessor) {

      for (j=0; j<i; j++) {

	if(&fQProcessor->GetParam(i)==&fQProcessor->GetParam(j)) {

	  if(fParams[i].IsFixed()!=fParams[j].IsFixed() || fParams[i].GetMaxVal()!=fParams[j].GetMaxVal() || fParams[i].GetMinVal()!=fParams[j].GetMinVal() || fParams[i].GetStartVal()!=fParams[j].GetStartVal() || fParams[i].GetStepVal()!=fParams[j].GetStepVal()) {
	    fprintf(stderr,"QSigExFit::InitFit: Error: Slave parameter '%s' does not have the same configuration than master parameter '%s'\n",fParams[i].GetName(),fParams[j].GetName());
	    throw 1;
	  }
	  ParamMasterIndex(i)=j;
	  printf("Parameter '%s' is a slave of parameter '%s'\n",fParams[i].GetName(),fParams[j].GetName());
	  break;
	}
      }
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
