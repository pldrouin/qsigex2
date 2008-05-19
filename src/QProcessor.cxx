#include "QProcessor.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QProcessor)

QProcessor::~QProcessor()
{
  delete fParams;
  fParams=NULL;
  delete fParamsNames;
  fParamsNames=NULL;
}

void QProcessor::AddParam(const char *parname, const Double_t &value, Int_t index)
{
  if(fParamsNames->FindFirst(parname) != -1) {
    fprintf(stderr,"QProcessor::AddParam: Error: Parameter '%s' already exists\n",parname);
    throw 1;
  }
  fParamsNames->Add(parname,index);
  fParams->Add(value,index);
}

void QProcessor::DelParam(const char *paramname)
{
  Int_t i;
  if((i=FindParamIndex(paramname))!=-1) DelParam(i);
}

Int_t QProcessor::FindParamIndex(const char *paramname) const
{
  Int_t ret=(*fParamsNames).FindFirst(paramname);

  if(ret == -1) fprintf(stderr,"QProcessor::FindParamIndex: Error: parameter '%s' not found\n",paramname);
  return ret;
}

const QProcessor& QProcessor::operator=(const QProcessor &rhs)
{
  *fParams=*rhs.fParams;
  *fParamsNames=*rhs.fParamsNames;
  return *this;
}

void QProcessor::SetParam(const char *paramname, const Double_t &value)
{
  Int_t i;
  if((i=FindParamIndex(paramname))!=-1) SetParam(i,value);
  else {
    fprintf(stderr,"QProcessor::SetParam: Error: Parameter '%s' does not exist\n",paramname);
    throw 1;
  }
}

void QProcessor::SetParams(Double_t *params)
{
  memcpy(fParams->GetArray(),params,fParams->Count()*sizeof(Double_t));
}

#include "debugger.h"
