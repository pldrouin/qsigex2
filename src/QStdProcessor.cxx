#include "QStdProcessor.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QStdProcessor)

QStdProcessor::~QStdProcessor()
{
  delete fParams;
  fParams=NULL;
  delete fParamsNames;
  fParamsNames=NULL;
}

void QStdProcessor::AddParam(const char *parname, const Double_t &value, Int_t index)
{
  if(fParamsNames->FindFirst(parname) != -1) {
    fprintf(stderr,"QStdProcessor::AddParam: Error: Parameter '%s' already exists\n",parname);
    throw 1;
  }
  fParamsNames->Add(parname,index);
  fParams->Add(value,index);
}

void QStdProcessor::DelParam(const char *paramname)
{
  Int_t i;
  if((i=FindParamIndex(paramname))!=-1) DelParam(i);
}

Int_t QStdProcessor::FindParamIndex(const char *paramname) const
{
  Int_t ret=(*fParamsNames).FindFirst(paramname);

  if(ret == -1) {
    fprintf(stderr,"QStdProcessor::FindParamIndex: Error: parameter '%s' not found\n",paramname);
    throw 1;
  }
  return ret;
}

const QStdProcessor& QStdProcessor::operator=(const QStdProcessor &rhs)
{
  QProcessor::operator=(rhs);
  *fParams=*rhs.fParams;
  *fParamsNames=*rhs.fParamsNames;
  return *this;
}

void QStdProcessor::SetParam(const char *paramname, const Double_t &value)
{
  Int_t i;
  if((i=FindParamIndex(paramname))!=-1) SetParam(i,value);
  else {
    fprintf(stderr,"QStdProcessor::SetParam: Error: Parameter '%s' does not exist\n",paramname);
    throw 1;
  }
}

void QStdProcessor::SetParams(Double_t *params)
{
  memcpy(fParams->GetArray(),params,fParams->Count()*sizeof(Double_t));
}

#include "debugger.h"
