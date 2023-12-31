// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#include "QProcessor.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QProcessor)

UInt_t QProcessor::fDefVerbosity=0;

QProcessor::QProcessor(const QProcessor &rhs): TNamed(rhs), fParams(new QList<Double_t*>(*rhs.fParams)),  fOwnsParams(new QList<Bool_t>), fParamsNames(new QList<TString>(*rhs.fParamsNames)), fParamsChildIndices(new QList<QList<Int_t > >(*rhs.fParamsChildIndices)), fChildParamsMapping(new QList<QList<Int_t> >(*rhs.fChildParamsMapping)), fForceExecAll(rhs.fForceExecAll), fVerbosity(rhs.fVerbosity), fPProcessing(rhs.fPProcessing)
{
  fOwnsParams->RedimList(rhs.fParams->Count(),-1,kFALSE);
}

QProcessor::~QProcessor()
{
  ClearParams();

  delete fParams;
  fParams=NULL;
  delete fOwnsParams;
  fOwnsParams=NULL;

  delete fParamsNames;
  fParamsNames=NULL;
  delete fParamsChildIndices;
  fParamsChildIndices=NULL;
  delete fChildParamsMapping;
  fChildParamsMapping=NULL;
}

void QProcessor::ClearParams()
{
  for(Int_t i=0; i<fParams->Count(); i++) {

    if((*fOwnsParams)[i]) {
      delete (*fParams)[i];
    }
  }
  fParams->Clear();
  fOwnsParams->Clear();
}

void QProcessor::CopyParamAddress(const Int_t &fromidx, const Int_t &toidx)
{
  SetParamAddress(toidx,(*fParams)[fromidx]);
}

const Double_t& QProcessor::GetParam(const char *paramname) const
{
  Int_t i;

  if((i=FindParamIndex(paramname))==-1) {
    fprintf(stderr,"QProcessor::GetParam: Error: Parameter '%s' does not exist\n",paramname);
    throw 1;
  }
  return GetParam(i);
}

void QProcessor::InitProcess(Bool_t allocateparammem)
{
  ClearParams();

  fOwnsParams->RedimList(fParamsNames->Count(),-1,kFALSE);
  fParams->RedimList(fParamsNames->Count(),-1,NULL);

  if(allocateparammem) for(Int_t i=0; i<fParamsNames->Count(); i++) SetParamAddress(i);
}

const QProcessor& QProcessor::operator=(const QProcessor &rhs)
{
  Int_t i;
  TNamed::operator=(rhs);

  for(i=0; i<fParams->Count(); i++) {

    if((*fOwnsParams)[i]) delete (*fParams)[i];
  }
  *fParams=*rhs.fParams;
  fOwnsParams->Clear();
  fOwnsParams->RedimList(rhs.fParams->Count(),-1,kFALSE);
  *fParamsNames=*rhs.fParamsNames;
  *fParamsChildIndices=*rhs.fParamsChildIndices;
  *fChildParamsMapping=*rhs.fChildParamsMapping;
  return *this;
}

void QProcessor::SetParam(const Int_t &index, const Double_t &value)
{
  if((*fOwnsParams)[index]) {
    *(*fParams)[index]=value;

  } else {
    fprintf(stderr,"QProcessor::SetParam: Error: Not allowed to set the value of parameter '%s' from processor '%s' since this processor does not own this parameter\n",(*fParamsNames)[index].Data(),GetName());
    throw 1;
  }
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

void QProcessor::SetParamActive(const char *paramname, const Bool_t &active)
{
  Int_t i;
  if((i=FindParamIndex(paramname))!=-1) SetParamActive(i,active);
  else {
    fprintf(stderr,"QProcessor::SetParam: Error: Parameter '%s' does not exist\n",paramname);
    throw 1;
  }
}

void QProcessor::SetParamAddress(const Int_t &index, Double_t* const paddr)
{
  if(paddr && (*fOwnsParams)[index]) {
    delete (*fParams)[index];
    (*fOwnsParams)[index]=kFALSE;
  }

  if(paddr) {
    (*fParams)[index]=paddr;

  } else if(!(*fOwnsParams)[index]) {
    (*fOwnsParams)[index]=kTRUE;
    (*fParams)[index]=new Double_t;
    *((*fParams)[index])=0;
  }
}

void QProcessor::SetParamAddress(const char *paramname, Double_t* const paddr)
{
  Int_t i;
  if((i=FindParamIndex(paramname))!=-1) SetParamAddress(i,paddr);
  else {
    fprintf(stderr,"QProcessor::SetParam: Error: Parameter '%s' does not exist\n",paramname);
    throw 1;
  }
}

void QProcessor::SetParams(Double_t const* const params)
{
  for(Int_t i=0; i<fParams->Count(); i++) SetParam(i,params[i]);
}

#include "debugger.h"
