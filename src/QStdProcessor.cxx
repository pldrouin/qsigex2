#include "QStdProcessor.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QStdProcessor)

QStdProcessor::~QStdProcessor()
{
  delete fProcs;
  fProcs=NULL;

  if(fAParams!=fParams) {
      delete fAParams;
      fAParams=NULL;
  }

  if(fActiveParams) {
      delete fActiveParams;
      fActiveParams=NULL;
  }

  if(fLastActiveParams) {
      delete fLastActiveParams;
      fLastActiveParams=NULL;
  }

  if(fAParIndexMapping) {
      delete fAParIndexMapping;
      fAParIndexMapping=NULL;
  }
  delete fLastParams;
  fLastParams=NULL;

  if(fProcsAParDepends!=fProcsParDepends) {
      delete fProcsAParDepends;
      fProcsAParDepends=NULL;
  }
  delete fProcsParDepends;
  fProcsParDepends=NULL;
}

void QStdProcessor::Analyze()
{
  Int_t i,j;
  QNamedProc *proc;
  Int_t nprocs=fProcs->Count();
  Int_t nparams;
  Int_t pidx;

  fParamsNames->Clear();
  fParamsChildIndices->Clear();
  fChildParamsMapping->Clear();

  for(i=0; i<nprocs; i++) {
    proc=&((*fProcs)[i]);
    nparams=proc->GetNParams();

    //Loop over the parameters for the current process
    for(j=0; j<nparams; j++) {
      pidx=fParamsNames->AddUnique(proc->GetParam(j).GetName());

      if(pidx==-1) {
	pidx=fParamsChildIndices->RedimList(fParamsNames->Count());
	fChildParamsMapping->RedimList(fParamsNames->Count());
      }
      (*fParamsChildIndices)[pidx].Add(i);
      (*fChildParamsMapping)[pidx].Add(j);
    }
  }
}

void QStdProcessor::ClearParams()
{
  QProcessor::ClearParams();

  if(fAParams!=fParams) {
      delete fAParams;
      fAParams=fParams;
  }

  if(fActiveParams) {
      delete fActiveParams;
      fActiveParams=NULL;
  }

  if(fLastActiveParams) {
      delete fLastActiveParams;
      fLastActiveParams=NULL;
  }

  if(fAParIndexMapping) {
      delete fAParIndexMapping;
      fAParIndexMapping=NULL;
  }

  if(fProcsAParDepends!=fProcsParDepends) {
      delete fProcsAParDepends;
      fProcsAParDepends=fProcsParDepends;
  } 
}

Int_t QStdProcessor::FindProcIndex(const char *procname) const
{
  for(Int_t i=0; i<fProcs->Count(); i++){
    if(!strcmp((*fProcs)[i].GetName(),procname)) return i;
  }
  return -1;
}

QNamedProc& QStdProcessor::GetProc(const char *procname) const
{
  Int_t i;
  if((i=FindProcIndex(procname))!=-1){
    return GetProc(i);
  }
  fprintf(stderr,"QStdProcessor::GetProc: Procedure '%s' does not exist\n",procname);
  throw 1;
  return GetProc(0);
}

const QStdProcessor& QStdProcessor::operator=(const QStdProcessor &rhs)
{
  QProcessor::operator=(rhs);
  *fProcs=*rhs.fProcs;

  if(rhs.fAParams==rhs.fParams) {

      if(fAParams!=fParams) {
	  delete fAParams;
	  fAParams=fParams;
      }

  } else {

      if(fAParams==fParams) fAParams=new QList<Double_t*>(*rhs.fAParams);
      else *fAParams=*rhs.fAParams;
  }

  if(rhs.fActiveParams) {

      if(fActiveParams) *fActiveParams=*rhs.fActiveParams;
      else fActiveParams=new QMask(*rhs.fActiveParams);

  } else {
      if(fActiveParams) {
	  delete fActiveParams;
	  fActiveParams=NULL;
      }
  }

  if(rhs.fLastActiveParams) {

      if(fLastActiveParams) *fLastActiveParams=*rhs.fLastActiveParams;
      else fLastActiveParams=new QMask(*rhs.fLastActiveParams);

  } else {
      if(fLastActiveParams) {
	  delete fLastActiveParams;
	  fLastActiveParams=NULL;
      }
  }

  if(!rhs.fAParIndexMapping) {

      if(fAParIndexMapping) {
	  delete fAParIndexMapping;
	  fAParIndexMapping=NULL;
      }

  } else {

      if(!fAParIndexMapping) fAParIndexMapping=new QList<Int_t>(*rhs.fAParIndexMapping);
      else *fAParIndexMapping=*rhs.fAParIndexMapping;
  }
  *fProcsParDepends=*rhs.fProcsParDepends;

  if(rhs.fProcsAParDepends==rhs.fProcsParDepends) {

      if(fProcsAParDepends!=fProcsParDepends) {
	  delete fProcsAParDepends;
	  fProcsAParDepends=fProcsParDepends;
      }

  } else {

      if(fProcsAParDepends==fProcsParDepends) fProcsAParDepends=new QList<QMask>(*rhs.fProcsAParDepends);
      else *fProcsAParDepends=*rhs.fProcsAParDepends;
  }

  return *this;
}

void QStdProcessor::SetParamActive(const Int_t &index, const Bool_t &active)
{
    if(index<0 || index>=fParams->Count()) {
	fprintf(stderr,"QStdProcessor::SetParamActive: Error: Illegal index value (%i). Make sure that InitProcess has been called\n",index);
	throw 1;
    }

    if(!fActiveParams) {

      if(!active) {
	fActiveParams=new QMask;
	fActiveParams->FillMask(fParams->Count());
        fActiveParams->SetBit(index,kFALSE);
      }
    } else fActiveParams->SetBit(index,active);
}

void QStdProcessor::SetParamAddress(const Int_t &index, Double_t* const paddr)
{
  QProcessor::SetParamAddress(index,paddr);
  Int_t i;

  if(fAParIndexMapping && (*fAParIndexMapping)[index]>=0) (*fAParams)[(*fAParIndexMapping)[index]]=(*fParams)[index];

  //Loop over the children that depend on the current parameter
  for(i=0; i<(*fParamsChildIndices)[index].Count(); i++) {
    //Set the address to the assign buffer for this parameter
    (*fProcs)[(*fParamsChildIndices)[index][i]].SetParamPtr((*fChildParamsMapping)[index][i],(*fParams)[index]);
  }
}

#include "debugger.h"
