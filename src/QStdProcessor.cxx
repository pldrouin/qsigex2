#include "QStdProcessor.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QStdProcessor)

QStdProcessor::~QStdProcessor()
{
  delete fProcs;
  fProcs=NULL;
  delete fLastParams;
  fLastParams=NULL;
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
	fParamsChildIndices->RedimList(fParamsNames->Count());
	fChildParamsMapping->RedimList(fParamsNames->Count());
      }
      (*fParamsChildIndices)[pidx].Add(i);
      (*fChildParamsMapping)[pidx].Add(j);
    }
  }
}

const QStdProcessor& QStdProcessor::operator=(const QStdProcessor &rhs)
{
  QProcessor::operator=(rhs);
  *fProcs=*rhs.fProcs;
  *fProcsParDepends=*rhs.fProcsParDepends;
  return *this;
}

void QStdProcessor::SetParamAddress(Int_t index, Double_t *paddr)
{
  QProcessor::SetParamAddress(index,paddr);
  Int_t i;

  //Loop over the children that depend on the current parameter
  for(i=0; i<(*fParamsChildIndices)[index].Count(); i++) {
    //Set the address to the assign buffer for this parameter
    (*fProcs)[(*fParamsChildIndices)[index][i]].SetParamPtr((*fChildParamsMapping)[index][i],(*fParams)[index]);
  }
}

#include "debugger.h"
