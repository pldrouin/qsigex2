#include "QProcList.h"

//#define DEBUG

ClassImp(QProcList)

const QProcList *gQProcList;

QProcList::~QProcList()
{
  delete fQPL;
  fQPL=NULL;
}

void QProcList::Analyze()
{
  Int_t i,j;
  Int_t pidx;

  fParamsNames->Clear();
  fParamsChildIndices->Clear();
  fChildParamsMapping->Clear();

  for(i=0; i<fQPL->Count(); i++) {
    try {
    ((QProcessor*)(*fQPL)[i])->Analyze();

    } catch (Int_t e) {
      fprintf(stderr,"QProcList::Analyze(): Error thrown by QProcessor index %i\n",i);
      throw e;
    }

    //Loop over all parameters for the current QProcessor object
    for(j=0; j<((QProcessor*)(*fQPL)[i])->GetNParams(); j++) {
      pidx=fParamsNames->AddUnique(((QProcessor*)(*fQPL)[i])->GetParamName(j));

      if(pidx==-1) {
	fParamsChildIndices->RedimList(fParamsNames->Count());
	fChildParamsMapping->RedimList(fParamsNames->Count());
      }
      (*fParamsChildIndices)[pidx].Add(i);
      (*fChildParamsMapping)[pidx].Add(j);
    }
  }
}

void QProcList::Exec() const
{
  if(GetVerbosity()&QProcessor::kShowExec2) printf("QProcList('%s')::Exec()\n",GetName());

  for(Int_t i=0; i<fQPL->Count(); i++) ((QProcessor*)(*fQPL)[i])->Exec();
}

void QProcList::InitProcess(Bool_t allocateparammem)
{
  Int_t i;

  for(i=0; i<fQPL->Count(); i++)  {

    try {
      ((QProcessor*)(*fQPL)[i])->InitProcess(kFALSE);

    } catch (Int_t e) {
      fprintf(stderr,"QProcList::InitProcess(): Error thrown by QProcessor index %i\n",i);
      throw e;
    }
  }
  QProcessor::InitProcess(allocateparammem);
}

void QProcList::PrintAnalysisResults() const
{
  Int_t i;

  printf("\n\n********************\nProcessor List '%s'\n********************\n",GetName());

  printf("\nList of parameters for processor list\n");
  for(i=0; i<fParamsNames->Count(); i++) {
    printf("%3i:\t%s\n",i,(*fParamsNames)[i].Data());
  }

  for(i=0; i<fQPL->Count(); i++)  {

    try {
      printf("\n====================\nProcessor '%s'\n====================\n",((QProcessor*)(*fQPL)[i])->GetName());
      ((QProcessor*)(*fQPL)[i])->PrintAnalysisResults();

    } catch (Int_t e) {
      fprintf(stderr,"QProcList::PrintAnalysisResults(): Error thrown by QProcessor index %i\n",i);
      throw e;
    }
  }
}

void QProcList::PrintProcesses(UInt_t level) const
{
  Int_t i;

  for(i=0; i<fQPL->Count(); i++) {
    printf("%*s%03i QProcessor '%s'\n",level*3,"",i,((QProcessor*)(*fQPL)[i])->GetName());
    ((QProcessor*)(*fQPL)[i])->PrintProcesses(level+1);
  }
}

const QProcList& QProcList::operator=(const QProcList &rhs)
{
  QProcessor::operator=(rhs);
  *fQPL=*rhs.fQPL;
  return *this;
}

void QProcList::SetParamAddress(Int_t index, Double_t *paddr)
{
  QProcessor::SetParamAddress(index,paddr);
  Int_t i;

  //Loop over the children that depend on the current parameter
  for(i=0; i<(*fParamsChildIndices)[index].Count(); i++) {
    //Set the address to the assign buffer for this parameter
    ((QProcessor*)(*fQPL)[(*fParamsChildIndices)[index][i]])->SetParamAddress((*fChildParamsMapping)[index][i],(*fParams)[index]);
  }
}

void QProcList::TerminateProcess()
{
  for(Int_t i=0; i<fQPL->Count(); i++) ((QProcessor*)(*fQPL)[i])->TerminateProcess();
}

void QProcList::Browse(TBrowser *b)
{
  Int_t i;
  for(i=0; i<fQPL->Count(); i++) b->Add((*fQPL)[i],"TTree Processors");
}

