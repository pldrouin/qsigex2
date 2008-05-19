#include "QProcList.h"

ClassImp(QProcList)

const QProcList *gQProcList;

QProcList::~QProcList()
{
  delete fQPL;
  fQPL=NULL;
}

void QProcList::Analyze()
{
  for(Int_t i=0; i<fQPL->Count(); i++) {
    try {
    ((QProcessor*)(*fQPL)[i])->Analyze();

    } catch (Int_t e) {
      fprintf(stderr,"QProcList::Analyze(): Error thrown by QProcessor index %i\n",i);
      throw e;
    }
  }
}

void QProcList::Exec() const
{
  for(Int_t i=0; i<fQPL->Count(); i++) ((QProcessor*)(*fQPL)[i])->Exec();
}

void QProcList::InitProcess()
{
  for(Int_t i=0; i<fQPL->Count(); i++)  {

    try {
      ((QProcessor*)(*fQPL)[i])->InitProcess();

    } catch (Int_t e) {
      fprintf(stderr,"QProcList::InitProcess(): Error thrown by QProcessor index %i\n",i);
      throw e;
    }
  }
}

const QProcList& QProcList::operator=(const QProcList &rhs)
{
  TNamed::operator=(rhs);
  *fQPL=*rhs.fQPL;
  return *this;
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

