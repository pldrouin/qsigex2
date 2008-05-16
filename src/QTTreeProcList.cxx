#include "QTTreeProcList.h"

ClassImp(QTTreeProcList)

const QTTreeProcList *gQTTreeProcList;

QTTreeProcList::~QTTreeProcList()
{
  delete fQPL;
  fQPL=NULL;
}

void QTTreeProcList::Analyze()
{
  for(Int_t i=0; i<fQPL->Count(); i++) {
    try {
    ((QTTreeProcessor*)(*fQPL)[i])->Analyze();

    } catch (Int_t e) {
      fprintf(stderr,"QTTreeProcList::Analyze(): Error thrown by QTTreeProcessor index %i\n",i);
      throw e;
    }
  }
}

void QTTreeProcList::Exec() const
{
  for(Int_t i=0; i<fQPL->Count(); i++) ((QTTreeProcessor*)(*fQPL)[i])->Exec();
}

void QTTreeProcList::InitProcess()
{
  for(Int_t i=0; i<fQPL->Count(); i++)  {

    try {
      ((QTTreeProcessor*)(*fQPL)[i])->InitProcess();

    } catch (Int_t e) {
      fprintf(stderr,"QTTreeProcList::InitProcess(): Error thrown by QTTreeProcessor index %i\n",i);
      throw e;
    }
  }
}

const QTTreeProcList& QTTreeProcList::operator=(const QTTreeProcList &rhs)
{
  TNamed::operator=(rhs);
  *fQPL=*rhs.fQPL;
  return *this;
}

void QTTreeProcList::TerminateProcess()
{
  for(Int_t i=0; i<fQPL->Count(); i++) ((QTTreeProcessor*)(*fQPL)[i])->TerminateProcess();
}

void QTTreeProcList::Browse(TBrowser *b)
{
  Int_t i;
  for(i=0; i<fQPL->Count(); i++) b->Add((*fQPL)[i],"TTree Processors");
}

