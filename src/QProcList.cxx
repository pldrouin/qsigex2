#include "QProcList.h"

ClassImp(QProcList)

const QProcList *gQProcList;

QProcList::~QProcList()
{
  delete fQPL;
  fQPL=NULL;
  delete fParamsNames;
  fParamsNames=NULL;
  delete fParamsMapping;
  fParamsMapping=NULL;
}

void QProcList::Analyze()
{
  Int_t i,j,k,l;

  fParamsNames->Clear();
  fParamsMapping->RedimList(fQPL->Count());

  for(i=0; i<fQPL->Count(); i++) {
    try {
    ((QProcessor*)(*fQPL)[i])->Analyze();

    } catch (Int_t e) {
      fprintf(stderr,"QProcList::Analyze(): Error thrown by QProcessor index %i\n",i);
      throw e;
    }
    (*fParamsMapping)[i].RedimList(((QProcessor*)(*fQPL)[i])->GetNParams());

    //Loop over all parameters for the current QProcessor object
    for(j=0; j<((QProcessor*)(*fQPL)[i])->GetNParams(); j++) {

      //Loop over all known parameters
      l=-1;
      for(k=0; k<fParamsNames->Count(); k++) {

	if(!strcmp((*fParamsNames)[k],((QProcessor*)(*fQPL)[i])->GetParamName(j))) l=k;
      }

      if(l == -1) {
	l=fParamsNames->Count();
	fParamsNames->RedimList(l+1);
	fParamsNames->GetLast()=((QProcessor*)(*fQPL)[i])->GetParamName(j);
      }
      (*fParamsMapping)[i][j]=l;
    }
  }
}

void QProcList::Exec() const
{
  for(Int_t i=0; i<fQPL->Count(); i++) ((QProcessor*)(*fQPL)[i])->Exec();
}

const char* QProcList::GetParamName(Int_t index) const
{
  Int_t i=0;

  while(i<fQPL->Count() && index >= ((QProcessor*)(*fQPL)[i])->GetNParams()) {
    i++;
    index-=((QProcessor*)(*fQPL)[i])->GetNParams();
  }

  if(i < fQPL->Count()) return ((QProcessor*)(*fQPL)[i])->GetParamName(index);
  return NULL;
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

void QProcList::PrintAnalysisResults() const
{
  for(Int_t i=0; i<fQPL->Count(); i++)  {

    try {
      printf("====================\nProcessor '%s'\n====================\n\n",((QProcessor*)(*fQPL)[i])->GetName());
      ((QProcessor*)(*fQPL)[i])->PrintAnalysisResults();

    } catch (Int_t e) {
      fprintf(stderr,"QProcList::PrintAnalysisResults(): Error thrown by QProcessor index %i\n",i);
      throw e;
    }
  }
}

void QProcList::SetParam(Int_t index, const Double_t &value)
{
  Int_t i=0;

  while(i<fQPL->Count() && index >= ((QProcessor*)(*fQPL)[i])->GetNParams()) {
    i++;
    index-=((QProcessor*)(*fQPL)[i])->GetNParams();
  }

  if(i < fQPL->Count())((QProcessor*)(*fQPL)[i])->SetParam(index,value);
  else {
    fprintf(stderr,"QProcList::SetParam: Error: Parameter index %i is invalid\n",index);
    throw 1;
  }
}

void QProcList::SetParam(const char *paramname, const Double_t &value)
{
  Int_t i,j;
  QProcessor *qp;
  Bool_t paramfound=kFALSE;

  for(i=0; i<fQPL->Count(); i++) {
    qp=(QProcessor*)(*fQPL)[i];

    for(j=0; j<qp->GetNParams(); j++) {

      if(!strcmp(qp->GetParamName(j),paramname)) {
	qp->SetParam(j,value);
	paramfound=kTRUE;
      }
    }
  }

  if(!paramfound) {
    fprintf(stderr,"QProcList::SetParam: Error: Parameter '%s' does not exist\n",paramname);
    throw 1;
  }
}

void QProcList::SetParams(Double_t *params)
{
  Int_t i,j;
  QProcessor *qp;

  for(i=0; i<fQPL->Count(); i++) {
    qp=(QProcessor*)(*fQPL)[i];

    for(j=0; j<qp->GetNParams(); j++) qp->SetParam(j,params[(*fParamsMapping)[i][j]]);
  }
}

const QProcList& QProcList::operator=(const QProcList &rhs)
{
  QProcessor::operator=(rhs);
  *fQPL=*rhs.fQPL;
  *fParamsNames=*rhs.fParamsNames;
  *fParamsMapping=*rhs.fParamsMapping;
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
