#include "QSigExFitParam.h"

ClassImp(QSigExFitParam)

QSigExFitParam::~QSigExFitParam()
{
  delete fStartVal;
  fStartVal=NULL;
  delete fMinVal;
  fMinVal=NULL;
  delete fMaxVal;
  fMaxVal=NULL;
  delete fStepVal;
  fStepVal=NULL;
  delete fFixed;
  fFixed=NULL;
}

void QSigExFitParam::Browse(TBrowser *b)
{
  b->Add(fStartVal);
  b->Add(fMinVal);
  b->Add(fMaxVal);
  b->Add(fStepVal);
  b->Add(fFixed);
}
