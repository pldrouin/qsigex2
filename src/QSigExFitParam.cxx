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
  delete fPlusFitError;
  fPlusFitError=NULL;
  delete fMinusFitError;
  fMinusFitError=NULL;
}

void QSigExFitParam::Print(const Option_t*) const
{
  printf("\nParameter '%s'\n",GetName());
  printf("\tFitted value:       %f\n",(const Double_t&)(*this));
  printf("\tMinus fit error:    %f\n",GetMinusFitError());
  printf("\tPlus fit error:     %f\n",GetPlusFitError());
  printf("\tStart value:        %f\n",GetStartVal());
  printf("\tMinimum value:      %f\n",GetMinVal());
  printf("\tMaximum value:      %f\n",GetMaxVal());
  printf("\tInitial step value: %f\n",GetStepVal());
  printf("\tFixed parameter:    %s\n",(IsFixed() ? "yes":"no"));
}

void QSigExFitParam::Browse(TBrowser *b)
{
  b->Add(fStartVal);
  b->Add(fMinVal);
  b->Add(fMaxVal);
  b->Add(fStepVal);
  b->Add(fFixed);
}
