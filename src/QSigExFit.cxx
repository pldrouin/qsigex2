#include "QSigExFit.h"

ClassImp(QSigExFit)

const QSigExFit* QSigExFit::fCurInstance;

QSigExFit::~QSigExFit()
{
  if(fCovMatrix) {
    delete fCovMatrix;
    fCovMatrix=NULL;
  }
  if(fCorMatrix) {
    delete fCorMatrix;
    fCorMatrix=NULL;
  }
}

Int_t QSigExFit::FindFreeParamIndex(const char *paramname) const
{
  Int_t ret=FindParamIndex(paramname);
  if(ret != -1) {
    ret=fParams[ret].GetFreeParamIndex();
  }
  return ret;
}

Int_t QSigExFit::FindParamIndex(const char *paramname) const
{
  Int_t ret=-1;
  for(Int_t i=0; i<fParams.Count(); i++) {

    if(!strcmp(fParams[i].GetName(),paramname)) ret=i;
  }
  return ret;
}

const TMatrixDSym& QSigExFit::GetCorMatrix()
{
  if(fCorMatrix) delete fCorMatrix;
  fCorMatrix=NULL;

  if(fCovMatrix) {
    fCorMatrix=new TMatrixDSym(fCovMatrix->GetNrows());

    Int_t j;

    for(Int_t i=fCovMatrix->GetNrows()-1; i>=0; --i) {

      for(j=fCovMatrix->GetNrows()-1; j>=i; --j) {
	(*fCorMatrix)(i,j)=(*fCorMatrix)(j,i)=(*fCovMatrix)(i,j)/TMath::Sqrt((*fCovMatrix)(i,i)*(*fCovMatrix)(j,j));
      }
    }
  }

  return *fCorMatrix;
}

void QSigExFit::Init()
{
  if(fQProcessor) {
    Int_t i;

    fParams.Clear();
    fParams.RedimList(fQProcessor->GetNParams());

    //Loop over all parameters for the QProcessor object
    for(i=0; i<fQProcessor->GetNParams(); i++) {

      fParams[i].SetNameIndex(fQProcessor->GetParamName(i),i);
      (Double_t&)fParams[i]=0.;
    }
  }
}

void QSigExFit::InitFit()
{
  for (Int_t i=0; i<fParams.Count(); ++i)
    if(!fParams[i].IsMaster())
      printf("Parameter '%s' is a slave of parameter '%s'\n",fParams[i].GetName(),fParams[fParams[i].GetMasterIndex()].GetName());
}

Int_t QSigExFit::GetNVarParams() const
{
  Int_t i,n=0;

  for(i=fParams.Count()-1; i>=0; --i) if(!fParams.GetArray()[i].IsFixed()) ++n;
  return n;
}

QSigExFitParam& QSigExFit::Param(const char* paramname) const
{
  Int_t i;
  if((i=FindParamIndex(paramname))==-1) {
    fprintf(stderr,"QSigExFit::Param: Error: Parameter '%s' does not exist\n",paramname);
    throw 1;
  }

  return Param(i);
}

void QSigExFit::PrintParams() const
{
  for(Int_t i=0; i<fParams.Count(); ++i) fParams[i].Print();
}

void QSigExFit::Browse(TBrowser *b)
{
  b->Add(&fParams,"Fit Parameters");
  if(fCorMatrix) b->Add(fCorMatrix,"Correlation Matrix");
  if(fCovMatrix) b->Add(fCovMatrix,"Covariance Matrix");
  b->Add(&fFCNMin);
}

void QSigExFit::Streamer(TBuffer &R__b)
{
  // Stream an object of class QSigExFit.

  UInt_t R__s, R__c;
  Int_t i,j;
  if (R__b.IsReading()) {
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c);

    TObject::Streamer(R__b);
    fParams.Streamer(R__b);

    for(i=fParams.Count()-1; i>=0; --i) {
      j=((long)fParams[i].fMaster)-1;
      fParams[i].fMaster=NULL;

      if(j!=-1) fParams[i].SetSlaveOf(fParams[j]);
    }

    fFCNError.Streamer(R__b);
    fFCNMin.Streamer(R__b);
    R__b >> fCovMatrix;
    R__b >> fVerbose;
    R__b.CheckByteCount(R__s, R__c, QSigExFit::IsA());

  } else {
    R__c = R__b.WriteVersion(QSigExFit::IsA(), kTRUE);
    TObject::Streamer(R__b);
    fParams.Streamer(R__b);
    fFCNError.Streamer(R__b);
    fFCNMin.Streamer(R__b);
    R__b << fCovMatrix;
    R__b << fVerbose;
    R__b.SetByteCount(R__c, kTRUE);
  }
}
