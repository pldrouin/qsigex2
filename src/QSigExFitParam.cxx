#include "QSigExFitParam.h"
#include "QList.h"

ClassImp(QSigExFitParam)

QSigExFitParam::QSigExFitParam(const QSigExFitParam& rhs): TObject(rhs), fName(rhs.fName), fIndex(rhs.fIndex), fData(NULL), fMaster(rhs.fMaster), fSlaves(rhs.fSlaves?new QList<void*>(*rhs.fSlaves):NULL) {if(!fMaster) fData=new QSFPData(this,*rhs.fData); else fData=rhs.fData;}

QSigExFitParam::~QSigExFitParam()
{
  if(!fMaster) delete fData;
  else fMaster=NULL;
  fData=NULL;

  if(fSlaves) {
    delete fSlaves;
    fSlaves=NULL;
  }
}

void QSigExFitParam::Print(const Option_t*) const
{
  printf("\nParameter '%s'\n",GetName());
  printf("\tFitted value:         %f\n",(const Double_t&)(*this));
  printf("\tMinus fit error:      %f\n",GetMinusFitError());
  printf("\tPlus fit error:       %f\n",GetPlusFitError());
  printf("\tStart value:          %f\n",GetStartVal());
  printf("\tMinimum value:        %f\n",GetMinVal());
  printf("\tMaximum value:        %f\n",GetMaxVal());
  printf("\tInitial step value:   %f\n",GetStepVal());
  printf("\tFree parameter index: %i\n",GetFreeParamIndex());
  printf("\tFixed parameter:      %i\n",IsFixed());
  printf("\tMaster index:         %i\n",GetMasterIndex());
}

void QSigExFitParam::SetFree()
{
  //If was slave
  if(fMaster) {
    //Remove this from the list of slaves of the old master
    fMaster->fSlaves->Del(fMaster->fSlaves->FindFirst((void*)this));

    if(!fMaster->fSlaves->Count()) {
      delete fMaster->fSlaves;
      fMaster->fSlaves=NULL;
    }
    //Create new data for this parameter using the actual data from the master
    fData=new QSFPData(this,*fData);
    fMaster=NULL;
    //Update the data pointer for the slaves of this
    UpdateDataPtr();
  }
}

void QSigExFitParam::SetSlaveOf(QSigExFitParam &master)
{
  if(fMaster!=&master) {

    //If was slave already from a different parameter
    if(fMaster) {
      //Remove this from the list of slave parameters
      fMaster->fSlaves->Del(fMaster->fSlaves->FindFirst((void*)this));

      if(!fMaster->fSlaves->Count()) {
	delete fMaster->fSlaves;
	fMaster->fSlaves=NULL;
      }

    //Else if was free
    } else {
      //Delete data
      delete fData;
    }

    //If the master is a slave of the current parameter, throw an error (slavery loops not allowed)
    if(master.fData == fData) {
      fprintf(stderr,"QSigExFitParam::SetSlaveOf: Error: Setting parameter '%s' as a slave of parameter '%s' creates a loop\n",GetName(),master.GetName());
      throw 1;
    }

    //Assign data from the new master
    fData=master.fData;
    fMaster=&master;

    //Assign this as a slave a new master
    if(!master.fSlaves) master.fSlaves=new QList<void*>;
    master.fSlaves->Add((void*)this);
    //Update the data pointer for the slaves of this
    UpdateDataPtr();
  }
}

void QSigExFitParam::UpdateDataPtr() const
{
  if(fSlaves) {
    QList<Int_t> treeidxs(0);
    Int_t level=0;
    const QSigExFitParam *parent=this;

    while(level>=0) {
      ((QSigExFitParam*)((*parent->fSlaves)[treeidxs[level]]))->fData=fData;

      if(((QSigExFitParam*)((*parent->fSlaves)[treeidxs[level]]))->fSlaves) {
	parent=(QSigExFitParam*)((*parent->fSlaves)[treeidxs[level]]);
	++level;

	if(level>=treeidxs.Count()) treeidxs.Add(0);
	else treeidxs[level]=0;

      } else {
	treeidxs[level]=treeidxs[level]+1;

	if(treeidxs[level]==parent->fSlaves->Count()) {
	  parent=parent->fMaster;
	  --level;
	}
      }
    }
  }
}

void QSigExFitParam::Browse(TBrowser *b)
{
  b->Add(fData->fMinusFitError);
  b->Add(fData->fPlusFitError);
  b->Add(fData->fStartVal);
  b->Add(fData->fMinVal);
  b->Add(fData->fMaxVal);
  b->Add(fData->fStepVal);
  b->Add(fData->fFreeParamIndex);
  b->Add(fData->fFixed);

  if(fMaster) {
    TString sbuf="Master parameter '";
    sbuf+=fMaster->GetName();
    sbuf+="'";
    b->Add(fMaster,sbuf);
  }
}

void QSigExFitParam::Streamer(TBuffer &R__b)
{
  // Stream an object of class QSigExFitParam.

  UInt_t R__s, R__c;
  if (R__b.IsReading()) {
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c);

    //If old version
    if(R__v==1) {
      //Create a QNamedVar<Double_t> to read parent class info
      QNamedVar<Double_t> qnv;
      //Create a pointer to QNamedVar<Int_t> to read masterindex object
      QNamedVar<Int_t> *qnvp;
      qnv.Streamer(R__b);
      fName=qnv.GetName();
      fData->fValue=qnv.GetValue();
      //Delete objects from fData since they will be reallocated by TBuffer
      delete fData->fStartVal;
      delete fData->fMinVal;
      delete fData->fMaxVal;
      delete fData->fStepVal;
      delete fData->fFixed;
      delete fData->fPlusFitError;
      delete fData->fMinusFitError;
      delete fData->fFreeParamIndex;
      R__b >> fData->fStartVal;
      R__b >> fData->fMinVal;
      R__b >> fData->fMaxVal;
      R__b >> fData->fStepVal;
      R__b >> fData->fFixed;
      R__b >> qnvp;

      //If masterindex is not -1, the current parameter is a slave. The master index is stored in the master pointer
      if(qnvp->GetValue()!=-1) fMaster=(QSigExFitParam*)(qnvp->GetValue()+1);
      //Read these even if they are not used
      R__b >> fData->fPlusFitError;
      R__b >> fData->fMinusFitError;
      R__b >> fData->fFreeParamIndex;
      R__b.CheckByteCount(R__s, R__c, QSigExFitParam::IsA());

    } else {
      Int_t i;
      R__b >> fName;
      R__b >> i;
      fMaster=(QSigExFitParam*)(i);

      //If the current parameter is a slave, the master index is stored in the master pointer
      if(i) fMaster=(QSigExFitParam*)((long)i);

      //Else if ultimate master
      else {
	R__b >> fData->fValue;
	R__b >> fData->fTitle;
	fData->fStartVal->Streamer(R__b);
	fData->fMinVal->Streamer(R__b);
	fData->fMaxVal->Streamer(R__b);
	fData->fStepVal->Streamer(R__b);
	fData->fFixed->Streamer(R__b);
	fData->fPlusFitError->Streamer(R__b);
	fData->fMinusFitError->Streamer(R__b);
	fData->fFreeParamIndex->Streamer(R__b);
      }
    }

  } else {
    R__c = R__b.WriteVersion(QSigExFitParam::IsA(), kTRUE);
    R__b << fName;

    if(fMaster) {
      R__b << fMaster->fIndex+1;

    } else {
      R__b << (Int_t)0;
      R__b << fData->fValue;
      R__b << fData->fTitle;
      fData->fStartVal->Streamer(R__b);
      fData->fMinVal->Streamer(R__b);
      fData->fMaxVal->Streamer(R__b);
      fData->fStepVal->Streamer(R__b);
      fData->fFixed->Streamer(R__b);
      fData->fPlusFitError->Streamer(R__b);
      fData->fMinusFitError->Streamer(R__b);
      fData->fFreeParamIndex->Streamer(R__b);
    }
    R__b.SetByteCount(R__c, kTRUE);
  }
}
