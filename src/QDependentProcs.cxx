#include "QDependentProcs.h"

ClassImp(QDependentProcs);

Int_t QDependentProcs::fInitIdx=-1;
QList<void*> QDependentProcs::fQPDObjs;
QList<Int_t> QDependentProcs::fPCalled;

QList<Int_t> QDependentProcs::GetAllDepends() const
{
  QList<Int_t> ret;

  if(fInitIdx == -1) {
    fInitIdx=fIdx;

  } else {
    ret.Add(fIdx);
  }

  for(Int_t i=0; i<fDepends.Count(); i++) {
    if(fDepends[i] != fInitIdx && fPCalled.FindFirst(fDepends[i]) == -1) {
      fPCalled.Add(fDepends[i]);
      ret.Add(((QDependentProcs*)fQPDObjs[fDepends[i]])->GetAllDepends());
    }
  }

  if(fInitIdx == fIdx) {
    fPCalled.Clear();
    fInitIdx=-1;
  }

  return ret;
}

