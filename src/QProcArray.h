// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _QPROCARRAY_
#define _QPROCARRAY_

#include "TString.h"
#include "QProcObj.h"
#include "QTypes.h"

using namespace QTypes;

class QProcArray: public QProcObj
{
  public:
  QProcArray(): QProcObj(), fBTypeID(-1) {}
  QProcArray(const QProcArray &rhs): QProcObj(rhs), fBTypeID(rhs.fBTypeID) {}
  virtual ~QProcArray(){}
  const QProcArray& operator=(const QProcArray &rhs){QProcObj::operator=(rhs); fBTypeID=rhs.fBTypeID; return *this;}
  virtual Int_t Fill()=0;
#ifndef __CINT__
  virtual void* const& GetBuffer() const=0;
#endif
  virtual Long64_t GetEntries() const=0;
  const Int_t& GetBTypeID() const{return fBTypeID;}
  virtual void LoadEntry(const Long64_t &entry = 0)=0;
  virtual void ResetArray()=0;
  virtual void SetBuffer(void* buffer)=0;
  virtual void UnloadArray()=0;

  protected:
  Int_t fBTypeID; //!

  private:
  ClassDef(QProcArray,1) //Pure abstract base class for QArrayProcessor arrays
};

#endif
