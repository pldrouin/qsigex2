#include "QMask.h"

#include "debugger.h"

ClassImp(QMask)

void QMask::Crop()
{
  Int_t i;
  for(i=Count()-1; i>=0 && operator[](i) == 0; i--){}
  if(i>-1) RedimList(i+1);
}

Bool_t QMask::GetBit(UInt_t n) const
{
  Int_t nbytes=n/8;
  if(nbytes >= Count()) return kFALSE;
  return (operator[](nbytes)>>(n%8))&1;
}

void QMask::Print(const Option_t *) const
{
  Int_t i,j;
  Bool_t skip=kTRUE;

  for(i=Count()-1; i>=0; i--) {
    if(skip && operator[](i) == 0) continue;
    else skip=kFALSE;
    
    for(j=7; j>=0; j--) printf("%i",GetBit(i*8+j));
    printf(" ");
  }
  if(skip) printf("00000000");
  printf("\n");
}

void QMask::SetBit(UInt_t n, Bool_t value)
{
  Int_t nbytes=n/8;
  Int_t bshift=n%8;
  if(value) {

    if(Count() < nbytes+1) RedimList(nbytes+1,-1,0);
    operator[](nbytes)|=(1<<bshift);

  } else {

    if(Count() < nbytes+1) return;
    operator[](nbytes)&=(~(1<<bshift));
  }
}

const QMask& QMask::operator&=(const QMask &rhs)
{

  if(Count() > rhs.Count()) RedimList(rhs.Count());

  for(Int_t i=0; i<rhs.Count(); i++) {
    operator[](i)&=rhs[i];
  }

  return *this;
}

const QMask& QMask::operator|=(const QMask &rhs)
{
  if(Count() > rhs.Count()) {

    for(Int_t i=0; i<rhs.Count(); i++) {
      operator[](i)|=rhs[i];
    }

  } else {
    Int_t size=Count();
    RedimList(rhs.Count());

    for(Int_t i=0; i<size; i++) {
      operator[](i)|=rhs[i];
    }
    memcpy(GetArray()+size,rhs.GetArray()+size,Count()-size);
  }

  return *this;
}

const QMask& QMask::operator^=(const QMask &rhs)
{
  if(Count() > rhs.Count()) {

    for(Int_t i=0; i<rhs.Count(); i++) {
      operator[](i)^=rhs[i];
    }

  } else {
    Int_t size=Count();
    RedimList(rhs.Count());

    for(Int_t i=0; i<size; i++) {
      operator[](i)^=rhs[i];
    }
    memcpy(GetArray()+size,rhs.GetArray()+size,Count()-size);
  }

  return *this;
}

QMask QMask::operator>>(UInt_t n) const
{
  QMask ret;
  Int_t nbytes=n/8;

  if(nbytes<Count()) {
    ret.RedimList(Count()-nbytes,-1,0);

    if(n%8 == 0) {
      memcpy(ret.GetArray(),GetArray()+nbytes,Count()-nbytes);

    } else {
      Int_t bshift=n%8;

      for(Int_t i=0; i<Count()-nbytes-1; i++) {
	ret[i]=(GetArray()[i+nbytes]>>bshift)|(GetArray()[i+nbytes+1]<<8-bshift);
      }
      ret[Count()-nbytes-1]=(GetArray()[Count()-1]>>bshift);
    }
  }
  return ret;
}

QMask QMask::operator<<(UInt_t n) const
{
  QMask ret;
  Int_t nbytes=n/8;

  if(nbytes<Count()) {
    ret.RedimList(Count(),-1,0);

    if(n%8 == 0) {
      memcpy(ret.GetArray()+nbytes,GetArray(),Count()-nbytes);

    } else {
      Int_t bshift=n%8;

      for(Int_t i=Count()-1; i>nbytes; i--) {
	ret[i]=(GetArray()[i-nbytes]<<bshift)|(GetArray()[i-nbytes-1]>>8-bshift);
      }
      ret[nbytes]=(GetArray()[0]<<bshift);
    }
  }
  return ret;
}

QMask operator&(const QMask &lhs, const QMask &rhs)
{
  QMask ret;

  if(lhs.Count() > rhs.Count()) {
    ret.RedimList(rhs.Count());
  } else {
    ret.RedimList(lhs.Count());
  }

  for(Int_t i=0; i<ret.Count(); i++) {
    ret[i]=lhs[i]&rhs[i];
  }

  return ret;
}

QMask operator|(const QMask &lhs, const QMask &rhs)
{
  QMask ret;

  if(lhs.Count() > rhs.Count()) {
    ret=lhs;

    for(Int_t i=0; i<rhs.Count(); i++) {
      ret[i]|=rhs[i];
    }

  } else {
    ret=rhs;

    for(Int_t i=0; i<lhs.Count(); i++) {
      ret[i]|=lhs[i];
    }
  }

  return ret;
}

QMask operator^(const QMask &lhs, const QMask &rhs)
{
  QMask ret;

  if(lhs.Count() > rhs.Count()) {
    ret=lhs;

    for(Int_t i=0; i<rhs.Count(); i++) {
      ret[i]^=rhs[i];
    }

  } else {
    ret=rhs;

    for(Int_t i=0; i<lhs.Count(); i++) {
      ret[i]^=lhs[i];
    }
  }

  return ret;
}

QMask operator~(const QMask &rhs)
{
  QMask ret;
  ret.RedimList(rhs.Count());

  for(Int_t i=0; i<rhs.Count(); i++) {
    ret[i]=~rhs[i];
  }

  return ret;
}

#include "debugger.h"
