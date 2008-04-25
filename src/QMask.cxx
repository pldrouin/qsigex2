#include "QMask.h"

#include "debugger.h"

ClassImp(QMask)

void QMask::Crop()
{
  Int_t i;
  for(i=Count()-1; i>=0 && GetArray()[i] == 0; i--){}
  if(i>-1) RedimList(i+1);
}

void QMask::FillMask(UInt_t nbits)
{
  Double_t nbytes=nbits/8.;
  Int_t nwbytes=(Int_t)nbytes;
  Int_t nnbytes=(Int_t)ceil(nbytes);
  RedimList(nnbytes);

  memset(GetArray(),255,nwbytes);

  switch(nbits%8) {
    case 1:
      GetArray()[nwbytes]=1;
      break;
    case 2:
      GetArray()[nwbytes]=3;
      break;
    case 3:
      GetArray()[nwbytes]=7;
      break;
    case 4:
      GetArray()[nwbytes]=15;
      break;
    case 5:
      GetArray()[nwbytes]=31;
      break;
    case 6:
      GetArray()[nwbytes]=63;
      break;
    case 7:
      GetArray()[nwbytes]=127;
      break;
  }
}

Bool_t QMask::GetBit(UInt_t n) const
{
  Int_t nbytes=n/8;
  if(nbytes >= Count()) return kFALSE;
  return (GetArray()[nbytes]>>(n%8))&1;
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
    GetArray()[nbytes]|=(1<<bshift);

  } else {

    if(Count() < nbytes+1) return;
    GetArray()[nbytes]&=(~(1<<bshift));
  }
}

const QMask& QMask::operator&=(const QMask &rhs)
{

  if(Count() > rhs.Count()) RedimList(rhs.Count());

  for(Int_t i=0; i<rhs.Count(); i++) {
    GetArray()[i]&=rhs[i];
  }

  return *this;
}

const QMask& QMask::operator|=(const QMask &rhs)
{
  if(Count() > rhs.Count()) {

    for(Int_t i=0; i<rhs.Count(); i++) {
      GetArray()[i]|=rhs[i];
    }

  } else {
    Int_t size=Count();
    RedimList(rhs.Count());

    for(Int_t i=0; i<size; i++) {
      GetArray()[i]|=rhs[i];
    }
    memcpy(GetArray()+size,rhs.GetArray()+size,Count()-size);
  }

  return *this;
}

const QMask& QMask::operator^=(const QMask &rhs)
{
  if(Count() > rhs.Count()) {

    for(Int_t i=0; i<rhs.Count(); i++) {
      GetArray()[i]^=rhs[i];
    }

  } else {
    Int_t size=Count();
    RedimList(rhs.Count());

    for(Int_t i=0; i<size; i++) {
      GetArray()[i]^=rhs[i];
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
      ret[Count()-nbytes-1]=(GetLast()>>bshift);
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

QMask::operator Bool_t() const
{
  if(!Count()) return kFALSE;

  for(Int_t i=0; i<Count(); i++) {

    if(GetArray()[i] != 0) return kTRUE;
  }
  return kFALSE;
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
    ret.GetArray()[i]=lhs.GetArray()[i]&rhs.GetArray()[i];
  }

  return ret;
}

QMask operator|(const QMask &lhs, const QMask &rhs)
{
  QMask ret;

  if(lhs.Count() > rhs.Count()) {
    ret=lhs;

    for(Int_t i=0; i<rhs.Count(); i++) {
      ret.GetArray()[i]|=rhs.GetArray()[i];
    }

  } else {
    ret=rhs;

    for(Int_t i=0; i<lhs.Count(); i++) {
      ret.GetArray()[i]|=lhs.GetArray()[i];
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
      ret.GetArray()[i]^=rhs.GetArray()[i];
    }

  } else {
    ret=rhs;

    for(Int_t i=0; i<lhs.Count(); i++) {
      ret.GetArray()[i]^=lhs.GetArray()[i];
    }
  }

  return ret;
}

QMask operator~(const QMask &rhs)
{
  QMask ret;
  ret.RedimList(rhs.Count());

  for(Int_t i=0; i<rhs.Count(); i++) {
    ret.GetArray()[i]=~rhs.GetArray()[i];
  }

  return ret;
}

#include "debugger.h"
