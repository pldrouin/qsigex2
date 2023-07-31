// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#include "QMask.h"

#include "debugger.h"

ClassImp(QMask)

QMask QMask::GetComplement(const UInt_t &nbits) const
{
  Int_t nwbytes=nbits/8;
  Int_t mod=nbits%8;
  Int_t nnbytes=nwbytes + (mod != 0);
  QMask ret;
  ret.RedimList(nnbytes);
  Int_t i;

  if(nnbytes <= Count()) {

    for(i=nnbytes-1; i>=0; --i) {
      ret.GetArray()[i]=~GetArray()[i];
    }

  } else {

    for(i=Count()-1; i>=0; --i) {
      ret.GetArray()[i]=~GetArray()[i];
    }

    for(i=Count(); i<nnbytes; ++i) {
      ret.GetArray()[i]=255;
    }
  }

  switch(mod) {
    case 1:
      ret.GetArray()[nwbytes]&=1;
      break;
    case 2:
      ret.GetArray()[nwbytes]&=3;
      break;
    case 3:
      ret.GetArray()[nwbytes]&=7;
      break;
    case 4:
      ret.GetArray()[nwbytes]&=15;
      break;
    case 5:
      ret.GetArray()[nwbytes]&=31;
      break;
    case 6:
      ret.GetArray()[nwbytes]&=63;
      break;
    case 7:
      ret.GetArray()[nwbytes]&=127;
      break;
  }

  return ret;
}

void QMask::Crop()
{
  Int_t i;
  for(i=Count()-1; i>=0 && GetArray()[i] == 0; --i){}
  if(i>-1) RedimList(i+1);
}

void QMask::FillMask(const UInt_t &nbits)
{
  UInt_t nwbytes=nbits/8;
  UInt_t mod=nbits%8;
  UInt_t nnbytes=nwbytes + (mod != 0);
  RedimList(nnbytes);

  memset(GetArray(),255,nwbytes);

  switch(mod) {
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

void QMask::Print(const Option_t *) const
{
  Int_t i,j;
  Bool_t skip=kTRUE;

  for(i=Count()-1; i>=0; --i) {
    if(skip && operator[](i) == 0) continue;
    skip=kFALSE;
    
    for(j=7; j>=0; --j) printf("%i",GetBit(i*8+j));
    printf(" ");
  }
  if(skip) printf("(empty)");
  printf("\n");
}

void QMask::SetBit(const UInt_t &n, const Bool_t &value)
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

  for(Int_t i=Count()-1; i>=0; --i) {
    GetArray()[i]&=rhs[i];
  }

  return *this;
}

const QMask& QMask::operator|=(const QMask &rhs)
{
  if(Count() > rhs.Count()) {

    for(Int_t i=rhs.Count()-1; i>=0; --i) {
      GetArray()[i]|=rhs[i];
    }

  } else {
    Int_t size=Count();
    RedimList(rhs.Count());

    for(Int_t i=size-1; i>=0; --i) {
      GetArray()[i]|=rhs[i];
    }
    memcpy(GetArray()+size,rhs.GetArray()+size,Count()-size);
  }

  return *this;
}

const QMask& QMask::operator^=(const QMask &rhs)
{
  if(Count() > rhs.Count()) {

    for(Int_t i=rhs.Count()-1; i>=0; --i) {
      GetArray()[i]^=rhs[i];
    }

  } else {
    Int_t size=Count();
    RedimList(rhs.Count());

    for(Int_t i=size-1; i>=0; --i) {
      GetArray()[i]^=rhs[i];
    }
    memcpy(GetArray()+size,rhs.GetArray()+size,Count()-size);
  }

  return *this;
}

QMask QMask::operator>>(const UInt_t &n) const
{
  QMask ret;
  Int_t nbytes=n/8;

  if(nbytes<Count()) {
    ret.RedimList(Count()-nbytes,-1,0);

    if(n%8 == 0) {
      memcpy(ret.GetArray(),GetArray()+nbytes,Count()-nbytes);

    } else {
      Int_t bshift=n%8;

      for(Int_t i=0; i<Count()-nbytes-1; ++i) {
	ret[i]=(GetArray()[i+nbytes]>>bshift)|(GetArray()[i+nbytes+1]<<(8-bshift));
      }
      ret[Count()-nbytes-1]=(GetLast()>>bshift);
    }
  }
  return ret;
}

QMask QMask::operator<<(const UInt_t &n) const
{
  QMask ret;
  Int_t nbytes=n/8;

  if(nbytes<Count()) {
    ret.RedimList(Count(),-1,0);

    if(n%8 == 0) {
      memcpy(ret.GetArray()+nbytes,GetArray(),Count()-nbytes);

    } else {
      Int_t bshift=n%8;

      for(Int_t i=Count()-1; i>nbytes; --i) {
	ret[i]=(GetArray()[i-nbytes]<<bshift)|(GetArray()[i-nbytes-1]>>(8-bshift));
      }
      ret[nbytes]=(GetArray()[0]<<bshift);
    }
  }
  return ret;
}

QMask::operator Bool_t() const
{
  if(!Count()) return kFALSE;

  for(Int_t i=0; i<Count(); ++i) {

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

  for(Int_t i=ret.Count()-1; i>=0; --i) {
    ret.GetArray()[i]=lhs.GetArray()[i]&rhs.GetArray()[i];
  }

  return ret;
}

QMask operator|(const QMask &lhs, const QMask &rhs)
{
  QMask ret;

  if(lhs.Count() > rhs.Count()) {
    ret=lhs;

    for(Int_t i=rhs.Count()-1; i>=0; --i) {
      ret.GetArray()[i]|=rhs.GetArray()[i];
    }

  } else {
    ret=rhs;

    for(Int_t i=lhs.Count()-1; i>=0; --i) {
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

    for(Int_t i=rhs.Count()-1; i>=0; --i) {
      ret.GetArray()[i]^=rhs.GetArray()[i];
    }

  } else {
    ret=rhs;

    for(Int_t i=lhs.Count()-1; i>=0; --i) {
      ret.GetArray()[i]^=lhs.GetArray()[i];
    }
  }

  return ret;
}

Bool_t operator&&(const QMask &lhs, const QMask &rhs)
{
  if(lhs.Count() > rhs.Count()) {

    for(Int_t i=0; i<rhs.Count(); ++i) {
      if(lhs.GetArray()[i]&rhs.GetArray()[i]) return kTRUE;
    }

  } else {

    for(Int_t i=0; i<lhs.Count(); ++i) {
      if(lhs.GetArray()[i]&rhs.GetArray()[i]) return kTRUE;
    }
  }

  return kFALSE;
}

Bool_t operator||(const QMask &lhs, const QMask &rhs)
{
  Int_t i;

  for(i=0; i<lhs.Count(); ++i) {
    if(lhs.GetArray()[i]) return kTRUE;
  }

  for(i=0; i<rhs.Count(); ++i) {
    if(rhs.GetArray()[i]) return kTRUE;
  }

  return kFALSE;
}

#include "debugger.h"
