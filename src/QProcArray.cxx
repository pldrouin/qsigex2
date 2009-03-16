#include "QProcArray.h"

ClassImp(QProcArray)

Int_t QProcArray::GetNameTypeID(const TString adesc, TString *name)
{
  Int_t type=-1;
  Int_t k;
  TString atype;

  k=adesc.Last('/');

  if(k!=-1) {
    *name=adesc(0,k);
    atype=adesc(k+1,adesc.Length()-k-1);

    if(!strcmp(atype,"D") || !strcmp(atype,"Double_t")) {
      type=kDouble;

    } else if(!strcmp(atype,"F") || !strcmp(atype,"Float_t")) {
      type=kFloat;

    } else if(!strcmp(atype,"I") || !strcmp(atype,"Int_t")) {
      type=kInt;

    } else if(!strcmp(atype,"i") || !strcmp(atype,"UInt_t")) {
      type=kUInt;

    } else if(!strcmp(atype,"0") || !strcmp(atype,"Bool_t")) {
      type=kBool;

    } else if(!strcmp(atype,"L") || !strcmp(atype,"Long64_t")) {
      type=kLong64;

    } else if(!strcmp(atype,"l") || !strcmp(atype,"ULong64_t")) {
      type=kULong64;

    } else if(!strcmp(atype,"B") || !strcmp(atype,"Char_t")) {
      type=kChar;

    } else if(!strcmp(atype,"b") || !strcmp(atype,"UChar_t")) {
      type=kUChar;

    } else if(!strcmp(atype,"S") || !strcmp(atype,"Short_t")) {
      type=kShort;

    } else if(!strcmp(atype,"s") || !strcmp(atype,"UShort_t")) {
      type=kUShort;

    } else {
      fprintf(stderr,"QProcArray::GetNameTypeID: Error: Data type '%s' is unknown\n",atype.Data());
      throw 1;
    }

  } else {
    *name=adesc;
  }
  return type;
}

UInt_t QProcArray::GetTypeIDSize(const Int_t &type)
{
  switch(type) {
    case kDouble:
      return sizeof(Double_t);
    case kFloat:
      return sizeof(Float_t);
    case kInt:
      return sizeof(Int_t);
    case kUInt:
      return sizeof(UInt_t);
    case kBool:
      return sizeof(Bool_t);
    case kLong64:
      return sizeof(Long64_t);
    case kULong64:
      return sizeof(ULong64_t);
    case kChar:
      return sizeof(Char_t);
    case kUChar:
      return sizeof(UChar_t);
    case kShort:
      return sizeof(Short_t);
    case kUShort:
      return sizeof(UShort_t);
    default:
      fprintf(stderr,"QProcArray::GetTypeIDSize: Error: type %i is unknown\n",type);
      throw 1;
  }
  return 0;
}
