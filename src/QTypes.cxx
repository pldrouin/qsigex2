#include "QTypes.h"

Int_t QTypes::GetNameTypeID(const TString &adesc, TString *name)
{
  Int_t k;
  Int_t type=-1;

  k=adesc.Last('/');

  if(k!=-1) {
    if(name) *name=adesc(0,k);
    type=GetTypeID((TString)adesc(k+1,adesc.Length()-k-1));

  } else {
    if(name) *name=adesc;
  }
  return type;
}

Int_t QTypes::GetTypeID(const Char_t *atype)
{
  Int_t type=-1;

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
    fprintf(stderr,"QTypes::GetNameTypeID: Error: Data type '%s' is unknown\n",atype);
    throw 1;
  }
  return type;
}

UInt_t QTypes::GetTypeSize(const Int_t &type)
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
      fprintf(stderr,"QTypes::GetTypeSize: Error: type %i is unknown\n",type);
      throw 1;
  }
  return 0;
}

const Char_t* QTypes::GetTypeName(const Int_t &type)
{
  switch(type) {
    case kDouble:
      return "Double_t";
    case kFloat:
      return "Float_t";
    case kInt:
      return "Int_t";
    case kUInt:
      return "UInt_t";
    case kBool:
      return "Bool_t";
    case kLong64:
      return "Long64_t";
    case kULong64:
      return "ULong64_t";
    case kChar:
      return "Char_t";
    case kUChar:
      return "UChar_t";
    case kShort:
      return "Short_t";
    case kUShort:
      return "UShort_t";
    default:
      fprintf(stderr,"QTypes::GetTypeName: Error: type %i is unknown\n",type);
      throw 1;
  }
  return NULL;
}

const Char_t* QTypes::GetTypeSName(const Int_t &type)
{
  switch(type) {
    case kDouble:
      return "D";
    case kFloat:
      return "F";
    case kInt:
      return "I";
    case kUInt:
      return "i";
    case kBool:
      return "0";
    case kLong64:
      return "L";
    case kULong64:
      return "l";
    case kChar:
      return "B";
    case kUChar:
      return "b";
    case kShort:
      return "S";
    case kUShort:
      return "s";
    default:
      fprintf(stderr,"QTypes::GetTypeSName: Error: type %i is unknown\n",type);
      throw 1;
  }
  return NULL;
}
