#ifndef _QTYPES_
#define _QTYPES_

#include <cstdlib>
#include "Rtypes.h"
#include "TString.h"

namespace QTypes {
    enum TypeID {kBool, kChar, kUChar, kShort, kUShort, kInt, kUInt, kFloat, kDouble, kLong64, kULong64};
    UInt_t GetTypeSize(const Int_t &type);
    inline void* AllocType(const Int_t &type){return malloc(GetTypeSize(type));}
    Int_t GetNameTypeID(const TString &adesc, TString *name=NULL);
    Int_t GetTypeID(const Char_t *atype);
    const Char_t* GetTypeName(const Int_t &type);
    const Char_t* GetTypeSName(const Int_t &type);
}

#endif
