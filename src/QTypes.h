#ifndef _QTYPES_
#define _QTYPES_

#include <cstdlib>
#include "Rtypes.h"
#include "TString.h"

namespace QTypes {
    enum TypeID {kBool, kChar, kUChar, kShort, kUShort, kInt, kUInt, kFloat, kDouble, kLong64, kULong64};
    UInt_t GetTypeIDSize(const Int_t &type);
    void* AllocType(const Int_t &type){return malloc(GetTypeIDSize(type));}
    Int_t GetNameTypeID(const TString adesc, TString *name=NULL);
    const Char_t* GetTypeName(const Int_t &type);
    const Char_t* GetTypeSName(const Int_t &type);
}

#endif
