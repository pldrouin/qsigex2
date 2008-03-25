// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>

#ifndef _FORMULAUTILS_
#define _FORMULAUTILS_

#include <cctype>

#include "Rtypes.h"

#include "TString.h"

#include "debugger.h"

class QFormulaUtils{
  public:
    QFormulaUtils(){}
    virtual ~QFormulaUtils(){}

    static Int_t IndexVar(const Char_t* formula, const Char_t* variable, Int_t start=0);
    static Int_t ReplaceVar(TString *formula, const Char_t* oldvar, const Char_t* newvar);

    ClassDef(QFormulaUtils,1) //Set of static member functions that operate on variables names in a formula
};

#include "debugger.h"

#endif
