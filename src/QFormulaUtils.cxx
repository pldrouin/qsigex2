// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>

#include "QFormulaUtils.h"

//#define DEBUG

#include "debugger.h"

ClassImp(QFormulaUtils)

Int_t QFormulaUtils::IndexVar(const Char_t* formula, const Char_t* variable, Int_t start)
{
  //This function reads formula, starting at position start, and looks for
  //variable string. It returns the index of the variable name occurrence if
  //found. It returns -1 if not. A variable name is considered to be any
  //alphanumeric expression that can also contain '_'.

  TString fstr=formula;
  Int_t flen=strlen(formula);
  Int_t vlen=strlen(variable);

  Int_t first;
  Int_t index=start-vlen;
  while(kTRUE){
    first=index+vlen;
    index=fstr.Index(variable,first);
    if(index==-1) return -1;
    if(index>0 && (isalnum(formula[index-1]) || formula[index-1]=='_')) continue;
    if(index+vlen<flen && (isalnum(formula[index+vlen]) || formula[index+vlen]=='_')) continue;
    break;
  }
  return index; 
}

Int_t QFormulaUtils::ReplaceVar(TString *formula, const Char_t* oldvar, const Char_t* newvar)
{
  //This function replaces in formula all the occurences of variable with name
  //oldvar by expression contained in newvar. It returns the number of replaced
  //occurences.

  Int_t start=0;
  Int_t pos;
  Int_t ovlen=strlen(oldvar);
  Int_t nvlen=strlen(newvar);
  Int_t n=0;
  while((pos=IndexVar(formula->Data(),oldvar,start))!=-1)
  {
    n++;
    formula->Replace(pos,ovlen,newvar);
    start=pos+nvlen; 
  }

  return n;
}

#include "debugger.h"
