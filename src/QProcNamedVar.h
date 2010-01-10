#ifndef _QPROCNAMEDVAR_
#define _QPROCNAMEDVAR_

#include "QNamedVar.h"
#include "QProcObj.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

using namespace std;

template <typename U> class QProcNamedVar: public QNamedVar<U>, public QProcObj
{
  public:
    QProcNamedVar():QNamedVar<U>(), QProcObj(){}

    QProcNamedVar(const char *name, const U& val): QNamedVar<U>(name,val), QProcObj(){}

    QProcNamedVar(const char *name, const char *valstr): QNamedVar<U>(name,valstr), QProcObj() {}

    QProcNamedVar(const QProcNamedVar<U>& namedval): QNamedVar<U>(namedval), QProcObj(namedval) {}

    const QProcNamedVar& operator=(const QProcNamedVar& rhs){QNamedVar<U>::operator=(rhs); QProcObj::operator=(rhs); return *this;}
    const QProcNamedVar& operator=(const U& rhs){QNamedVar<U>::operator=(rhs); return *this;}
    const QProcNamedVar& operator=(const char *valstr) {QNamedVar<U>::operator=(valstr); return *this;}

    virtual ~QProcNamedVar(){}
    
    template<typename V> friend Bool_t operator==(const QProcNamedVar<V> &lhs, const QProcNamedVar<V> &rhs){return (dynamic_cast<const QNamedVar&>(lhs)==dynamic_cast<const QNamedVar&>(rhs));}

  private:

  ClassDef(QProcNamedVar,1) //Generic named value template class with proc obj properties
};

#include "debugger.h"

#include "QProcNamedVar_cxx.h"

#endif
