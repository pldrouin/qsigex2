#ifndef _QPROCDOUBLE_
#define _QPROCDOUBLE_

#include "QProcObj.h"

class QProcDouble: public QProcObj
{
  public:
    QProcDouble(): QProcObj(), fDouble(0.){}
    QProcDouble(const Double_t& rhs): QProcObj(), fDouble(rhs){}
    QProcDouble(const QProcDouble &rhs): QProcObj(rhs), fDouble(rhs.fDouble){}
    virtual ~QProcDouble(){}
    void InitProcObj(){fDouble=0;}
    operator const Double_t&() const{return fDouble;}
    operator Double_t&() {return fDouble;}
    const QProcDouble& operator=(const QProcDouble &rhs){fDouble=rhs.fDouble; return *this;}
    const QProcDouble& operator=(const Double_t& rhs){fDouble=rhs; return *this;}
    const QProcDouble& operator+=(const Double_t& rhs){fDouble+=rhs; return *this;}
    const QProcDouble& operator-=(const Double_t& rhs){fDouble-=rhs; return *this;}
    const QProcDouble& operator*=(const Double_t& rhs){fDouble*=rhs; return *this;}
    const QProcDouble& operator/=(const Double_t& rhs){fDouble/=rhs; return *this;}
  protected:
    Double_t fDouble;
    ClassDef(QProcDouble,1) //Double_t with QProcObj properties
};

#endif
