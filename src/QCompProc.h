#ifndef _QCOMPPROC_
#define _QCOMPPROC_

#include "QProcedure.h"

#include "debugger.h"

class QCompProc: public QProcedure
{
  public:
    QCompProc(): QProcedure(){}
    QCompProc(void (*proc)(Double_t**,Double_t**,Double_t**)): QProcedure(), fProc(proc){}
    QCompProc(const QCompProc &rhs):QProcedure(rhs), fProc(rhs.fProc){}
    virtual ~QCompProc(){}
    const QCompProc& operator=(const QCompProc &rhs){QProcedure::operator=(rhs); fProc=rhs.fProc; return *this;}

    QProcedure* Clone(){return new QCompProc(*this);}

    virtual void Exec();
    void SetProc(void (*proc)(Double_t**,Double_t**,Double_t**)){fProc=proc;}
  private:
    void (*fProc)(Double_t**,Double_t**,Double_t**);

    ClassDef(QCompProc,1)
};

#include "debugger.h"

#endif
