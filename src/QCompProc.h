// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#ifndef _QCOMPPROC_
#define _QCOMPPROC_

#include "QProcedure.h"

#include "debugger.h"

class QCompProc: public QProcedure
{
  public:
    QCompProc(): QProcedure(){}
    QCompProc(Bool_t (*proc)(QProcArgs&)): QProcedure(), fProc(proc){}
    QCompProc(const QCompProc &rhs):QProcedure(rhs), fProc(rhs.fProc){}
    virtual ~QCompProc(){}
    const QProcedure& operator=(const QProcedure& rhs){return QProcedure::operator=(rhs);}
    const QCompProc& operator=(const QCompProc &rhs){QProcedure::operator=(rhs); fProc=rhs.fProc; return *this;}

    QProcedure* Clone() const{return new QCompProc(*this);}

    virtual Bool_t Exec();
    void SetProc(Bool_t (*proc)(QProcArgs&)){fProc=proc;}
  private:
    Bool_t (*fProc)(QProcArgs&);

//    ClassDef(QCompProc,1) //Procedure class for compiled procedures having the signature Bool_t (*proc)(QProcArgs&)
};

#include "debugger.h"

#endif
