#ifndef _QCOMPPROC_
#define _QCOMPPROC_

#include "QProcedure.h"

#include "debugger.h"

class QCompProc: public QProcedure
{
  public:
    QCompProc(): QProcedure(){}
    QCompProc(Bool_t (*proc)(Double_t**,Double_t**,Double_t**,const Int_t*)): QProcedure(), fProc(proc){}
    QCompProc(const QCompProc &rhs):QProcedure(rhs), fProc(rhs.fProc){}
    virtual ~QCompProc(){}
    const QCompProc& operator=(const QCompProc &rhs){QProcedure::operator=(rhs); fProc=rhs.fProc; return *this;}

    QProcedure* Clone() const{return new QCompProc(*this);}

    virtual Bool_t Exec() const;
    void SetProc(Bool_t (*proc)(Double_t**,Double_t**,Double_t**,const Int_t*)){fProc=proc;}
  private:
    Bool_t (*fProc)(Double_t**,Double_t**,Double_t**,const Int_t*);

    ClassDef(QCompProc,1) //Procedure class for compiled procedures having the format Bool_t (*proc)(Double_t** inputs, Double_t **outputs, Double_t **params)
};

#include "debugger.h"

#endif
