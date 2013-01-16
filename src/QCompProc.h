// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

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

    ClassDef(QCompProc,1) //Procedure class for compiled procedures having the signature Bool_t (*proc)(QProcArgs&)
};

#include "debugger.h"

#endif
