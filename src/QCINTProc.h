// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _QCINTPROC_
#define _QCINTPROC_

#include "G__ci.h"
#include "TROOT.h"
#include "TMethodCall.h"
#include "QProcedure.h"

#include "debugger.h"

class QCINTProc: public QProcedure
{
  public:
    QCINTProc(): QProcedure(), fMethodCall(NULL){}
    QCINTProc(void *proc): QProcedure(), fMethodCall(NULL){SetProc(proc);}
    QCINTProc(const char *procname): QProcedure(), fMethodCall(NULL){SetProc(procname);}
    QCINTProc(const QCINTProc &rhs): QProcedure(rhs), fMethodCall(new TMethodCall(*(rhs.fMethodCall))){}
    virtual ~QCINTProc(){if(fMethodCall) delete fMethodCall;}
    const QProcedure& operator=(const QProcedure& rhs){return QProcedure::operator=(rhs);}
    const QCINTProc& operator=(const QCINTProc &rhs){QProcedure::operator=(rhs); if(fMethodCall) delete fMethodCall; fMethodCall=new TMethodCall(*(rhs.fMethodCall)); return *this;}

    QProcedure* Clone() const{return new QCINTProc(*this);}

    virtual Bool_t Exec();
    void InitArgs();
    void SetProc(void *proc);
    void SetProc(const char *procname);
  protected:
    TMethodCall *fMethodCall;

    ClassDef(QCINTProc,1) //Procedure class for CINT procedures having the signature Bool_t (*proc)(QProcArgs&)
};

#include "debugger.h"

#endif
