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
    const QCINTProc& operator=(const QCINTProc &rhs){QProcedure::operator=(rhs); if(fMethodCall) delete fMethodCall; fMethodCall=new TMethodCall(*(rhs.fMethodCall)); return *this;}

    virtual void AddInput(Int_t index=-1, Double_t *buf=NULL){QProcedure::AddInput(index,buf); InitArgs();}
    virtual void AddOutput(Int_t index=-1, Double_t *buf=NULL){QProcedure::AddOutput(index,buf); InitArgs();}
    virtual void AddParam(Int_t index=-1, Double_t *buf=NULL){QProcedure::AddParam(index,buf); InitArgs();}

    QProcedure* Clone() const{return new QCINTProc(*this);}

    virtual void DelInput(Int_t index=-1){QProcedure::DelInput(index); InitArgs();}
    virtual void DelOutput(Int_t index=-1){QProcedure::DelOutput(index); InitArgs();}
    virtual void DelParam(Int_t index=-1){QProcedure::DelParam(index); InitArgs();}

    virtual void SetNInputs(Int_t n){QProcedure::SetNInputs(n); InitArgs();}
    virtual void SetNOutputs(Int_t n){QProcedure::SetNOutputs(n); InitArgs();}
    virtual void SetNParams(Int_t n){QProcedure::SetNParams(n); InitArgs();}

    virtual void Exec() const;
    void InitArgs();
    void SetProc(void *proc);
    void SetProc(const char *procname);
  protected:
    TMethodCall *fMethodCall;

    ClassDef(QCINTProc,1) //Procedure class for CINT procedures having the format void (*proc)(Double_t** inputs, Double_t **outputs, Double_t **params)
};

#include "debugger.h"

#endif
