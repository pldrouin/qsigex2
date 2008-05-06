#ifndef _QPROCEDURE_
#define _QPROCEDURE_

#include "Rtypes.h"
#include "QList.h"

#include "debugger.h"

class QProcedure
{
  public:
    QProcedure(): fInputsBufs(), fOutputsBufs(), fParamsBufs(), fN(){}
    QProcedure(const QProcedure &rhs): fInputsBufs(rhs.fInputsBufs), fOutputsBufs(rhs.fOutputsBufs), fParamsBufs(rhs.fParamsBufs), fN(){fN[0]=rhs.fN[0]; fN[1]=rhs.fN[1]; fN[2]=rhs.fN[2];}
    virtual ~QProcedure(){}
    const QProcedure& operator=(const QProcedure &rhs){fInputsBufs=rhs.fInputsBufs; fOutputsBufs=rhs.fOutputsBufs; fParamsBufs=rhs.fParamsBufs; fN[0]=rhs.fN[0]; fN[1]=rhs.fN[1]; fN[2]=rhs.fN[2]; return *this;}

    virtual void AddInput(Int_t index=-1, Double_t *buf=NULL){fInputsBufs.Add(buf,index); fN[0]=fInputsBufs.Count();}
    virtual void AddOutput(Int_t index=-1, Double_t *buf=NULL){fOutputsBufs.Add(buf,index); fN[1]=fOutputsBufs.Count();}
    virtual void AddParam(Int_t index=-1, Double_t *buf=NULL){fParamsBufs.Add(buf,index); fN[2]=fParamsBufs.Count();}

    virtual void DelInput(Int_t index=-1){fInputsBufs.Del(index); fN[0]=fInputsBufs.Count();}
    virtual void DelOutput(Int_t index=-1){fOutputsBufs.Del(index); fN[1]=fOutputsBufs.Count();}
    virtual void DelParam(Int_t index=-1){fParamsBufs.Del(index); fN[2]=fParamsBufs.Count();}

    virtual QProcedure* Clone() const=0;

    Int_t GetNInputs() const{return fInputsBufs.Count();}
    Int_t GetNOutputs() const{return fOutputsBufs.Count();}
    Int_t GetNParams() const{return fParamsBufs.Count();}

    virtual void* GetProc() const=0;

    void SetInputBuf(Int_t index, Double_t *buf){fInputsBufs[index]=buf;}
    void SetOutputBuf(Int_t index, Double_t *buf){fOutputsBufs[index]=buf;}
    void SetParamBuf(Int_t index, Double_t *buf){fParamsBufs[index]=buf;}

    virtual void SetNInputs(Int_t n){fInputsBufs.RedimList(n,-1,NULL); fN[0]=fInputsBufs.Count();}
    virtual void SetNOutputs(Int_t n){fOutputsBufs.RedimList(n,-1,NULL); fN[1]=fOutputsBufs.Count();}
    virtual void SetNParams(Int_t n){fParamsBufs.RedimList(n,-1,NULL); fN[2]=fParamsBufs.Count();}

    virtual Bool_t Exec() const=0;

    friend Bool_t operator==(const QProcedure &lhs, const QProcedure &rhs);
  protected:
    QList<Double_t*> fInputsBufs;
    QList<Double_t*> fOutputsBufs;
    QList<Double_t*> fParamsBufs;
    Int_t fN[3];

    ClassDef(QProcedure,1) //The procedure base class
};

#include "debugger.h"

#endif
