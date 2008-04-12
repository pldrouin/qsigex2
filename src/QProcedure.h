#ifndef _QPROCEDURE_
#define _QPROCEDURE_

#include "Rtypes.h"
#include "QList.h"

#include "debugger.h"

class QProcedure
{
  public:
    QProcedure(): fInputsBufs(), fOutputsBufs(), fParamsBufs(){}
    QProcedure(const QProcedure &rhs): fInputsBufs(rhs.fInputsBufs), fOutputsBufs(rhs.fOutputsBufs), fParamsBufs(rhs.fParamsBufs){}
    virtual ~QProcedure(){}
    const QProcedure& operator=(const QProcedure &rhs){fInputsBufs=rhs.fInputsBufs; fOutputsBufs=rhs.fOutputsBufs; fParamsBufs=rhs.fParamsBufs; return *this;}

    virtual void AddInput(Int_t index=-1, Double_t *buf=NULL){fInputsBufs.Add(buf,index);}
    virtual void AddOutput(Int_t index=-1, Double_t *buf=NULL){fOutputsBufs.Add(buf,index);}
    virtual void AddParam(Int_t index=-1, Double_t *buf=NULL){fParamsBufs.Add(buf,index);}

    virtual void DelInput(Int_t index=-1){fInputsBufs.Del(index);}
    virtual void DelOutput(Int_t index=-1){fOutputsBufs.Del(index);}
    virtual void DelParam(Int_t index=-1){fParamsBufs.Del(index);}

    virtual QProcedure* Clone() const=0;

    Int_t GetNInputs() const{return fInputsBufs.Count();}
    Int_t GetNOutputs() const{return fOutputsBufs.Count();}
    Int_t GetNParams() const{return fParamsBufs.Count();}

    void SetInputBuf(Int_t index, Double_t *buf){fInputsBufs[index]=buf;}
    void SetOutputBuf(Int_t index, Double_t *buf){fOutputsBufs[index]=buf;}
    void SetParamBuf(Int_t index, Double_t *buf){fParamsBufs[index]=buf;}

    virtual void SetNInputs(Int_t n){fInputsBufs.RedimList(n,-1,NULL);}
    virtual void SetNOutputs(Int_t n){fOutputsBufs.RedimList(n,-1,NULL);}
    virtual void SetNParams(Int_t n){fParamsBufs.RedimList(n,-1,NULL);}

    virtual void Exec() const=0;

    friend Bool_t operator==(const QProcedure &lhs, const QProcedure &rhs);
  protected:
    QList<Double_t*> fInputsBufs;
    QList<Double_t*> fOutputsBufs;
    QList<Double_t*> fParamsBufs;

    ClassDef(QProcedure,1)
};

#include "debugger.h"

#endif
