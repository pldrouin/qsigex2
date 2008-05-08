#ifndef _QPROCARGS_
#define _QPROCARGS_

#include "Rtypes.h"
#include "QList.h"
#include "QProcObj.h"

class QProcArgs
{
  public:
    QProcArgs():fIBuffers(), fIObjects(), fOBuffers(), fOObjects(), fPBuffers(){}
    QProcArgs(const QProcArgs& qprocargs):fIBuffers(qprocargs.fIBuffers), fIObjects(qprocargs.fIObjects), fOBuffers(qprocargs.fOBuffers), fOObjects(qprocargs.fOObjects), fPBuffers(qprocargs.fPBuffers){}
    virtual ~QProcArgs(){}
    virtual const QProcArgs& operator=(const QProcArgs& rhs);
    Int_t GetNIVars() const{return fIBuffers.Count();}
    Int_t GetNIObjs() const{return fIObjects.Count();}
    Int_t GetNOVars() const{return fOBuffers.Count();}
    Int_t GetNOObjs() const{return fOObjects.Count();}
    Int_t GetNParams() const{return fPBuffers.Count();}
    Double_t& IVar(Int_t i) const{return *fIBuffers[i];}
    QProcObj*& IObj(Int_t i) const{return fIObjects[i];}
    Double_t& OVar(Int_t i) const{return *fOBuffers[i];}
    QProcObj*& OObj(Int_t i) const{return fOObjects[i];}
    Double_t& Param(Int_t i) const{return *fPBuffers[i];}

  protected:
    void AddIVar(Int_t index=-1, Double_t *buf=NULL){fIBuffers.Add(buf,index);}
    void AddIObj(Int_t index=-1, QProcObj *obj=NULL){fIObjects.Add(obj,index);}
    void AddOVar(Int_t index=-1, Double_t *buf=NULL){fOBuffers.Add(buf,index);}
    void AddOObj(Int_t index=-1, QProcObj *obj=NULL){fOObjects.Add(obj,index);}
    void AddParam(Int_t index=-1, Double_t *buf=NULL){fPBuffers.Add(buf,index);}
    void ClearIVars(){fIBuffers.Clear();}
    void ClearIObjs(){fIObjects.Clear();}
    void ClearOVars(){fOBuffers.Clear();}
    void ClearOObjs(){fOObjects.Clear();}
    void ClearParams(){fPBuffers.Clear();}
    void DelIVar(Int_t index=-1){fIBuffers.Del(index);}
    void DelIObj(Int_t index=-1){fIObjects.Del(index);}
    void DelOVar(Int_t index=-1){fOBuffers.Del(index);}
    void DelOObj(Int_t index=-1){fOObjects.Del(index);}
    void DelParam(Int_t index=-1){fPBuffers.Del(index);}
    void SetIVarPtr(Int_t index, Double_t *buf){fIBuffers[index]=buf;}
    void SetOVarPtr(Int_t index, Double_t *buf){fOBuffers[index]=buf;}
    void SetParamPtr(Int_t index, Double_t *buf){fPBuffers[index]=buf;}
    void SetNIVars(Int_t n){fIBuffers.RedimList(n,-1,NULL);}
    void SetNIObjs(Int_t n){fIObjects.RedimList(n,-1,NULL);}
    void SetNOVars(Int_t n){fOBuffers.RedimList(n,-1,NULL);}
    void SetNOObjs(Int_t n){fOObjects.RedimList(n,-1,NULL);}
    void SetNParams(Int_t n){fPBuffers.RedimList(n,-1,NULL);}

    friend class QProcedure;
    friend Bool_t operator==(const QProcArgs &lhs, const QProcArgs &rhs);

  private:
    QList<Double_t*> fIBuffers;
    QList<QProcObj*> fIObjects;
    QList<Double_t*> fOBuffers;
    QList<QProcObj*> fOObjects;
    QList<Double_t*> fPBuffers;

    ClassDef(QProcArgs,0)
};

#endif
