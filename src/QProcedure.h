#ifndef _QPROCEDURE_
#define _QPROCEDURE_

#include "Rtypes.h"
#include "QList.h"
#include "QProcArgs.h"

#include "debugger.h"

class QProcedure
{
  public:
    QProcedure(): fArgs(){}
    QProcedure(const QProcedure &rhs): fArgs(rhs.fArgs){}
    virtual ~QProcedure(){}
    virtual const QProcedure& operator=(const QProcedure &rhs){fArgs=rhs.fArgs; return *this;}

    virtual void AddIVar(Int_t index=-1, Double_t *buf=NULL){fArgs.AddIVar(index,buf);}
    virtual void AddIObj(Int_t index=-1, QProcObj *obj=NULL){fArgs.AddIObj(index,obj);}
    virtual void AddOVar(Int_t index=-1, Double_t *buf=NULL){fArgs.AddOVar(index,buf);}
    virtual void AddOObj(Int_t index=-1, QProcObj *obj=NULL){fArgs.AddOObj(index,obj);}
    virtual void AddParam(Int_t index=-1, Double_t *buf=NULL){fArgs.AddParam(index,buf);}

    void ClearIVars(){fArgs.ClearIVars();}
    void ClearIObjs(){fArgs.ClearIObjs();}
    void ClearOVars(){fArgs.ClearOVars();}
    void ClearOObjs(){fArgs.ClearOObjs();}
    void ClearParams(){fArgs.ClearParams();}

    virtual QProcedure* Clone() const=0;

    void DelIVar(Int_t index=-1){fArgs.DelIVar(index);}
    void DelIObj(Int_t index=-1){fArgs.DelIObj(index);}
    void DelOVar(Int_t index=-1){fArgs.DelOVar(index);}
    void DelOObj(Int_t index=-1){fArgs.DelOObj(index);}
    void DelParam(Int_t index=-1){fArgs.DelParam(index);}

    const Int_t& GetNIVars() const{return fArgs.GetNIVars();}
    const Int_t& GetNIObjs() const{return fArgs.GetNIObjs();}
    const Int_t& GetNOVars() const{return fArgs.GetNOVars();}
    const Int_t& GetNOObjs() const{return fArgs.GetNOObjs();}
    const Int_t& GetNParams() const{return fArgs.GetNParams();}

    const Double_t& IVar(Int_t i) const{return fArgs.IVar(i);}
    const QProcObj* IObj(Int_t i) const{return fArgs.IObj(i);}
    Double_t& OVar(Int_t i) const{return fArgs.OVar(i);}
    QProcObj* OObj(Int_t i) const{return fArgs.OObj(i);}
    const Double_t& Param(Int_t i) const{return fArgs.Param(i);}

    void SetIVarPtr(Int_t index, Double_t *buf){fArgs.SetIVarPtr(index,buf);}
    void SetIObjPtr(Int_t index, QProcObj *obj){fArgs.SetIObjPtr(index,obj);}
    void SetOVarPtr(Int_t index, Double_t *buf){fArgs.SetOVarPtr(index,buf);}
    void SetOObjPtr(Int_t index, QProcObj *obj){fArgs.SetOObjPtr(index,obj);}
    void SetParamPtr(Int_t index, Double_t *buf){fArgs.SetParamPtr(index,buf);}

    void SetNIVars(Int_t n){fArgs.SetNIVars(n);}
    void SetNIObjs(Int_t n){fArgs.SetNIObjs(n);}
    void SetNOVars(Int_t n){fArgs.SetNOVars(n);}
    void SetNOObjs(Int_t n){fArgs.SetNOObjs(n);}
    void SetNParams(Int_t n){fArgs.SetNParams(n);}

    virtual Bool_t Exec()=0;

    friend class QNamedProc;
    friend Bool_t operator==(const QProcedure &lhs, const QProcedure &rhs){return (lhs.fArgs==rhs.fArgs);}
  protected:
    QProcArgs fArgs;

    ClassDef(QProcedure,1) //The procedure base class
};

#include "debugger.h"

#endif
