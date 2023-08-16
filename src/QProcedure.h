// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#ifndef _QPROCEDURE_
#define _QPROCEDURE_

#include "Rtypes.h"
#include "QList.h"
#include "QProcArgs.h"

#include "debugger.h"

class QNamedProc;

class QProcedure
{
  public:
    QProcedure(): fArgs(){}
    QProcedure(const QProcedure &rhs): fArgs(rhs.fArgs){}
    virtual ~QProcedure(){}
    virtual const QProcedure& operator=(const QProcedure &rhs){fArgs=rhs.fArgs; return *this;}

    virtual void AddIVar(const Int_t &index=-1, void* const buf=NULL){fArgs.AddIVar(index,buf);}
    virtual void AddIObj(const Int_t &index=-1, QProcObj const* const obj=NULL){fArgs.AddIObj(index,obj);}
    virtual void AddOVar(const Int_t &index=-1, void* const buf=NULL){fArgs.AddOVar(index,buf);}
    virtual void AddOObj(const Int_t &index=-1, QProcObj* const obj=NULL){fArgs.AddOObj(index,obj);}
    virtual void AddParam(const Int_t &index=-1, Double_t* const buf=NULL){fArgs.AddParam(index,buf);}

    void ClearIVars(){fArgs.ClearIVars();}
    void ClearIObjs(){fArgs.ClearIObjs();}
    void ClearOVars(){fArgs.ClearOVars();}
    void ClearOObjs(){fArgs.ClearOObjs();}
    void ClearParams(){fArgs.ClearParams();}

    virtual QProcedure* Clone() const=0;

    void DelIVar(const Int_t &index=-1){fArgs.DelIVar(index);}
    void DelIObj(const Int_t &index=-1){fArgs.DelIObj(index);}
    void DelOVar(const Int_t &index=-1){fArgs.DelOVar(index);}
    void DelOObj(const Int_t &index=-1){fArgs.DelOObj(index);}
    void DelParam(const Int_t &index=-1){fArgs.DelParam(index);}

    const Int_t& GetNIVars() const{return fArgs.GetNIVars();}
    const Int_t& GetNIObjs() const{return fArgs.GetNIObjs();}
    const Int_t& GetNOVars() const{return fArgs.GetNOVars();}
    const Int_t& GetNOObjs() const{return fArgs.GetNOObjs();}
    const Int_t& GetNParams() const{return fArgs.GetNParams();}

    const QProcObj* IObj(const Int_t &i) const{return fArgs.IObj(i);}
    QProcObj* OObj(const Int_t &i) const{return fArgs.OObj(i);}
    Double_t const& Param(const Int_t &i) const{return fArgs.Param(i);}

    void SetIVarPtr(const Int_t &index, void* const buf, const Int_t &type=-1){fArgs.SetIVarPtr(index,buf,type);}
    void SetIObjPtr(const Int_t &index, QProcObj* const obj){fArgs.SetIObjPtr(index,obj);}
    void SetOVarPtr(const Int_t &index, void* const buf, const Int_t &type=-1){fArgs.SetOVarPtr(index,buf,type);}
    void SetOObjPtr(const Int_t &index, QProcObj* const obj){fArgs.SetOObjPtr(index,obj);}
    void SetParamPtr(const Int_t &index, Double_t* const buf){fArgs.SetParamPtr(index,buf);}

    void SetNIVars(const Int_t &n){fArgs.SetNIVars(n);}
    void SetNIObjs(const Int_t &n){fArgs.SetNIObjs(n);}
    void SetNOVars(const Int_t &n){fArgs.SetNOVars(n);}
    void SetNOObjs(const Int_t &n){fArgs.SetNOObjs(n);}
    void SetNParams(const Int_t &n){fArgs.SetNParams(n);}

    void SetNamedProcAddr(const QNamedProc *namedproc){fArgs.SetNamedProcAddr(namedproc);}

    virtual Bool_t Exec()=0;

    friend class QNamedProc;
    friend Bool_t operator==(const QProcedure &lhs, const QProcedure &rhs){return (lhs.fArgs==rhs.fArgs);}
  protected:
    QProcArgs fArgs;

    ClassDef(QProcedure,1) //The procedure base class
};

#include "debugger.h"

#endif
