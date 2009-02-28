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
    const QList<QProcObj*>& GetListOfIObjs() const{return fIObjects;}
    const QList<QProcObj*>& GetListOfOObjs() const{return fOObjects;}
    const Int_t& GetNIVars() const{return fIBuffers.Count();}
    const Int_t& GetNIObjs() const{return fIObjects.Count();}
    const Int_t& GetNOVars() const{return fOBuffers.Count();}
    const Int_t& GetNOObjs() const{return fOObjects.Count();}
    const Int_t& GetNParams() const{return fPBuffers.Count();}
    const Double_t& IVar(const Int_t &i) const{return *fIBuffers[i];}
#ifdef __CINT__
    Double_t ** IVars() const{return fIBuffers.GetArray();}
#else
    Double_t const* const* IVars() const{return fIBuffers.GetArray();}
#endif
    const QProcObj* IObj(const Int_t &i) const{return fIObjects[i];}
#ifdef __CINT__
    QProcObj ** IObjs() const{return fIObjects.GetArray();}
#else
    QProcObj const* const* IObjs() const{return fIObjects.GetArray();}
#endif
    Double_t& OVar(const Int_t &i) const{return *fOBuffers[i];}
#ifdef __CINT__
    Double_t ** OVars() const{return fOBuffers.GetArray();}
#else
    Double_t const* const* OVars() const{return fOBuffers.GetArray();}
#endif
    QProcObj* OObj(const Int_t &i) const{return fOObjects[i];}
#ifdef __CINT__
    QProcObj ** OObjs() const{return fOObjects.GetArray();}
#else
    QProcObj const* const* OObjs() const{return fOObjects.GetArray();}
#endif
    const Double_t& Param(const Int_t &i) const{return *fPBuffers[i];}
#ifdef __CINT__
    Double_t ** Params() const{return fPBuffers.GetArray();}
#else
    Double_t const* const* Params() const{return fPBuffers.GetArray();}
#endif

  protected:
    void AddIVar(const Int_t &index=-1, Double_t* const buf=NULL){fIBuffers.Add(buf,index);}
    void AddIObj(const Int_t &index=-1, QProcObj* const obj=NULL){fIObjects.Add(obj,index);}
    void AddOVar(const Int_t &index=-1, Double_t* const buf=NULL){fOBuffers.Add(buf,index);}
    void AddOObj(const Int_t &index=-1, QProcObj* const obj=NULL){fOObjects.Add(obj,index);}
    void AddParam(const Int_t &index=-1, Double_t* const buf=NULL){fPBuffers.Add(buf,index);}
    void ClearIVars(){fIBuffers.Clear();}
    void ClearIObjs(){fIObjects.Clear();}
    void ClearOVars(){fOBuffers.Clear();}
    void ClearOObjs(){fOObjects.Clear();}
    void ClearParams(){fPBuffers.Clear();}
    void DelIVar(const Int_t &index=-1){fIBuffers.Del(index);}
    void DelIObj(const Int_t &index=-1){fIObjects.Del(index);}
    void DelOVar(const Int_t &index=-1){fOBuffers.Del(index);}
    void DelOObj(const Int_t &index=-1){fOObjects.Del(index);}
    void DelParam(const Int_t &index=-1){fPBuffers.Del(index);}
    void SetIVarPtr(const Int_t &index, Double_t* const buf){fIBuffers[index]=buf;}
    void SetIObjPtr(const Int_t &index, QProcObj* const obj){fIObjects[index]=obj;}
    void SetOVarPtr(const Int_t &index, Double_t* const buf){fOBuffers[index]=buf;}
    void SetOObjPtr(const Int_t &index, QProcObj* const obj){fOObjects[index]=obj;}
    void SetParamPtr(const Int_t &index, Double_t* const buf){fPBuffers[index]=buf;}
    void SetNIVars(const Int_t &n){fIBuffers.RedimList(n,-1,NULL);}
    void SetNIObjs(const Int_t &n){fIObjects.RedimList(n,-1,NULL);}
    void SetNOVars(const Int_t &n){fOBuffers.RedimList(n,-1,NULL);}
    void SetNOObjs(const Int_t &n){fOObjects.RedimList(n,-1,NULL);}
    void SetNParams(const Int_t &n){fPBuffers.RedimList(n,-1,NULL);}

    friend class QProcedure;
    friend Bool_t operator==(const QProcArgs &lhs, const QProcArgs &rhs);

  private:
    QList<Double_t*> fIBuffers;
    QList<QProcObj*> fIObjects;
    QList<Double_t*> fOBuffers;
    QList<QProcObj*> fOObjects;
    QList<Double_t*> fPBuffers;

    ClassDef(QProcArgs,0) //Encapsulated container for QProcedure arguments
};

#endif
