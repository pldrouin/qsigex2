// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#ifndef _QPROCARGS_
#define _QPROCARGS_

#include "Rtypes.h"
#include "QList.h"
#include "QProcObj.h"
#include "QTypes.h"

#ifndef __CINT__
#ifndef QSFAST
#define QPATRY try{
#define QPACATCH } catch(Int_t e) {fprintf(stderr,"Error caught by QNamedProc '%s'\n",GetProcName()); throw e;}
#define CHECKIBTYPE(index,type) QPATRY CheckIBType(index,type)
#define CHECKOBTYPE(index,type) QPATRY CheckOBType(index,type)
#define QPACATCHRETI(type) QPACATCH return *(type*)fIBuffers[i];
#define QPACATCHRETO(type) QPACATCH return *(type*)fOBuffers[i];
#define QPACATCHRETP QPACATCH return *fPBuffers[i];
#else
#define QPATRY
#define QPACATCH
#define CHECKIBTYPE(index,type)
#define CHECKOBTYPE(index,type)
#define QPACATCHRETI(type)
#define QPACATCHRETO(type)
#define QPACATCHRETP
#endif
#else
#define QPATRY
#define QPACATCH
#define CHECKIBTYPE(index,type)
#define CHECKOBTYPE(index,type)
#define QPACATCHRETI(type)
#define QPACATCHRETO(type)
#define QPACATCHRETP
#endif

using namespace QTypes;

class QNamedProc;

class QProcArgs
{
  public:
  QProcArgs():fIBuffers(), fIObjects(), fOBuffers(), fOObjects(), fPBuffers(), fNamedProc(NULL){}
  QProcArgs(const QProcArgs& qprocargs):fIBuffers(qprocargs.fIBuffers), fIObjects(qprocargs.fIObjects), fOBuffers(qprocargs.fOBuffers), fOObjects(qprocargs.fOObjects), fPBuffers(qprocargs.fPBuffers), fNamedProc(qprocargs.fNamedProc){}
  virtual ~QProcArgs(){}
  virtual const QProcArgs& operator=(const QProcArgs& rhs);
  const QList<QProcObj const*>& GetListOfIObjs() const{return fIObjects;}
  const QList<QProcObj*>& GetListOfOObjs() const{return fOObjects;}
  const Int_t& GetNIVars() const{return fIBuffers.Count();}
  const Int_t& GetNIObjs() const{return fIObjects.Count();}
  const Int_t& GetNOVars() const{return fOBuffers.Count();}
  const Int_t& GetNOObjs() const{return fOObjects.Count();}
  const Int_t& GetNParams() const{return fPBuffers.Count();}
  const QNamedProc& GetProc() const{return *fNamedProc;}
  const Char_t* GetProcName() const;
  const Double_t& IVar(const Int_t &i) const{CHECKIBTYPE(i,kDouble); return *(Double_t*)fIBuffers[i]; QPACATCHRETI(Double_t)}
  const Bool_t& IVarB(const Int_t &i) const{CHECKIBTYPE(i,kBool); return *(Bool_t*)fIBuffers[i]; QPACATCHRETI(Bool_t)}
  const Char_t& IVarC(const Int_t &i) const{CHECKIBTYPE(i,kChar); return *(Char_t*)fIBuffers[i]; QPACATCHRETI(Char_t)}
  const Double_t& IVarD(const Int_t &i) const{CHECKIBTYPE(i,kDouble); return *(Double_t*)fIBuffers[i]; QPACATCHRETI(Double_t)}
  const Float_t& IVarF(const Int_t &i) const{CHECKIBTYPE(i,kFloat); return *(Float_t*)fIBuffers[i]; QPACATCHRETI(Float_t)}
  const Int_t& IVarI(const Int_t &i) const{CHECKIBTYPE(i,kInt); return *(Int_t*)fIBuffers[i]; QPACATCHRETI(Int_t)}
  const Long64_t& IVarL(const Int_t &i) const{CHECKIBTYPE(i,kLong64); return *(Long64_t*)fIBuffers[i]; QPACATCHRETI(Long64_t)}
  const Short_t& IVarS(const Int_t &i) const{CHECKIBTYPE(i,kShort); return *(Short_t*)fIBuffers[i]; QPACATCHRETI(Short_t)}
  const UChar_t& IVarUC(const Int_t &i) const{CHECKIBTYPE(i,kUChar); return *(UChar_t*)fIBuffers[i]; QPACATCHRETI(UChar_t)}
  const UInt_t& IVarUI(const Int_t &i) const{CHECKIBTYPE(i,kUInt); return *(UInt_t*)fIBuffers[i]; QPACATCHRETI(UInt_t)}
  const ULong64_t& IVarUL(const Int_t &i) const{CHECKIBTYPE(i,kULong64); return *(ULong64_t*)fIBuffers[i]; QPACATCHRETI(ULong64_t)}
  const UShort_t& IVarUS(const Int_t &i) const{CHECKIBTYPE(i,kUShort); return *(UShort_t*)fIBuffers[i]; QPACATCHRETI(UShort_t)}
  QProcObj const*& IObj(const Int_t &i) const{return fIObjects[i];}
  Double_t& OVar(const Int_t &i) const{CHECKOBTYPE(i,kDouble); return *(Double_t*)fOBuffers[i]; QPACATCHRETO(Double_t)}
  Bool_t& OVarB(const Int_t &i) const{CHECKOBTYPE(i,kBool); return *(Bool_t*)fOBuffers[i]; QPACATCHRETO(Bool_t)}
  Char_t& OVarC(const Int_t &i) const{CHECKOBTYPE(i,kChar); return *(Char_t*)fOBuffers[i]; QPACATCHRETO(Char_t)}
  Double_t& OVarD(const Int_t &i) const{CHECKOBTYPE(i,kDouble); return *(Double_t*)fOBuffers[i]; QPACATCHRETO(Double_t)}
  Float_t& OVarF(const Int_t &i) const{CHECKOBTYPE(i,kFloat); return *(Float_t*)fOBuffers[i]; QPACATCHRETO(Float_t)}
  Int_t& OVarI(const Int_t &i) const{CHECKOBTYPE(i,kInt); return *(Int_t*)fOBuffers[i]; QPACATCHRETO(Int_t)}
  Long64_t& OVarL(const Int_t &i) const{CHECKOBTYPE(i,kLong64); return *(Long64_t*)fOBuffers[i]; QPACATCHRETO(Long64_t)}
  Short_t& OVarS(const Int_t &i) const{CHECKOBTYPE(i,kShort); return *(Short_t*)fOBuffers[i]; QPACATCHRETO(Short_t)}
  UChar_t& OVarUC(const Int_t &i) const{CHECKOBTYPE(i,kUChar); return *(UChar_t*)fOBuffers[i]; QPACATCHRETO(UChar_t)}
  UInt_t& OVarUI(const Int_t &i) const{CHECKOBTYPE(i,kUInt); return *(UInt_t*)fOBuffers[i]; QPACATCHRETO(UInt_t)}
  ULong64_t& OVarUL(const Int_t &i) const{CHECKOBTYPE(i,kULong64); return *(ULong64_t*)fOBuffers[i]; QPACATCHRETO(ULong64_t)}
  UShort_t& OVarUS(const Int_t &i) const{CHECKOBTYPE(i,kShort); return *(UShort_t*)fOBuffers[i]; QPACATCHRETO(UShort_t)}
  QProcObj* const& OObj(const Int_t &i) const{return fOObjects[i];}
  const Double_t& Param(const Int_t &i) const{QPATRY return *fPBuffers[i]; QPACATCHRETP}
#ifdef __CINT__
  Double_t ** Params() const{return fPBuffers.GetArray();}
#else
  Double_t** const & Params() const{return fPBuffers.GetArray();}
#endif

  protected:
  void AddIVar(const Int_t &index=-1, void* const buf=NULL, const Int_t &type=-1){
    fIBuffers.Add(buf,index);
#ifndef QSFAST
    fIBTypes.Add(type,index);
#endif
  }
  void AddIObj(const Int_t &index=-1, QProcObj const* const obj=NULL){fIObjects.Add(obj,index);}
  void AddOVar(const Int_t &index=-1, void* const buf=NULL, const Int_t &type=-1){
    fOBuffers.Add(buf,index);
#ifndef QSFAST
    fOBTypes.Add(type,index);
#endif
  }
  void AddOObj(const Int_t &index=-1, QProcObj* const obj=NULL){fOObjects.Add(obj,index);}
  void AddParam(const Int_t &index=-1, Double_t* const buf=NULL){fPBuffers.Add(buf,index);}
  void ClearIVars(){
    fIBuffers.Clear();
#ifndef QSFAST
    fIBTypes.Clear();
#endif
  }
#ifndef QSFAST
  void CheckIBType(const Int_t &index, const Int_t &type) const{
    if(fIBTypes[index]!=type) {
      fprintf(stderr,"QProcArgs::CheckIBType: Error: Input variable %i has type ID '%s' instead of '%s'\n",index,GetTypeName(fIBTypes[index]),GetTypeName(type));
      throw 1;
    }
  }
  void CheckOBType(const Int_t &index, const Int_t &type) const{
    if(fOBTypes[index]!=type) {
      fprintf(stderr,"QProcArgs::CheckOBType: Error: Output variable %i has type ID '%s' instead of '%s'\n",index,GetTypeName(fOBTypes[index]),GetTypeName(type));
      throw 1;
    }
  }
#endif
  void ClearIObjs(){fIObjects.Clear();}
  void ClearOVars(){
    fOBuffers.Clear();
#ifndef QSFAST
    fOBTypes.Clear();
#endif
  }
  void ClearOObjs(){fOObjects.Clear();}
  void ClearParams(){fPBuffers.Clear();}
  void DelIVar(const Int_t &index=-1){
    fIBuffers.Del(index);
#ifndef QSFAST
    fIBTypes.Del(index);
#endif
  }
  void DelIObj(const Int_t &index=-1){fIObjects.Del(index);}
  void DelOVar(const Int_t &index=-1){
    fOBuffers.Del(index);
#ifndef QSFAST
    fOBTypes.Del(index);
#endif
  }
  void DelOObj(const Int_t &index=-1){fOObjects.Del(index);}
  void DelParam(const Int_t &index=-1){fPBuffers.Del(index);}
  void SetIVarPtr(const Int_t &index, void* const buf, const Int_t &type=-1){
    fIBuffers[index]=buf;
#ifndef QSFAST
    fIBTypes[index]=type;
#endif
  }
  void SetIObjPtr(const Int_t &index, QProcObj* const obj){fIObjects[index]=obj;}
  void SetOVarPtr(const Int_t &index, void* const buf, const Int_t &type=-1){
    fOBuffers[index]=buf;
#ifndef QSFAST
    fOBTypes[index]=type;
#endif
  }
  void SetOObjPtr(const Int_t &index, QProcObj* const obj){fOObjects[index]=obj;}
  void SetParamPtr(const Int_t &index, Double_t* const buf){fPBuffers[index]=buf;}
  void SetNIVars(const Int_t &n){
    fIBuffers.RedimList(n,-1,NULL);
#ifndef QSFAST
    fIBTypes.RedimList(n,-1,-1);
#endif
  }
  void SetNIObjs(const Int_t &n){fIObjects.RedimList(n,-1,NULL);}
  void SetNOVars(const Int_t &n){
    fOBuffers.RedimList(n,-1,NULL);
#ifndef QSFAST
    fOBTypes.RedimList(n,-1,-1);
#endif
  }
  void SetNOObjs(const Int_t &n){fOObjects.RedimList(n,-1,NULL);}
  void SetNParams(const Int_t &n){fPBuffers.RedimList(n,-1,NULL);}
  void SetNamedProcAddr(const QNamedProc *namedproc){fNamedProc=namedproc;}

  friend class QProcedure;
  friend Bool_t operator==(const QProcArgs &lhs, const QProcArgs &rhs);

  private:
  QList<void*> fIBuffers;
  QList<QProcObj const*> fIObjects;
  QList<void*> fOBuffers;
  QList<QProcObj*> fOObjects;
  QList<Double_t*> fPBuffers;
  const QNamedProc *fNamedProc;
#ifndef __CINT__
#ifndef QSFAST
  QList<Int_t> fIBTypes;
  QList<Int_t> fOBTypes;
#endif
#endif

  ClassDef(QProcArgs,0) //Encapsulated container for QProcedure arguments
};

#endif
