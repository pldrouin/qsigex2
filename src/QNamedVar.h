#ifndef _QNAMEDVAR_
#define _QNAMEDVAR_

#include <iostream>
#include "TObject.h"
#include "TString.h"
#include "TROOT.h"
#include "TClass.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

using namespace std;

template <typename U> class QNamedVar: public TObject
{
  public:
    QNamedVar():TObject(){}

    QNamedVar(const char *name, const U& val): TObject(), fName(name) {fVal=val;}

    QNamedVar(const char *name, const char *valstr): TObject(), fName(name) {SetValtoStr(fVal,valstr);}

    QNamedVar(const QNamedVar<U>& namedval): TObject(namedval), fName(namedval.fName), fVal(namedval.fVal) {}

    const QNamedVar& operator=(const QNamedVar& rhs){TObject::operator=(rhs); fName=rhs.fName; fVal=rhs.fVal; return *this;}

    const QNamedVar& operator=(const U& rhs){fVal=rhs; return *this;}

    const QNamedVar& operator=(const char *valstr) {SetValtoStr(fVal,valstr); return *this;}

    operator const U&() const{PRINTF6(this,"\tQNamedVar<",typeid(U).name(),">::operator ",typeid(U).name(),"() const\n") PRINTF3("Returned val=",fVal,"\n") return fVal;}
    virtual operator U&() {PRINTF6(this,"\tQNamedVar<",typeid(U).name(),">::operator ",typeid(U).name(),"() const\n") PRINTF3("Returned val=",fVal,"\n") return fVal;}

    void Copy(TObject &obj) const{TObject::Copy(obj); dynamic_cast<QNamedVar<U>&>(obj).fVal = fVal;}

    const char* GetName() const{return fName;}
    const char* GetTitle() const{fTitle=""; fTitle+=fVal; return fTitle;}

    ULong_t Hash() const{return fName.Hash();}

    void ls(Option_t *option="") const;

    void SetName(const char *name){fName=name;}

    Int_t Sizeof() const{return fName.Sizeof()+sizeof(U);}

    virtual ~QNamedVar(){}

    template<typename V> friend Bool_t operator==(const QNamedVar<V> &lhs, const QNamedVar<V> &rhs);

  protected:
    void SetValtoStr(TString &val, const char* str){val=str;}
    void SetValtoStr(Bool_t &val, const char* str){int i; sscanf(str,"%i",&i); val=i;}
    void SetValtoStr(char &val, const char* str){sscanf(str,"%c",&val);}
    void SetValtoStr(short int &val, const char* str){sscanf(str,"%hd",&val);}
    void SetValtoStr(unsigned short int &val, const char* str){sscanf(str,"%hu",&val);}
    void SetValtoStr(int &val, const char* str){sscanf(str,"%d",&val);}
    void SetValtoStr(unsigned int &val, const char* str){sscanf(str,"%u",&val);}
    void SetValtoStr(long int &val, const char* str){sscanf(str,"%ld",&val);}
    void SetValtoStr(unsigned long int &val, const char* str){sscanf(str,"%lu",&val);}
    void SetValtoStr(float &val, const char* str){PRINTF6(this,"\tQNamedVar<",typeid(U).name(),">::SetValtoStr(float &val, const char* str<",str,">)\n") sscanf(str,"%f",&val);}
    void SetValtoStr(double &val, const char* str){sscanf(str,"%lf",&val);}
    void SetValtoStr(long long int &val, const char* str){sscanf(str,"%lld",&val);}
    void SetValtoStr(unsigned long long int &val, const char* str){sscanf(str,"%llu",&val);}

  private:
    TString fName;
    U fVal;
    mutable TString fTitle; //!

  ClassDef(QNamedVar,1) //Generic named value template class
};

#include "debugger.h"

#include "QNamedVar_cxx.h"

#endif
