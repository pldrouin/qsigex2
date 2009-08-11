#ifndef _QPROCTOBJECT_
#define _QPROCTOBJECT_

#include "QProcObj.h"

class QProcTObject: public QProcObj
{
  public:
    QProcTObject(): QProcObj(), fObject(NULL){}
    QProcTObject(TObject* rhs): QProcObj(), fObject(rhs){}
    QProcTObject(const QProcTObject &rhs): QProcObj(rhs), fObject(rhs.fObject){}
    virtual ~QProcTObject(){}
    void InitProcObj(){}
    TObject* GetObject() const{return fObject;}
    const Bool_t& IsReadThreadSafe() const{return kTRUE;}
    operator TObject*() const{return fObject;}
    operator const TObject*() const{return fObject;}
    const QProcTObject& operator=(const QProcTObject &rhs){fObject=rhs.fObject; return *this;}
    const QProcTObject& operator=(TObject*& rhs){fObject=rhs; return *this;}
  protected:
    TObject* fObject;
    ClassDef(QProcTObject,1) //TObject* with QProcObj properties
};

#endif
