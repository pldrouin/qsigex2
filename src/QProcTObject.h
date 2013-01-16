// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

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
    TObject* const& GetObject() const{return fObject;}
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
