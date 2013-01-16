// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _QPROCOBJ_
#define _QPROCOBJ_

#include "Rtypes.h"
#include "TTimeStamp.h"

class QProcObj
{
  public:
    QProcObj(): fLastModified(0,0){}
    QProcObj(const QProcObj& qprocobj): fLastModified(qprocobj.fLastModified){}
    virtual ~QProcObj(){}
    virtual const TTimeStamp& GetTimeStamp() const{return fLastModified;}
    virtual const Bool_t& IsReadThreadSafe() const{return kTRUE;}
    virtual Bool_t NewerThan(const TTimeStamp &time) const;
    const QProcObj& operator=(const QProcObj& rhs){fLastModified=rhs.fLastModified; return *this;}
    virtual void UpdateModTime(){fLastModified.Set();}
    virtual void InitProcObj(){}
    virtual void TerminateProcObj(){}

  private:
    TTimeStamp fLastModified;
    ClassDef(QProcObj,1) //Object for QProcArgs arguments with modification time, init and terminate member functions
};

#endif
