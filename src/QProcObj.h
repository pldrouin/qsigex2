#ifndef _QPROCOBJ_
#define _QPROCOBJ_

#include "Rtypes.h"
#include "TTimeStamp.h"

class QProcObj
{
  public:
    QProcObj(): fLastModified(){}
    QProcObj(const QProcObj& qprocobj): fLastModified(qprocobj.fLastModified){}
    virtual ~QProcObj(){}
    const TTimeStamp& GetTimeStamp(){return fLastModified;}
    Bool_t NewerThan(const TTimeStamp &time) const;
    const QProcObj& operator=(const QProcObj& rhs){fLastModified=rhs.fLastModified; return *this;}
    void UpdateModTime(){fLastModified.Set();}
    virtual void InitProcObj(){}
    virtual void TerminateProcObj(){}

  private:
    TTimeStamp fLastModified;

    ClassDef(QProcObj,1)
};

#endif
