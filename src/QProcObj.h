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
    const QProcObj& operator=(const QProcObj& rhs){fLastModified=rhs.fLastModified; return *this;}

  private:
    TTimeStamp fLastModified;

    ClassDef(QProcObj,1)
};

#endif
