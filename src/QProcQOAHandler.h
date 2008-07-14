#ifndef _QPROCQOAHANDLER_
#define _QPROCQOAHANDLER_

#include "QList.h"
#include "QFileUtils.h"
#include "QProcArray.h"
#include "QProcQOA.h"

class QProcQOAHandler
{
  public:
    QProcQOAHandler(){}
    virtual ~QProcQOAHandler(){}
    static QProcArray* LoadBranch(const char *treelocation, const char *branchname, QProcArray::omode openmode);
  protected:
    static QList<TObject*> fTBObjs;     //TBranch objects required by this class
  private:
    static void UnloadBranch(TBranch *branch);
    const QProcQOAHandler& operator=(const QProcQOAHandler &){return *this;}
    ClassDef(QProcQOAHandler,1)
};

#endif
