#ifndef _QPROCQOAHANDLER_
#define _QPROCQOAHANDLER_

#include "QList.h"
#include "QFileUtils.h"
#include "QProcArray.h"
#include "QProcQOA.h"

class QProcQOA;

class QProcQOAHandler
{
  public:
    QProcQOAHandler(){}
    virtual ~QProcQOAHandler(){}
    static QProcArray* LoadQOA(const char *arraylocation, const char *arrayname, QProcArray::omode openmode);
    friend class QProcQOA;
  protected:
    static QList<TString> fFiles;    //Filenames of QOA objects required by this class
    static QList<TObject*> fQOAObjs;     //QOA objects required by this class
    static QList<Int_t> fNObjReqQOA;     //Number of objects requiring the QOA objects
  private:
    static void UnloadQOA(QProcQOA *array);
    const QProcQOAHandler& operator=(const QProcQOAHandler &){return *this;}
    ClassDef(QProcQOAHandler,1)
};

#endif
