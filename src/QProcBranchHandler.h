#ifndef _QPROCBRANCHHANDLER_
#define _QPROCBRANCHHANDLER_

#include "TFile.h"
#include "TBranch.h"
#include "QList.h"
#include "QFileUtils.h"
#include "QProcArray.h"
#include "QProcTree.h"
#include "QProcTBranchWrapper.h"

class QProcBranchHandler
{
  public:
    QProcBranchHandler(){}
    virtual ~QProcBranchHandler(){}
    static QProcArray* LoadBranch(const char *treelocation, const char *branchname, QProcArray::omode openmode);
    friend class QProcBranch;
    friend class QProcTBranchWrapper;
  protected:
    static QList<TObject*> fIFiles;     //Input files loaded by this class
    static QList<Int_t> fNObjReqIFiles; //Number of objects requiring the input files
    static QList<TObject*> fOFiles;     //Output files loaded by this class
    static QList<Int_t> fNObjReqOFiles; //Number of objects requiring the output files
    static QList<TObject*> fTBObjs;     //TBranch objects required by this class
    static QList<TObject*> fQPTBWObjs;  //QProcTBranchWrapper objects associated with TBranch instances
    static QList<Int_t> fNObjReqTB;     //Number of objects requiring the TBranch objects
  private:
    static void UnloadBranch(TBranch *branch);
    const QProcBranchHandler& operator=(const QProcBranchHandler &){return *this;}
    ClassDef(QProcBranchHandler,1)
};

#endif
