#ifndef _QPROCOBJPROCESSOR_
#define _QPROCOBJPROCESSOR_

#include "TFile.h"
#include "TLeaf.h"
#include "QStdProcessor.h"
#include "QDependentProcs.h"
#include "QProcTree.h"
#include "QNamedProc.h"
#include "QFileUtils.h"
#include "QMask.h"
#include "QProgress.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QProcObjProcessor: public QStdProcessor
{
  public:
    QProcObjProcessor(): QStdProcessor(), fProcs(new QList<QNamedProc>), fLastParams(new QList<Double_t>), fLastExec(0,0), fIOIndices(new QList<QList<Int_t> >), fOOIndices(new QList<QList<Int_t> >), fProcsParDepends(new QList<QMask>), fObjsPDepends(new QList<QMask>), fIObjects(new QList<QProcObj*>), fOObjects(new QList<QProcObj*>) {}
    QProcObjProcessor(const char* name, const char* title): QStdProcessor(name,title), fProcs(new QList<QNamedProc>), fLastParams(new QList<Double_t>), fLastExec(0,0), fIOIndices(new QList<QList<Int_t> >), fOOIndices(new QList<QList<Int_t> >), fProcsParDepends(new QList<QMask>), fObjsPDepends(new QList<QMask>), fIObjects(new QList<QProcObj*>), fOObjects(new QList<QProcObj*>) {}
    QProcObjProcessor(const QProcObjProcessor &rhs): QStdProcessor(rhs), fProcs(new QList<QNamedProc>(*rhs.fProcs)), fLastParams(new QList<Double_t>), fLastExec(0,0), fIOIndices(new QList<QList<Int_t> >(*rhs.fIOIndices)), fOOIndices(new QList<QList<Int_t> >(*rhs.fOOIndices)), fProcsParDepends(new QList<QMask>(*rhs.fProcsParDepends)), fObjsPDepends(new QList<QMask>(*rhs.fObjsPDepends)), fIObjects(new QList<QProcObj*>(*rhs.fIObjects)), fOObjects(new QList<QProcObj*>(*rhs.fOObjects)) {}
    virtual ~QProcObjProcessor();

    void AddProc(const char* name, const char* title=NULL, Int_t index=-1);
    void AddProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname=NULL, Int_t index=-1);
    void AddProc(const char *name, const char *title, const char *procname, Int_t index=-1);
    void AddProc(const char *name, const char *title, void *proc, const char *procname=NULL, Int_t index=-1);

    void Analyze();

    void DelProc(Int_t index=-1){fProcs->Del(index);}
    void DelProc(const char *procname);

    void Exec() const;

    Int_t FindProcIndex(const char *procname) const;

    Int_t GetNProcs() const{return fProcs->Count();}

    QNamedProc& GetProc(Int_t index) const{return (*fProcs)[index];}
    QNamedProc& GetProc(const char *procname) const;

    void InitProcess();

    const QProcObjProcessor& operator=(const QProcObjProcessor &rhs);

    void PrintAnalysisResults() const;

    void TerminateProcess();

    void Browse(TBrowser *b);
    Bool_t IsFolder() const {return kTRUE;}

  protected:

  private:
    QList<QNamedProc> *fProcs;           //-> QNamedProc objects
    QList<Double_t>   *fLastParams;      //!  Parameters value from last Exec() call
    mutable TTimeStamp fLastExec;        //!  Time stamp from last Exec() call
    QList<QList<Int_t> > *fIOIndices;    //-> Indices of objects that are used as input for each process
    QList<QList<Int_t> > *fOOIndices;    //-> Indices of objects that are used as output for each process
    QList<QMask>           *fProcsParDepends; //-> Dependencies of processes on parameters
    QList<QMask >             *fObjsPDepends; //-> Dependencies of processes on input objects
    QList<QProcObj*>         *fIObjects; //! Input objects 
    QList<QProcObj*>         *fOObjects; //! Output objects 

    ClassDef(QProcObjProcessor,1) //QProcObj processor
};

#include "debugger.h"

#endif
