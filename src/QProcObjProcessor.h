#ifndef _QPROCOBJPROCESSOR_
#define _QPROCOBJPROCESSOR_

#include "TFile.h"
#include "TLeaf.h"
#include "QStdProcessor.h"
#include "QFileUtils.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QProcObjProcessor: public QStdProcessor
{
  public:
    QProcObjProcessor(): QStdProcessor(), fIOIndices(new QList<QList<Int_t> >), fOOIndices(new QList<QList<Int_t> >), fObjsPDepends(new QList<QMask>), fIObjects(new QList<QProcObj*>), fOObjects(new QList<QProcObj*>) {}
    QProcObjProcessor(const char* name, const char* title): QStdProcessor(name,title), fIOIndices(new QList<QList<Int_t> >), fOOIndices(new QList<QList<Int_t> >), fObjsPDepends(new QList<QMask>), fIObjects(new QList<QProcObj*>), fOObjects(new QList<QProcObj*>) {}
    QProcObjProcessor(const QProcObjProcessor &rhs): QStdProcessor(rhs), fIOIndices(new QList<QList<Int_t> >(*rhs.fIOIndices)), fOOIndices(new QList<QList<Int_t> >(*rhs.fOOIndices)), fObjsPDepends(new QList<QMask>(*rhs.fObjsPDepends)), fIObjects(new QList<QProcObj*>(*rhs.fIObjects)), fOObjects(new QList<QProcObj*>(*rhs.fOObjects)) {}
    virtual ~QProcObjProcessor();

    Int_t AddProc(const char* name, const char* title=NULL, Int_t index=-1);
    Int_t AddProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname=NULL, Int_t index=-1);
    Int_t AddProc(const char *name, const char *title, const char *procname, Int_t index=-1);
    Int_t AddProc(const char *name, const char *title, void *proc, const char *procname=NULL, Int_t index=-1);

    void Analyze();

    void DelProc(const Int_t &index=-1){fProcs->Del(index);}
    void DelProc(const char *procname);

    void Exec() const;

    void InitProcess(Bool_t allocateparammem=kTRUE);

    const QProcObjProcessor& operator=(const QProcObjProcessor &rhs);

    void PrintAnalysisResults() const;
    void PrintProcesses(const UInt_t &level=0, const Bool_t &printdeps=kTRUE) const;

    void TerminateProcess();

    void Browse(TBrowser *b);

  protected:
    void BuildObjLists(){}
    void ClearObjLists(){}
    const QList<QProcObj*>& GetIObjList() const{return *fIObjects;}
    const QList<QProcObj*>& GetOObjList() const{return *fOObjects;}

  private:
    QList<QList<Int_t> > *fIOIndices;    //-> Indices of objects that are used as input for each process
    QList<QList<Int_t> > *fOOIndices;    //-> Indices of objects that are used as output for each process
    QList<QMask >             *fObjsPDepends; //-> Dependencies of processes on input objects
    QList<QProcObj*>         *fIObjects; //! Input objects 
    QList<QProcObj*>         *fOObjects; //! Output objects 

    //mutable QMask lExecpardiffs;            //! Modified parameters since the last call
    //mutable QMask lExecdepmods;             //! Required processes due to modified input objects
    //mutable Bool_t lExecrunall;
    //mutable Int_t lExeci;
    //mutable Int_t lExecj;
    //mutable QList<QProcObj*> lExecoobjects; //! List for output objects needing update
    //mutable QList<TObject*> lExecprocs;     //! List of needed processes

    mutable QList<Bool_t>	fNeededOO; //Needed output objects

    ClassDef(QProcObjProcessor,1) //QProcObj processor
};

#include "debugger.h"

#endif
