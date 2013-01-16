// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

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
    QProcObjProcessor(): QStdProcessor(), fIOIndices(new QList<QList<Int_t> >), fOOIndices(new QList<QList<Int_t> >), fObjsPDepends(new QList<QMask>), fAObjsPDepends(fObjsPDepends), fIObjects(new QList<QProcObj*>), fAIObjects(new QList<QProcObj*>), fAAIObjects(fAIObjects), fOObjects(new QList<QProcObj*>), fActiveAIO(NULL), fLastActiveAIO(NULL) {}
    QProcObjProcessor(const char* name, const char* title): QStdProcessor(name,title), fIOIndices(new QList<QList<Int_t> >), fOOIndices(new QList<QList<Int_t> >), fObjsPDepends(new QList<QMask>), fAObjsPDepends(fObjsPDepends), fIObjects(new QList<QProcObj*>), fAIObjects(new QList<QProcObj*>), fAAIObjects(fAIObjects), fOObjects(new QList<QProcObj*>), fActiveAIO(NULL), fLastActiveAIO(NULL) {}
    QProcObjProcessor(const QProcObjProcessor &rhs): QStdProcessor(rhs), fIOIndices(new QList<QList<Int_t> >(*rhs.fIOIndices)), fOOIndices(new QList<QList<Int_t> >(*rhs.fOOIndices)), fObjsPDepends(new QList<QMask>(*rhs.fObjsPDepends)), fAObjsPDepends(rhs.fAObjsPDepends==rhs.fObjsPDepends?fObjsPDepends:new QList<QMask>(*rhs.fAObjsPDepends)), fIObjects(new QList<QProcObj*>(*rhs.fIObjects)), fAIObjects(new QList<QProcObj*>(*rhs.fAIObjects)), fAAIObjects(rhs.fAAIObjects==rhs.fAIObjects?fAIObjects:new QList<QProcObj*>(*rhs.fAAIObjects)), fOObjects(new QList<QProcObj*>(*rhs.fOObjects)), fActiveAIO(rhs.fActiveAIO?new QMask(*rhs.fActiveAIO):NULL), fLastActiveAIO(rhs.fLastActiveAIO?new QMask(*rhs.fLastActiveAIO):NULL) {}
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
    void UpdateProcessFlags();

    const QProcObjProcessor& operator=(const QProcObjProcessor &rhs);

    void PrintAnalysisResults() const;
    void PrintProcesses(const UInt_t &level=0, const Bool_t &printdeps=kTRUE) const;

    void SetAIOActive(QProcObj *const obj, const Bool_t &active=kTRUE);

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
    QList<QMask >             *fObjsPDepends; //-> Dependencies of processes on absolute input objects
    QList<QMask>         *fAObjsPDepends; //! Dependencies of processes on active absolute input objects. fAObjsPDepends==fObjsPDepends if all absolute input objects are active
    QList<QProcObj*>         *fIObjects; //! Input objects 
    QList<QProcObj*>         *fAIObjects; //! Absolute input objects 
    QList<QProcObj*>         *fAAIObjects; //! Active absolute input objects. fAAIObjects==fAIObjects if all absolute input objects are active 
    QList<QProcObj*>         *fOObjects; //! Output objects 
    QMask   		    *fActiveAIO; //! Indicates which absolute input objects are active within this processor (i.e. not the other processors). NULL if all absolute input objects are active 
    QMask   	        *fLastActiveAIO; //! Indicates which absolute input objects are active at the moment of the last call to UpdateProcessFlags. NULL if all absolute input objects are active 

    mutable QMask lExecpardiffs;            //! Modified parameters since the last call
    mutable QMask lExecdepmods;             //! Required processes due to modified input objects
    //mutable Bool_t lExecrunall;
    //mutable Int_t lExeci;
    //mutable Int_t lExecj;
    mutable QList<QProcObj*> lExecoobjects; //! List for output objects needing update
    mutable QList<TObject*> lExecprocs;     //! List of needed processes

    mutable QList<Bool_t>	fNeededOO; //Needed output objects

    ClassDef(QProcObjProcessor,1) //QProcObj processor
};

#include "debugger.h"

#endif
