#ifndef _QPROCLIST_
#define _QPROCLIST_

#include "QList.h"
#include "QProcessor.h"

class QProcList: public QProcessor
{
  public:
    QProcList(): QProcessor(), fQPL(new QList<TObject*>) {}
    QProcList(const char *name, const char *title): QProcessor(name,title), fQPL(new QList<TObject*>) {}
    QProcList(const QProcList &rhs): QProcessor(rhs), fQPL(new QList<TObject*>(*rhs.fQPL)) {}

    virtual ~QProcList();

    void AddQProc(QProcessor *qproc, Int_t index=-1){fQPL->Add(qproc,index);}

    void Analyze();

    void DelQProcs(Int_t index=-1){fQPL->Del(index);}

    void Exec(const Bool_t &forceall=kFALSE) const;

    Int_t GetNQProcs() const{return fQPL->Count();}

    void InitProcess(Bool_t allocateparammem=kTRUE);

    void PrintAnalysisResults() const;
    void PrintProcesses(UInt_t level=0) const;

    const QProcList& operator=(const QProcList &rhs);
    QProcessor& operator[](Int_t index) const{return *((QProcessor*)(*fQPL)[index]);}

    void SetParamAddress(Int_t index, Double_t *paddr=NULL);
    void SetParamAddress(const char *paramname, Double_t *paddr=NULL){QProcessor::SetParamAddress(paramname,paddr);}

    void TerminateProcess();

    void Browse(TBrowser *b);
  protected:
    QList<TObject*> *fQPL;         //-> List of QProcessor objects
  private:
    ClassDef(QProcList,1) //Processor of processor objects
};

extern const QProcList *gQProcList;

#endif
