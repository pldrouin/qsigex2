#ifndef _QPROCLIST_
#define _QPROCLIST_

#include "QList.h"
#include "QProcessor.h"

class QProcList: public QProcessor
{
  public:
    QProcList(): QProcessor(), fQPL(new QList<TObject*>), fParamsNames(new QList<TString>), fParamsMapping(new QList<QList<Int_t> >) {}
    QProcList(const char *name, const char *title): QProcessor(name,title), fQPL(new QList<TObject*>), fParamsNames(new QList<TString>), fParamsMapping(new QList<QList<Int_t> >) {}
    QProcList(const QProcList &rhs): QProcessor(rhs), fQPL(new QList<TObject*>(*rhs.fQPL)), fParamsNames(new QList<TString>(*rhs.fParamsNames)), fParamsMapping(new QList<QList<Int_t> >(*rhs.fParamsMapping)) {}
    virtual ~QProcList();

    void AddQProc(QProcessor *qproc, Int_t index=-1){fQPL->Add(qproc,index);}

    void Analyze();

    void DelQProcs(Int_t index=-1){fQPL->Del(index);}

    void Exec() const;

    Int_t FindParamIndex(const char *paramname) const{return (*fParamsNames).FindFirst(paramname);}

    Int_t GetNParams() const{return fParamsNames->Count();}
    Int_t GetNQProcs() const{return fQPL->Count();}

    const char* GetParamName(Int_t index) const{return (*fParamsNames)[index];}

    void InitProcess();

    void PrintAnalysisResults() const;

    void SetParam(Int_t index=-1, const Double_t &value=0);
    void SetParam(const char *paramname, const Double_t &value=0);
    void SetParams(Double_t *params);

    const QProcList& operator=(const QProcList &rhs);
    QProcessor& operator[](Int_t index) const{return *((QProcessor*)(*fQPL)[index]);}

    void TerminateProcess();

    void Browse(TBrowser *b);
  protected:
    QList<TObject*> *fQPL;         //-> List of QProcessor objects
    QList<TString>  *fParamsNames; //-> List of Parameter Names
    QList<QList<Int_t> > *fParamsMapping; //-> Maps QProcessor objects parameters to QProcList parameters
  private:
    ClassDef(QProcList,1) //Processor of processor objects
};

extern const QProcList *gQProcList;

#endif
