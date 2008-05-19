#ifndef _QPROCLIST_
#define _QPROCLIST_

#include "TNamed.h"
#include "QList.h"
#include "QProcessor.h"

class QProcList: public TNamed
{
  public:
    QProcList(): TNamed(), fQPL(new QList<TObject*>){}
    QProcList(const QProcList &rhs): TNamed(rhs), fQPL(new QList<TObject*>(*rhs.fQPL)){}
    virtual ~QProcList();

    void AddQProc(QProcessor *qproc, Int_t index=-1){fQPL->Add(qproc,index);}

    void Analyze();

    void DelQProcs(Int_t index=-1){fQPL->Del(index);}

    void Exec() const;

    Int_t GetNQProcs() const{return fQPL->Count();}

    void InitProcess();

    const QProcList& operator=(const QProcList &rhs);
    QProcessor& operator[](Int_t index) const{return *((QProcessor*)(*fQPL)[index]);}

    void TerminateProcess();

    void Browse(TBrowser *b);
    Bool_t IsFolder() const {return kTRUE;}
  protected:
    QList<TObject*> *fQPL;         //-> List of QProcessor objects
  private:
    ClassDef(QProcList,1)
};

extern const QProcList *gQProcList;

#endif
