#ifndef _QTTREEPROCLIST_
#define _QTTREEPROCLIST_

#include "TNamed.h"
#include "QList.h"
#include "QTTreeProcessor.h"

class QTTreeProcList: public TNamed
{
  public:
    QTTreeProcList(): TNamed(), fQPL(new QList<TObject*>){}
    QTTreeProcList(const QTTreeProcList &rhs): TNamed(rhs), fQPL(new QList<TObject*>(*rhs.fQPL)){}
    virtual ~QTTreeProcList();

    void AddQTTreeProc(QTTreeProcessor *qttreeproc, Int_t index=-1){fQPL->Add(qttreeproc,index);}

    void Analyze();

    void DelQTTreeProcs(Int_t index=-1){fQPL->Del(index);}

    void Exec() const;

    Int_t GetNQTTreeProcs() const{return fQPL->Count();}

    void InitProcess();

    const QTTreeProcList& operator=(const QTTreeProcList &rhs);
    QTTreeProcessor& operator[](Int_t index) const{return *((QTTreeProcessor*)(*fQPL)[index]);}

    void TerminateProcess();

    void Browse(TBrowser *b);
    Bool_t IsFolder() const {return kTRUE;}
  protected:
    QList<TObject*> *fQPL;         //-> List of QTTreeProcessor objects
  private:
    ClassDef(QTTreeProcList,1)
};

extern const QTTreeProcList *gQTTreeProcList;

#endif
