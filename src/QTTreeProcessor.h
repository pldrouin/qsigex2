#ifndef _QTTREEPROCESSOR_
#define _QTTREEPROCESSOR_

#include "QNamedProc.h"

#define DEBUG
#define DEBUG2

#include "debugger.h"

class QTTreeProcessor
{
  public:
    QTTreeProcessor(): fProcs(){}
    QTTreeProcessor(const QTTreeProcessor &rhs): fProcs(rhs.fProcs){}
    virtual ~QTTreeProcessor(){}

    void AddProc(const char* name=NULL, const char* title=NULL, Int_t index=-1);
    void AddProc(const char *name, const char *title, void (*proc)(Double_t**, Double_t**, Double_t**),const char *procname=NULL, Int_t index=-1);
    void AddProc(const char *name, const char *title, const char *procname, Int_t index=-1);
    void AddProc(const char *name, const char *title, void *proc, const char *procname=NULL, Int_t index=-1);

    void DelProc(Int_t index=-1){fProcs.Del(index);}
    void DelProc(const char *procname);

    Int_t FindProcIndex(const char *procname) const;

    Int_t GetNProcs(){return fProcs.Count();}

    QNamedProc& GetProc(Int_t i) const{return fProcs[i];}
    QNamedProc& GetProc(const char *procname) const;

    const QTTreeProcessor& operator=(const QTTreeProcessor &rhs){return *this;}
  private:
    QList<QNamedProc> fProcs;
    ClassDef(QTTreeProcessor,1)
};

#include "debugger.h"

#endif
