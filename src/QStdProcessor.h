#ifndef _QSTDPROCESSOR_
#define _QSTDPROCESSOR_

#include "TNamed.h"
#include "QList.h"
#include "QProcessor.h"
#include "QDependentProcs.h"
#include "QNamedProc.h"
#include "QMask.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QStdProcessor: public QProcessor
{
  public:
    QStdProcessor(): QProcessor(), fProcs(new QList<QNamedProc>), fLastParams(new QList<Double_t>), fLastExec(0,0), fProcsParDepends(new QList<QMask>) {}
    QStdProcessor(const char* name, const char* title): QProcessor(name,title), fProcs(new QList<QNamedProc>), fLastParams(new QList<Double_t>), fLastExec(0,0), fProcsParDepends(new QList<QMask>) {}
    QStdProcessor(const QStdProcessor &rhs): QProcessor(rhs), fProcs(new QList<QNamedProc>(*rhs.fProcs)), fLastParams(new QList<Double_t>), fLastExec(0,0), fProcsParDepends(new QList<QMask>(*rhs.fProcsParDepends)){}
    virtual ~QStdProcessor();

    virtual void Analyze();

    Int_t FindProcIndex(const char *procname) const;

    Int_t GetNProcs() const{return fProcs->Count();}

    QNamedProc& GetProc(const Int_t &index) const{return (*fProcs)[index];}
    QNamedProc& GetProc(const char *procname) const;

    const QStdProcessor& operator=(const QStdProcessor &rhs);

    void SetParamAddress(const Int_t &index, Double_t* const paddr=NULL);
    void SetParamAddress(const char *paramname, Double_t* const paddr=NULL){QProcessor::SetParamAddress(paramname,paddr);}

  protected:
    QList<QNamedProc> *fProcs;           //-> QNamedProc objects
    QList<Double_t>   *fLastParams;      //!  Parameters value from last Exec() call
    mutable TTimeStamp fLastExec;        //!  Time stamp from last Exec() call
    QList<QMask>           *fProcsParDepends; //-> Dependencies of processes on parameters
  private:

    ClassDef(QStdProcessor,1) //QProcessor class for standard processors (virtual abstract base class)
};

#include "debugger.h"

#endif
