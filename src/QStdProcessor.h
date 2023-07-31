// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#ifndef _QSTDPROCESSOR_
#define _QSTDPROCESSOR_

#include "TNamed.h"
#include "strdiffer.h"
#include "QList.h"
#include "QProcessor.h"
#include "QNamedProc.h"
#include "QMask.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QStdProcessor: public QProcessor
{
  public:
    QStdProcessor(): QProcessor(), fProcs(new QList<QNamedProc>), fAParams(fParams), fActiveParams(NULL), fLastActiveParams(NULL), fAParIndexMapping(NULL), fLastParams(new QList<Double_t>), fLastExec(0,0), fProcsParDepends(new QList<QMask>), fProcsAParDepends(fProcsParDepends) {}
    QStdProcessor(const char* name, const char* title): QProcessor(name,title), fProcs(new QList<QNamedProc>), fAParams(fParams), fActiveParams(NULL), fLastActiveParams(NULL), fAParIndexMapping(NULL), fLastParams(new QList<Double_t>), fLastExec(0,0), fProcsParDepends(new QList<QMask>) , fProcsAParDepends(fProcsParDepends){}
    QStdProcessor(const QStdProcessor &rhs): QProcessor(rhs), fProcs(new QList<QNamedProc>(*rhs.fProcs)), fAParams(rhs.fAParams==rhs.fParams?fParams:new QList<Double_t*>(*rhs.fAParams)), fActiveParams(rhs.fActiveParams?new QMask(*rhs.fActiveParams):NULL), fLastActiveParams(rhs.fLastActiveParams?new QMask(*rhs.fLastActiveParams):NULL), fAParIndexMapping(rhs.fAParIndexMapping?new QList<Int_t>(*rhs.fAParIndexMapping):NULL), fLastParams(new QList<Double_t>), fLastExec(0,0), fProcsParDepends(new QList<QMask>(*rhs.fProcsParDepends)), fProcsAParDepends(rhs.fProcsAParDepends==rhs.fProcsParDepends?fProcsParDepends:new QList<QMask>(*rhs.fProcsAParDepends)) {}
    virtual ~QStdProcessor();

    virtual void Analyze();

    void ClearParams();

    Int_t FindProcIndex(const char *procname) const;

    Int_t GetNProcs() const{return fProcs->Count();}

    QNamedProc& GetProc(const Int_t &index) const{return (*fProcs)[index];}
    QNamedProc& GetProc(const char *procname) const;

    const QStdProcessor& operator=(const QStdProcessor &rhs);

    void SetParamActive(const Int_t &index, const Bool_t &active=kTRUE);
    void SetParamActive(const char *paramname, const Bool_t &active=kTRUE){QProcessor::SetParamActive(paramname,active);}

    void SetParamAddress(const Int_t &index, Double_t* const paddr=NULL);
    void SetParamAddress(const char *paramname, Double_t* const paddr=NULL){QProcessor::SetParamAddress(paramname,paddr);}

  protected:
    QList<QNamedProc> *fProcs;                 //-> QNamedProc objects
    QList<Double_t*>  *fAParams;               //! Buffers for active parameters values. fAParams==fParams if all parameters are active
    QMask             *fActiveParams;          //! Indicates which parameters are active within this processor (i.e. not the other processors). NULL if all params are active
    QMask             *fLastActiveParams;      //! Indicates which parameters were active at the moment of the last call to UpdateProcessFlags. NULL if all params were previously active
    QList<Int_t>      *fAParIndexMapping;      //! Active parameter indices for each parameter
    QList<Double_t>   *fLastParams;            //!  Parameters value from last Exec() call
    mutable TTimeStamp fLastExec;              //!  Time stamp from last Exec() call
    QList<QMask>           *fProcsParDepends;  //-> Dependencies of processes on parameters
    QList<QMask>	   *fProcsAParDepends; //! Dependencies of processes on active parameters. fProcsAParDepends==fProcsParDepends if all parameters are active
  private:

    ClassDef(QStdProcessor,1) //QProcessor class for standard processors (virtual abstract base class)
};

#include "debugger.h"

#endif
