// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#ifndef _QPROCESSOR_
#define _QPROCESSOR_

#include "TNamed.h"
#include "QList.h"
#include "QMask.h"
#include "QDepTree.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QProcessor: public TNamed
{
  public:
    QProcessor(): TNamed(), fParams(new QList<Double_t*>), fOwnsParams(new QList<Bool_t>), fParamsNames(new QList<TString>), fParamsChildIndices(new QList<QList<Int_t> >), fChildParamsMapping(new QList<QList<Int_t> >), fForceExecAll(kFALSE), fVerbosity(fDefVerbosity), fPProcessing(kTRUE) {}
    QProcessor(const char* name, const char* title): TNamed(name,title), fParams(new QList<Double_t*>), fOwnsParams(new QList<Bool_t>), fParamsNames(new QList<TString>), fParamsChildIndices(new QList<QList<Int_t> >), fChildParamsMapping(new QList<QList<Int_t> >), fForceExecAll(kFALSE), fVerbosity(fDefVerbosity), fPProcessing(kTRUE) {}
    QProcessor(const QProcessor &rhs);
    virtual ~QProcessor();

    const Int_t &GetNParams() const{return fParamsNames->Count();}

    virtual void Analyze()=0;
    virtual void Exec() const=0;
    virtual void InitProcess(Bool_t allocateparammem=kTRUE)=0;
    virtual void UpdateProcessFlags()=0;

    virtual void ClearParams();

    void CopyParamAddress(const Int_t &fromidx, const Int_t &toidx);

    Int_t FindParamIndex(const char *paramname) const{return (*fParamsNames).FindFirst(paramname);}

    static const UInt_t& GetDefVerbosity(){return fDefVerbosity;}

    const Bool_t& GetForceExecAll() const{return fForceExecAll;}

    const Double_t& GetParam(const Int_t &index) const{return *((*fParams)[index]);}
    const Double_t& GetParam(const char *paramname) const;
    const Bool_t& GetParamOwnsBuffer(const Int_t &index) const{return (*fOwnsParams)[index];}
#ifdef __CINT__
    Double_t ** GetParams() const{return fParams->GetArray();}
#else
    Double_t const* const* GetParams() const{return fParams->GetArray();}
#endif

    const char* GetParamName(const Int_t &index) const{return (*fParamsNames)[index];}

    const Bool_t& GetPProcessing() const{return fPProcessing;}
    const UInt_t& GetVerbosity() const{return fVerbosity;}

    virtual void KillThreads(){}

    const QProcessor& operator=(const QProcessor &rhs);

    virtual void PrintAnalysisResults() const=0;
    virtual void PrintProcesses(const UInt_t &level=0, const Bool_t &printdeps=kTRUE) const=0;

    static void SetDefVerbosity(const UInt_t &verbosity=0){fDefVerbosity=verbosity;}

    virtual void SetForceExecAll(const Bool_t &forceexecall=kFALSE){fForceExecAll=forceexecall;}

    void SetParam(const Int_t &index, const Double_t &value=0);
    void SetParam(const char *paramname, const Double_t &value=0);
    virtual void SetParamActive(const Int_t &index, const Bool_t &active=kTRUE)=0;
    void SetParamActive(const char *paramname, const Bool_t &active=kTRUE);
    virtual void SetParamAddress(const Int_t &index, Double_t* const paddr=NULL);
    virtual void SetParamAddress(const char *paramname, Double_t* const paddr=NULL);
    void SetParams(Double_t const* const params);

    virtual void SetPProcessing(const Bool_t &pprocessing=kFALSE){fPProcessing=pprocessing;}
    virtual void SetVerbosity(const UInt_t &verbosity=0){fVerbosity=verbosity;}

    virtual void TerminateProcess()=0;

    virtual void Browse(TBrowser *b)=0;
    Bool_t IsFolder() const {return kTRUE;}

    enum eVerbosity {kShowExec=1, kShowExec2=2};

    friend class QProcList;

  protected:
    virtual void BuildObjLists()=0;
    virtual void ClearObjLists()=0;
    virtual const QList<QProcObj*>& GetIObjList() const=0;
    virtual const QList<QProcObj*>& GetOObjList() const=0;
    QList<Double_t*>     *fParams;             //-> Buffers for parameters values
    QList<Bool_t>        *fOwnsParams;         //-> Indicates which parameter buffers are owned by the instance
    QList<TString>       *fParamsNames;        //-> Parameters names
    QList<QList<Int_t> > *fParamsChildIndices; //-> List of child indices for each parameter
    QList<QList<Int_t> > *fChildParamsMapping; //-> For each parameter, parameter index for each child listed in fParamsChildIndices
    Bool_t fForceExecAll;
  private:
    UInt_t fVerbosity;
    Bool_t fPProcessing;
    static UInt_t fDefVerbosity;

    ClassDef(QProcessor,1) //Virtual abstract base class for processor classes
};

#include "debugger.h"

#endif
