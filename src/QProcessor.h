#ifndef _QPROCESSOR_
#define _QPROCESSOR_

#include "TNamed.h"
#include "QList.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QProcessor: public TNamed
{
  public:
    QProcessor(): TNamed(), fParams(new QList<Double_t*>), fOwnsParams(new QList<Bool_t>), fParamsNames(new QList<TString>), fParamsChildIndices(new QList<QList<Int_t> >), fChildParamsMapping(new QList<QList<Int_t> >), fVerbosity(0) {}
    QProcessor(const char* name, const char* title): TNamed(name,title), fParams(new QList<Double_t*>), fOwnsParams(new QList<Bool_t>), fParamsNames(new QList<TString>), fParamsChildIndices(new QList<QList<Int_t> >), fChildParamsMapping(new QList<QList<Int_t> >), fVerbosity(fDefVerbosity) {}
    QProcessor(const QProcessor &rhs);
    virtual ~QProcessor();

    Int_t GetNParams() const{return fParamsNames->Count();}

    virtual void Analyze()=0;
    virtual void Exec(const Bool_t &forceall=kFALSE) const=0;
    virtual void InitProcess(Bool_t allocateparammem=kTRUE)=0;

    void ClearParams();

    Int_t FindParamIndex(const char *paramname) const{return (*fParamsNames).FindFirst(paramname);}

    static const UInt_t& GetDefVerbosity(){return fDefVerbosity;}

    const Double_t& GetParam(Int_t index) const{return *((*fParams)[index]);}
    const Double_t& GetParam(const char *paramname) const;
#ifdef __CINT__
    Double_t ** GetParams() const{return fParams->GetArray();}
#else
    Double_t const* const* GetParams() const{return fParams->GetArray();}
#endif

    const char* GetParamName(Int_t index) const{return (*fParamsNames)[index];}

    const UInt_t& GetVerbosity() const{return fVerbosity;}

    const QProcessor& operator=(const QProcessor &rhs);

    virtual void PrintAnalysisResults() const=0;
    virtual void PrintProcesses(UInt_t level=0) const=0;

    static void SetDefVerbosity(UInt_t verbosity=0){fDefVerbosity=verbosity;}

    void SetParam(Int_t index=-1, const Double_t &value=0);
    virtual void SetParamAddress(Int_t index, Double_t *paddr=NULL);
    void SetParam(const char *paramname, const Double_t &value=0);
    void SetParamAddress(const char *paramname, Double_t *paddr=NULL);
    void SetParams(Double_t *params);

    virtual void SetVerbosity(UInt_t verbosity=0){fVerbosity=verbosity;}

    virtual void TerminateProcess()=0;

    virtual void Browse(TBrowser *b)=0;
    Bool_t IsFolder() const {return kTRUE;}

    enum eVerbosity {kShowExec=1, kShowExec2=2};

  protected:
    QList<Double_t*>     *fParams;             //-> Buffers for parameters values
    QList<Bool_t>        *fOwnsParams;         //-> Indicates which parameter buffers are owned by the instance
    QList<TString>       *fParamsNames;        //-> Parameters names
    QList<QList<Int_t> > *fParamsChildIndices; //-> List of child indices for each parameter
    QList<QList<Int_t> > *fChildParamsMapping; //-> For each parameter, parameter index for each child listed in fParamsChildIndices
  private:
    UInt_t fVerbosity;
    static UInt_t fDefVerbosity;

    ClassDef(QProcessor,1) //Virtual abstract base class for processor classes
};

#include "debugger.h"

#endif
