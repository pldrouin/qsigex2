#ifndef _QPROCESSOR_
#define _QPROCESSOR_

#include "TNamed.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QProcessor: public TNamed
{
  public:
    QProcessor(): TNamed(), fVerbosity(0) {}
    QProcessor(const char* name, const char* title): TNamed(name,title), fVerbosity(0) {}
    QProcessor(const QProcessor &rhs): TNamed(rhs), fVerbosity(0) {}
    virtual ~QProcessor(){}

    virtual Int_t GetNParams() const=0;

    virtual const char* GetParamName(Int_t index) const=0;

    virtual void Analyze()=0;
    virtual void Exec() const=0;
    virtual void InitProcess()=0;

    const UInt_t& GetVerbosity() const{return fVerbosity;}

    const QProcessor& operator=(const QProcessor &rhs){TNamed::operator=(rhs); return *this;}

    virtual void PrintAnalysisResults() const=0;

    virtual void SetParam(Int_t index=-1, const Double_t &value=0)=0;
    virtual void SetParam(const char *paramname, const Double_t &value=0)=0;
    virtual void SetParams(Double_t *params)=0;

    virtual void SetVerbosity(UInt_t verbosity=0){fVerbosity=verbosity;}

    virtual void TerminateProcess()=0;

    virtual void Browse(TBrowser *b)=0;
    Bool_t IsFolder() const {return kTRUE;}

    enum eVerbosity {kShowExec=1};

  protected:
  private:
    UInt_t fVerbosity;

    ClassDef(QProcessor,1) //Virtual abstract base class for processor classes
};

#include "debugger.h"

#endif
