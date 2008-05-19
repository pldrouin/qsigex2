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
    QProcessor(): TNamed(), fParams(new QList<Double_t>), fParamsNames(new QList<TString>) {}
    QProcessor(const QProcessor &rhs): TNamed(rhs), fParams(new QList<Double_t>(*rhs.fParams)), fParamsNames(new QList<TString>(*rhs.fParamsNames)) {}
    virtual ~QProcessor();

    void AddParam(const char *parname, const Double_t &value=0, Int_t index=-1);

    void DelParam(Int_t index=-1){fParams->Del(index); fParamsNames->Del(index);}
    void DelParam(const char *paramname);

    Int_t FindParamIndex(const char *paramname) const;

    Int_t GetNParams() const{return fParams->Count();}

    const char* GetParamName(Int_t i) const{return (*fParamsNames)[i];}

    virtual void Analyze()=0;
    virtual void Exec() const=0;
    virtual void InitProcess()=0;

    const QProcessor& operator=(const QProcessor &rhs);

    virtual void PrintAnalysisResults() const=0;

    void SetParam(Int_t index=-1, const Double_t &value=0){(*fParams)[index]=value;}
    void SetParam(const char *paramname, const Double_t &value=0);
    void SetParams(Double_t *params);

    virtual void TerminateProcess()=0;

  protected:
    QList<Double_t>   *fParams;          //-> Buffers for parameters values
    QList<TString>    *fParamsNames;     //-> Parameters names
  private:

    ClassDef(QProcessor,1)          //The TTree processor class
};

#include "debugger.h"

#endif
