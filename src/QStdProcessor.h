#ifndef _QSTDPROCESSOR_
#define _QSTDPROCESSOR_

#include "TNamed.h"
#include "QProcessor.h"
#include "QList.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QStdProcessor: public QProcessor
{
  public:
    QStdProcessor(): QProcessor(), fParams(new QList<Double_t>), fParamsNames(new QList<TString>) {}
    QStdProcessor(const char* name, const char* title): QProcessor(name,title), fParams(new QList<Double_t>), fParamsNames(new QList<TString>) {}
    QStdProcessor(const QStdProcessor &rhs): QProcessor(rhs), fParams(new QList<Double_t>(*rhs.fParams)), fParamsNames(new QList<TString>(*rhs.fParamsNames)) {}
    virtual ~QStdProcessor();

    void AddParam(const char *parname, const Double_t &value=0., Int_t index=-1);

    void DelParam(Int_t index=-1){fParams->Del(index); fParamsNames->Del(index);}
    void DelParam(const char *paramname);

    Int_t FindParamIndex(const char *paramname) const{return (*fParamsNames).FindFirst(paramname);}

    Int_t GetNParams() const{return fParamsNames->Count();}

    const char* GetParamName(Int_t index) const{return (*fParamsNames)[index];}

    const QStdProcessor& operator=(const QStdProcessor &rhs);

    void SetParam(Int_t index=-1, const Double_t &value=0){(*fParams)[index]=value;}
    void SetParam(const char *paramname, const Double_t &value=0);
    void SetParams(Double_t *params);

  protected:
    QList<Double_t>   *fParams;          //-> Buffers for parameters values
    QList<TString>    *fParamsNames;     //-> Parameters names
  private:

    ClassDef(QStdProcessor,1) //QProcessor class for standard processors (virtual abstract base class)
};

#include "debugger.h"

#endif
