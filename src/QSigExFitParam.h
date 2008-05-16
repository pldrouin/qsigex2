#ifndef _QSIGEXFITPARAM_
#define _QSIGEXFITPARAM_

#include "TBrowser.h"
#include "QNamedVar.h"

class QSigExFitParam: public QNamedVar<Double_t>
{
  public:
    QSigExFitParam(const char* name): QNamedVar<Double_t>(name,0.), fStartVal(new QNamedVar<Double_t>("Start Value",0.)), fMinVal(new QNamedVar<Double_t>("Minimum Value",-1.7e308)), fMaxVal(new QNamedVar<Double_t>("Maximum Value",1.7e308)), fStepVal(new QNamedVar<Double_t>("Step Value",1.)), fFixed(new QNamedVar<Bool_t>("Is Fixed",kFALSE)), fPlusFitError(new QNamedVar<Double_t>("Plus Fit Error",-1.0)), fMinusFitError(new QNamedVar<Double_t>("Minus Fit Error","-1.0")){}
    QSigExFitParam(): QNamedVar<Double_t>(), fStartVal(new QNamedVar<Double_t>("Start Value",0.)), fMinVal(new QNamedVar<Double_t>("Minimum Value",-1.7e308)), fMaxVal(new QNamedVar<Double_t>("Maximum Value",1.7e308)), fStepVal(new QNamedVar<Double_t>("Step Value",1.)), fFixed(new QNamedVar<Bool_t>("Is Fixed",kFALSE)), fPlusFitError(new QNamedVar<Double_t>("Plus Fit Error",-1.0)), fMinusFitError(new QNamedVar<Double_t>("Minus Fit Error","-1.0")){}
    QSigExFitParam(const QSigExFitParam& rhs): QNamedVar<Double_t>(rhs), fStartVal(new QNamedVar<Double_t>(*rhs.fStartVal)), fMinVal(new QNamedVar<Double_t>(*rhs.fMinVal)), fMaxVal(new QNamedVar<Double_t>(*rhs.fMaxVal)), fStepVal(new QNamedVar<Double_t>(*rhs.fStepVal)), fFixed(new QNamedVar<Bool_t>(*rhs.fFixed)), fPlusFitError(new QNamedVar<Double_t>(*rhs.fPlusFitError)), fMinusFitError(new QNamedVar<Double_t>(*rhs.fMinusFitError)){}
    virtual ~QSigExFitParam();

    const Double_t& GetStartVal(){return fStartVal->GetValue();}
    const Double_t& GetMinVal(){return fMinVal->GetValue();}
    const Double_t& GetMaxVal(){return fMaxVal->GetValue();}
    const Double_t& GetMinusFitError(){return fMinusFitError->GetValue();}
    const Double_t& GetPlusFitError(){return fPlusFitError->GetValue();}
    const Double_t& GetStepVal(){return fStepVal->GetValue();}
    Bool_t IsFixed(){return fFixed->GetValue();}

    void SetFix(Bool_t fix=kTRUE){*fFixed=fix;}
    void SetStartVal(const Double_t& startval){*fStartVal=startval;}
    void SetMinVal(const Double_t& minval=-1.7e308){*fMinVal=minval;}
    void SetMaxVal(const Double_t& maxval=1.7e308){*fMaxVal=maxval;}
    void SetStepVal(const Double_t& stepval){*fStepVal=stepval;}
    void SetRange(const Double_t& minval=-1.7e308, const Double_t& maxval=1.7e308){*fMinVal=minval; *fMaxVal=maxval;}

    const QSigExFitParam& operator=(const QSigExFitParam &rhs){QNamedVar<Double_t>::operator=(rhs); return *this;}

    void Browse(TBrowser *b);
    Bool_t IsFolder() const {return kTRUE;}

    friend class QSigExFit;
  protected:
    QNamedVar<Double_t> *fStartVal; //->
    QNamedVar<Double_t> *fMinVal; //->
    QNamedVar<Double_t> *fMaxVal; //->
    QNamedVar<Double_t> *fStepVal; //->
    QNamedVar<Bool_t>   *fFixed; //->
    QNamedVar<Double_t> *fPlusFitError; //->
    QNamedVar<Double_t> *fMinusFitError; //->
  private:
  ClassDef(QSigExFitParam,1)
};

#endif
