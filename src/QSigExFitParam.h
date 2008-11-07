#ifndef _QSIGEXFITPARAM_
#define _QSIGEXFITPARAM_

#include "TBrowser.h"
#include "QNamedVar.h"

class QSigExFitParam: public QNamedVar<Double_t>
{
  public:
    QSigExFitParam(const char* name): QNamedVar<Double_t>(name,0.), fStartVal(new QNamedVar<Double_t>("Start Value",0.)), fMinVal(new QNamedVar<Double_t>("Minimum Value",-0.5e7)), fMaxVal(new QNamedVar<Double_t>("Maximum Value",0.5e7)), fStepVal(new QNamedVar<Double_t>("Step Value",1.)), fFixed(new QNamedVar<Int_t>("Is Fixed",kFALSE)), fPlusFitError(new QNamedVar<Double_t>("Plus Fit Error",-1.0)), fMinusFitError(new QNamedVar<Double_t>("Minus Fit Error","-1.0")), fFreeParamIndex(new QNamedVar<Int_t>("Free Parameter Index",-1)) {}
    QSigExFitParam(): QNamedVar<Double_t>(), fStartVal(new QNamedVar<Double_t>("Start Value",0.)), fMinVal(new QNamedVar<Double_t>("Minimum Value",-0.5e7)), fMaxVal(new QNamedVar<Double_t>("Maximum Value",0.5e7)), fStepVal(new QNamedVar<Double_t>("Step Value",1.)), fFixed(new QNamedVar<Int_t>("Is Fixed",kFALSE)), fPlusFitError(new QNamedVar<Double_t>("Plus Fit Error",-1.0)), fMinusFitError(new QNamedVar<Double_t>("Minus Fit Error","-1.0")), fFreeParamIndex(new QNamedVar<Int_t>("Free Parameter Index",-1)) {}
    QSigExFitParam(const QSigExFitParam& rhs): QNamedVar<Double_t>(rhs), fStartVal(new QNamedVar<Double_t>(*rhs.fStartVal)), fMinVal(new QNamedVar<Double_t>(*rhs.fMinVal)), fMaxVal(new QNamedVar<Double_t>(*rhs.fMaxVal)), fStepVal(new QNamedVar<Double_t>(*rhs.fStepVal)), fFixed(new QNamedVar<Int_t>(*rhs.fFixed)), fPlusFitError(new QNamedVar<Double_t>(*rhs.fPlusFitError)), fMinusFitError(new QNamedVar<Double_t>(*rhs.fMinusFitError)), fFreeParamIndex(new QNamedVar<Int_t>(*rhs.fFreeParamIndex)) {}
    virtual ~QSigExFitParam();

    const Double_t& GetStartVal() const{return *fStartVal;}
    const Double_t& GetMinVal() const{return *fMinVal;}
    const Double_t& GetMaxVal() const{return *fMaxVal;}
    const Double_t& GetMinusFitError() const{return *fMinusFitError;}
    const Double_t& GetPlusFitError() const{return *fPlusFitError;}
    const Double_t& GetStepVal() const{return *fStepVal;}
    const Int_t& GetFreeParamIndex() const{return *fFreeParamIndex;}
    Int_t IsFixed() const{return *fFixed;}

    void Print(const Option_t* opt=NULL) const;

    void Setup(const Double_t &startval, const Double_t& stepval, const Double_t& minval=-0.5e7, const Double_t &maxval=0.5e7, Int_t fix=kFALSE){*fStartVal=startval; *fStepVal=stepval; *fMinVal=minval; *fMaxVal=maxval; *fFixed=fix; if((Double_t&)*fMinVal == (Double_t&)*fMaxVal && fix!=kTRUE) *fFixed=2;}
    void SetFix(Int_t fix=kTRUE){*fFixed=fix;}
    void SetStartVal(const Double_t& startval){*fStartVal=startval;}
    void SetMinVal(const Double_t& minval=-0.5e7){*fMinVal=minval; if((Double_t&)*fMinVal == (Double_t&)*fMaxVal && *fFixed!=kTRUE) *fFixed=2;}
    void SetMaxVal(const Double_t& maxval=0.5e7){*fMaxVal=maxval; if((Double_t&)*fMinVal == (Double_t&)*fMaxVal && *fFixed!=kTRUE) *fFixed=2;}
    void SetStepVal(const Double_t& stepval){*fStepVal=stepval;}
    void SetRange(const Double_t& minval=-0.5e7, const Double_t& maxval=0.5e7){*fMinVal=minval; *fMaxVal=maxval; if((Double_t&)*fMinVal == (Double_t&)*fMaxVal && *fFixed!=kTRUE) *fFixed=2;}

    const QSigExFitParam& operator=(const QSigExFitParam &rhs){QNamedVar<Double_t>::operator=(rhs); return *this;}

    void Browse(TBrowser *b);
    Bool_t IsFolder() const {return kTRUE;}

    friend class QSigExFit;
  protected:
    operator Double_t&(){return QNamedVar<Double_t>::operator Double_t&();}
    Double_t& MinusFitError(){return *fMinusFitError;}
    Double_t& PlusFitError(){return *fPlusFitError;}
    Int_t& FreeParamIndex(){return *fFreeParamIndex;}

    QNamedVar<Double_t> *fStartVal; //->
    QNamedVar<Double_t> *fMinVal; //->
    QNamedVar<Double_t> *fMaxVal; //->
    QNamedVar<Double_t> *fStepVal; //->
    QNamedVar<Int_t>    *fFixed; //->
    QNamedVar<Double_t> *fPlusFitError; //->
    QNamedVar<Double_t> *fMinusFitError; //->
    QNamedVar<Int_t>    *fFreeParamIndex; //->
  private:
    ClassDef(QSigExFitParam,1) //Parameter for QSigExFit object
};

#endif
