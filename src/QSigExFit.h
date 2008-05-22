#ifndef _QSIGEXFIT_
#define _QSIGEXFIT_

#include "TMatrixDSym.h"
#include "QList.h"
#include "QProcessor.h"
#include "QProcDouble.h"
#include "QSigExFitParam.h"

class QSigExFit: public TObject
{
  public:
    QSigExFit(): TObject(), fQProcessor(NULL), fQPD(NULL), fParams(), fFCNError("Min Function Error",1.0), fFCNMin("Minimization Function Minimum",1.7e308), fCorMatrix(NULL) {}
    QSigExFit(const QSigExFit &rhs): TObject(rhs), fQProcessor(rhs.fQProcessor), fQPD(rhs.fQPD), fParams(rhs.fParams), fFCNError(rhs.fFCNError), fFCNMin(rhs.fFCNMin), fCorMatrix(NULL) {};
    virtual ~QSigExFit();

    void ExecProc() const{fQProcessor->Exec();}

    Int_t FindParamIndex(const char *paramname) const;

    virtual Double_t Fit()=0;

    const TMatrixDSym& GetCorMatrix() const{return *fCorMatrix;}
    const static QSigExFit& GetCurInstance(){return *fCurInstance;}
    const Double_t& GetFCNError() const{return fFCNError;}
    const Double_t& GetFCNMin() const{return fFCNMin;}
    const QProcDouble& GetProcOutput() const{return *fQPD;}

    void Init();
    virtual void InitFit()=0;

    QSigExFitParam& Param(Int_t index) const{return fParams[index];}
    QSigExFitParam& Param(const char* paramname) const;

    void SetFCNError(Double_t fcnerror){fFCNError=fcnerror;}
    static void SetParams(Double_t *params); //SHOULD ONLY BE CALLED DURING THE FIT
    void SetProcessor(QProcessor* processor){fQProcessor=processor; Init();}
    void SetProcOutput(const QProcDouble* procobj){fQPD=procobj;}

    const QSigExFit& operator=(const QSigExFit &rhs){TObject::operator=(rhs); return *this;}

  protected:
    Double_t& ParamFitVal(Int_t i){return (Double_t&)fParams[i];}
    Double_t& ParamMinusFitError(Int_t i){return fParams[i].MinusFitError();}
    Double_t& ParamPlusFitError(Int_t i){return fParams[i].PlusFitError();}

    QProcessor *fQProcessor; //!
    const QProcDouble *fQPD; //!
    QList<QSigExFitParam> fParams;
    QNamedVar<Double_t> fFCNError; //Minimization function error used to compute asymmetric errors
    QNamedVar<Double_t> fFCNMin;   //Minimum value reached for the minimization function
    TMatrixDSym *fCorMatrix;
    static const QSigExFit   *fCurInstance; //!
  private:
    ClassDef(QSigExFit,1)
};

#endif
