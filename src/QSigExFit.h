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
    QSigExFit(): TObject(), fQProcessor(NULL), fQPOutputs(), fParams(), fFCNError("Min Function Error",1.0), fFCNMin("Minimization Function Minimum",1.7e308), fCovMatrix(NULL), fVerbose(0) {}
    QSigExFit(const QSigExFit &rhs): TObject(rhs), fQProcessor(rhs.fQProcessor), fQPOutputs(rhs.fQPOutputs), fParams(rhs.fParams), fFCNError(rhs.fFCNError), fFCNMin(rhs.fFCNMin), fCovMatrix(new TMatrixDSym(*rhs.fCovMatrix)), fVerbose(0) {};
    virtual ~QSigExFit();

    void AddProcOutput(const QProcObj* procobj, Int_t index=-1){fQPOutputs.Add(const_cast<QProcObj*>(procobj),index);}
    void DelProcOutput(Int_t index=-1){fQPOutputs.Del(index);}

    void ExecProc() const{fQProcessor->Exec();}

    Int_t FindFreeParamIndex(const char *paramname) const;
    Int_t FindParamIndex(const char *paramname) const;

    virtual Double_t Fit()=0;

    const TMatrixDSym& GetCovMatrix() const{return *fCovMatrix;}
    const static QSigExFit& GetCurInstance(){return *fCurInstance;}
    const Double_t& GetFCNError() const{return fFCNError;}
    const Double_t& GetFCNMin() const{return fFCNMin;}
    const Int_t& GetNParams() const{return fParams.Count();}
    const QProcObj& GetProcOutput(Int_t index=-1) const{return *fQPOutputs[index];}

    void Init();
    virtual void InitFit()=0;

    QSigExFitParam& Param(Int_t index) const{return fParams[index];}
    QSigExFitParam& Param(const char* paramname) const;

    void PrintParams() const;

    void SetFCNError(Double_t fcnerror){fFCNError=fcnerror;}
    static void SetParams(Double_t *params); //SHOULD ONLY BE CALLED DURING THE FIT
    void SetProcessor(QProcessor* processor){fQProcessor=processor; Init();}
    void SetVerbose(Int_t verbose=0){fVerbose=verbose;}

    const QSigExFit& operator=(const QSigExFit &rhs){TObject::operator=(rhs); return *this;}

  protected:
    Double_t& ParamFitVal(Int_t i){return (Double_t&)fParams[i];}
    Double_t& ParamMinusFitError(Int_t i){return fParams[i].MinusFitError();}
    Double_t& ParamPlusFitError(Int_t i){return fParams[i].PlusFitError();}
    Int_t& ParamFreeParamIndex(Int_t i){return fParams[i].FreeParamIndex();}

    QProcessor *fQProcessor; //!
    QList<QProcObj*> fQPOutputs; //!
    QList<QSigExFitParam> fParams;
    QNamedVar<Double_t> fFCNError; //Minimization function error used to compute asymmetric errors
    QNamedVar<Double_t> fFCNMin;   //Minimum value reached for the minimization function
    TMatrixDSym *fCovMatrix;
    Int_t fVerbose;
    static const QSigExFit   *fCurInstance; //!
  private:
    ClassDef(QSigExFit,1) //Base fitter class that uses QProcessor objects
};

#endif
