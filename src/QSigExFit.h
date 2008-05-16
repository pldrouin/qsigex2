#ifndef _QSIGEXFIT_
#define _QSIGEXFIT_

#include "TMinuit.h"
#include "QTTreeProcList.h"
#include "QSigExFitParam.h"

class QSigExFit: public TNamed
{
  public:
    QSigExFit(): TNamed(), fQTPL(NULL), fCompiledFunc(NULL), fInterpretedFunc(NULL), fParams(), fMinuit(NULL), fFCNError("Min Function Error",1.0), fMinimName("Minimizer Name","MIGrad"), fMinimArgs(new QList<QNamedVar<Double_t> >), fMinosMaxCalls("Minos Max Calls",-1), fMinuitStatus("Minuit Status",""), fFCNMin("Minimization Function Minimum",1.7e308){}
    QSigExFit(const QSigExFit &rhs): TNamed(rhs), fQTPL(rhs.fQTPL), fCompiledFunc(NULL), fInterpretedFunc(NULL), fParams(rhs.fParams), fMinuit(NULL), fFCNError(rhs.fFCNError), fMinimName(rhs.fMinimName), fMinimArgs(new QList<QNamedVar<Double_t> >(*rhs.fMinimArgs)), fMinosMaxCalls(rhs.fMinosMaxCalls), fMinuitStatus(rhs.fMinuitStatus), fFCNMin(rhs.fFCNMin){};
    virtual ~QSigExFit(){if(fMinuit) delete fMinuit; fMinuit=NULL;};

    void Init();
    void InitFit();

    Int_t FindMinimArg(const char* name);

    Double_t Fit();

    const Double_t& GetFCNError(){return fFCNError.GetValue();}
    const Double_t& GetFCNMin(){return fFCNMin.GetValue();}
    const Double_t& GetMinimArg(Int_t index){return (*fMinimArgs)[index].GetValue();}
    const char* GetMinimName(){return fMinimName.GetValue();}
    const Int_t& GetMinosMaxCalls(){return fMinosMaxCalls.GetValue();}
    const char* GetMinuitStatus(){return fMinuitStatus.GetValue();}

    QSigExFitParam& Param(Int_t index){return fParams[index];}

    void SetFCN(void (*fcn)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t));
    void SetFCN(void* fcn);

    void SetFCNError(Double_t fcnerror){fFCNError=fcnerror;}
    void SetMinimArg(Int_t index, const char* name, const Double_t& value){(*fMinimArgs)[index].SetName(name); (*fMinimArgs)[index]=value;}
    void SetMinimName(const char* minimname){fMinimName=minimname;}
    void SetMinosMaxCalls(Int_t ncalls){fMinosMaxCalls=ncalls;}
    void SetNMinimArgs(Int_t n){fMinimArgs->RedimList(n);}
    void SetProcessor(const QTTreeProcList* processor){fQTPL=processor; Init();}

    const QSigExFit& operator=(const QSigExFit &rhs){TNamed::operator=(rhs); return *this;}

    void Browse(TBrowser *b);
    Bool_t IsFolder() const {return kTRUE;}
  protected:
    const QTTreeProcList *fQTPL; //!
    void (*fCompiledFunc)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t); //! Pointer to a compiled minimization function
    void *fInterpretedFunc;  //! Pointer to an interpreted minimization function
    QList<QSigExFitParam> fParams;
    TMinuit *fMinuit;              //!
    QNamedVar<Double_t> fFCNError; //Minimization function error used to compute asymmetric errors
    QNamedVar<TString> fMinimName; //Minimizer name
    QList<QNamedVar<Double_t> > *fMinimArgs; //->Minimizer arguments
    QNamedVar<Int_t> fMinosMaxCalls;  //Maximum number of calls of MINOS
    QNamedVar<TString> fMinuitStatus; //TMinuit Status
    QNamedVar<Double_t> fFCNMin;      //Minimum value reached for the minimization function
  private:
    ClassDef(QSigExFit,1)
};

#endif
