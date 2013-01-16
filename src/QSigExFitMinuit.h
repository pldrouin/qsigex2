// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _QSIGEXFITMINUIT_
#define _QSIGEXFITMINUIT_

#include "G__ci.h"
#include "TMinuit.h"
#include "TMatrixDSym.h"
#include "QSigExFit.h"

class QSigExFitMinuit: public QSigExFit
{
  public:
    QSigExFitMinuit(): QSigExFit(), fMinuit(NULL), fCompiledFunc(NULL), fInterpretedFunc(NULL), fMinimName("Minimizer Name","MIGrad"), fMinimArgs(new QList<QNamedVar<Double_t> >), fMinosMaxCalls("Minos Max Calls",-1), fMinuitStrategy("Minuit Strategy",0), fMinuitAccuracy("Minuit Accuracy",-1), fMinuitStatus("Minuit Status","") {}
    QSigExFitMinuit(const QSigExFitMinuit &rhs): QSigExFit(rhs), fMinuit(NULL), fCompiledFunc(rhs.fCompiledFunc), fInterpretedFunc(rhs.fInterpretedFunc), fMinimName(rhs.fMinimName), fMinimArgs(new QList<QNamedVar<Double_t> >(*rhs.fMinimArgs)), fMinosMaxCalls(rhs.fMinosMaxCalls), fMinuitStrategy("Minuit Strategy",rhs.fMinuitStrategy), fMinuitAccuracy("Minuit Accuracy",rhs.fMinuitAccuracy), fMinuitStatus(rhs.fMinuitStatus) {};
    virtual ~QSigExFitMinuit();

    Double_t EvalFCN() const;

    Int_t FindMinimArg(const char* name) const;

    Double_t Fit(Bool_t fituncerts=kTRUE);

    const Double_t& GetMinimArg(Int_t index) const{return (*fMinimArgs)[index];}
    const char* GetMinimName() const
    {
#if (__GNUC__ >= 4)
      return (TString&)fMinimName;
#else
      return (TString&)const_cast<QNamedVar<TString>&>(fMinimName);
#endif
    }
    const char* GetMinuitStatus() const{return (const TString&)fMinuitStatus;}

    void InitFit();
    void TerminateFit();

    void SetMinuitAccuracy(const double& accuracy){fMinuitAccuracy=accuracy;}

    void SetFCN(void (*fcn)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t));
    void SetFCN(void* fcn);

    void SetMinimArg(Int_t index, const char* name, const Double_t& value){(*fMinimArgs)[index].SetName(name); (*fMinimArgs)[index]=value;}
    void SetMinimName(const char* minimname){fMinimName=minimname;}
    void SetMinosMaxCalls(Int_t ncalls){fMinosMaxCalls=ncalls;}
    void SetNMinimArgs(Int_t n){fMinimArgs->RedimList(n);}

    void SetStrategy(const Int_t &strat=1){fMinuitStrategy=strat;}

    const QSigExFitMinuit& operator=(const QSigExFitMinuit &rhs){QSigExFit::operator=(rhs); return *this;}

    void Browse(TBrowser *b);

  protected:
    TMinuit *fMinuit;              //!
    void (*fCompiledFunc)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t); //! Pointer to a compiled minimization function
    void *fInterpretedFunc;                                                 //! Pointer to an interpreted minimization function
    QNamedVar<TString> fMinimName; //Minimizer name
    QList<QNamedVar<Double_t> > *fMinimArgs; //->Minimizer arguments
    QNamedVar<Int_t> fMinosMaxCalls;  //Maximum number of calls of MINOS
    QNamedVar<Int_t> fMinuitStrategy; //Minuit strategy
    QNamedVar<Double_t> fMinuitAccuracy; //Minuit accuracy
    QNamedVar<TString> fMinuitStatus; //TMinuit Status
  private:
    ClassDef(QSigExFitMinuit,1) //QSigExFit using TMinuit minimizer
};

#endif
