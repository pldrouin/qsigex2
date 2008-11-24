#ifndef _QSIGEXFITMCMC_
#define _QSIGEXFITMCMC_

#include "G__ci.h"
#include "TMatrixDSym.h"
#include "TRandom3.h"
#include "QSigExFit.h"
#include "QProcBranchHandler.h"
#include "QProcQOAHandler.h"
#include "TDirectory.h"

class QSigExFitMCMC: public QSigExFit
{
  public:
    QSigExFitMCMC(): QSigExFit(), fCompiledFunc(NULL), fNBurnInIterations("Number of Burn-In Iterations",100000), fNParsingIterations("Number of Parsing Iterations",100000), fParVals(NULL), fParJumps(NULL), fParOANames(new QList<QNamedVar<TString> >), fParOAIndices(new QList<Int_t>), fParOArrays(new QList<QProcArray*>){}
    QSigExFitMCMC(const QSigExFitMCMC &rhs): QSigExFit(rhs), fCompiledFunc(rhs.fCompiledFunc), fNBurnInIterations(rhs.fNBurnInIterations), fNParsingIterations(rhs.fNParsingIterations), fParVals(NULL), fParOANames(new QList<QNamedVar<TString> >(*rhs.fParOANames)), fParOAIndices(new QList<Int_t>(*rhs.fParOAIndices)), fParOArrays(new QList<QProcArray*>){};
    virtual ~QSigExFitMCMC();

    void AddParOutput(const Int_t index, const char *name, const char *title);
    void AddParOutput(const char *paramname, const char *name, const char *title);
    void DelParOutput(const Int_t index);
    void DelParOutput(const char *paramname);

    Double_t EvalFCN() const;

    Double_t Fit(Bool_t fituncerts=kTRUE);

    const Int_t& GetNParOutputs(){return fParOANames->Count();}
    QNamedVar<TString>& GetParOutputNameTitle(Int_t index) const{return (*fParOANames)[index];}

    void InitFit();

    void SetFCN(void (*fcn)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t));

    void SetNBurnInIterations(Int_t niters){fNBurnInIterations=niters;}
    void SetNParsingIterations(Int_t niters){fNParsingIterations=niters;}

    void TerminateFit();

    const QSigExFitMCMC& operator=(const QSigExFitMCMC &rhs);

    void Browse(TBrowser *b);

  protected:
    void (*fCompiledFunc)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t); //! Pointer to a compiled function
    QNamedVar<Int_t> fNBurnInIterations;  //Number of iterations used for burn-in phase
    QNamedVar<Int_t> fNParsingIterations;  //Number of iterations used to parse parameter space
    Double_t *fParVals; //! Internal parameter values
    Double_t *fParJumps; //!
    QList<QNamedVar<TString> > *fParOANames; //->
    QList<Int_t> *fParOAIndices; //->
    QList<QProcArray*>  *fParOArrays; //!
  private:
    ClassDef(QSigExFitMCMC,1) //QSigExFit using Markov chain Monte Carlo with Metropolis-Hastings algorithm
};

#endif
