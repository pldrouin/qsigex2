#ifndef _QSIGEXSYS_
#define _QSIGEXSYS_

#include "TBrowser.h"
#include "TNamed.h"
#include "TString.h"
#include "QList.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QSigExSys: public TNamed
{
  public:
    QSigExSys():TNamed(), fInputsNames(), fOutputsNames(), fArgsNames(), fProcName(), fInputs(NULL), fOutputs(NULL), fArgs(NULL){}
    QSigExSys(const char *name, const char *title=NULL): TNamed(name,title), fInputsNames(), fOutputsNames(), fArgsNames(), fProcName(), fInputs(NULL), fOutputs(NULL), fArgs(NULL){}

    void AddInput(const char *tree, const char *branch, Int_t index=-1);
    void AddOutput(const char *tree, const char *branch, Int_t index=-1);
    void AddArg(const char *name, Int_t index=-1);

    void DelInput(Int_t index){if(index>-1 && index<fInputsNames.Count()) fInputsNames.Del(index);}
    void DelOutput(Int_t index){if(index>-1 && index<fOutputsNames.Count()) fOutputsNames.Del(index);}
    void DelArg(Int_t index){if(index>-1 && index<fArgsNames.Count()) fArgsNames.Del(index);}

    const QNamedVar<TString>& GetInput(Int_t index) const{return fInputsNames[index];}
    const QNamedVar<TString>& GetOutput(Int_t index) const{return fOutputsNames[index];}
    const QNamedVar<TString>& GetArg(Int_t index) const{return fArgsNames[index];}

    void Browse(TBrowser *b);
    Bool_t IsFolder() const {return kTRUE;}
    const QSigExSys& operator=(const QSigExSys& rhs){TNamed::operator=(rhs); return *this;}

    virtual ~QSigExSys(){}

  protected:

  private:
    QSigExSys(const QSigExSys &sigexsys){}

    QList<QNamedVar<TString> > fInputsNames;
    QList<QNamedVar<TString> > fOutputsNames;
    QList<QNamedVar<TString> > fArgsNames;
    TString fProcName;
    Double_t *fInputs; //!
    Double_t *fOutputs; //!
    Double_t *fArgs; //!

  ClassDef(QSigExSys,1) //Generic named value template class
};

#include "debugger.h"

#endif
