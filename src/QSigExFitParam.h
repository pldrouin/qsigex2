// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _QSIGEXFITPARAM_
#define _QSIGEXFITPARAM_

#include <cstdio>
#include "TBrowser.h"
#include "TObject.h"
#include "TString.h"
#include "QNamedVar.h"

#ifndef __CINT__
template <typename U> class QList;
#endif

class QSigExFitParam: public TObject
{
  public:
    QSigExFitParam(const char *name, const Int_t &index): TObject(), fName(name), fIndex(index), fData(new QSFPData(this)), fMaster(NULL)
#ifndef __CINT__
  , fSlaves(NULL)
#endif
    {}
    QSigExFitParam(): TObject(), fName(), fIndex(-1), fData(new QSFPData(this)), fMaster(NULL)
#ifndef __CINT__
  , fSlaves(NULL)
#endif
    {}
    QSigExFitParam(const QSigExFitParam& rhs);
    virtual ~QSigExFitParam();

    const Double_t& GetStartVal() const{return *fData->fStartVal;}
    const Double_t& GetMinVal() const{return *fData->fMinVal;}
    const Double_t& GetMaxVal() const{return *fData->fMaxVal;}
    const Double_t& GetMinusFitError() const{return *fData->fMinusFitError;}
    const char* GetName() const{return fName;}
    const Double_t& GetPlusFitError() const{return *fData->fPlusFitError;}
    const Double_t& GetStepVal() const{return *fData->fStepVal;}
    const Int_t& GetFreeParamIndex() const{return *fData->fFreeParamIndex;}
    const Double_t& GetValue() const{return fData->fValue;}
    ULong_t Hash() const{return fName.Hash();}
    const Int_t& IsFixed() const{return *fData->fFixed;}
    QSigExFitParam* const& GetMaster() const{return fMaster;}
    Int_t GetMasterIndex() const{return (fMaster?fMaster->fIndex:-1);}
    QSigExFitParam* GetTopMaster() const{return (fMaster?fData->fMaster:NULL);}
    Int_t GetTopMasterIndex() const{return (fMaster?fData->fMaster->fIndex:-1);}
    Bool_t IsMaster() const{return !fMaster;}

    const char* GetTitle() const{fData->fTitle=""; fData->fTitle+=fData->fValue; return fData->fTitle;}

    operator const Double_t&() const{return fData->fValue;}
    const QSigExFitParam& operator=(const double& rhs){fData->fValue=rhs; return *this;}
    const QSigExFitParam& operator=(const char* valstr){sscanf(valstr,"%lf",&fData->fValue); return *this;}
    const QSigExFitParam& operator=(const QSigExFitParam &rhs){if(fData!=rhs.fData) fData->fValue=rhs.fData->fValue; return *this;}

    void Print(const Option_t* opt=NULL) const;

    void Setup(const Double_t &startval, const Double_t& stepval, const Double_t& minval=-0.5e7, const Double_t &maxval=0.5e7, Int_t fix=kFALSE){*fData->fStartVal=startval; *fData->fStepVal=stepval; *fData->fMinVal=minval; *fData->fMaxVal=maxval; *fData->fFixed=fix; if((Double_t&)*fData->fMinVal == (Double_t&)*fData->fMaxVal && fix!=kTRUE) *fData->fFixed=2;}
    void SetFix(Int_t fix=kTRUE){*fData->fFixed=fix;}
    void SetFree();
    void SetIndex(const Int_t &index){fIndex=index;}
    void SetStartVal(const Double_t& startval){*fData->fStartVal=startval;}
    void SetMinVal(const Double_t& minval=-0.5e7){*fData->fMinVal=minval; if((Double_t&)*fData->fMinVal == (Double_t&)*fData->fMaxVal && *fData->fFixed!=kTRUE) *fData->fFixed=2;}
    void SetMaxVal(const Double_t& maxval=0.5e7){*fData->fMaxVal=maxval; if((Double_t&)*fData->fMinVal == (Double_t&)*fData->fMaxVal && *fData->fFixed!=kTRUE) *fData->fFixed=2;}
    void SetName(const char *name){fName=name;}
    void SetNameIndex(const char *name, const Int_t &index){fName=name; fIndex=index;}
    void SetSlaveOf(QSigExFitParam &master);
void SetStepVal(const Double_t& stepval){*fData->fStepVal=stepval;}
    void SetRange(const Double_t& minval=-0.5e7, const Double_t& maxval=0.5e7){*fData->fMinVal=minval; *fData->fMaxVal=maxval; if((Double_t&)*fData->fMinVal == (Double_t&)*fData->fMaxVal && *fData->fFixed!=kTRUE) *fData->fFixed=2;}

    void Browse(TBrowser *b);
    Bool_t IsFolder() const {return kTRUE;}

    friend class QSigExFit;
  protected:
    operator Double_t&(){return fData->fValue;}
    Double_t& MinusFitError(){return *fData->fMinusFitError;}
    Double_t& PlusFitError(){return *fData->fPlusFitError;}
    Double_t& StartVal(){return *fData->fStartVal;}
    Int_t& FreeParamIndex(){return *fData->fFreeParamIndex;}

  private:
    class QSFPData {
      public:
	QSFPData(QSigExFitParam *master): fMaster(master), fValue(0), fTitle("0"), fStartVal(new QNamedVar<Double_t>("Start Value",0.)), fMinVal(new QNamedVar<Double_t>("Minimum Value",-0.5e7)), fMaxVal(new QNamedVar<Double_t>("Maximum Value",0.5e7)), fStepVal(new QNamedVar<Double_t>("Step Value",1.)), fFixed(new QNamedVar<Int_t>("Is Fixed",kFALSE)), fPlusFitError(new QNamedVar<Double_t>("Plus Fit Error",-1.0)), fMinusFitError(new QNamedVar<Double_t>("Minus Fit Error","-1.0")), fFreeParamIndex(new QNamedVar<Int_t>("Free Parameter Index",-1)){} 
	QSFPData(QSigExFitParam *master,const QSFPData &rhs): fMaster(master), fValue(rhs.fValue), fTitle(rhs.fTitle), fStartVal(new QNamedVar<Double_t>(*rhs.fStartVal)), fMinVal(new QNamedVar<Double_t>(*rhs.fMinVal)), fMaxVal(new QNamedVar<Double_t>(*rhs.fMaxVal)), fStepVal(new QNamedVar<Double_t>(*rhs.fStepVal)), fFixed(new QNamedVar<Int_t>(*rhs.fFixed)), fPlusFitError(new QNamedVar<Double_t>(*rhs.fPlusFitError)), fMinusFitError(new QNamedVar<Double_t>(*rhs.fMinusFitError)), fFreeParamIndex(new QNamedVar<Int_t>(*rhs.fFreeParamIndex)){} 
	~QSFPData() {
	  delete fStartVal; fStartVal=NULL;
	  delete fMinVal; fMinVal=NULL;
	  delete fMaxVal; fMaxVal=NULL;
	  delete fStepVal; fStepVal=NULL;
	  delete fFixed; fStartVal=NULL;
	  delete fPlusFitError; fPlusFitError=NULL;
	  delete fMinusFitError; fMinusFitError=NULL;
	  delete fFreeParamIndex; fFreeParamIndex=NULL;
	}

	QSigExFitParam	  *fMaster;
	Double_t		   fValue;
	mutable TString fTitle;
	QNamedVar<Double_t> *fStartVal;
	QNamedVar<Double_t> *fMinVal;
	QNamedVar<Double_t> *fMaxVal;
	QNamedVar<Double_t> *fStepVal;
	QNamedVar<Int_t>    *fFixed;
	QNamedVar<Double_t> *fPlusFitError;
	QNamedVar<Double_t> *fMinusFitError;
	QNamedVar<Int_t>    *fFreeParamIndex;

      private:
	QSFPData(){} 
	QSFPData(const QSFPData &rhs){} 
    };

    TString fName;
    Int_t		 fIndex;
    QSFPData		*fData;
    QSigExFitParam      *fMaster;
#ifndef __CINT__
    QList<void*>	*fSlaves;
#endif

    void UpdateDataPtr() const;
    ClassDef(QSigExFitParam,2) //Parameter for QSigExFit object
};

#endif
