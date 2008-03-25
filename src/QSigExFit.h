// Author: Kathryn Miknaitis <mailto:gator@u.washington.edu>, Pierre-Luc Drouin <http://www.pldrouin.net>
// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QSIGEXFIT_
#define _QSIGEXFIT__

#ifndef __CINT__
#include "Api.h"
#endif
#include "TMinuit.h"
#include "TList.h"
#include "TCollection.h"
#include "TMatrixDSym.h"
#include "QSigExDirHandler.h"
#include "QSigExFitDataHolder.h"
#include "QList.h"
#include "QNamedVar.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

////////////////////////////////////////////////////////////////////////
//                                                                    //
// QSigExFit                                                          //
//                                                                    //
// This class uses the "JointPDFsProbs" joint probability densities   //
// TTree stored in "Probs/JointPDFsProbs" to estimate the population  //
// parameters values. To achieve this, it first reads different       //
// configuration parameters of population parameters and some         //
// constants from a card file. Then, it uses a parameters fitter      //
// function chosen by the user to estimate the parameters and their   //
// error. The card file information and the results are placed in     //
// subfolders of "Fit" QSigExStruct.                                  //
//                                                                    //
////////////////////////////////////////////////////////////////////////

class QSigExFit: public QSigExDirHandler{
 public:
  QSigExFit():QSigExDirHandler(),fFitDir(NULL),fJProbsDirs(),fCompiledFunc(NULL),fInterpretedFunc(NULL){

    PRINTF2(this,"\tQSigExFit::QSigExFit()\n")
  }

  QSigExFit(const QSigExFit& rhs):QSigExDirHandler(*this),fFitDir(rhs.fFitDir),fJProbsDirs(),fCompiledFunc(rhs.fCompiledFunc),fInterpretedFunc(rhs.fInterpretedFunc),fFluxCard(rhs.fFluxCard),fVariabsCard(rhs.fVariabsCard),fMinimCard(rhs.fMinimCard),fMinosCard(rhs.fMinosCard){
    PRINTF2(this,"\tQSigExFit::QSigExFit(const QSigExFit& rhs)\n")
  }

  QSigExFit(QSigExStruct* dir,void (*fcn)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t),const Char_t* cardfilename=NULL):QSigExDirHandler(),fFitDir(NULL),fJProbsDirs(),fCompiledFunc(NULL),fInterpretedFunc(NULL)
    {
      PRINTF6(this,"\tQSigExFit::QSigExFit(QSigExStruct* dir<",
	      dir,">,const Char_t* cardfilename<",cardfilename,">)\n")
      try{
	SetDir(dir);
 
	if(dir){
	  AddInDir(dir);
	}
	SetFCN(fcn);
	LoadCardFile(cardfilename);
      }catch(int e){
	cout << "Exception handled by QSigExFit::QSigExFit\n";
	throw e;
      }
    }

  QSigExFit(QSigExStruct* dir,void *fcn,const Char_t* cardfilename=NULL):QSigExDirHandler(),fFitDir(NULL),fJProbsDirs(),fCompiledFunc(NULL),fInterpretedFunc(NULL)
    {
      PRINTF6(this,"\tQSigExFit::QSigExFit(QSigExStruct* dir<",
	      dir,">,const Char_t* cardfilename<",cardfilename,">)\n")
	try{
	  SetDir(dir);

	  if(dir){
	    AddInDir(dir);
	  }
	  SetFCN(fcn);
	  LoadCardFile(cardfilename);
	}catch(int e){
	cout << "Exception handled by QSigExFit::QSigExFit\n";
	throw e;
      }
    }

  virtual ~QSigExFit(){}

  void LoadCardFile(const Char_t* cardfilename=NULL);

  void AddFlux(const Char_t* fgroup, Bool_t active, Float_t startval, Float_t minval, Float_t maxval, Float_t stepval); 
  void AddVariab(const Char_t* vname, Float_t vvalue);

  void SetMinimizer(const Char_t* minimname, const Char_t* fcnerror, const Char_t* param1 = "NULL", const Char_t* param2 = "NULL", const Char_t* param3 = "NULL", const Char_t* param4 = "NULL", const Char_t* param5 = "NULL", const Char_t* param6 = "NULL", const Char_t* param7 = "NULL", const Char_t* param8 = "NULL");

  void SetMinosMaxNCalls(Int_t maxcalls);

  void AddInDir(TDirectory* folder)
  {
    PRINTF4(this,"\tQSigExFit::AddInDir(TDirectory* folder<",folder,">)\n")
      try{
	if(folder) fJProbsDirs.Add(folder);
	else {cout << "A NULL pointer has been passed\n"; throw 5000;}
      }catch(int e){
	cout << "Exception handled by QSigExFit::AddInDir\n";
	throw e;
      }
  }

  TDirectory* RmInDir(TDirectory* folder)
  {     
    PRINTF4(this,"\tQSigExFit::RmInDir(TDirectory* folder<",folder,">)\n")

      TDirectory *ret;
      try{              
	if(folder) ret=dynamic_cast<TDirectory*>(fJProbsDirs.Remove(folder));
	else {cout << "A NULL pointer has been passed\n"; throw 5000;}
      }catch(int e){                            
	cout << "Exception handled by QSigExFit::RmInDir\n";
	throw e;                                                  
      }
      return ret;
  }

  const TList& GetInDirList() const {return fJProbsDirs;}

  void CleanDir();
  void ClearCardBuf();

  void SetFCN(void (*fcn)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t));
  void SetFCN(void* fcn);

  Int_t Get();

  enum{
    kSetupIdx,
    kNumbersIdx,
    kMinuitIdx
  };

  enum{
    kSParamsIdx,
    kSConstantsIdx,
    kSMinimizerIdx,
    kSMINOsIdx
  };

  enum{
    kPStartValIdx,
    kPMinValIdx,
    kPMaxValIdx,
    kPStepValIdx,
    kPActiveIdx,
    kPIndexIdx
  };

  enum{
    kMMinimizerIdx,
    kMFCNErrorIdx,
    kMOtherFieldsIdx
  };

  enum{
    kMINOsMaxCallsIdx
  };

 protected:
  void FormatDir();
  void GetParametersParams();
  void GetConstants();
  void GetMinimizerSettings();
  void GetMinosSettings();
  void CheckParamsSetup() const;
  void CheckConstantsSetup() const;
  void CheckMinimizerSetup() const;
  void CheckJointPDFsProbs() const;

 private:
  
  QSigExStruct* fFitDir; //Pointer to the "Fit" QSigExStruct 
  TList fJProbsDirs; //! List of pointers to extra joint prob trees
  void (*fCompiledFunc)(Int_t&, Double_t*, Double_t&f, Double_t*, Int_t); //! Pointer to a compiled minimization function
  void* fInterpretedFunc;  //! Pointer to an interpreted minimization function
  QList<QList<TString> > fFluxCard; //List of setup parameters of population parameters in card file format
  QList<QList<TString> > fVariabsCard; //List of fit constants in card file format
  QList<TString> fMinimCard; //Minimizer configuration parameters in card file format
  QList<TString> fMinosCard; //MINOs configuration parameters in card file format

  ClassDef (QSigExFit,1) //Evaluates the population parameters using Minuit minimization package
};


#include "debugger.h"
#endif
