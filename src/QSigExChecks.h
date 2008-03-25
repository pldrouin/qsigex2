// Author: Juergen Wendland <mailto:juergen@phas.ubc.ca>

//////////////////////////////////////////////////////////////////
//                                                              //
// QSigExChecks                                                 //
//                                                              //
// This class implements a number of sanity checks to verify    //
// a successful fit. The most useful check is the comparison of //
// the sum of the covariance matrix with the total number of    //
// events in the input data.                                    //
// the class interfaces with the Fit result via the output file //  
// from the main qsigex classes.                                //
//////////////////////////////////////////////////////////////////

#ifndef _QSIGEXCHECKS_
#define _QSIGEXCHECKS__

#include <cmath>
#include <iostream>
#include <cstdlib>
#include "Rtypes.h"
#include "TString.h"
#include "TMatrixDSym.h"

#include "QSigExDis.h"

#define DEBUG
#define DEBUG2

#include "debugger.h"

using std::cout;
using std::endl;

class TDirectory;
class TFile;

class QSigExChecks 
{
 public:
  QSigExChecks(TFile *aFile=NULL);
  virtual ~QSigExChecks();
  
  void GetFitResult(TFile *aFile=NULL, Char_t* aDir=NULL);
  TMatrixDSym *GetCovariance(TFile *aFile=NULL, Char_t* aDir=NULL, Char_t* aName=NULL);
  void GetParameters(TFile *aFile=NULL, Char_t* aDir=NULL);
  Double_t GetParameter(Int_t i){return fFitPar[i];};
  Double_t GetParPlusError(Int_t i){return fFitParPErr[i];};
  Double_t GetParMinusError(Int_t i){return fFitParMErr[i];};
  Int_t GetParActive(Int_t i){return fFitParActive[i];};
  void GetF2EMaps(TFile *aFile=NULL, Char_t* aDir=NULL);
  void GetPMTF2EMaps(TFile *aFile=NULL){GetF2EMaps(aFile,(Char_t*)fPMTF2EDir.Data());};
  void GetNCDF2EMaps(TFile *aFile=NULL){GetF2EMaps(aFile,(Char_t*)fNCDF2EDir.Data());};

  void AddEvents(TFile *aFile=NULL, Char_t* aDir = NULL, Char_t* aTreeName = NULL);
  void AddPMTEvents(TFile *aFile=NULL){cout << "<QSigExChecks::AddPMTEvents> " << endl; AddEvents(aFile,(Char_t*)fPMTEventDir.Data());};
  void AddNCDEvents(TFile *aFile=NULL){cout << "<QSigExChecks::AddNCDEvents> " << endl; AddEvents(aFile,(Char_t*)fNCDEventDir.Data());};

  Double_t GetSum(TMatrixDSym *aMatrix=NULL);
  Bool_t CheckEigenValues(TMatrixDSym *aMatrix=NULL);
  Bool_t CheckSum(TMatrixDSym *aMatrix=NULL, Int_t aNoEvents=0);

  Int_t GetNoEvents(){return fNoEvents;};

  Double_t PlotLikelihoodFunction(Int_t ipar, Char_t* outbase=NULL); // plot and print the likelihood function for one parameter.
  Double_t PlotLikelihoodFunction(Int_t ipar, Int_t jpar, Char_t* outbase=NULL); // plot and print the likelihood function for two parameters.

  void PrintParameters(){
    for(Int_t i=0;i<fnFluxes;i++)cout << "<QSigExChecks::PrintParameters> " 
				      << i << " " << fFluxName[i] << endl;
  };
  void PrintCorrelationMatrix(){
    for(Int_t i=0; i<fnActiveFluxes; i++) {
      for(Int_t j=0; j<fnActiveFluxes; j++) { 
	printf("  %12.4lf",(*fCovariance)[i][j]/sqrt((*fCovariance)[i][i]*(*fCovariance)[j][j])); 
      }
      printf("\n");
    }
  }
  void PrintFlux2Events();
  void PrintEventNumbers();

 private:

  static const Int_t fMaxFitPars; // this is for the quantile calculation
  static const Double_t fQuantileProbability;
  static const Double_t fQuantile[];

  TFile *fFile;
  TString fPMTEventDir;
  TString fPMTF2EDir;
  TString fNCDEventDir;
  TString fNCDF2EDir;

  TString fFitNumbersDir;  
  TString fFitMinuitDir;
  
  TString fFitParametersDir;

  TString fCovarianceName;
  TString fStatusName;

  Long64_t fNoEvents; // total number of events in data sample

  TMatrixDSym *fCovariance;

  Int_t fnFluxes;
  Int_t fnActiveFluxes;
  TString *fFluxName;

  Int_t fnF2EMaps;
  QSigExDis  **fF2EMap;     //!
  TString    **fF2EFluxName; //!
  Int_t      *fF2EFluxIndex; //!
  TString    **fF2EEvtName; //!

  Int_t fnFitPars;
  TString **fFitParName;  //!
  Int_t    *fFitParActive;//!
  Int_t    *fFitParIndexOfActive; //! the index of the parameter as function of the active index 
  Double_t *fFitPar;      //!
  Double_t *fFitParMErr;  //!
  Double_t *fFitParPErr;  //!
  Double_t *fFitParErr;   //!


  ClassDef(QSigExChecks,1) // implements checks of the qsigex fit result
};

#include "debugger.h"

#endif
