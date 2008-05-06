// Author: J. Wendland juergen@phas.ubc.ca

// QSigExChecks

//////////////////////////////////////////////////////////////////
// this class implements a number of sanity checks to verify    //
// a successful fit. The most useful check is the comparison of //
// the sum of the covariance matrix with the total number of    //
// events in the input data.                                    //
// the class interfaces with the Fit result via the output file //  
// from the main qsigex classes.                                //
//////////////////////////////////////////////////////////////////


#include "QSigExChecks.h"
#include "TROOT.h"
#include "TFile.h"
#include "TString.h"
#include "TTree.h"
#include "TMatrixDSym.h"
#include "TMatrixDEigen.h"
#include "TStyle.h"
#include "TCanvas.h"
#include "TGraph.h"
#include "TGraph2D.h"
#include "TGraph.h"
#include "TF2.h"
#include "TLatex.h"
#include "TMinuit.h"
#include "math.h"

#include "QDis.h"
#include "QDisTF.h"
#include "QF2EExtendedLikelihood.h"

ClassImp(QSigExChecks)

const Int_t QSigExChecks::fMaxFitPars = 20;
const Double_t QSigExChecks::fQuantileProbability = 0.99;
const Double_t QSigExChecks::fQuantile[] = { 6.63, 9.21, 11.3, 13.277, 15.086, 16.812, 18.475, 20.090, 21.666,
					     23.209, 24.725, 26.217, 27.688, 29.141, 30.578, 32.000, 33.409, 34.805, 36.191, 
					     37.566 }; // values from http://www.ds.unifi.it/VL/VL_EN/special/special4.html

//___________________________________________________________________________________
QSigExChecks::QSigExChecks(TFile *theFile){

  fFile=theFile;

  // directory structure
  fPMTEventDir = "pmt/Event Info";
  fNCDEventDir = "ncd/Event Info";

  fPMTF2EDir = "pmt/Physics/Flux2Events";
  fNCDF2EDir = "ncd/Physics/Flux2Events";

  fFitNumbersDir = "Fit/Numbers";
  fCovarianceName = "TMatrixTSym<double>";

  fFitMinuitDir = "Fit/Minuit";
  fStatusName   = "Status";

  fFitParametersDir = "Fit/Setup/Parameters";

  // initialization
  fCovariance = NULL;
  fNoEvents = 0;
  fnF2EMaps = 0;
  fnFluxes  = 0;
  fnActiveFluxes = 0;

  if(fFile){
    GetFitResult(); 
    GetCovariance();
    GetParameters();
    GetPMTF2EMaps();
    GetNCDF2EMaps();
    AddPMTEvents(); 
    AddNCDEvents();
  }

  return;
}

//___________________________________________________________________________________
QSigExChecks::~QSigExChecks()
{
  // Clean up memory

  if(fnF2EMaps>0){
    for(Int_t i=0; i<fnF2EMaps; i++){
      delete fF2EEvtName[i];
      delete fF2EFluxName[i];
    }
    delete[] fF2EEvtName;
    delete[] fF2EFluxName;
    delete[] fF2EMap;
    delete[] fF2EFluxIndex;
  }

  if(fnFitPars>0){
    cout << "fnFitPars: " << fnFitPars << endl;
    for(Int_t i=0;i<fnFitPars;i++)delete fFitParName[i];
    delete[] fFitParName;
    delete[] fFitParActive;
    delete[] fFitParIndexOfActive;
    delete[] fFitPar;
    delete[] fFitParPErr;
    delete[] fFitParMErr;
    delete[] fFitParErr;
  }

  return;
}

//___________________________________________________________________________________
void QSigExChecks::AddEvents(TFile *aFile, Char_t* aDir, Char_t* aTreeName)
{
  if(!aFile)aFile=fFile;
  if(!aFile){
    cout << "<QSigExChecks::AddEvents> ERROR: no file defined." << endl;
    throw 24;
  }
  if(!aDir) {
    cout << "<QSigExChecks::AddEvents> ERROR: no directory defined." << endl;
    throw 23;
  }
  
  aFile->cd(aDir);
  TTree *aTree=0;
  if(!aTreeName){ // search for the tree
    TList *ls = gDirectory->GetListOfKeys();
    if(ls->GetEntries()!=1){
      cout << "<QSigExChecks::AddEvents> ERROR: Number of keys in directory != 1: " 
	   << ls->GetEntries() << endl;
      gDirectory->pwd();
      gDirectory->ls();
      throw 25;
    }
    aTree = dynamic_cast<TTree*> (gDirectory->Get(ls->First()->GetName()));
  }
  else aTree = dynamic_cast<TTree*> (gDirectory->Get(aTreeName));

  // add number of entries to counter
  cout << "<QSigExChecks::AddEvents>: Found " << aTree->GetEntries() << " events in this tree." << endl;
  fNoEvents += aTree->GetEntries();

  return;
}

//___________________________________________________________________________________
void QSigExChecks::GetParameters(TFile *aFile, Char_t* aDir)
{
  //  cout << "<QSigExChecks::GetParameters>" << endl;
  Bool_t newdir = kFALSE;
  if(!aFile)aFile=fFile;
  if(!aDir) {
    aDir = new Char_t[fFitParametersDir.Length()+1];
    sprintf(aDir,"%s",(Char_t*) fFitParametersDir.Data());
    newdir = kTRUE;
  }

  aFile->cd(aDir);
  TList *ls = gDirectory->GetListOfKeys();
  fnFluxes=ls->GetEntries();
  fFluxName = new TString[fnFluxes];
  for(Int_t i=0;i<ls->GetEntries();i++){
    TString sbuf(ls->At(i)->GetName());
    gDirectory->cd(sbuf.Data());
    TNamed *nbuf=dynamic_cast<TNamed*>(gDirectory->Get("Index"));
    Int_t index;
    sscanf(nbuf->GetTitle(),"%d",&index);
    fFluxName[index]=sbuf;

    aFile->cd(aDir);
  }

  if(newdir){delete aDir;}
  return;
}

//___________________________________________________________________________________
void QSigExChecks::GetF2EMaps(TFile *aFile, Char_t* aDir)
{
  // read the flux 2 event mapping functions
  cout << "<QSigExChecks::GetF2EMaps>" << endl;
  Bool_t newdir = kFALSE;
  if(!aFile)aFile=fFile;

  aFile->cd(aDir);
  
  TList *ls = gDirectory->GetListOfKeys();
  Int_t i0 = 0;
  Int_t nNewF2EMaps = ls->GetEntries();
  if(fnF2EMaps==0){
    fnF2EMaps=nNewF2EMaps;
    fF2EMap       = new QDis*[fnF2EMaps];
    fF2EFluxName  = new TString*[fnF2EMaps];
    fF2EFluxIndex = new Int_t[fnF2EMaps];
    fF2EEvtName   = new TString*[fnF2EMaps];
  }
  else {
    QDis** newfF2EMap    = new QDis*[fnF2EMaps+nNewF2EMaps];
    TString** newfF2EFluxName = new TString*[fnF2EMaps+nNewF2EMaps];
    TString** newfF2EEvtName  = new TString*[fnF2EMaps+nNewF2EMaps];
    Int_t* newfF2EFluxIndex   = new Int_t[fnF2EMaps+nNewF2EMaps];

    memset(newfF2EMap,0,fnF2EMaps+nNewF2EMaps*sizeof(QDis*));
    memset(newfF2EFluxName,0,fnF2EMaps+nNewF2EMaps*sizeof(TString*));
    memset(newfF2EEvtName,0,fnF2EMaps+nNewF2EMaps*sizeof(TString*));
    memset(newfF2EFluxIndex,0,fnF2EMaps+nNewF2EMaps*sizeof(Int_t));

    memcpy(newfF2EMap,fF2EMap,fnF2EMaps*sizeof(QDis*));
    memcpy(newfF2EFluxIndex,fF2EFluxIndex,fnF2EMaps*sizeof(Int_t));
    memcpy(newfF2EEvtName,fF2EEvtName,fnF2EMaps*sizeof(TString*));
    memcpy(newfF2EFluxName,fF2EFluxName,fnF2EMaps*sizeof(TString*));

    delete[] fF2EMap;
    delete[] fF2EFluxIndex;
    delete[] fF2EEvtName;
    delete[] fF2EFluxName;

    fF2EMap=newfF2EMap;
    fF2EFluxIndex=newfF2EFluxIndex;
    fF2EFluxName=newfF2EFluxName;
    fF2EEvtName=newfF2EEvtName;
    
    i0=fnF2EMaps;
    fnF2EMaps+=nNewF2EMaps;
  }

  for(Int_t i=i0;i<fnF2EMaps;i++){
    aFile->cd(aDir);

    fF2EEvtName[i] = new TString(ls->At(i-i0)->GetName());
    gDirectory->cd(fF2EEvtName[i]->Data());
    fF2EMap[i] = dynamic_cast<QDis*>(gDirectory->Get(gDirectory->GetName()));//(fF2EFluxName[i].Data()));

    gDirectory->cd("Inputs");
    
    TNamed *nbuf=dynamic_cast<TNamed*>(gDirectory->Get("Input 0"));
    fF2EFluxName[i] = new TString(nbuf->GetTitle()); 

    Int_t index = 0;
    for (index=0; index<fnFluxes; index++){
      if(*(fF2EFluxName[i])==fFluxName[index])break;
    }
    fF2EFluxIndex[i]=index;

  }
  if(newdir)delete aDir;

  return;
}

//___________________________________________________________________________________
void QSigExChecks::PrintFlux2Events(){
  cout << "<QSigExChecks::PrintFlux2Events> *** " << fnF2EMaps << endl;
  for(Int_t i=0;i<fnF2EMaps;i++){
    cout <<"<QSigExChecks::PrintFlux2Events> " 
	 << i << " " 
      	 << *(fF2EEvtName[i]) << " "
 	 << fF2EFluxIndex[i] << " "
 	 << *(fF2EFluxName[i]) << " "
	 << fF2EMap[i]->Derivative(fFitPar[i]) 
	 << endl;
      ;
  }
}

//___________________________________________________________________________________
void QSigExChecks::PrintEventNumbers(){

  cout << "<QSigExChecks::PrintEventNumbers> ";
  cout.width(4);  cout << "i";
  cout.width(10); cout << "flux";
  cout.width(4);  cout << "index";
  cout.width(10); cout << "param";
  cout.width(10); cout << "fitval";
  cout.width(10); cout << "f2e(1)";
  cout.width(10); cout << "d(f2e(1))";
  cout.width(10); cout << "#(Evts)";
  cout << endl;
  Double_t theTotalEvents = 0;
  for(Int_t i=0;i<fnF2EMaps;i++){
    Double_t theNoEvents=fF2EMap[i]->ProbDensity(fFitPar[fF2EFluxIndex[i]],0.,0.);
    cout << "<QSigExChecks::PrintEventNumbers> ";
    cout.width(4);  cout << i;
    cout.width(10); cout << *fF2EFluxName[i];
    cout.width(4);  cout << fF2EFluxIndex[i];
    cout.width(10); cout << *fFitParName[fF2EFluxIndex[i]];
    cout.width(10); cout <<  fFitPar[fF2EFluxIndex[i]];
    cout.width(10); cout << fF2EMap[i]->ProbDensity(1.,0.,0.);
    cout.width(10); cout << fF2EMap[i]->Derivative(1.);
    cout.width(10); cout << theNoEvents << endl;

    theTotalEvents += theNoEvents;
  } 
  cout <<  "<QSigExChecks::PrintEventNumbers> Total # of Events: (Fit and Input)";
  cout.width(10); cout << theTotalEvents;
  cout.width(10); cout << fNoEvents;
  cout << endl;
  return;
}

//___________________________________________________________________________________
void QSigExChecks::GetFitResult(TFile *aFile, Char_t* aDir)
{
  Bool_t newdir = kFALSE;

  if(!aFile)aFile=fFile;
  if(!aDir) {
    aDir = new Char_t[fFitNumbersDir.Length()+1];
    sprintf(aDir,"%s",(Char_t*) fFitNumbersDir.Data());
    newdir = kTRUE;
  }
  if(!aFile){
    cout << "<QSigExChecks::GetFitResult> ERROR: no file defined." << endl;
    throw 26;
  }

  aFile->cd(aDir);
  TList *ls = gDirectory->GetListOfKeys();
  fnFitPars=ls->GetEntries()-1;
  fFitParName  = new TString*[fnFitPars];
  fFitParActive = new Int_t[fnFitPars];
  fFitParIndexOfActive = new Int_t[fnFitPars];
  fFitPar     = new Double_t[fnFitPars];
  fFitParMErr = new Double_t[fnFitPars];
  fFitParPErr = new Double_t[fnFitPars]; 
  fFitParErr  = new Double_t[fnFitPars];
  for(Int_t i=0; i<fnFitPars; i++){
    aFile->cd(aDir);
    TString sbuf(ls->At(i)->GetName());
    if(sbuf==fCovarianceName)continue;
    gDirectory->cd(sbuf);

    //    TNamed *nbuf=dynamic_cast<TNamed*>(gDirectory->Get("ActParamIndex"));
    TNamed *nbuf=dynamic_cast<TNamed*>(gDirectory->Get("ParamIndex"));
    Int_t index=-100;
    sscanf(nbuf->GetTitle(),"%d",&index);

    nbuf=dynamic_cast<TNamed*>(gDirectory->Get("ActParamIndex"));
    sscanf(nbuf->GetTitle(),"%d",&fFitParActive[index]);

    fFitParName[index] = new TString(sbuf);

//     cout << "<QSigExChecks::GetFitResult> " << index << " " << fFitParActive[index] << " "  << (*fFitParName[index]) << endl;

    nbuf=dynamic_cast<TNamed*>(gDirectory->Get("FitValue"));
    sscanf(nbuf->GetTitle(),"%lf",&fFitPar[index]);
    if(fFitParActive[index]>=0){
      fFitParIndexOfActive[fFitParActive[index]]=index;
      fnActiveFluxes++;
      nbuf=dynamic_cast<TNamed*>(gDirectory->Get("MinusFitError"));
      sscanf(nbuf->GetTitle(),"%lf",&fFitParMErr[index]);
      nbuf=dynamic_cast<TNamed*>(gDirectory->Get("PlusFitError"));
      sscanf(nbuf->GetTitle(),"%lf",&fFitParPErr[index]);
    }
    else {
      fFitParMErr[index]=-999;
      fFitParPErr[index]=-999;
    }
    fFitParErr[index] = (fabs(fFitParMErr[index]) + fabs(fFitParPErr[index])) * 0.5;
    

  }

  if(newdir){delete aDir;}
  return;
}

//___________________________________________________________________________________
TMatrixDSym *QSigExChecks::GetCovariance(TFile *aFile, Char_t* aDir, Char_t* aName)
{
  Bool_t newdir = kFALSE;
  Bool_t newname = kFALSE;

  if(!aFile)aFile=fFile;
  if(!aDir) {
    aDir = new Char_t[fFitNumbersDir.Length()+1];
    sprintf(aDir,"%s",fFitNumbersDir.Data());
    newdir = kTRUE;
  }
  if(!aName) {
    aName = new Char_t[fCovarianceName.Length()+1];
    sprintf(aName,"%s",fCovarianceName.Data());
    newname = kTRUE;
  }

  if(!aFile){
    cout << "<QSigExChecks::GetCovariance> ERROR: no file defined." << endl;
    throw 20;
  }

  aFile->cd(aDir);
  fCovariance = new TMatrixDSym;
  fCovariance = dynamic_cast<TMatrixDSym*>  (gDirectory->Get(fCovarianceName.Data()));

  if(newname)delete aName;
  if(newdir)delete aDir;

  return fCovariance;
}

//___________________________________________________________________________________
Bool_t QSigExChecks::CheckEigenValues(TMatrixDSym *aMatrix)
{
  // check whether the eigenvalues are all positive as they should be for a covariance matrix
  // return true if all are positive (>=0), false if at least one is negative.
  if(!aMatrix)aMatrix=fCovariance;
  if(!aMatrix){
    cout << "<QSigExChecks::CheckEigenValues> ERROR: no matrix defined" << endl;
    throw 21;
  }

  TMatrixDEigen *eigenmatrix= new TMatrixDEigen(*aMatrix);
  TVectorD eigenvaluesre = eigenmatrix->GetEigenValuesRe();

  // print some stuff
  cout << "<QSigExChecks::CheckEigenValues> " << eigenvaluesre.GetNoElements();
  for(Int_t k=0; k<eigenvaluesre.GetNoElements(); k++)cout << " " << eigenvaluesre[k];
  cout << endl;

  // check that they are positive
  for(Int_t k=0; k<eigenvaluesre.GetNoElements(); k++){
    if(eigenvaluesre[k]<0){
      cout << "<QSigExChecks::CheckEigenValues> Found negative eigenvalue!" << endl; 
      delete eigenmatrix;
      return kFALSE;
    }
  }

  delete eigenmatrix;

  return kTRUE;
}


//_____________________________________________________________________________________
Double_t QSigExChecks::GetSum(TMatrixDSym *aMatrix)
{
  if(!aMatrix)aMatrix=fCovariance;
  if(!aMatrix){
    cout << "<QSigExChecks::CheckSum> ERROR: no matrix defined" << endl;
    throw 22;
  }

  Double_t aSum=0;
  for(Int_t i=0; i<aMatrix->GetNrows(); i++){ // loop over active parameters using ActParamIndex
    Double_t iDerivSum=0;
    for(Int_t k=0; k<fnF2EMaps; k++){
      if(fF2EFluxIndex[k]==fFitParIndexOfActive[i])iDerivSum+=fF2EMap[k]->Derivative(fFitPar[fFitParIndexOfActive[i]]);
    }
    for(Int_t j=0; j<aMatrix->GetNcols(); j++){
      Double_t jDerivSum=0;
      for(Int_t k=0; k<fnF2EMaps; k++){
	if(fF2EFluxIndex[k]==fFitParIndexOfActive[j])jDerivSum+=fF2EMap[k]->Derivative(fFitPar[fFitParIndexOfActive[j]]);
      }
      
      aSum += iDerivSum * jDerivSum * (*aMatrix)[i][j];
      cout << "<QSigExChecks::GetSum> " << i << " " << j;
      cout.width(12);
      cout << iDerivSum;
      cout.width(12);
      cout << jDerivSum;
      cout.width(12);
      cout << (*aMatrix)[i][j];
      cout.width(12);
      cout << iDerivSum * jDerivSum * (*aMatrix)[i][j];
      cout.width(12);
      cout << aSum << endl;

//       if(i==j)cout << "GetSum " << sqrt((*aMatrix)[i][j]) << " " <<  fFitParErr[i] << " " 
// 		   << sqrt((*aMatrix)[i][j])-sqrt(fFitParErr[i]*fFitParErr[i]) << endl;
    }
  }

  return aSum;
}

//_____________________________________________________________________________________
Bool_t QSigExChecks::CheckSum(TMatrixDSym *aMatrix, Int_t aNoEvents)
{
  if(!aMatrix)aMatrix=fCovariance;
  if(!aMatrix){
    cout << "<QSigExChecks::GetSum> ERROR: no matrix defined" << endl;
    throw 22;
  }
  if(!aNoEvents)aNoEvents=fNoEvents;

  cout << "<QSigExChecks::CheckSum> Sum(Cov) = " << GetSum(aMatrix) << ", #(Evts) = " << aNoEvents << endl ;

  return kTRUE;
}

//_____________________________________________________________________________________
Double_t QSigExChecks::PlotLikelihoodFunction(Int_t ipar, Char_t* outbase)
{
  // plot and print the likelihood function for one parameter.
  // presently this is done for one parameter
  Double_t aSigmaRange = 3.;

  if(fnFitPars==0){
    cout << "<QSigExChecks::PlotLikelihoodFunction>: fnFitPars==0, fit no properly initialized." << endl;
    return 0.;
  }

  TString theOutputBase;
  if(outbase)theOutputBase=outbase;
  else theOutputBase = "QSigexChecksPLF";
	       
  Int_t npar = 0;
  Double_t gin = 0;
  Double_t f = 0;
  Double_t *thePars = new Double_t[fnFitPars];
  Double_t *thePErr = new Double_t[fnFitPars];
  Double_t *theMErr = new Double_t[fnFitPars];
  Int_t iflag = 0;

  // read the parameters
  npar = fnFitPars;
  for(Int_t i=0; i<npar; i++){
    thePars[i] = fFitPar[i];
    thePErr[i] = fFitParPErr[i];
    theMErr[i] = fFitParMErr[i];
  }

  // create an array of values
  const Int_t nPoints = 100;
  Double_t *par = new Double_t[nPoints];
  Double_t *L   = new Double_t[nPoints];

  Double_t parmin = thePars[ipar] + aSigmaRange*theMErr[ipar];
  Double_t parmax = thePars[ipar] + aSigmaRange*thePErr[ipar];

  for(Int_t i=0;i<nPoints;i++){
    par[i] = parmin + (parmax-parmin)*(Double_t)i/(Double_t)(nPoints-1);
    thePars[ipar] = par[i];
    QF2EExtendedLikelihood(npar, &gin, f, thePars, iflag);
    L[i] = f;
    // printf("<QSigExChecks::PlotLikelihoodFunction>: %d   %13.5f   %13.5f\n",i,par[i],L[i]);
  }

  TGraph *aGraph = new TGraph(nPoints, par, L);

  TCanvas *c = new TCanvas("c","C",1000,600);
  aGraph->SetTitle(fFitParName[ipar]->Data());
  aGraph->SetMarkerStyle(24);
  aGraph->SetMarkerColor(2);
  aGraph->SetMarkerSize(0.6);
  aGraph->SetLineStyle(2);
  aGraph->Draw("AP");
  aGraph->Fit("1++x++x*x","W SAME");

  TString outfile = theOutputBase;
  outfile += ".png";
  c->Print(outfile.Data());
  outfile = theOutputBase;
  outfile += ".eps";
  c->Print(outfile.Data());

  // clean up
  delete c;
  delete aGraph;
  delete[] L;
  delete[] par;
  delete[] thePars;
  delete[] thePErr;
  delete[] theMErr;

  return 0.;
}


//_____________________________________________________________________________________
Double_t QSigExChecks::PlotLikelihoodFunction(Int_t ipar, Int_t jpar, Char_t* outbase)
{
  // plot and print the likelihood function for one parameter.
  // presently this is done for one parameter
  
  if(fnFitPars==0){
    cout << "<QSigExChecks::PlotLikelihoodFunction>: fnFitPars==0, fit no properly initialized." << endl;
    return -999.;
  }
  if(ipar==jpar){
    cout << "<QSigExChecks::PlotLikelihoodFunction>: ERROR, it's not possible to plot a 2d contour for the same parameter." << endl;
    return -999.;
  }

  TString theOutputBase;
  if(outbase)theOutputBase=outbase;
  else theOutputBase = "QSigexChecksPLF";
	       
  Int_t npar = 0;
  Double_t gin = 0;
  Double_t f = 0;
  Double_t *thePars = new Double_t[fnFitPars];
  Double_t *thePErr = new Double_t[fnFitPars];
  Double_t *theMErr = new Double_t[fnFitPars];
  Int_t iflag = 0; // = 1 -> generate debug output

  // read the parameters
  npar = fnFitPars;
  cout << "Number of fit parameters: " << npar << endl;
  for(Int_t i=0; i<npar; i++){
    thePars[i] = fFitPar[i];
    thePErr[i] = fFitParPErr[i];
    theMErr[i] = fFitParMErr[i];
    cout << i << " " << (*fFitParName[i]) << " " << fFitPar[i] << " " << fFitParMErr[i] << " " << fFitParPErr[i]  << endl;
  }

  // create an array of values
  const Int_t nPoints = 15;
  const Int_t mPoints = 15;
  Double_t *par1 = new Double_t[nPoints*mPoints];
  Double_t *par2 = new Double_t[nPoints*mPoints];
  Double_t *logL = new Double_t[nPoints*mPoints]; // this (-1)*log(L)
  Double_t *L    = new Double_t[nPoints*mPoints];

  Double_t *par1X = new Double_t[nPoints];
  Double_t *par2Y = new Double_t[mPoints];
  Double_t *logLX = new Double_t[nPoints];
  Double_t *LX    = new Double_t[nPoints];
  Double_t *logLY = new Double_t[mPoints];
  Double_t *LY    = new Double_t[mPoints];

  Double_t iparmin = 0;
  Double_t iparmax = 0;
  Double_t jparmin = 0;
  Double_t jparmax = 0;


  Double_t logLmax = 0;
  QF2EExtendedLikelihood(npar, &gin, logLmax, thePars, iflag); // function returns negative log(L)
  logLmax *= -1;
  Double_t logLBoundary = logLmax - fQuantile[npar]/ 64;

  f=logLmax;
  while(logLBoundary<f){
    thePars[ipar]+=0.001*fFitPar[ipar];
    QF2EExtendedLikelihood(npar, &gin, f, thePars, iflag);
    f *= -1;
  }
  iparmax=thePars[ipar];

  thePars[ipar]=fFitPar[ipar];
  f=logLmax;
  while(logLBoundary<f){
    thePars[ipar]-=0.001*fFitPar[ipar];
    QF2EExtendedLikelihood(npar, &gin, f, thePars, iflag);
    f *= -1;
  }
  iparmin=thePars[ipar];

  thePars[ipar]=fFitPar[ipar];
  f=logLmax;
  while(logLBoundary<f){
    thePars[jpar]+=0.01*fFitPar[jpar];
    QF2EExtendedLikelihood(npar, &gin, f, thePars, iflag);
    f *= -1;
  }
  jparmax=thePars[jpar];

  thePars[jpar]=fFitPar[jpar];
  f=logLmax;
  while(logLBoundary<f){
    thePars[jpar]-=0.01*fFitPar[jpar];
    QF2EExtendedLikelihood(npar, &gin, f, thePars, iflag);
    f *= -1;
  }
  jparmin=thePars[jpar];

//   // use the 3 sigma ranges
//   iparmin = thePars[ipar]-3*fabs(theMErr[ipar]);
//   iparmax = thePars[ipar]+3*fabs(thePErr[ipar]);

//   jparmin = thePars[jpar]-3*fabs(theMErr[jpar]);
//   jparmax = thePars[jpar]+3*fabs(thePErr[jpar]);

  Double_t logLmin = 1e100;

  Double_t logLXmax = -1e100;
  Double_t logLYmax = -1e100;

  memset(logLX,0,nPoints*sizeof(Double_t));
  memset(logLY,0,mPoints*sizeof(Double_t));

  for(Int_t i=0;i<nPoints;i++){
    par1X[i] = iparmin + (iparmax-iparmin)*(Double_t)i/(Double_t)(nPoints-1);
    for(Int_t j=0;j<mPoints;j++){
      if(i==0)par2Y[j]=jparmin + (jparmax-jparmin)*(Double_t)j/(Double_t)(mPoints-1);
      par1[i*mPoints+j] = iparmin + (iparmax-iparmin)*(Double_t)i/(Double_t)(nPoints-1);
      par2[i*mPoints+j] = jparmin + (jparmax-jparmin)*(Double_t)j/(Double_t)(mPoints-1);
      thePars[ipar] = par1[i*mPoints+j];
      thePars[jpar] = par2[i*mPoints+j];
      QF2EExtendedLikelihood(npar, &gin, f, thePars, iflag);
      f *= -1;
      logL[i*mPoints+j] = f;
      if(f<logLmin)logLmin=f;
      if(f>logLmax){
	cout << "WARNING: Found new maximum: " << logLmax << " " << f << endl;
	logLmax=f;
      }
//       if(par2[i*mPoints+j]==fFitPar[jpar])logLX[i] = f;
//       if(par1[i*mPoints+j]==fFitPar[ipar])logLY[j] = f;

      logLX[i] += f;
      logLY[j] += f;

      //      printf("%12.4lf %12.4lf %12.4lf\n",par1[i*mPoints+j],par2[i*mPoints+j],logL[i*mPoints+j]);
    }
  }

  for(Int_t i=0;i<nPoints;i++)if(logLX[i]>logLXmax)logLXmax=logLX[i];
  for(Int_t j=0;j<mPoints;j++)if(logLY[j]>logLYmax)logLYmax=logLY[j];

  // compute logLmax - logL for covariance matrix fit, and shift logLmax to zero in par1, par2 plane
  for(Int_t i=0;i<nPoints;i++){
    logLX[i] -= logLXmax;
    par1X[i] -= fFitPar[ipar];
    LX[i] = exp(logLX[i]);
    for(Int_t j=0;j<mPoints;j++){
      if(i==0){
	logLY[j] -= logLYmax;
	par2Y[j] -= fFitPar[jpar];
	LY[j] = exp(logLY[j]);
      }
      logL[i*mPoints+j] -= logLmax;
      par1[i*mPoints+j] -= fFitPar[ipar];
      par2[i*mPoints+j] -= fFitPar[jpar];
      L[i*mPoints+j] = exp(logL[i*mPoints+j]);
//       if(j==12)cout << i << "\t " << i*mPoints+j << "\t " << par1[i*mPoints+j] << "\t " <<  par2[i*mPoints+j] 
// 		    << "\t " << logL[i*mPoints+j] << "\t " << L[i*mPoints+j] << endl;
    }
  }

  TFile *fout = new TFile("/home/juergen/ncd/sigex/sigex/QSigexChecks.root","recreate");

  // set up TGraphs
  TGraph2D *aGraph = new  TGraph2D("logL","log(L)",nPoints*mPoints, par1, par2, logL);
  TGraph2D *aGraphL = new TGraph2D("L","L",nPoints*mPoints, par1, par2, L);
  TGraph   *aGraphX = new TGraph(nPoints, par1X, LX);
  TGraph   *aGraphY = new TGraph(mPoints, par2Y, LY);

  aGraphX->SetName("LX");
  aGraphY->SetName("LY");

  //*** compute the correlation
  // fit projections to get initial values
  aGraphX->Fit("gaus","Q");
  aGraphY->Fit("gaus","Q");
  Double_t VarX = pow(aGraphX->GetFunction("gaus")->GetParameter(2),2);
  Double_t VarY = pow(aGraphY->GetFunction("gaus")->GetParameter(2),2);

  // fit!
  TF2 *Gaus2 = new TF2("gaus2","[3]*exp(-0.5/([0]*[1]-[2]*[2])*( x*x*[1]-2.*x*y*[2]+y*y*[0] ))");
  Gaus2->SetParameters(VarX,VarY,0.1*sqrt(VarX*VarY),0.);
  Gaus2->SetRange(iparmin-fFitPar[ipar],jparmin-fFitPar[jpar],iparmax-fFitPar[ipar],jparmax-fFitPar[jpar]);
  aGraphL->Fit(Gaus2,"WE");

//   cout << "Fit L -->> Correlation " << Gaus2->GetParameter(2)/sqrt(Gaus2->GetParameter(0)*Gaus2->GetParameter(1)) << endl;

  TF2 *LogGaus2 = new TF2("loggaus2","-0.5/([0]*[1]-[2]*[2])*( x*x*[1]-2.*x*y*[2]+y*y*[0] ) + [3]");
  LogGaus2->SetParameters(VarX,VarY,0.1*sqrt(VarX*VarY),0.);
  LogGaus2->SetRange(iparmin-fFitPar[ipar],jparmin-fFitPar[jpar],iparmax-fFitPar[ipar],jparmax-fFitPar[jpar]);
  aGraph->Fit(LogGaus2,"WE");

  //  TString *theMinuitStatus = gMinuit->GetCStatus();
  TString *theMinuitStatus = &(gMinuit->fCstatu);

  Double_t cova = LogGaus2->GetParameter(0);
  Double_t covb = LogGaus2->GetParameter(1);
  Double_t covc = LogGaus2->GetParameter(2);
//   cout << "Fit LogL -> Correlation  " << covc/sqrt(cova*covb) << endl;
  
//   cout << "Gaus2: range " << iparmin-fFitPar[ipar] << " " << jparmin-fFitPar[jpar] << " " << iparmax-fFitPar[ipar] << " " << jparmax-fFitPar[jpar] << endl;
//   cout << "Gaus2: initial values " << VarX << " " << VarY << endl;

//   cout << "Gaus2: " << cova << " " << covb << " " << covc << " " << covc/sqrt(cova*covb) << endl;
//   cout << "Gaus2: Status : " << *(theMinuitStatus) << endl;

  Double_t aCorrelation = covc/sqrt(cova*covb);
  if(*theMinuitStatus!="SUCCESSFUL"){
    aCorrelation=-999;
  }

  aGraph->Write();
  aGraphL->Write();
  aGraphX->Write();
  aGraphY->Write();

  // compute it from the likelihood L=exp(logL)
  Double_t sumL = 0;
  Double_t xAve = 0;
  Double_t yAve = 0;
  Double_t xCov = 0;
  Double_t yCov = 0;
  Double_t xyCo = 0;
  Int_t n = nPoints < mPoints ? nPoints : mPoints;
  for(Int_t i=0;i<n;i++){
    for(Int_t j=0;j<n;j++){
      sumL+=L[i*mPoints+j];
      xAve+=par1[i*mPoints+j]*L[i*mPoints+j];
      yAve+=par2[i*mPoints+j]*L[i*mPoints+j];
    }
  }
  xAve/=sumL;
  yAve/=sumL;
  for(Int_t i=0;i<n;i++){
    for(Int_t j=0;j<n;j++){
      xCov+=(par1[i*mPoints+j]-xAve)*(par1[i*mPoints+j]-xAve)*L[i*mPoints+j];
      yCov+=(par2[i*mPoints+j]-yAve)*(par2[i*mPoints+j]-yAve)*L[i*mPoints+j];
      xyCo+=(par1[i*mPoints+j]-xAve)*(par2[i*mPoints+j]-yAve)*L[i*mPoints+j];
//       printf("%3d %3d %8.3f %8.3f  %8.3f %8.3f  %10.3f  %10.3f %10.3f %10.3f %10.3f  %10.3f %10.3f\n",
// 	     i, j, par1[i*mPoints+j], xAve, par2[i*mPoints+j], yAve, L[i*mPoints+j], 
// 	     (par1[i*mPoints+j]-xAve)*(par1[i*mPoints+j]-xAve)*L[i*mPoints+j], xCov, 
// 	     (par2[i*mPoints+j]-yAve)*(par2[i*mPoints+j]-yAve)*L[i*mPoints+j],yCov, 
// 	     (par1[i*mPoints+j]-xAve)*(par2[i*mPoints+j]-yAve)*L[i*mPoints+j], xyCo );
    }
  }
//   cout << "Averages " << ipar << " " << jpar << " " << xAve << " " << yAve << endl;
//   cout << "Correlation (x-xbar)..." << ipar << " " << jpar << " " << xyCo/sqrt(xCov*yCov) << endl;

  //*** draw
  gROOT->SetStyle("Plain");
  TCanvas *c = new TCanvas("c","C",600,1000);
  c->Divide(1,3);
  TString title = fFitParName[ipar]->Data();
  title += fFitParName[jpar]->Data();
  aGraph->SetTitle(title);

  c->cd(1);
  aGraph->Draw("cont2");
  TLatex tl;
  tl.SetTextSize(0.06);
  tl.SetTextFont(131);
  tl.SetTextAlign(33);
  tl.SetNDC();
  tl.SetTextColor(2);
  tl.DrawLatex(0.88,0.99,Form("Corr = %7.4lf",aCorrelation));

  c->cd(2);
//   TH1F *agx = aGraph->Project("x");
//   agx->Draw();
  aGraphX->Draw("*A");
  aGraphX->Fit("gaus","Q");
  c->cd(3);
//   TH1F *agy = aGraph->Project("y");
//   agy->Draw();
  aGraphY->Draw("*A");
  aGraphY->Fit("gaus","Q");

  TString outfile = theOutputBase;
  outfile += ".png";
  c->Print(outfile.Data());
  outfile = theOutputBase;
  outfile += ".eps";
  c->Print(outfile.Data());  

  // clean up
  delete c;
//   delete agx;
//   delete agy;
  delete aGraph;
  delete aGraphL;
  delete aGraphX;
  delete aGraphY;
  delete Gaus2;
  delete[] logL;
  delete[] logLX;
  delete[] logLY;
  delete[] L;
  delete[] LX;
  delete[] LY;
  delete[] par1;
  delete[] par2;
  delete[] par1X;
  delete[] par2Y;
  delete[] thePars;
  delete[] thePErr;
  delete[] theMErr;

  return aCorrelation;
}
