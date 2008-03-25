//
// Run an ensemble test
// 
// Authors: P.L. Drouin and A. Bellerive
//

#include <typeinfo>

#include "bias.h"
#include "TString.h"
#include "TRandom.h"
#include "TMath.h"

// Run
int main(int nargs, char* args[])
{
  if(nargs==1 || nargs>4){
    cout <<
   "Error: Wrong arguments format\nShould be: bias cardfile [rootfile] [rho]\nwhere cardfile is the configuration file and where rootfile is an optional\nargument that specifies the root file that will be created to put the\nresults (default is results.root) and where rho is an optional argument that specifies the correlation for the first class\n(default is rho=0.0)\n";
    exit(1);
  }
  TString outfile="qsigex.root";
  const Char_t* outfile1=outfile;
  cout << outfile1 << "\n";
  const Char_t* outfile2;
  if(nargs==2){
    outfile2="results.root";
  }else{
    outfile2=args[2];
  }

  //
  // Generate the PDF for classes 1 (C1), 2 (C2) and 3 (C3) 
  // Set the correlation for the inputs of C1
  //
  UInt_t seed=1;
  Float_t rho=0.0;
  if(nargs==4){
    rho=atof(args[3]);
  }
  cout << " * * * START HERE * * * " << "\n";
  cout << "args1 = " << args[1] << "\n";
  cout << "rho = " << rho << "\n";

  //
  // Here you tell the program how many events for the template PDF
  // Generate three classes of event (see mcgen.cxx)
  //
  //  seed=mcgen(1000000,1000000,1000000,seed,rho);
  seed=mcgen(10000,10000,10000,seed,rho);
  //  seed=mcgen(100,100,100,seed,rho);

  TFile f1(outfile1,"RECREATE");

  QSigExCuts cuts(&f1,args[1]);
  cuts.Get();

  QSigExStdPDFs spdfs(&f1,args[1]);
  spdfs.Get();

  QSigExTTreePDF tpdfs(&f1,args[1]);
  tpdfs.Get();

  QSigExGaussCor gcor(&f1);
  gcor.Get();

  f1.Write();
  f1.Close();

  //
  // Prepare the root file for the output
  //
  TNamed *nbuf;
  Double_t c1f,c2f,c3f,c1e,c2e,c3e,c1g,c2g,c3g,ebuf;
  TFile f2(outfile2,"RECREATE");
  TTree* btree=new TTree("btree","Bias Tree");
  btree->Branch("c1fit",&c1f,"c1fit/D");
  btree->Branch("c2fit",&c2f,"c2fit/D");
  btree->Branch("c3fit",&c3f,"c3fit/D");
  btree->Branch("c1err",&c1e,"c1err/D");
  btree->Branch("c2err",&c2e,"c2err/D");
  btree->Branch("c3err",&c3e,"c3err/D");
  btree->Branch("c1gen",&c1g,"c1gen/D");
  btree->Branch("c2gen",&c2g,"c2gen/D");
  btree->Branch("c3gen",&c3g,"c3gen/D");

  QSigExCleanData cleandata;
  cleandata.LoadCardFile(args[1]);
  QSigExProbs<> probs;
  QSigExGCJointProbs gcjprobs;
  QSigExFit fitter;
  fitter.SetFCN(QExtendedLikelihood);
  fitter.LoadCardFile(args[1]);

  //
  // Generate the ensemble test
  //
  // Here you tell the program how many experiments (i.e. sample)
  // to generate (see datagen.cxx)
  //
  //
  //  Int_t nsamples=10000;
  //  Int_t nsamples=1000;
  Int_t nsamples=100;
  //  Int_t nsamples=10;
  QProgress progress(nsamples);
  Int_t i;
  for(i=0;i<nsamples;i++){

    TFile f3(outfile1,"UPDATE");
    cout << "\n";
    progress(i);
    cout << "\n";

    // The total number of events and the fraction for each class
    // Stotal is Poisson distributed with mean SMean and uses a uniform 
    // random generator with a test on the fraction 1/3 - 2/3 to assign 
    // S_1, S_2, and S_3.
    // REMARK: this is equivalent at generating three S_i to be Poisson
    // generated with Smean/3 (it was checked!).
    Int_t stot;
    Double_t smean=900,sprob=0;
    cout << "Seed Poisson: " << seed << "\n";
    TRandom rnd(seed);
    stot=rnd.Poisson(smean);  
    seed=rnd.GetSeed();    
    cout << "Seed Samples: " << seed << "\n";
    Int_t j,s1=0,s2=0,s3=0;
    for(j=0;j<stot;j++){
      sprob=rnd.Rndm();
      if (sprob>0.6667) s3=s3++;
      else if (sprob<0.3334) s1=s1++;
      else s2=s2++; 
    }
    cout << "Total number of events: " << stot  << "\n";
    cout << "Per class s1-s2-s3: " << s1 << " " << s2 << " " << s3  << "\n";
    seed=datagen(s1,s2,s3,seed,rho);
    c1g=s1;
    c2g=s2;
    c3g=s3;
    cout << "Seed: " << seed << "\n";

    //QSigExCleanData cleandata(&f3,args[1]);
    cleandata.SetDir(&f3);
    cleandata.CleanDir();
    cleandata.Get();
    //QSigExProbs probs(&f3);
    probs.SetDir(&f3);
    probs.CleanDir();
    probs.Get();
    //QSigExGCJointProbs gcjprobs(&f3);
    gcjprobs.SetDir(&f3);
    gcjprobs.CleanDir();
    gcjprobs.Get();
    //QSigExFit fitter(&f3,QExtendedLikelihood,args[1]);
    fitter.SetDir(&f3);
    fitter.CleanDir();
    fitter.Get();

    // Do the fit and get the info from the TTree
    f3.cd("Fit");
    gDirectory->cd("Numbers");
    gDirectory->cd("c1");
    nbuf=dynamic_cast<TNamed*>(gDirectory->Get("FitValue"));
    sscanf(nbuf->GetTitle(),"%lf",&c1f);
    nbuf->Delete();
    nbuf=dynamic_cast<TNamed*>(gDirectory->Get("PlusFitError"));
    sscanf(nbuf->GetTitle(),"%lf",&c1e);
    nbuf->Delete();
    nbuf=dynamic_cast<TNamed*>(gDirectory->Get("MinusFitError"));
    sscanf(nbuf->GetTitle(),"%lf",&ebuf);
    c1e=(c1e-ebuf)/2;

    f3.cd("Fit");
    gDirectory->cd("Numbers");
    gDirectory->cd("c2");
    nbuf=dynamic_cast<TNamed*>(gDirectory->Get("FitValue"));
    sscanf(nbuf->GetTitle(),"%lf",&c2f);
    nbuf->Delete();
    nbuf=dynamic_cast<TNamed*>(gDirectory->Get("PlusFitError"));
    sscanf(nbuf->GetTitle(),"%lf",&c2e);
    nbuf->Delete();
    nbuf=dynamic_cast<TNamed*>(gDirectory->Get("MinusFitError"));
    sscanf(nbuf->GetTitle(),"%lf",&ebuf);
    c2e=(c2e-ebuf)/2;

    f3.cd("Fit");
    gDirectory->cd("Numbers");
    gDirectory->cd("c3");
    nbuf=dynamic_cast<TNamed*>(gDirectory->Get("FitValue"));
    sscanf(nbuf->GetTitle(),"%lf",&c3f);
    nbuf->Delete();
    nbuf=dynamic_cast<TNamed*>(gDirectory->Get("PlusFitError"));
    sscanf(nbuf->GetTitle(),"%lf",&c3e);
    nbuf->Delete();
    nbuf=dynamic_cast<TNamed*>(gDirectory->Get("MinusFitError"));
    sscanf(nbuf->GetTitle(),"%lf",&ebuf);
    c3e=(c3e-ebuf)/2;

    f2.cd();
    btree->Fill();

    f3.Write();
    f3.Close();
  }

  progress(i,kTRUE);


  f2.Write();
  f2.Close();

  cout << " * * * DONE HERE * * * " << "\n";

}
