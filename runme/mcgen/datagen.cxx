//
// Generate the data for the experiments (see generator.cxx)
// 
// Authors: P.L. Drouin and A. Bellerive
//

#include "datagen.h"

UInt_t datagen(Int_t nc1, Int_t nc2, Int_t nc3, UInt_t seed, Float_t rho)
{
  Float_t x,y;

  TString outfile="data.root";
  const Char_t* datafile=outfile;
  TFile data(datafile,"RECREATE");
  TTree* datatree=new TTree("datatree","datatree");
  datatree->Branch("x",&x,"x/F");
  datatree->Branch("y",&y,"y/F");
  seed=c1gen(datatree,seed,nc1,rho);
  seed=c2gen(datatree,seed,nc2);
  seed=c3gen(datatree,seed,nc3);
  data.Write();
  data.Close();

  return seed;
}

