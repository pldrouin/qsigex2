//
// Generate the templates PDF (see generator.cxx)
// 
// Authors: P.L. Drouin and A. Bellerive
//

#include "mcgen.h"

UInt_t mcgen(Int_t nc1, Int_t nc2, Int_t nc3, UInt_t seed, Float_t rho)
{
  TString outfile="mc.root";
  const Char_t* mcfile=outfile;
  TFile mc(mcfile,"RECREATE");
  Float_t x,y;

  TTree* treec1=new TTree("c1mctree","c1mctree");
  treec1->Branch("x",&x,"x/F");
  treec1->Branch("y",&y,"y/F");
  seed=c1gen(treec1,seed,nc1,rho);
  TH2F* c1xy=new TH2F("c1xy","c1xy",100,treec1->GetMinimum("x"),treec1->GetMaximum("x"),100,treec1->GetMinimum("y"),treec1->GetMaximum("y"));
  treec1->Draw("y:x>>c1xy","1","gOff");
  TH1F* c1x=new TH1F("c1x","c1x",100,treec1->GetMinimum("x"),treec1->GetMaximum("x"));
  treec1->Draw("x>>c1x","1","gOff");
  TH1F* c1y=new TH1F("c1y","c1y",100,treec1->GetMinimum("y"),treec1->GetMaximum("y"));
  treec1->Draw("y>>c1y","1","gOff");

  TTree* treec2=new TTree("c2mctree","c2mctree");
  treec2->Branch("x",&x,"x/F");
  treec2->Branch("y",&y,"y/F");
  seed=c2gen(treec2,seed,nc2);
  TH2F* c2xy=new TH2F("c2xy","c2xy",100,treec2->GetMinimum("x"),treec2->GetMaximum("x"),100,treec2->GetMinimum("y"),treec2->GetMaximum("y"));
  treec2->Draw("y:x>>c2xy","1","gOff");
  TH1F* c2x=new TH1F("c2x","c2x",100,treec2->GetMinimum("x"),treec2->GetMaximum("x"));
  treec2->Draw("x>>c2x","1","gOff");
  TH1F* c2y=new TH1F("c2y","c2y",100,treec2->GetMinimum("y"),treec2->GetMaximum("y"));
  treec2->Draw("y>>c2y","1","gOff");

  TTree* treec3=new TTree("c3mctree","c3mctree");
  treec3->Branch("x",&x,"x/F");
  treec3->Branch("y",&y,"y/F");
  seed=c3gen(treec3,seed,nc3);
  TH2F* c3xy=new TH2F("c3xy","c3xy",100,treec3->GetMinimum("x"),treec3->GetMaximum("x"),100,treec3->GetMinimum("y"),treec3->GetMaximum("y"));
  treec3->Draw("y:x>>c3xy","1","gOff");
  TH1F* c3x=new TH1F("c3x","c3x",100,treec3->GetMinimum("x"),treec3->GetMaximum("x"));
  treec3->Draw("x>>c3x","1","gOff");
  TH1F* c3y=new TH1F("c3y","c3y",100,treec3->GetMinimum("y"),treec3->GetMaximum("y"));
  treec3->Draw("y>>c3y","1","gOff");

  mc.Write();
  mc.Close();

  return seed;
}
