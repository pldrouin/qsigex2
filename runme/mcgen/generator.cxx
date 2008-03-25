//
// Generate the samples
// 
// Authors: P.L. Drouin and A. Bellerive
//

#include <typeinfo>
#include "generator.h"
#include "datsrc.h"

//
// Class 1: two inputs with correlation [Two Gaussians]
//
UInt_t c1gen(TTree* tree, UInt_t seed, Int_t nc, Float_t rho)
{

  TRandom rnd(seed);

  QProgress progress(nc);
  Int_t i;
  Float_t m1=0.0,m2=-2.2,sig1=0.8,sig2=1.0;
  Float_t &x=*((Float_t*)tree->GetBranch("x")->GetAddress());
  Float_t &y=*((Float_t*)tree->GetBranch("y")->GetAddress());
  static integer n, iseed1, iseed2;
  static double a[2], c[4], xx[2], dplus[4];

  cout << "generator: rho = " << rho << "\n";
  a[0] = m1; // mean x1
  a[1] = m2; // mean x2 
  // covariance matrix
  c[0] = sig1*sig1;
  c[3] = sig2*sig2;
  c[1] = rho*sig1*sig2;
  c[2] = rho*sig1*sig2;
  // initiate generator for multivariate normally distributes numbers
  n = 2;
  rnmnpr_(c, dplus, &n); // routine by Brandt

  // show current seeds for datan routines
    rne2ot_(&iseed1, &iseed2);
    cout << "Current seeds are = " << iseed1 << " " << iseed2 << "\n";

  if(nc>0){
    cout << "Generating c1\n";
    for(i=0;i<nc;i++){
      progress(i);

	rnmngn_(dplus, a, xx, &n); // routine by Brandt
        x=xx[0];
        y=xx[1];
     
      tree->Fill();

    }
    progress(i,kTRUE);
    cout << "\n";
  }

  return rnd.GetSeed();
}

//
// Class 2: two inputs with no correlation [acceptance-rejection method]
//
UInt_t c2gen(TTree* tree, UInt_t seed, Int_t nc)
{

  TRandom rnd(seed);

  QProgress progress(nc);
  Int_t i;
  Float_t f,s,t,u;
  Float_t &x=*((Float_t*)tree->GetBranch("x")->GetAddress());
  Float_t &y=*((Float_t*)tree->GetBranch("y")->GetAddress());

  if(nc>0){
    cout << "Generating c2\n";
    progress.SetMaxVal(nc);
    progress.Init();
    for(i=0;i<nc;i++){
      progress(i);
      do{
	u=rnd.Rndm();
	s=(rnd.Rndm()*6-3);
	f=TMath::Exp(-0.25*(s+3));
      }while(f<=u);
      x=s; //x betwwen -3 and 3

      do{
	u=rnd.Rndm();
	t=(rnd.Rndm()*12-6);
	f=TMath::Exp(-0.25*(t+6));
      }while(f<u); //y betwwen -6 and 6
      y=t;

      tree->Fill();
    }
    progress(i,kTRUE);
    cout << "\n";
  }

  return rnd.GetSeed();
}

//
// Class 3: two inputs with no correlation [acceptance-rejection method]
//
UInt_t c3gen(TTree* tree, UInt_t seed, Int_t nc)
{

  TRandom rnd(seed);

  QProgress progress(nc);
  Int_t i;
  Float_t f,s,t,u;
  Float_t &x=*((Float_t*)tree->GetBranch("x")->GetAddress());
  Float_t &y=*((Float_t*)tree->GetBranch("y")->GetAddress());

  if(nc>0){
    cout << "Generating c3\n";
    progress.SetMaxVal(nc);
    progress.Init();
    for(i=0;i<nc;i++){
      progress(i);
      do{     
	u=rnd.Rndm()/TMath::Sqrt(8*TMath::Pi()); // nomalized [-inft,+inft]
	s=(rnd.Rndm()*6-3);
	f=TMath::Gaus(s,1,2,kTRUE);
      }while(f<u);                   
      x=s;        

      do{         
	u=rnd.Rndm()/TMath::Sqrt(8*TMath::Pi()); // nomalized [-inft,+inft]
	t=(rnd.Rndm()*12-6);
	f=TMath::Gaus(t,1,2,kTRUE);
      }while(f<=u);                       
      y=t;

      tree->Fill();
    }                   
    progress(i,kTRUE);
    cout << "\n";
  }

  return rnd.GetSeed();
}



