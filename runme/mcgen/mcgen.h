#ifndef _MCGEN_
#define _MCGEN_

#include "Rtypes.h"
#include "TString.h"
#include "TFile.h"
#include "TTree.h"
#include "TH1F.h"
#include "TH2F.h"
#include "generator.h"

UInt_t mcgen(Int_t nc1, Int_t nc2, Int_t nc3, UInt_t seed=0, Float_t rho=0.0);

#endif
