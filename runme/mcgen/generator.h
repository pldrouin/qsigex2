#include <iostream>
#include <cstdio>
#include "Rtypes.h"
#include "TTree.h"
#include "TRandom.h"
#include "TMath.h"
#include "QProgress.h"

using namespace std;

int main(int nargs, char* args[]);
UInt_t c1gen(TTree* tree, UInt_t seed, Int_t nc, Float_t rho);
UInt_t c2gen(TTree* tree, UInt_t seed, Int_t nc);
UInt_t c3gen(TTree* tree, UInt_t seed, Int_t nc);
