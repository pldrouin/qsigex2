#include <cstdio>
#include "Rtypes.h"
#include "TFile.h"
#include "TTree.h"
#include "QHN.h"
#include "QSigExFitMinuit.h"
#include "QArrayProcessor.h"

int main(int nargs, char* args[]);
Bool_t PDFEval(QProcArgs &args);
Bool_t Selector(QProcArgs &args);
Bool_t Multiply(QProcArgs &args);
Bool_t LLSum(QProcArgs &args);

void ELLFunction(Int_t&, Double_t*, Double_t &f, Double_t *, Int_t);
