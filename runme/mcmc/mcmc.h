#include <cstdio>
#include "Rtypes.h"
#include "TFile.h"
#include "QDisTH.h"
#include "QSigExFitMinuit.h"
#include "QSigExFitMCMC.h"
#include "QProcObjProcessor.h"
#include "QProcBranchHandler.h"
#include "QProcDouble.h"

int main(int nargs, char* args[]);

Bool_t pdf(QProcArgs &args);
Bool_t fillparams(QProcArgs &args);

void PDFFunction(Int_t&, Double_t*, Double_t &f, Double_t *, Int_t);
void ELLFunction(Int_t&, Double_t*, Double_t &f, Double_t *, Int_t);
