#include <typeinfo>

// Check:
// http://www.physics.carleton.ca/research/sno/anal/software/qsigex_run.html
//

#include "rund2o.h"
//#include "TRandom.h"
//#include "TMath.h"

int main(int nargs, char* args[])
{
  if(nargs==1 || nargs>3){
    cout <<
      "Error: Wrong arguments format\nShould be: runme cardfile [rootfile]\nwhere cardfile is the configuration file and where rootfile is the optional\nargument that specifies the file that will be created to put the results\n(default is results.root)\n";
    exit(1);
  }
  const Char_t* outfile;
  if(nargs==2){
    outfile="results.root";
  }else{
    outfile=args[2];
  }

  TFile f1(outfile,"RECREATE");
  QSigExStruct* qstruct=new QSigExStruct("QSigEx","QSigEx");

  QSigExCuts cuts(qstruct,args[1]);
  cuts.Get();

  QSigExCleanData cleandata(qstruct,args[1]);
  cleandata.Get();

  QSigExStdPDFs spdfs(qstruct,args[1]);
  spdfs.Get();

  QSigExTTreePDF tpdfs(qstruct,args[1]);
  tpdfs.Get();

  QSigExGaussCor gcor(qstruct);
  gcor.Get();

  QSigExProbs<> probs(qstruct);
  probs.Get();

  QSigExGCJointProbs gcjprobs(qstruct);
  gcjprobs.Get();

  QSigExFit *fitter;

  for(Int_t i=0; i<1; i++){
    fitter=new QSigExFit(qstruct,QExtendedLikelihood,args[1]);
    fitter->Get();
    fitter->CleanDir();
    delete fitter;
  }

  f1.Write();
  f1.Close();
}
