#include "QExtendedLikelihood.h"

// version 1.1- D.Grant - fixed bug (initialize P=0 on for each event)

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

void QExtendedLikelihood(Int_t&, Double_t*,Double_t &f, Double_t *par,Int_t)
{
  PRINTF11("\t\tvoid QExtendedLikelihood(Int_t &npar<",npar,">, Double_t *gin<",gin,">,Double_t &f<",f,">, Double_t *par<",par,">,Int_t iflag<",iflag,">)\n")

  //Fitting function used by Minuit.  Returns the negative log likelihood.  
  //Designed to handle any number of parameters, some of which can have 
  //fixed amplitudes (such as backgrounds)
  
  Int_t i, j,l;
  
  QSigExFitDataHolder NuFit;

  Int_t NuNData = NuFit.GetNEvents(0); //get # of events
  Int_t NumGroup = NuFit.GetNGroups(0);

  Double_t P=0,F=0,N = 0; //N is the total number of expected events

  const Double_t* entry;
  const Int_t* parindices=NuFit.GetParIndices(0);

  //cout << "ok-1\n";

  for (i=0; i<NuNData; i++) {
    P = 0;
    entry=NuFit.GetEntry(0,i);
    for (j=0; j<NumGroup; j++) {
      //cout << "Group " << j << "/" << NumGroup << "... ";
      //      cout << "\t" << *(entry+j);
      P += par[parindices[j]] * (*(entry+j)); //return pdf value for parameter
      //j, given values of coordinates at event i.
      //cout << "done\n";
    }
    //    cout << "\n";
	if ( P > 0 ) { F += log(P);}
  }

  for (l=0; l<NumGroup; l++) { N += par[l];} //add up the parameters

  //for (m=0; m<NumFixPar; m++) { N += FixPar[m];} //add up the fixed par.

  f = 
    (2. * N) //the "extended" part of the log L function
    -2. * F;
}

#include "debugger.h"



