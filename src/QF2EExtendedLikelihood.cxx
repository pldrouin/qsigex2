#include "QF2EExtendedLikelihood.h"

// version 0.1- J. Wendland

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

void QF2EExtendedLikelihood(Int_t&, Double_t*,Double_t &f, Double_t* par,Int_t iflag)
{
  PRINTF11("\t\tvoid QF2EExtendedLikelihood(Int_t &npar<",npar,">, Double_t *gin<",gin,">,Double_t &f<",f,">, Double_t *par<",par,">,Int_t iflag<",iflag,">)\n")

  // Fitting function used by Minuit.  Returns the **negative** log
  // likelihood.  Designed to combine several data sets in one
  // likelihood function.  In the fit the fit parameters are fluxes
  // that give event numbers through mapping functions (flux2events).
  // this function can handle any number of parameters, some of which
  // can have fixed amplitudes (such as backgrounds)
  
  Int_t i, j, k, l;
  
  QSigExFitDataHolder NuFit;
  QSigExF2EDataHolder F2EFit;

  Int_t NumF2EMaps = F2EFit.GetNMappings();

  const Int_t NTrees = NuFit.GetNJPTrees();

  f=0;

  for(Int_t itree=0; itree<NTrees; itree++){
//     cout << "next tree " << itree << "\n";
//    if(itree==0)continue; // TMP TMP fit only ncd data
    Int_t NuNData = NuFit.GetNEvents(itree); //get # of events for tree itree
    Int_t NumGroup = NuFit.GetNGroups(itree);

    Double_t P=0, F=0, N=0; //N is the total number of expected events

    const Double_t* entry;
    const Int_t* parindices=NuFit.GetParIndices(itree);
//     for(i=0; i<NumGroup; i++)cout<<"parindices "<<itree<<" "<<i<<" "<<parindices[i]<<"\n";

    Double_t *events = new Double_t[NumGroup]; // number of events estimated given the flux in par[.]
    for (i=0; i<NuNData; i++) { // loop over data points
      P = 0;
      entry=NuFit.GetEntry(itree,i);

      for (j=0; j<NumGroup; j++) { // loop over fit parameters par[.]
	events[j]=-999;
//  	cout << "next j = " << j << " " << NumGroup << " " << parindices[j] << " " << npar << "\n";
	for(k=0; k<NumF2EMaps; k++){
//   	  if(iflag==1)cout << "<QF2EExtendedlikelihood>: " << NumF2EMaps << " " << k << " " << j << " " 
//  	       << parindices[j] << " " << F2EFit.GetAxesIndex(k,0) << " "
//  	       << F2EFit.GetTreeIndex(k) << " " << itree << "\n";
	    if(F2EFit.GetAxesIndex(k,0)==parindices[j] && F2EFit.GetTreeIndex(k)==itree){
	      events[j] = F2EFit.GetMapping(k)->ProbDensity(par[parindices[j]], 0., 0.);
//   	      if(i==0)cout << "match " << i << " " << itree << " " << j << " " << parindices[j] << " " << par[parindices[j]] << " " 
//   		   << F2EFit.GetMapping(k)->ProbDensity(par[parindices[j]], 0., 0.) << " " << events[j] << "\n";
	    break;
	  }	  
	}
	if(events[j]<-900){
	  cout << "<QF2EExtendedLikelihood>: No match found " << itree << " " << NumGroup << " " << j << " " 
	       << parindices[j] << "\n";
	  throw 1;
	}

//	Int_t ioffset = parindices[0];
// 	if(itree==1){
// 	cout << "Tree " << itree;
// 	cout << " Event " << i << "/" << NuNData;
//   	cout << " Group " << j << "/" << NumGroup << "... " << parindices[j];
// 	cout << "\t" << events[j];
//  	cout << "\t" << par[parindices[j]];
// 	cout << "\t" << ioffset;
//   	cout << "\t" << *(entry+parindices[j]-ioffset);
//   	cout << "\n";
// 	}
	P += events[j] * (*(entry+j)); //return pdf value for parameter j, given values of coordinates at event i.
      }

//       cout << "\n";

      if ( P > 0 ) { F += log(P);} //else {cout << "P <= 0: itree/idata = " << itree<<"/"<<i<< ", P = " << P << "\n";}
    }
    for (l=0; l<NumGroup; l++) { N += events[l];} //add up the parameters
    delete events;
  
    f += 
      (2. * N) //the "extended" part of the log L function
      -2. * F;

    if(iflag==1) for(l=0; l<NumGroup; l++) cout << "parameter " << l << " " << par[parindices[l]] << "\n";
    if(iflag==1) cout << "\t\tlikelihood: " << f << ")\n";
  }
  
}

#include "debugger.h"



