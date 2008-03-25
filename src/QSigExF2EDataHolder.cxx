// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Modified for flux2events: J. Wendland <mailto:juergen@phas.ubc.ca>

#include "TList.h"
#include "TString.h"
#include "TDirectory.h"
#include "QDatReader.h"
#include "QList.h"
#include "QSigExDis.h"
#include "QSigExF2EDataHolder.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExF2EDataHolder                                                  //
//                                                                      //
// This class holds static member variables and can be used to pass     //
// information between QSigExF2E and the minimization function. Only    //
// the first instance can modify the internal variables, so there's no  //
// risk of making unwanted changes on these ones with other instances.  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(QSigExF2EDataHolder)

QSigExDis**  QSigExF2EDataHolder::fFlux2Events=NULL;
TString** QSigExF2EDataHolder::fAxesNames=NULL;
TString* QSigExF2EDataHolder::fFluxes=NULL;
Int_t** QSigExF2EDataHolder::fAxesIndex=NULL;
Int_t* QSigExF2EDataHolder::fNAxes=NULL;
Int_t* QSigExF2EDataHolder::fTreeIndex=NULL;
Int_t QSigExF2EDataHolder::fNFluxes=0;
Int_t  QSigExF2EDataHolder::fNTrees=0;
Int_t  QSigExF2EDataHolder::fNEntries=0;
Int_t  QSigExF2EDataHolder::fNInstances=0;


//_______________________________________________________________
Int_t QSigExF2EDataHolder::AddMapping(TList* aF2EList)
{
  //  cout << "\t<QSigExF2EDataHolder::AddMapping>" << endl;
  try{
    CheckRights();
    if(aF2EList){
      fNTrees++;
      // get pointers for flux2events TF's
      Int_t nF2EList=aF2EList->GetSize();

      // loop over flux2events entries.
      for(Int_t i=0; i<nF2EList; i++){
	fNEntries++;

	TDirectory *af2emapdir = dynamic_cast<TDirectory*> (aF2EList->At(i));
	// cout << "jw <QSigExF2EDataHolder::AddMapping> " << af2emapdir->GetName() << endl;
	QSigExDis *af2emap = NULL;
	if((af2emap=dynamic_cast<QSigExDis*>(af2emapdir->Get(af2emapdir->GetName())))){

	  // store the address of the QSigExDis mapping object.
	  fFlux2Events=(QSigExDis**)realloc(fFlux2Events,fNEntries*sizeof(QSigExDis*));
	  fFlux2Events[fNEntries-1]=af2emap;

	  // remember the tree, so we can retrieve the correct mapping
	  fTreeIndex=(Int_t*)realloc(fTreeIndex,fNEntries*sizeof(Int_t));
	  fTreeIndex[fNEntries-1]=fNTrees-1;

	  // store the axesnames
	  fAxesNames=(TString**) realloc(fAxesNames,fNEntries*sizeof(TString*));
	  TDirectory *inputsdir = dynamic_cast<TDirectory*> (af2emapdir->Get("Inputs"));
	  Int_t nInputs = inputsdir->GetList()->GetSize();
	  fNAxes = (Int_t*) realloc(fNAxes,fNEntries*sizeof(Int_t));
	  fAxesNames[fNEntries-1] = new TString[nInputs]; 
	  fNAxes[fNEntries-1] = nInputs;
	  for(Int_t j=0; j<nInputs; j++){
// 	    cout << "jw inputs " << inputsdir->GetList()->At(j)->GetName()
// 		 << " " << inputsdir->GetList()->At(j)->GetTitle() << endl;
	    fAxesNames[fNEntries-1][j]=inputsdir->GetList()->At(j)->GetTitle();
	  }
	}
	else{
	  cout << "ERROR in <QSigExF2EDataHolder::AddMapping> no TF object found" << endl;
	  throw 3000;
	}
	  
      }


    }

  }catch(Int_t e){
    cout << "Exception handled by QSigExF2EDataHolder::AddMapping " << e << "\n";
    throw e;
  }
  return 0;
}

//____________________________________________________________________
Int_t QSigExF2EDataHolder::AddFluxIndexing(const Char_t* cardfilename)
{
  PRINTF2(this,"\tvoid QSigExF2EDataHolder::AddFluxIndexing()\n")

  QList<QList<TString> > aFluxCard;
  const Int_t fluxgroupindex = 1;

  if(cardfilename){
    QDatReader aReader;
    aReader.SetFilename(cardfilename);
    aReader.SetKeyword("flux");
    aFluxCard = aReader.GetMany();
  }
  else {
    cout << "<QSigExF2EDataHolder::AddFluxIndexing> ERROR: cardfile not specified.\n";
    throw 3333;
  }

  CheckRights();

  // store the names of the fluxes in the order they are given in the cardfile
  fNFluxes = aFluxCard.Count();
  fFluxes = new TString[fNFluxes];
  for(Int_t i=0; i<fNFluxes; i++){
//     cout << "jw QSigExF2EDataHolder::AddFluxIndexing aFluxCard " << fNFluxes 
//  	 << " " << i << " " << "  "<< aFluxCard[i][fluxgroupindex].Data() << "\n";

    fFluxes[i] = aFluxCard[i][fluxgroupindex].Data();
  }

  fAxesIndex = new Int_t*[fNEntries];
  for(Int_t i=0; i<fNEntries; i++){
    fAxesIndex[i] = new Int_t[fNAxes[i]];
    for(Int_t j=0; j<fNAxes[i]; j++){
      fAxesIndex[i][j]=-999;
      for(Int_t k=0; k<fNFluxes; k++){
	if (fAxesNames[i][j]==fFluxes[k]) {
//	  cout << "jw QSigExF2EDataHolder::AddFluxIndexing " << i << " " << fAxesNames[i][0] << " " << k << "\n";
// 	  cout << "Match " << i << j << k << "\n";
	  fAxesIndex[i][j]=k;
	}
      }
      if(fAxesIndex[i][j]<0){
	cout << "<QSigExF2EDataHolder::AddFluxIndexing> ERROR, axes name " << fAxesNames[i][j] << " not found among fluxes.\n";
	throw 999;
      }
    }
  }

  return 0;
}


//_____________________________________
void QSigExF2EDataHolder::CheckRights()
{
  PRINTF2(this,"\tvoid QSigExF2EDataHolder::CheckRights()\n")
    if(fNInstances!=1){
      cout << "QSigExF2EDataHolder::CheckRights: Cannot set variables using this instance, only the first instance can\n";
      throw 2001;
    }
}

#include "debugger.h"
