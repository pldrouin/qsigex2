// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QSigExFitDataHolder.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExFitDataHolder                                                  //
//                                                                      //
// This class holds static member variables and can be used to pass     //
// information between QSigExFit and the minimization function. Only    //
// one instance can modify the internal variables, so there is no       //
// risk of making unwanted changes on these ones with other instances.  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(QSigExFitDataHolder)

TTree** QSigExFitDataHolder::fJProbsTrees=NULL;
Bool_t* QSigExFitDataHolder::fOwnsJPTree=NULL;
Int_t QSigExFitDataHolder::fNJPTrees=0;
Double_t** QSigExFitDataHolder::fEntries=NULL;
Int_t** QSigExFitDataHolder::fParIndexes=NULL;
const QList<TString>* QSigExFitDataHolder::fParNames=NULL;
const TList* QSigExFitDataHolder::fVariabs=NULL;
Int_t QSigExFitDataHolder::fNInstances=0;
QSigExFitDataHolder* QSigExFitDataHolder::fPrincipalInstance=NULL;

QSigExFitDataHolder::~QSigExFitDataHolder() {
  fNInstances--;
  if(!fNInstances){

    for(Int_t i=0; i<fNJPTrees; i++){

      if(fJProbsTrees[i]){
	QTTreeUtils::ClearBranchesAddresses(fJProbsTrees[i]);

	if(fOwnsJPTree[i]){
	  delete fJProbsTrees[i];
	  fJProbsTrees[i]=NULL;
	}
      }
      delete[] fEntries[i];
      delete[] fParIndexes[i];
    }
    free(fJProbsTrees);
    free(fEntries);
    free(fParIndexes);
    fNJPTrees=0;
    fParIndexes=NULL;
    fEntries=NULL;
    fJProbsTrees=NULL;
  }
}

void QSigExFitDataHolder::AddJProbs(TTree* jprobstree, Bool_t ownsjptree)
{
  try{
    SetPrincipalInstance(this);

    if(fParNames){
      fJProbsTrees=(TTree**)realloc(fJProbsTrees,(++fNJPTrees)*sizeof(TTree*));
      fOwnsJPTree=(Bool_t*)realloc(fOwnsJPTree,fNJPTrees*sizeof(Bool_t));
      fEntries=(Double_t**)realloc(fEntries,fNJPTrees*sizeof(Double_t*));
      fParIndexes=(Int_t**)realloc(fParIndexes,fNJPTrees*sizeof(Int_t*));
      fJProbsTrees[fNJPTrees-1]=jprobstree;
      fOwnsJPTree[fNJPTrees-1]=ownsjptree;
      fEntries[fNJPTrees-1]=new Double_t[jprobstree->GetNbranches()];
      fParIndexes[fNJPTrees-1]=new Int_t[jprobstree->GetNbranches()];
      Int_t i;
      TString sbuf;
      QList<Int_t> lbuf;

      for(i=0; i<jprobstree->GetNbranches(); i++){
	dynamic_cast<TBranch*>(jprobstree->GetListOfBranches()->At(i))->SetAddress(fEntries[fNJPTrees-1]+i);
	sbuf=jprobstree->GetListOfBranches()->At(i)->GetName();
	lbuf=fParNames->Find(sbuf);

	if(!lbuf.Count()){
	  cout << "Error: Joint Probabilities Tree Branch '" << sbuf.Data() << "' not found among fit parameters\n";
	  throw 1;

	}else{
	  (fParIndexes[fNJPTrees-1])[i]=lbuf[0];
	}
      }

    }else{
      cout << "Error: fParNames pointer is NULL\n";
      throw 1;
    }

  }catch(Int_t e){
    cout << "Exception handled by QSigExFitDataHolder::AddJProbs\n";
    throw e;
  }
}


void QSigExFitDataHolder::RmJProbs(TTree* jprobstree)
{
  try{
    SetPrincipalInstance(this);

    Int_t ibuf=-1;
    Int_t i;

    for(i=0; i<fNJPTrees; i++){

      if(fJProbsTrees[i]==jprobstree){
	ibuf=i;
	break;
      }
    }

    if(ibuf==-1){
      cout << "Error: jprobstree not found among the list of joint probabilities trees\n";
      throw 1;
    }

    if(fNJPTrees>1){
      if(fOwnsJPTree[ibuf]) delete fJProbsTrees[ibuf];
      delete[] fEntries[ibuf];
      delete[] fParIndexes[ibuf];

      for(i=ibuf; i<fNJPTrees-1; i++){
	fJProbsTrees[i]=fJProbsTrees[i+1];
	fOwnsJPTree[i]=fOwnsJPTree[i+1];
	fEntries[i]=fEntries[i+1];
	fParIndexes[i]=fParIndexes[i+1];
      }
      fJProbsTrees=(TTree**)realloc(fJProbsTrees,(--fNJPTrees)*sizeof(TTree*));
      fOwnsJPTree=(Bool_t*)realloc(fOwnsJPTree,fNJPTrees*sizeof(Bool_t));
      fEntries=(Double_t**)realloc(fEntries,fNJPTrees*sizeof(Double_t*));
      fParIndexes=(Int_t**)realloc(fParIndexes,fNJPTrees*sizeof(Int_t*));

    }else{
      if(fOwnsJPTree[0]) delete fJProbsTrees[0];
      delete[] fEntries[0];
      delete[] fParIndexes[0];
      free(fJProbsTrees);
      free(fOwnsJPTree);
      free(fEntries);
      free(fParIndexes);
      fNJPTrees=0;
      fParIndexes=NULL;
      fEntries=NULL;
      fJProbsTrees=NULL;
      fOwnsJPTree=NULL;
    }

  }catch(Int_t e){
    cout << "Exception handled by QSigExFitDataHolder::RmJProbs\n";
    throw e;
  }
}

void QSigExFitDataHolder::Clear() {
  try{
    SetPrincipalInstance(this);

    for(Int_t i=0; i<fNJPTrees; i++){
      QTTreeUtils::ClearBranchesAddresses(fJProbsTrees[i]);
      if(fOwnsJPTree[i]) delete fJProbsTrees[i];
      delete[] fEntries[i];
      delete[] fParIndexes[i];
    }
    free(fJProbsTrees);
    free(fEntries);
    free(fParIndexes);
    fNJPTrees=0;
    fParIndexes=NULL;
    fEntries=NULL;
    fJProbsTrees=NULL;
  }catch(Int_t e){
    cout << "Exception handled by QSigExFitDataHolder::Clear\n";
    throw e;
  }
}

#include "debugger.h"
