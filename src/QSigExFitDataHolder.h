// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExFitDataHolder                                                  //
//                                                                      //
// This class holds static member variables and can be used to pass     //
// information between QSigExFit and the minimization function. Only    //
// one instance can modify the internal variables, so there is          //
// no risk of making unwanted changes on these ones with other          //
// instances.                                                           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef _QSIGEXFITDATAHOLDER_
#define _QSIGEXFITDATAHOLDER_

#include <iostream>
#include <cstdlib>
#include "Rtypes.h"
#include "TTree.h"
#include "TList.h"
#include "TNamed.h"
#include "QList.h"
#include "QTTreeUtils.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

using std::cout;

class QSigExFitDataHolder
{
  public:
  QSigExFitDataHolder() {fNInstances++;}

  QSigExFitDataHolder(const QList<TString>* parnames, const TList* variabs) {
    fNInstances++;
    SetParNames(parnames);
    SetVariabs(variabs);
  }

  virtual ~QSigExFitDataHolder();

  void AddJProbs(TTree* jprobstree, Bool_t ownsjptree=kTRUE);

  void RmJProbs(TTree* jprobstree);

  void SetVariabs(const TList* variabs)
  {
    try{
      SetPrincipalInstance(this);
      fVariabs=variabs;
    }catch(Int_t e){
      cout << "Exception handled by QSigExFitDataHolder::SetVariabs\n";
      throw e;
    }
  }

  void SetParNames(const QList<TString>* parnames)
  {
    try{
      SetPrincipalInstance(this);
      fParNames=parnames;
    }catch(Int_t e){
      cout << "Exception handled by QSigExFitDataHolder::SetParNames\n";
      throw e;
    }
  }

  void SetPrincipalInstance(QSigExFitDataHolder* principalinstance)
  {

    if(!fPrincipalInstance || fPrincipalInstance==this || fNInstances==1) fPrincipalInstance=principalinstance;

    else {
      cout << "QSigExFitDataHolder::SetPrincipalInstance: The principal instance is already set\n";
      throw 2002;
    }
  }

  Int_t GetNJPTrees() const{return fNJPTrees;}
  Int_t GetNEvents(Int_t jptreeindex) const
  {
    if(jptreeindex<0 || jptreeindex>=fNJPTrees){
      cout << "QSigExFitDataHolder::GetNEvents: invalid index value\n";
      throw 1;
    }
    return (Int_t)(fJProbsTrees[jptreeindex]->GetEntries());
  }

  Int_t GetNGroups(Int_t jptreeindex) const
  {
    if(jptreeindex<0 || jptreeindex>=fNJPTrees){
      cout << "QSigExFitDataHolder::GetNGroups: invalid index value\n";
      throw 1;
    }
    return fJProbsTrees[jptreeindex]->GetNbranches();
  }

  Int_t GetNParams() const{return fParNames->Count();}

  const Double_t* GetEntry(Int_t jptreeindex, Int_t entry) const
  {
    PRINTF6(this,"\tconst Double_t* QSigExFitDataHolder::GetEntry(Int_t jptreeindex<",jptreeindex,">, Int_t entry<",entry,">) const\n")
    if(jptreeindex<0 || jptreeindex>=fNJPTrees){
      cout << "QSigExFitDataHolder::GetEntry: invalid jptreeindex value\n";
      throw 1;
    }
    fJProbsTrees[jptreeindex]->GetEntry(entry);
    return fEntries[jptreeindex];
  }

  const Int_t* GetParIndices(Int_t jptreeindex) const
  {
    if(jptreeindex<0 || jptreeindex>=fNJPTrees){
      cout << "QSigExFitDataHolder::GetParIndices: invalid jptreeindex value\n";
      throw 1;
    }
    return fParIndexes[jptreeindex];
  }

  const QList<TString> * GetParNames()
  {
    return fParNames;
  }

  void Clear();

 private:
  QSigExFitDataHolder(const QSigExFitDataHolder &);
  static TTree **fJProbsTrees; //!
  static Bool_t *fOwnsJPTree; //!
  static Int_t fNJPTrees; //!
  static Double_t **fEntries; //!
  static Int_t **fParIndexes; //!
  static const QList<TString> *fParNames; //!
  static const TList *fVariabs; //!
  static Int_t fNInstances; //!
  static QSigExFitDataHolder* fPrincipalInstance; //!

  ClassDef(QSigExFitDataHolder,1) //Securely passes information from QSigExFit to a minimization function 
};

#include "debugger.h"

#endif
