// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QSIGEXCUTS_
#define _QSIGEXCUTS_

#include "QSigExDirHandler.h"
#include "TTree.h"
#include "TList.h"
#include "TFormula.h"
#include "TCut.h"
#include "TCollection.h"
#include "TFile.h"
#include "TBranch.h"
#include "TLeaf.h"
#include "TEventList.h"
#include "TKey.h"
#include "QNamedVar.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

////////////////////////////////////////////////////////////////////////
//                                                                    //
// QSigExCuts                                                         //
//                                                                    //
// This class loads a set of cuts into the QSigExStruct structure. The//
// cuts can be defined as C/C++ conditional expressions. To simplify  //
// these expressions, equivalences (alias expresions) can be created. //
//                                                                    //
// The output of this class is a set of QNamedVar objects located in  //
// subfolders "Cuts Expressions" or "Equivalences" in "Cuts"          //
// QSigExStruct.                                                      //
//                                                                    //
////////////////////////////////////////////////////////////////////////


class QSigExCuts: public QSigExDirHandler{
 public:
  QSigExCuts():QSigExDirHandler(),fCutsDir(NULL){

    PRINTF2(this,"\tQSigExCuts::QSigExCuts()\n")
  }

  QSigExCuts(const QSigExCuts& rhs):QSigExDirHandler(*this), fCutsDir(rhs.fCutsDir), fCutsCard(rhs.fCutsCard), fEqCard(rhs.fEqCard){
    PRINTF2(this,"\tQSigExCuts::QSigExCuts(const QSigExCuts& rhs)\n")
  }

  QSigExCuts(QSigExStruct* dir,const Char_t* cardfilename=NULL):QSigExDirHandler(),fCutsDir(NULL)
    {
      PRINTF6(this,"\tQSigExCuts::QSigExCuts(QSigExStruct* dir<",
	      dir,">,const Char_t* cardfilename<",cardfilename,">)\n")
      try{
	SetDir(dir);
	LoadCardFile(cardfilename);
      }catch(int e){
	cout << "Exception handled by QSigExCuts::QSigExCuts\n";
	throw e;
      }
    }

  virtual ~QSigExCuts(){}

  void LoadCardFile(const Char_t* cardfilename=NULL);

  void CleanDir();
  void ClearCardBuf();

  Int_t Get();

  void AddCut(const Char_t* name, const Char_t* expression);
  void AddEquivalence(const Char_t* name, const Char_t* expression);

  void DelCut(const Char_t* name);
  void DelEquivalence(const Char_t* name);

  void ShowCuts();

 protected:
  void FormatDir();

  Int_t GetEquivalences();

  Int_t GetCuts();

 private:
  TDirectory* fCutsDir; //Pointer to the TDirectory that contains the cuts
  QList<QList<TString> > fCutsCard; //List of cuts in card file format
  QList<QList<TString> > fEqCard;   //List of equivalences in card file format

  ClassDef (QSigExCuts,1)  //Loads equivalences/cuts in the TDirectory structure 
};


#include "debugger.h"
#endif
