// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QSIGEXSYSTEMATICS_
#define _QSIGEXSYSTEMATICS_

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
#include "QFormulaUtils.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"


//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExSystematics                                                    //
//                                                                      //
// This class applies the cuts defined in the QSigExStruct structure and//
// applies them on raw data events to produce a TTree containing clean  //
// data. The new TTree is placed in "Event Info" QSigExStruct           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

class QSigExSystematics: public QSigExDirHandler{
 public:
  QSigExSystematics():QSigExDirHandler(),fEInfoDir(NULL),fDataFName(),fDataOName(){

    PRINTF2(this,"\tQSigExSystematics::QSigExSystematics()\n")
  }

  QSigExSystematics(const QSigExSystematics& rhs):QSigExDirHandler(*this),fEInfoDir(rhs.fEInfoDir),fDataFName(rhs.fDataFName),fDataOName(rhs.fDataOName){
    PRINTF2(this,"\tQSigExSystematics::QSigExSystematics(const QSigExSystematics& rhs)\n")
  }

  QSigExSystematics(QSigExStruct* dir,const Char_t* cardfilename=NULL):QSigExDirHandler(),fEInfoDir(NULL),fDataFName(),fDataOName()
    {
      PRINTF6(this,"\tQSigExSystematics::QSigExSystematics(QSigExStruct* dir<",
	      dir,">,const Char_t* cardfilename<",cardfilename,">)\n")
      try{
	SetDir(dir);
	LoadCardFile(cardfilename);
      }catch(int e){
	cout << "Exception handled by QSigExSystematics::QSigExSystematics\n";
	throw e;
      }
    }

  virtual ~QSigExSystematics(){}

  void LoadCardFile(const Char_t* cardfilename=NULL);

  void CleanDir();
  void ClearCardBuf();

  Int_t Get();
  const Char_t* GetDataFName() {return fDataFName;}
  const Char_t* GetDataOName() {return fDataOName;}

  void SetDataFName(const Char_t* datafname) {fDataFName=datafname;}
  void SetDataOName(const Char_t* dataoname) {fDataOName=dataoname;}

 protected:
  void FormatDir();

  void SetDataFilename(const Char_t* fname);
  void SetDataObjectName(const Char_t* fname);

  void CheckIfDataFilename() const;
  void CheckIfDataObjectName() const;

  void CheckCuts() const;
 private:
  TDirectory* fEInfoDir; //Pointer to the TDirectory that contains event information
  TString fDataFName; //String that contains the raw data filename
  TString fDataOName; //String that contains the raw data object name

  ClassDef(QSigExSystematics,1)  //Applies cuts on raw data events and fills a TTree with clean data
};


#include "debugger.h"
#endif
