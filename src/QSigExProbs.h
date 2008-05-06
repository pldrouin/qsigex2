// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QSIGEXPROBS_
#define _QSIGEXPROBS_

#include "TList.h"
#include "TObjArray.h"
#include "TCollection.h"
#include "TTree.h"
#include "TBranch.h"
#include "TFormula.h"
#include "QSigExDirHandler.h"
#include "QDis.h"
#include "QProgress.h"
#include "QTTreeUtils.h"
#include "arrayutils.h"
#include "QFormulaUtils.h"
#include "QNamedVar.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

////////////////////////////////////////////////////////////////////////
//                                                                    //
// QSigExProbs                                                        //
//                                                                    //
// This class uses the marginal PDFs stored in "PDFs" QSigExStruct to //
// compute the marginal probability densities of the events stored in //
// "Event Info" QSigExStruct. Each PDF can be an instance of any class//
// derived from QDis class. The output of this function is a set      //
// of TTree objects (one per flux group) located in "Probs/PDFsProbs" //
// QSigExStruct.                                                      //
//                                                                    //
// PLEASE REFER TO FILE QSigExProbs_cxx.h FOR DESCRIPTION OF MEMBER   //
// FUNCTIONS                                                          //
////////////////////////////////////////////////////////////////////////

template <typename U = Float_t> class QSigExProbs: public QSigExDirHandler{
 public:
  QSigExProbs(): QSigExDirHandler(),fProbsDir(NULL),fPDFsProbsDir(NULL){

    PRINTF2(this,"\tQSigExProbs::QSigExProbs()\n")
  }

  QSigExProbs(const QSigExProbs& rhs):QSigExDirHandler(*this),fProbsDir(rhs.fProbsDir),fPDFsProbsDir(rhs.fPDFsProbsDir){
    PRINTF2(this,"\tQSigExProbs::QSigExProbs(const QSigExProbs& rhs)\n")
  }

  QSigExProbs(QSigExStruct* dir,const Char_t* cardfilename=NULL): QSigExDirHandler(),fProbsDir(NULL),fPDFsProbsDir(NULL)
    {
      PRINTF6(this,"\tQSigExProbs::QSigExProbs(QSigExStruct* dir<",
	      dir,">,const Char_t* cardfilename<",cardfilename,">)\n")
      try{
	SetDir(dir);
	LoadCardFile(cardfilename);
      }catch(int e){
	cout << "Exception handled by QSigExProbs::QSigExProbs\n";
	throw e;
      }
    }

  virtual ~QSigExProbs(){}

  void LoadCardFile(const Char_t* cardfilename=NULL);

  Int_t Get();

  void CleanDir();
  void ClearCardBuf(){}

 protected:
  void FormatDir();
  void CheckPDFs() const;
  void CheckCleanData() const;

 private:
  TDirectory* fProbsDir; //Pointer to the TDirectory that contains the probabilities
  TDirectory* fPDFsProbsDir; //Pointer to the TDirectory that contains the marginal probabilities 

  ClassDef (QSigExProbs,1) //Computes marginal probability densities using marginal PDFs and data TTree
};


#include "debugger.h"

#include "QSigExProbs_cxx.h"

#endif
