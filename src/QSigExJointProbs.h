// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QSIGEXJOINTPROBS_
#define _QSIGEXJOINTPROBS_

#include "TList.h"
#include "TCollection.h"
#include "TObjArray.h"
#include "TTree.h"
#include "QSigExDirHandler.h"
#include "QProgress.h"
#include "QTTreeUtils.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExJointProbs                                                     //
//                                                                      //
// This class computes joint probability densities of                   //
// NON-CORRELATED VARIABLES. For each flux group, it reads the marginal //
// probability densities from the corresponding TTree in                //
// "Probs/PDFsProbs", multiplies them together and put the result in a  //
// branch of the output TTree "Probs/PDFsJointProbs/PDFsJointProbs".    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

class QSigExJointProbs: public QSigExDirHandler{
 public:
  QSigExJointProbs(): QSigExDirHandler(),fProbsDir(NULL), fJointProbsDir(NULL) {
    PRINTF2(this,"\tQSigExJointProbs::QSigExJointProbs()\n")
  }

  QSigExJointProbs(const QSigExJointProbs& rhs):QSigExDirHandler(*this),fProbsDir(rhs.fProbsDir),fJointProbsDir(rhs.fJointProbsDir){
    PRINTF2(this,"\tQSigExJointProbs::QSigExJointProbs(const QSigExJointProbs& rhs)\n")
  }

  QSigExJointProbs(QSigExStruct* dir,const Char_t* cardfilename=NULL): QSigExDirHandler(),fProbsDir(NULL), fJointProbsDir(NULL)
    {
      PRINTF6(this,"\tQSigExJointProbs::QSigExJointProbs(QSigExStruct* dir<",
	      dir,">,const Char_t* cardfilename<",cardfilename,">)\n")
      try{
	SetDir(dir);
	LoadCardFile(cardfilename);
      }catch(int e){
	cout << "Exception handled by QSigExJointProbs::QSigExJointProbs\n";
	throw e;
      }
    }

  virtual ~QSigExJointProbs(){}

  void LoadCardFile(const Char_t* cardfilename=NULL);
  int Get();

  void CleanDir();
  void ClearCardBuf(){}

 protected:
  void FormatDir();
  void CheckPDFsProbs() const;

 private:
  TDirectory* fProbsDir; //Pointer to the TDirectory that contains the probabilities
  TDirectory* fJointProbsDir; //Pointer to the TDirectory that contains the joint probability densities

  ClassDef (QSigExJointProbs,1) //Computes joint probability densities of non-correlated variables
};


#include "debugger.h"
#endif





