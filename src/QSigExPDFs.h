// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QSIGEXPDFS_
#define _QSIGEXPDFS_

#include "TDirectory.h"
#include "TList.h"
#include "TCut.h"
#include "TCollection.h"
#include "TList.h"
#include "TMath.h"
#include "TFile.h"
#include "TROOT.h"
#include "QSigExDirHandler.h"
#include "QDis.h"
#include "QFormulaUtils.h"
#include "QNamedVar.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExPDFs                                                           //
//                                                                      //
// This abstract base class uses a card file to create a set of         //
// marginal PDFs.  Binned/unbinned functions are loaded via derived     //
// class GetFunction member function (GetFunction is a pure virtual     //
// member function of QSigExPDFs) and returned to QSigExPDFs class.     //
// Then, cuts defined in the TDirectory structure are applied (or not,  //
// depending on the derived class) and the functions are normalized.    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

class QSigExPDFs: public QSigExDirHandler{
 public:
  QSigExPDFs():QSigExDirHandler(),fPDFsDir(NULL){

    PRINTF2(this,"\tQSigExPDFs::QSigExPDFs()\n")
  }

  QSigExPDFs(const QSigExPDFs& rhs):QSigExDirHandler(*this),fPDFCard(rhs.fPDFCard),fPDFsDir(rhs.fPDFsDir){
    PRINTF2(this,"\tQSigExPDFs::QSigExPDFs(const QSigExPDFs& rhs)\n")
  }

  virtual ~QSigExPDFs(){fPDFsDir=NULL;}

  void LoadCardFile(const Char_t* cardfilename=NULL);

  Int_t Get();

  virtual void AddPDF(Int_t flags, const Char_t* type, const Char_t* fgroup, const Char_t* sgroup, const Char_t* param1=NULL, const Char_t* param2=NULL, const Char_t* param3=NULL, const Char_t* param4=NULL, const Char_t* param5=NULL, const Char_t* param6=NULL, const Char_t* param7=NULL, const Char_t* param8=NULL);

  virtual void DelPDF(const Char_t* fgroup, const Char_t* sgroup, const Char_t* pdfname);

  void CleanDir();
  void ClearCardBuf();

 protected:
  virtual void FormatDir();
  virtual QDis* GetFunction(const QList<TString>& pdfentry, TDirectory* fluxdir, const TCut& fgcuts, QList<TString>* inputs, Bool_t *pdfneedscuts)=0;
  virtual const Char_t* GetPDFName(Int_t i)=0;
  void CheckCuts() const;

  QList<QList<TString> > fPDFCard; //List of pdfs in card file format
 private:
  TDirectory* fPDFsDir; //TDirectory that contains the pdfs

  ClassDef (QSigExPDFs,1) //Abstract base class for QSigExDirHandler derived classes that loads marginal PDFs
};

const Int_t kDisAct=1;
const Int_t kDisSkipNorm=2;

#include "debugger.h"
#endif





