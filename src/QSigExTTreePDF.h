// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QSIGEXTTREEPDF_
#define _QSIGEXTTREEPDF_

#include "QSigExPDFs.h"
#include "QSigExDisTH.h"
#include "QTTreeUtils.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"


////////////////////////////////////////////////////////////////////////
//                                                                    //
// QSigExTTreePDF                                                     //
//                                                                    //
// This class is a QSigExPDFs derived class that creates 1d           //
// QSigExDisTH marginal PDFs from raw TTree objects. For each flux    //
// group, it also creates a clean TTree which branches are those used //
// to create the marginal PDFs.                                       //
//                                                                    //
////////////////////////////////////////////////////////////////////////

class QSigExTTreePDF: public QSigExPDFs{
 public:
  QSigExTTreePDF():QSigExPDFs(){

    PRINTF2(this,"\tQSigExTTreePDF::QSigExTTreePDF()\n")
  }

  QSigExTTreePDF(QSigExStruct* dir,const Char_t* cardfilename=NULL):QSigExPDFs()
  {
      PRINTF6(this,"\tQSigExTTreePDF::QSigExTTreePDF(QSigExStruct* dir<",
	      dir,">,const Char_t* cardfilename<",cardfilename,">)\n")
	try{
	SetDir(dir);
	LoadCardFile(cardfilename);
      }catch(int e){
	cout << "Exception handled by QSigExTTreePDF::QSigExTTreePDF\n";
	throw e;
      }
  }

  virtual ~QSigExTTreePDF(){}

 protected:
  QSigExDis* GetFunction(const QList<TString>& pdfentry, TDirectory* fluxdir, const TCut& fgcuts, QList<TString>* inputs, Bool_t *pdfneedscuts);
  const Char_t* GetPDFName(Int_t i);

 private:
  ClassDef (QSigExTTreePDF,1) //Produces marginal PDFs and flux group clean TTree from TTree objects
};


#include "debugger.h"
#endif





