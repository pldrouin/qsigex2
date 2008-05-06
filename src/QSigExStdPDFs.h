// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QSIGEXSTDPDFS_
#define _QSIGEXSTDPDFS_

#include "QSigExPDFs.h"
#include "QDisTH.h"
#include "QDisTF.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExStdPDFs                                                        //
//                                                                      //
// This class is a QSigExPDFs class that produces QDisTH and/or         //
// QDisTF marginal PDFs from TH* and/or TF* functions respective-       //
// ly. See QSigExPDFs documentation for more details.                   //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

class QSigExStdPDFs: public QSigExPDFs{
 public:
  QSigExStdPDFs():QSigExPDFs(){

    PRINTF2(this,"\tQSigExStdPDFs::QSigExStdPDFs()\n")
  }

  QSigExStdPDFs(QSigExStruct* dir,const Char_t* cardfilename=NULL):QSigExPDFs()
    {
      PRINTF6(this,"\tQSigExStdPDFs::QSigExStdPDFs(QSigExStruct* dir<",
	      dir,">,const Char_t* cardfilename<",cardfilename,">)\n")
	try{
	SetDir(dir);
	LoadCardFile(cardfilename);
      }catch(int e){
	cout << "Exception handled by QSigExStdPDFs::QSigExStdPDFs\n";
	throw e;
      }
    }
  
  virtual ~QSigExStdPDFs(){}

 protected:
  QDis* GetFunction(const QList<TString>& pdfentry, TDirectory* fluxdir, const TCut& fgcuts, QList<TString>* inputs, Bool_t *pdfneedscuts);
  const Char_t* GetPDFName(Int_t i);

 private:
  ClassDef (QSigExStdPDFs,1) //Produces marginal PDFs from TH* and/or TF* functions
};



#include "debugger.h"
#endif





