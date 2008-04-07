// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExDirHandler                                                     //
//                                                                      //
// This class is an abstract base class that provides a basic interface //
// for all the classes that need to handle the QSigExStruct instance in //
// which QSigEx stores its information. A new handler can be created by //
// deriving a new class from QSigExDirHandler.                          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef _QSIGEXDIRHANDLER_
#define _QSIGEXDIRHANDLER_

#include "Rtypes.h"
#include "TObject.h"
#include "QSigExStruct.h"
#include "TList.h"
#include "TKey.h"
#include "QDatReader.h"

#include "TROOT.h"
#include "TChain.h"
#include "TEventList.h"
#include "TH1.h"
#include "TH2.h"


//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QSigExDirHandler: public TObject{
 public:
  QSigExDirHandler():TObject(),fMyDir(NULL),fReader(){PRINTF2(this,"\tQSigExDirHandler::QSigExDirHandler()\n")}

  QSigExDirHandler(const QSigExDirHandler &rhs): TObject(*this),fMyDir(rhs.fMyDir),fReader(rhs.fReader) {PRINTF2(this,"\tQSigExDirHandler::QSigExDirHandler(const QSigExDirHandler &rhs)")}

  virtual ~QSigExDirHandler(){PRINTF2(this,"\t~QSigExDirHandler::QSigExDirHandler()\n")}

  virtual void LoadCardFile(const Char_t* cardfilename=NULL)=0;

  void SetDir(QSigExStruct* folder)
    {
      PRINTF4(this,"\tQSigExDirHandler::SetDir(QSigExStruct* folder<",folder,">)\n")
      try{
	if(folder) fMyDir=folder;
	else {cout << "A NULL pointer has been passed\n"; throw 5000;}
      }catch(int e){
	cout << "Exception handled by QSigExDirHandler::SetDir\n";
	throw e;
      }
    }

  QSigExStruct* GetDir() const
    {
      PRINTF2(this,"\tQSigExDirHandler::GetDir()\n")
      return fMyDir;
    }

  virtual Int_t Get()=0;

  virtual void CleanDir()=0;
  virtual void ClearCardBuf()=0;
  void GetObjs(TList* list, TDirectory* dir);
  void GetDirs(TList* list, TDirectory* dir);
  void GetListOfObjsKeys(TList* list, TDirectory* dir);
  void DelObjsKeys(const Char_t* name, TDirectory* dir);
  Bool_t FindObjKey(const Char_t* name, const TDirectory* dir) const;
  TDirectory* CopyDir(TDirectory* fromdir, TDirectory* todir); 
  TDirectory* SetDirectory(TDirectory* dir, TObject* obj);

 protected:
  virtual void FormatDir()=0;
 
  QSigExStruct* fMyDir; //! Pointer to the QSigExStruct instance filled by QSigExDirHandler
  QDatReader fReader; //! QDatReader instance used to read the card file

  void CheckCardNFields(Int_t nfields, Int_t min, Int_t max=-1);

  ClassDef(QSigExDirHandler,1) //Abstract base class for QSigExStruct handler classes
};

#include "debugger.h"
#endif





