// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#ifndef _QSIGEXIO_
#define _QSIGEXIO_

#include <iostream>
#include <typeinfo>
#include <cstring>
#include "TChain.h"
#include "TEventList.h"

#include "TH1.h"
#include "TH2.h"

#include "TString.h"
#include "TObject.h"
#include "TFile.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

using std::cout;

////////////////////////////////////////////////////////////////////////
//                                                                    //
// QSigExIO                                                           //
//                                                                    //
// This class is responsible or reading and writing ROOT objects in   //
// ROOT files. It can be used only with objects which are independent //
// of the TFile once they have been loaded by their streamer. It      //
// means, for example, that a TTree object cannot be loaded in memory //
// using QSigExIO.                                                    //
//                                                                    //
////////////////////////////////////////////////////////////////////////

class QSigExIO
{
 public:
  QSigExIO():fFilename(),fDirectory(),fClassName(),fObject(NULL){}
  QSigExIO(const Char_t* classname):fFilename(),fDirectory(),fClassName(classname),fObject(NULL){}
  QSigExIO(const Char_t* classname, const TObject& object):fFilename(),fDirectory(),fClassName(classname),fObject(NULL){*this=object;}

  QSigExIO(const Char_t* classname, const Char_t* filename, const Char_t* objectname):fFilename(),fDirectory(),fClassName(classname),fObject(NULL)
    {
      PRINTF6(this,"\tQSigExIO::QSigExIO(const Char_t* filename<",filename,">, const Char_t* objectname<",objectname,">)\n")
      Load(filename,objectname);
    }

  virtual ~QSigExIO()
    {
      PRINTF2(this,"\tQSigExIO::~QSigExIO()\n")
      Clear();
    }

  void Load(const Char_t* filename, const Char_t* objectname);

  void Save() const;

  void SaveAs(const Char_t* filename, const Char_t* directory=NULL);

  const QSigExIO& operator=(const QSigExIO& newqio);

  const QSigExIO& operator=(const TObject& newobject)
    {
      PRINTF2(this,"\tQSigExIO& QSigExIO::operator=(const TObject& newobject)\n")
      TryCast(&newobject,fClassName);
      fObject=newobject.Clone();
      return *this;
    }

  TObject* GetObject() const;
  const Char_t* GetClassName() const{return fClassName.Data();}

  QSigExIO* Clone() const
    {
      PRINTF2(this,"\tQSigExIO* QSigExIO::Clone()\n")
      return (new QSigExIO(*this));
    }

  void Clear();

 protected:
  TObject* TryCast(const TObject* uptr, const Char_t* classname) const;

 private:
  QSigExIO(const QSigExIO& newqio){*this=newqio;}
  TString fFilename; //ROOT filename
  TString fDirectory; //Path to object
  TString fClassName; //Class name or base class of the object
  TObject* fObject; //Object pointer

  ClassDef(QSigExIO,1) //Allows to load a TObject from a ROOT file and to perform basic IO operations on it
};

#include "debugger.h"

#endif








