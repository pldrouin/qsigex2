// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExStaticList                                                     //
//                                                                      //
// This class securely implements a static simplified version of TList  //
// ROOT class.  It allows to create a list that can be read by any      //
// class/function that makes an instance of the class. Only the first   //
// instance can modify or delete the list, so there's no risk of making //
// unwanted changes on the list with other instances. This class can be //
// useful to pass information to a function without passing it via its  //
// arguments.                                                           //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef _QSIGEXSTATICLIST_
#define _QSIGEXSTATICLIST_

//#define DEBUG
//#define DEBUG2

#include <iostream>
#include "Rtypes.h"
#include "TList.h"
#include "TObject.h"

#include "debugger.h"

using namespace std;

class QSigExStaticList{
 public:
  QSigExStaticList(){fNInstances++;}

  virtual ~QSigExStaticList(){fNInstances--; Clear();}

  Int_t Add(TObject* object){
    CheckRights();
    fList.AddLast(object);
    return fList.GetSize();
  }

  const TObject* At(Int_t i){
    return fList.At(i);
  }

  Int_t GetSize(){
    return fList.GetSize();
  }

  void Clear(Option_t* option=NULL){
    if(!fNInstances){
      fList.Clear(option);
    }
  }
 private:
  void CheckRights();
  static Int_t fNInstances; //Number of instances of QSigExStaticList
  static TList fList; //TList object

  ClassDef(QSigExStaticList,1) //Secure static simplified version of TList
};

#include "debugger.h"

#endif
