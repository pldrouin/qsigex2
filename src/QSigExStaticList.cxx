// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QSigExStaticList.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

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

ClassImp(QSigExStaticList)

Int_t QSigExStaticList::fNInstances=0;
TList QSigExStaticList::fList;

void QSigExStaticList::CheckRights()
{
  //This protected member function throws an exception if the number of
  //instances of QSigExStaticList is not equal to 1.
  PRINTF2(this,"\tvoid QSigExStaticList::CheckRights()\n")

    if(fNInstances!=1){
      cout << "QSigExStaticList::CheckRights: Cannot set variables using this instance, only the first instance can\n";
      throw 1;
    }
}

#include "debugger.h"
