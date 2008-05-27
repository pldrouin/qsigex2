// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QDis.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QDis                                                                 //
//                                                                      //
// This class is an abstract base class which allows to give a common   //
// interface to all types of ROOT "functions" (TH1, TH2, TH3, TF1, TF2, //
// TF3) such that they can be transparently used as probability density //
// functions. QDis declares a Normalize function that allows to         //
// normalize the functions using complexe cuts and a ProbDensity        //
// function that returns the probability density associated with a      //
// given point.                                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(QDis)

QDis::~QDis()
{
  PRINTF2(this,"\tvirtual QDis::~QDis()\n")

}

void QDis::SetNameTitleToObject()
{
  TNamed *nptr=dynamic_cast<TNamed*>(GetObject());

  if(nptr) SetNameTitle(nptr->GetName(),nptr->GetTitle());
}

#include "debugger.h"