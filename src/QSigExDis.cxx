// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QSigExDis.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExDis                                                            //
//                                                                      //
// This class is an abstract base class which allows to give a common   //
// interface to all types of ROOT "functions" (TH1, TH2, TH3, TF1, TF2, //
// TF3) such that they can be transparently used as probability density //
// functions. QSigExDis declares a Normalize function that allows to    //
// normalize the functions using complexe cuts and a ProbDensity        //
// function that returns the probability density associated with a      //
// given point.                                                         //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(QSigExDis)

QSigExDis::~QSigExDis()
{
  PRINTF2(this,"\tvirtual QSigExDis::~QSigExDis()\n")

}

#include "debugger.h"
