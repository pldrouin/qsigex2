// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

#include "QDis.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

////////////////////////////////////////////////////////////////////////
//                                                                    //
// QDis                                                               //
//                                                                    //
// This class is an abstract base class which allows to give a common //
// interface to all types of ROOT "functions" (TH1, TH2, TH3, TF1,    //
// TF2, TF3) such that they can be transparently used as probability  //
// density functions. QDis declares a Normalize function that allows  //
// to normalize the functions and a ProbDensity function that returns //
// the probability density associated with a given point.             //
//                                                                    //
////////////////////////////////////////////////////////////////////////

ClassImp(QDis)

QDis::~QDis()
{
  PRINTF2(this,"\tvirtual QDis::~QDis()\n")
}

#include "debugger.h"
