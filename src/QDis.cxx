// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

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
