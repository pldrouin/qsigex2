// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#include "QCompProc.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//ClassImp(QCompProc)

Bool_t QCompProc::Exec()
{
  PRINTF2(this,"\tQCompProc::Exec()\n")
  return fProc(fArgs);
}

#include "debugger.h"
