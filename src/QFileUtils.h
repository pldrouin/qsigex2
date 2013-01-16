// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _QFILEUTILS_
#define _QFILEUTILS_

#include "Rtypes.h"
#include "TString.h"
#include "TSystem.h"
#include "QList.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QFileUtils{
  public:
    virtual ~QFileUtils(){}
    static QList<TString> DecodeObjName(TString name);
    static QList<TString> DecodePathName(const TString path);
    static TString SimplifyPathName(TString path);

  private:
    QFileUtils(){}
    QFileUtils(const QFileUtils&){}
    const QFileUtils& operator=(const QFileUtils&){return *this;}

    ClassDef(QFileUtils,1) //Contains static member functions that do path name decoding
};

#include "debugger.h"

#endif
