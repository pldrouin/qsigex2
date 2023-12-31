// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

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
