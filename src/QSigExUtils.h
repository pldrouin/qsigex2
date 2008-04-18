// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>

#ifndef _QSIGEXUTILS_
#define _QSIGEXUTILS_

#include "Rtypes.h"
#include "TString.h"
#include "TSystem.h"
#include "QList.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QSigExUtils{
  public:
    virtual ~QSigExUtils(){}
    static QList<TString> DecodeObjName(const TString name);
    static TString DecodePathName(TString path);

  private:
    QSigExUtils(){}
    QSigExUtils(const QSigExUtils&){}
    const QSigExUtils& operator=(const QSigExUtils&){return *this;}

    ClassDef(QSigExUtils,1) //Contains static member functions that handle TTree branches buffers
};

#include "debugger.h"

#endif
