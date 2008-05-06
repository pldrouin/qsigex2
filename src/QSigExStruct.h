#ifndef _QSIGEXSTRUCT_
#define _QSIGEXSTRUCT_

#include "Rtypes.h"
#include "TObject.h"
#include "TKey.h"
#include "TDirectoryFile.h"
#include "TObjArray.h"
#include "TTree.h"
#include "TChain.h"
#include "TEventList.h"
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"
#include "TROOT.h"
#include "TStreamerInfo.h"
#include "TError.h"
#include "Bytes.h"
#include "TClass.h"
#include "TRegexp.h"
#include "TSystem.h"
#include "TStreamerElement.h"
#include "TProcessUUID.h"
#include "TVirtualMutex.h"
#include "THashList.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QSigExStruct: public TDirectoryFile
{
  public:
    QSigExStruct():TDirectoryFile(){}

    QSigExStruct(const QSigExStruct& newqds):TDirectoryFile(){PRINTF2(this,"\tQSigExStruct::QSigExStruct(const QSigExStruct& newqds)\n") *this=newqds; }

    QSigExStruct(const char *name, const char *title, TDirectory* initMotherDir = 0):TDirectoryFile(){Init(name,title,initMotherDir);}

    virtual ~QSigExStruct(){PRINTF4(this,"\tQSigExStruct('",GetName(),"')::~QSigExStruct()\n")}

    QSigExStruct& operator=(const QSigExStruct &rhs){fprintf(stderr,"Warning: TDirectoryFile::operator= function is private and cannot be called by derived class QSigExStruct. Address of rhs object: %p\n",&rhs); return *this;}

    void Delete();
    void Delete(const char* namecycle);

    void FillBuffer(char*& buffer);
    TObject* Get(const char* namecycle){return TDirectoryFile::Get(namecycle);}
    void Init(const char* name, const char *title, TDirectory* initMotherDir=0);
    TDirectory *mkdir(const char *name, const char *title);
    void ReadAll(Option_t *option="");

  protected:

    ClassDef(QSigExStruct,1) //QSigEx Data Structure Class
};

#include "debugger.h"

#endif
