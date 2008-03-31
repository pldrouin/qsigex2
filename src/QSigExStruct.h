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
#include "QIdxHashList.h"


//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QSigExStruct: public TDirectoryFile
{
  public:
    QSigExStruct():TDirectoryFile(){}

    QSigExStruct(const QSigExStruct& newqds):TDirectoryFile(){PRINTF2(this,"\tQSigExStruct::QSigExStruct(const QSigExStruct& newqds)\n") *this=newqds; }

    QSigExStruct(const char *name, const char *title, TDirectory* initMotherDir = 0):TDirectoryFile(){Init(name,title,initMotherDir);}
    QSigExStruct(const char *name, const char *title, QSigExStruct* initMotherDir, Int_t idx):TDirectoryFile(){Init(name,title,initMotherDir); if(initMotherDir) initMotherDir->SetIdx(this,idx);}

    virtual ~QSigExStruct(){PRINTF2(this,"\tQSigExStruct::~QSigExStruct()\n")}

    QSigExStruct& operator=(const QSigExStruct &rhs){fprintf(stderr,"Warning: TDirectoryFile::operator= function is private and cannot be called by derived class QSigExStruct. Address of rhs object: %p\n",&rhs); return *this;}

    void Add(TObject *obj){TDirectoryFile::Add(obj);}
    void Add(TObject *obj, Int_t idx){dynamic_cast<QIdxHashList*>(GetList())->AddLast(obj,idx);}
    void SetIdx(TObject *obj, Int_t idx){dynamic_cast<QIdxHashList*>(GetList())->SetObjQIdx(obj,idx);}
    void Build(TFile* motherFile = 0, TDirectory* motherDir = 0);
    void Delete(const char* namecycle="");
    void Delete(Int_t idx);
    TObject *Remove(TObject *obj){return GetList()->Remove(obj);}
    TObject *Remove(Int_t idx){return dynamic_cast<QIdxHashList*>(GetList())->Remove(idx);}

    void FillBuffer(char*& buffer);
    TObject* Get(const char* namecycle){return TDirectoryFile::Get(namecycle);}
    TObject* Get(Int_t idx){return dynamic_cast<QIdxHashList*>(GetList())->AtQIdx(idx);}
    void Init(const char* name, const char *title, TDirectory* initMotherDir=0);
    TDirectory *mkdir(const char *name, const char *title=""){return mkdir(name,title,-1);}
    TDirectory *mkdir(const char *name, const char *title, Int_t idx);
    void ReadAll(Option_t *option="");
    void WriteDirHeader();
    void WriteKeys();

  protected:

    ClassDef(QSigExStruct,1) //QSigEx Data Structure Class
};

#include "debugger.h"

#endif
