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


//#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QSigExStruct: public TDirectoryFile
{
  public:
    QSigExStruct():TDirectoryFile(), fQArray(NULL){}

    QSigExStruct(const QSigExStruct& newqds):TDirectoryFile(),fQArray(NULL){PRINTF2(this,"\tQSigExStruct::QSigExStruct(const QSigExStruct& newqds)\n") *this=newqds; }

    QSigExStruct(const char *name, const char *title, TDirectory* initMotherDir = 0):TDirectoryFile(),fQArray(NULL){Init(name,title,initMotherDir);}
    QSigExStruct(const char *name, const char *title, QSigExStruct* initMotherDir, Int_t idx):TDirectoryFile(),fQArray(NULL){Init(name,title,initMotherDir); if(initMotherDir) initMotherDir->AddIdx(this,idx);}

    virtual ~QSigExStruct(){PRINTF2(this,"\tQSigExStruct::~QSigExStruct()\n") if(fQArray) delete fQArray;}

    QSigExStruct& operator=(const QSigExStruct &rhs){fprintf(stderr,"Warning: TDirectoryFile::operator= function is private and cannot be called by derived class QSigExStruct\n"); return *this;}

    void Add(TObject *obj){TDirectoryFile::Add(obj);}
    void Add(TObject *obj, Int_t idx){TDirectoryFile::Add(obj); if(!fQArray) fQArray=new TObjArray; fQArray->AddAtAndExpand(obj,idx);}
    void AddIdx(TObject *obj, Int_t idx){if(GetList()->FindObject(obj)){if(!fQArray) fQArray=new TObjArray; fQArray->AddAtAndExpand(obj,idx);}}
    void Delete(const char* namecycle="");
    void Delete(Int_t idx);
    TObject *Remove(TObject *obj){RemoveIdx(obj); return GetList()->Remove(obj);}
    TObject *Remove(Int_t idx){if(fQArray){TObject *obj=fQArray->At(idx); fQArray->RemoveAt(idx); return GetList()->Remove(obj);} return NULL;}
    TObject *RemoveIdx(TObject *obj){if(fQArray) fQArray->Remove(obj); return NULL;}
    TObject *RemoveIdx(Int_t idx){if(fQArray) return fQArray->RemoveAt(idx); return NULL;}

    void FillBuffer(char*& buffer);
    TObject* Get(const char* namecycle){return TDirectoryFile::Get(namecycle);}
    TObject* Get(Int_t idx){if(fQArray) return fQArray->At(idx); return NULL;}
    Int_t GetKeyIdx(const TKey *key);
    void Init(const char* name, const char *title, TDirectory* initMotherDir=0);
    TDirectory *mkdir(const char *name, const char *title=""){return mkdir(name,title,-1);}
    TDirectory *mkdir(const char *name, const char *title, Int_t idx);
    void WriteDirHeader();
    void WriteKeys();

  protected:
    TObjArray *fQArray; //! Array of indices

    ClassDef(QSigExStruct,1) //QSigEx Data Structure Class
};

#include "debugger.h"

#endif
