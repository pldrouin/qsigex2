#ifndef _QMYDIR_
#define _QMYDIR_

#include "Rtypes.h"
#include "TObject.h"
#include "TDirectoryFile.h"
#include "TFile.h"
#include "TBufferFile.h"
#include "TKey.h"
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

#define DEBUG
//#define DEBUG2

#include "debugger.h"

class QMyDir: public TDirectoryFile
{
  public:
    QMyDir():TDirectoryFile(){}

    QMyDir(const QMyDir& newqds):TDirectoryFile(){}

    QMyDir(const char *name, const char *title, TDirectory* initMotherDir = 0);

    virtual ~QMyDir(){}

    QMyDir& operator=(const QMyDir &rhs){fprintf(stderr,"Warning: TDirectoryFile::operator= cannot be called from derived class QMyDir\n"); return *this;}

    void FillBuffer(char*& buffer);
    void WriteDirHeader();

  private:

    ClassDef(QMyDir,1) //QSigEx Data Structure Class
};

#include "debugger.h"

#endif
