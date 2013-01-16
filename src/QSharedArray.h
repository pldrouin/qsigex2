// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _QSHAREDARRAY_
#define _QSHAREDARRAY_

#ifndef __CINT__
#include <pthread.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/uio.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/param.h>
#include <sys/mount.h>
#include <cstdlib>
#include <climits>
#include <cstdio>
#include <cstring>
#include <string>
#include <cmath>
#include <errno.h>
#include <stdint.h>

#include "strdiffer.h"
#include "qatomic.h"
#include "sigcontrol.h"
#else
struct pthread_mutex_t;
typedef UInt_t uint32_t;
typedef Int_t int32_t;
typedef Long64_t int64_t;
#endif
#include <sys/types.h>
#include <sys/cdefs.h>

#include "TTimeStamp.h"

#ifdef WITH_LIBPROCINFO
#include "procinfo.h"
#endif

#define MAX_WAITERS 256

#define MAX_ANAMELEN 256

void dummy_h(int sig);

extern "C" void R__zip (int cxlevel, int *nin, char *bufin, int *lout, char *bufout, int *nout);
extern "C" void R__unzip(int *nin, unsigned char *bufin, int *lout, char *bufout, int *nout);

using std::string;

class QSharedArray;

class QSharedArray
{
  public:
  QSharedArray(const char *filename, const char *arraydescr, const unsigned int &objectsize=0, const unsigned int &nobjectsperbuffer=0);
  virtual ~QSharedArray();

  static void ClearAllShMem();
  void ClearShMem();

  void Fill();

  void* GetBuffer() const{return fBuffer;}
  long long int GetEntries() const{return fNObjects;}

  const char* GetArrayName() const{return fArrayName.c_str();}
  const unsigned int& GetObjSize() const{return fObjectSize;}
  const char* GetObjTypeName() const{return fObjectTypeName.c_str();}

  void InitShMem();

  const bool& IsReadThreadSafe() const{return kFALSE;}

  void LoadEntry(const long long int &entry = 0);

  int GetOpenMode(){return 0;}

  const TTimeStamp& GetTimeStamp() const{return fTStamp;}

  void PrintInfo() const;

  void SetBuffer(void *buffer){fBuffer=buffer;}

  void ShowMemStats();

  void UpdateTimeStamp(){}

  protected:
  QSharedArray() {}
  void Terminate();

  private:
  QSharedArray(const QSharedArray &) {}
  void LoadArray();
  const QSharedArray& operator=(const QSharedArray &){return *this;}

  struct sharst {
    char lstatus;
    int32_t susers;
    uint32_t waiters[MAX_WAITERS];
  } *fShArSt;

  string fFilename;
  string fShPath;		//!
  int fFDesc;                   //!
  string fShAPath;
  int fFADesc;
  bool fOwns;			//!
  int fWIdx;			//!
  //Enforce 1-byte alignment
#pragma pack(push)  /* push current alignment to stack */
#pragma pack(1)     /* set alignment to 1 byte boundary */
  struct sharpars {
    uint32_t anamelength;
    char arrayname[MAX_ANAMELEN];
    uint32_t objectsize;
    uint32_t noperbuffer;
    int64_t nobjects;
    time_t sec;
    int32_t nsec;
  } *fArrayPars;
#pragma pack(pop)   /* restore original alignment from stack */

  string fArrayName;
  TTimeStamp fTStamp;          // Timestamp associated with the array
  uint32_t fObjectSize;          // Size of a single stored object
  string fObjectTypeName;
  void *fBuffer;               //! Pointer to the current object
  unsigned int fNOPerBuffer;   // Maximum number of objects per buffer
  unsigned long long int fMemPerBuffer;  // Memory per buffer
  long long int fNObjects;               // Total number of objects
  void **fBuffers;                  //! Buffer Addresses
  unsigned int fNBuffers;           //! Number of buffers

  static pthread_mutex_t fIMutex;   //!Instances Mutex
  static QSharedArray **fInstances; //!
  static int          fNInstances;  //!
  static const unsigned int sSHPAMSize; //Shared Header Page-Aligned memory size

  ClassDef(QSharedArray,1) //Shared memory read-only array class
};

#endif
