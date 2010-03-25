#include "QOversizeArray.h"

ClassImp(QOversizeArray)

QList<QOversizeArray*> QOversizeArray::fInstances;
const unsigned int QOversizeArray::sSHPAMSize=(sizeof(struct sharst)/sysconf(_SC_PAGESIZE)+sizeof(struct sharst)%sysconf(_SC_PAGESIZE)!=0)*sysconf(_SC_PAGESIZE);
int QOversizeArray::fShFDesc=0;
bool QOversizeArray::fShOwns=0;
struct QOversizeArray::sharst* QOversizeArray::fShArSt=NULL;
QList<Float_t> QOversizeArray::fICumulPriority;
Long64_t QOversizeArray::fMinMemSize=0;
Long64_t QOversizeArray::fLevel1MemSize=0;
Float_t  QOversizeArray::fLevel1=0;
Long64_t QOversizeArray::fLevel2MemSize=0;
Float_t  QOversizeArray::fLevel2=0;
Long64_t QOversizeArray::fCritMemSize=0;
Float_t  QOversizeArray::fCritLevel=0;
Long64_t QOversizeArray::fCThreshMemSize=0;
Float_t  QOversizeArray::fCThreshLevel=0;
Long64_t *QOversizeArray::fTotalMemSize=NULL;
#ifdef WITH_LIBPROCINFO
time_t QOversizeArray::fMemUpdateInt=2;
time_t QOversizeArray::fMemUpdateTime=0;
#endif
UInt_t   QOversizeArray::fNLoaders=1;
pthread_mutex_t QOversizeArray::fCMSCMutex=PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t QOversizeArray::fFileWMutex=PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t QOversizeArray::fCMSCond=PTHREAD_COND_INITIALIZER;
Bool_t QOversizeArray::fCLReached=kFALSE;
pthread_t QOversizeArray::fMMThread;
pthread_mutex_t QOversizeArray::fMMMutex=PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t QOversizeArray::fMMCond=PTHREAD_COND_INITIALIZER;
pthread_mutex_t QOversizeArray::fILMutex=PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t QOversizeArray::fPriorityMutex=PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  QOversizeArray::fMWCDCond=PTHREAD_COND_INITIALIZER;
pthread_mutex_t QOversizeArray::fMWCDMutex=PTHREAD_MUTEX_INITIALIZER;
pthread_t       QOversizeArray::fMCThread;
pthread_t*      QOversizeArray::fBLThreads=NULL;
pthread_mutex_t QOversizeArray::fMCGMutex=PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t QOversizeArray::fMCMutex=PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  QOversizeArray::fMCCond=PTHREAD_COND_INITIALIZER;
pthread_mutex_t QOversizeArray::fMCCMutex=PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  QOversizeArray::fMCPCond=PTHREAD_COND_INITIALIZER;
pthread_cond_t  QOversizeArray::fMCCCond=PTHREAD_COND_INITIALIZER;
pthread_cond_t  QOversizeArray::fMCDCond=PTHREAD_COND_INITIALIZER;
pthread_mutex_t QOversizeArray::fMCDMutex=PTHREAD_MUTEX_INITIALIZER;
Char_t		QOversizeArray::fMCAction=0;
QOversizeArray*	QOversizeArray::fMCQOA=NULL;
QOABuffer*	QOversizeArray::fMCBuffer=NULL;
QOABuffer*	QOversizeArray::fMCCBuffer=NULL;
Int_t		QOversizeArray::fMaxNPCBuffers=-1;
struct QOAQueue*	QOversizeArray::fFirstB2L=NULL;
struct QOAQueue**	QOversizeArray::fLastB2Ls=NULL;
pthread_mutex_t QOversizeArray::fB2LMutex=PTHREAD_MUTEX_INITIALIZER;
sem_t		QOversizeArray::fB2LSem;

//#define FuncDef(a,b) const char* thefunc=#a; Bool_t show=b; const void* me=NULL;
//#define CFuncDef(a,b) const char* thefunc=#a; Bool_t show=b; const void* me=this;
//#define printstatus(a) {if(show) printf("%p\t%s\n",me,a);}
//#define pthread_mutex_lock(a) {if(show) printf("\n %p%slocking %s in function %s...\n",me,indent.Data(),#a,thefunc); pthread_mutex_lock(a); if(show) printf("%p %s%s is now locked in function %s\n",me,indent.Data(),#a,thefunc); indent+="     ";}
//#define pthread_mutex_unlock(a) {indent=indent(0,indent.Length()-5); if(show) printf("\n%p %sunlocking %s in function %s...\n",me,indent.Data(),#a,thefunc); pthread_mutex_unlock(a); if(show) printf("%p %s%s is now unlocked in function %s\n",me,indent.Data(),#a,thefunc);}
//#define pthread_cond_signal(a) {if(show) printf("%p sending signal %s in function %s...\n",me,#a,thefunc); pthread_cond_signal(a);}
//#define pthread_cond_wait(a,b) {if(show) printf("%p waiting for signal %s with mutex %s in function %s...\n",me,#a,#b,thefunc); pthread_cond_wait(a,b);}
//#define ASSERT(a) if(!(a) && show) {fprintf(stderr,"Error in function %s: Assertion failed: %s\n",thefunc,#a); throw 1;}

#define FuncDef(a,b)
#define CFuncDef(a,b)
#define printstatus(a)
#define ASSERT(a)

QOversizeArray::QOversizeArray(const char *filename, const char *arraydescr, const Int_t& openmode, const UInt_t &objectsize, const UInt_t &nobjectsperbuffer, const Int_t &npcbuffers, const UInt_t &nobjectsallocblock): fFilename(filename), fArrayName(), fTStamp(), fFDesc(0), fFirstDataByte(sizeof(UInt_t)+256+sizeof(fObjectSize)+sizeof(fNOPerBuffer)+sizeof(fNObjects)+sizeof(time_t)+sizeof(Int_t)), fBufferHeaderSize(sizeof(UInt_t)), fOpenMode(openmode), fObjectSize(objectsize), fObjectTypeName(), fBuffer(NULL), fNOPerBuffer(nobjectsperbuffer), fNOAllocBlock(nobjectsallocblock),fMaxBDataSize(objectsize*nobjectsperbuffer), fMaxBHBDataSize(fBufferHeaderSize+fMaxBDataSize), fNObjects(0), fCurReadBuffer(NULL), fFirstReadBuffer(NULL), fLastReadBuffer(NULL), fWriteBuffer(NULL), fCRBData(NULL), fWBFirstObjIdx(0), fWBNAllocObjs(0), fCurRBIdx(-1), fCurBLRBIdx(-2), fNReadBuffers(0), fPTNRBObjects(-1), fUMBuffers(NULL), fNUMBuffers(0), fFirstParkedBuffer(NULL), fNPCBuffers(npcbuffers>0?npcbuffers:0), fArrayMemSize(0), fArrayIO(0), fAPriority(0), fFileMutex(), fBuffersMutex(), fPBuffersMutex(), fMWThread(), fMWMutex(), fMWCond(), fMWCMutex(), fMWPCond(), fMWCCond(), fMWAction(kFALSE), fMWBuffer(NULL), fMWWBuffer(NULL), fSigMMThread(kFALSE), fBLCSem(), fBLWSem(), fBLAction(kFALSE), fQOAQ(), fCurBLBuffer(NULL), fUZQOAB(NULL), fUZBuffers(NULL), fUZBMutex()
{
  CFuncDef(QOversizeArray,1);
  TString sbuf=arraydescr;
  Int_t k=sbuf.Last('/');

  if(k==-1) fArrayName=sbuf;
  else {
    fArrayName=sbuf(0,k);
    fObjectTypeName=sbuf(k+1,sbuf.Length()-k-1);
  }
  sem_init(&fBLCSem,0,0);
  sem_init(&fBLWSem,0,0);
  fQOAQ.Array=NULL;
  fQOAQ.Next=NULL;

  pthread_mutex_init(&fFileMutex,NULL);
  pthread_mutex_init(&fBuffersMutex,NULL);
  pthread_mutex_init(&fPBuffersMutex,NULL);
  pthread_mutex_init(&fMWMutex,NULL);
  pthread_cond_init(&fMWCond,NULL);
  pthread_mutex_init(&fMWCMutex,NULL);
  pthread_cond_init(&fMWPCond,NULL);
  pthread_cond_init(&fMWCCond,NULL);
  pthread_mutex_init(&fUZBMutex,NULL);

  if(npcbuffers>fMaxNPCBuffers) {
    pthread_mutex_lock(&fB2LMutex);
    fMaxNPCBuffers=npcbuffers;
    fLastB2Ls=(struct QOAQueue**)realloc(fLastB2Ls,(fMaxNPCBuffers+1)*sizeof(struct QOAQueue*));
    memset(fLastB2Ls,0,(fMaxNPCBuffers+1)*sizeof(struct QOAQueue*));
    pthread_mutex_unlock(&fB2LMutex);
  }

  //printf("%p\tTotal memory at array creation: %lli\n",this,q_load(fTotalMemSize));
  OpenFile();
}

QOversizeArray::~QOversizeArray()
{
  CFuncDef(~QOversizeArray,1);
  printstatus("QOversizeArray::~QOversizeArray()");
  CloseFile();
  //printf("%p\tArray size at array destruction: %lli\n",this,fArrayMemSize);
  //printf("%p\tTotal memory at array destruction: %lli\n",this,q_load(fTotalMemSize));
  
  pthread_mutex_destroy(&fFileMutex);
  pthread_mutex_destroy(&fBuffersMutex);
  pthread_mutex_destroy(&fPBuffersMutex);
  pthread_mutex_destroy(&fMWMutex);
  pthread_cond_destroy(&fMWCond);
  pthread_mutex_destroy(&fMWCMutex);
  pthread_cond_destroy(&fMWPCond);
  pthread_cond_destroy(&fMWCCond);
  pthread_mutex_destroy(&fUZBMutex);
  sem_destroy(&fBLCSem);
  sem_destroy(&fBLWSem);
}

void QOversizeArray::ClearShMem()
{
  //Block Signals in case the function is called by a signal handler
  bool error=false;
  block_signals_once();
  int i,j;

  //If the shared memory was ready to be accessed
  if(fShArSt) {

    //If has index in waiters list
    if(fTotalMemSize) {
      printf("Freeing shared memory segment at ID %i\n",int(fTotalMemSize-fShArSt->memory));
      //Erase it
      q_store(fTotalMemSize,(Long64_t)0);
      q_store(fShArSt->used+int(fTotalMemSize-fShArSt->memory),(bool)0);
      fTotalMemSize=0;
    }

retry2:
    //If last user
    if((i=q_fetch_and_compare_and_set(&fShArSt->susers,(int32_t)1,(int32_t)0))==1) {
      //printf("Destroying the shared memory space\n");

      if(shm_unlink("/QOversizeArray")==-1){
	perror("shm_unlink");
	error=true;
      }

      //If not last user 
    } else {

      //Try decrement if value has not changed. If value has changed and was 1, goto retry2. Otherwise retry
      while((j=q_fetch_and_compare_and_set(&fShArSt->susers,(int32_t)i,(int32_t)(i-1)))!=i) {if(j==1) goto retry2; i=j;}
      //printf("Decremented the shared memory usage to %i\n",j-1);
    }

    if(munmap(fShArSt,sSHPAMSize)) {
      perror("munmap");
    }
    fShArSt=NULL;

    //Else if owner but could not get shared memory
  } else if(fShOwns) {

    if(shm_unlink("/QOversizeArray")==-1){
      perror("shm_unlink");
      error=true;
    }
  }

  if(fShFDesc>0) {

    if(close(fShFDesc)) {
      fShFDesc=0;
      perror("close");
      error=true;
    }
    fShFDesc=0;
  }

  unblock_signals_once();

  if(error) throw 1;
}

void QOversizeArray::CloseFile()
{
  CFuncDef(CloseFile,1);
  printstatus("void QOversizeArray::CloseFile()");
  if(fFDesc) {
    block_signals_once();
    pthread_mutex_lock(&fILMutex);

    Int_t idx=fInstances.FindFirst(this);
    fICumulPriority.Del(idx);
    fInstances.Del(idx);
    pthread_mutex_unlock(&fILMutex);
    unblock_signals_once();
    //From here QOAMMThread won't try to send a new job to either fMWThread of fMCThread for this array

    if(fOpenMode!=kRead) {
      //Send a signal to fMWThread to terminate and wait for termination
      pthread_mutex_lock(&fMWMutex);
      fMWAction=2;
      //It is important to unlock fMWMutex after locking fMWCMutex to ensure getting the right condition from QOAMWThread
      pthread_mutex_lock(&fMWCMutex);
      pthread_mutex_unlock(&fMWMutex);
      pthread_cond_signal(&fMWCond);
      pthread_mutex_unlock(&fMWCMutex);
      //printf("Waiting for MW thread %p to terminate...\n",this);
      pthread_join(fMWThread,NULL);
      //printf("MW thread %p has terminated\n",this);
    }

    //If no instances are left, wait for QOAMCThread to stop
    if(!fInstances.Count()) {
      
      //Wait for QOAMMThread to exit if it is the last instance
      pthread_mutex_lock(&fMMMutex);
      pthread_cond_signal(&fMMCond);
      pthread_mutex_unlock(&fMMMutex);
      pthread_join(fMMThread,NULL);

      if(fCThreshLevel<=fCritLevel) {
	//Send a signal to fMCThread to terminate and wait for termination
	pthread_mutex_lock(&fMCMutex);
	fMCAction=2;
	//It is important to unlock fMCMutex after locking fMCCMutex to ensure getting the right condition from QOAMCThread
	pthread_mutex_lock(&fMCCMutex);
	pthread_mutex_unlock(&fMCMutex);
	pthread_cond_signal(&fMCCond);
	pthread_mutex_unlock(&fMCCMutex);
	//printf("Waiting for MC thread %p to terminate...\n",this);
	pthread_join(fMCThread,NULL);
	//printf("MC thread %p has terminated\n",this);
      }

      free(fLastB2Ls);
      fLastB2Ls=NULL;
      fMaxNPCBuffers=-1;

      pthread_mutex_lock(&fBuffersMutex);
      if(fCurRBIdx>=0) {
	fCurReadBuffer=NULL;
	fCurRBIdx=-1;

	if(q_load(&fQOAQ.Array)) {
	  pthread_mutex_unlock(&fBuffersMutex);
	  //Wait for confirmation

	  //Make sure the BLThread is not waiting for memory
	  if(q_load(&fCLReached)) {
	    pthread_mutex_lock(&fCMSCMutex);
	    pthread_cond_broadcast(&fCMSCond);
	    pthread_mutex_unlock(&fCMSCMutex);
	  }
	  sem_wait(&fBLWSem);
	  pthread_mutex_lock(&fBuffersMutex);
	}
	ASSERT(q_load(&fQOAQ.Array)==NULL);
	fCurBLBuffer=NULL;

	if(fCurBLRBIdx!=-2) {   
	  fCurBLRBIdx=-2;       
	  CleanUZBuffers();     
	}
      }
      pthread_mutex_unlock(&fBuffersMutex);

      //Send a signal to fBLThreads to terminate and wait for termination
      for(Int_t i=fNLoaders-1; i>=0; --i) sem_post(&fB2LSem);
      for(Int_t i=fNLoaders-1; i>=0; --i) {
	//printf("pthread_join fBLThreads[%i] (%p)\n",i,fBLThreads+i);
	pthread_join(fBLThreads[i], NULL);
      }
      delete[] fBLThreads;
      sem_destroy(&fB2LSem);
      //printf("BL thread %p has terminated\n",this);

      Terminate();
      ClearShMem();

      //else if there are still some instances, wait for the current job to finish
    } else {
      //printf("Waiting for MC thread %p to finish compressing...\n",this);
      pthread_mutex_lock(&fMCDMutex);
      if(fMCCBuffer) pthread_cond_wait(&fMCDCond, &fMCDMutex);
      pthread_mutex_unlock(&fMCDMutex);
      //printf("MC thread %p is done compressing\n",this);

      pthread_mutex_lock(&fBuffersMutex);
      if(fCurRBIdx>=0) {
	fCurReadBuffer=NULL;
	fCurRBIdx=-1;

	if(q_load(&fQOAQ.Array)) {
	  //Wait for confirmation
	  pthread_mutex_unlock(&fBuffersMutex);
	  sem_wait(&fBLWSem);
	  pthread_mutex_lock(&fBuffersMutex);
	}
	ASSERT(q_load(&fQOAQ.Array)==NULL);
	fCurBLBuffer=NULL;

	if(fCurBLRBIdx!=-2) {   
	  fCurBLRBIdx=-2;       
	  CleanUZBuffers();     
	}
      }
      pthread_mutex_unlock(&fBuffersMutex);

      Terminate();
    }

    if(close(fFDesc)) {
      perror("QOversizeArray::~QOversizeArray(): Error: ");
      throw 1;
    }
    fFDesc=0;
  }
}

void QOversizeArray::CheckMemory()
{
  FuncDef(CheckMemory,1);
  //printf("fCritMemSize=%lli, fTotalMemSize=%lli\n",fCritMemSize,q_load(fTotalMemSize));
  if(q_load(&fLevel2MemSize) && q_load(fTotalMemSize) > q_load(&fLevel2MemSize)) {
    pthread_mutex_lock(&fMMMutex);
    pthread_cond_signal(&fMMCond);
    pthread_mutex_unlock(&fMMMutex);
  }

  if(q_load(&fCritMemSize) && q_load(fTotalMemSize) > q_load(&fCritMemSize)) {
    //Need to use this order for unlocking to avoid deadlock with MMThread due to wait on fC<SCond
    printstatus("***** Critical memory size has been reached");
    //printf("***** Critical memory size has been reached\n");
    q_store(&fCLReached,(Bool_t)kTRUE);
    pthread_mutex_lock(&fCMSCMutex);
    pthread_cond_wait(&fCMSCond,&fCMSCMutex);
    pthread_mutex_unlock(&fCMSCMutex);
  }
}

void QOversizeArray::CleanUZBuffers()
{
  CFuncDef(CleanUZBuffers,0);
  printstatus("QOversizeArray::CleanUZBuffers()");
  //****** Make sure fBuffersMutex is locked and that fCurBLRBIdx!=-2 when calling this function! Also this function should be called immediately after changing the fCurBLRBIdx (i.e. before releasing fBuffersMutex)
  Int_t i,j;

  pthread_mutex_lock(&fUZBMutex);
  j=0;
  for(i=fNPCBuffers+1; i>=0; --i) {
    //if(fUZQOAB[i]) printf("CleanUZBuffers: fUZQOAB[%i] (%p)\tfCurBLRBIdx==%i\tfUZQOAB[i]->fBufferIdx==%i\n",i,fUZQOAB[i],fCurBLRBIdx,fUZQOAB[i]->fBufferIdx);

    if(fUZQOAB[i] && (fCurBLRBIdx==-2 || fUZQOAB[i]->fBufferIdx<fCurBLRBIdx || fUZQOAB[i]->fBufferIdx>fCurBLRBIdx+fNPCBuffers)) {
      //printf("Freeing buffer %i (%p) from fUZBuffers[%i]\n",fUZQOAB[i]->fBufferIdx,fUZQOAB[i],i);
      fUZQOAB[i]->fIsCompressed=1;
      free(fUZBuffers[i]);
      ++j;
      fUZQOAB[i]=NULL;
    }
  }
  q_add(fTotalMemSize,-j*fMaxBDataSize);
  q_add(&fArrayMemSize,-j*fMaxBDataSize);
  pthread_mutex_unlock(&fUZBMutex);
}

void QOversizeArray::Init()
{
  CFuncDef(Init,1);
  fUZQOAB=(QOABuffer**)malloc((fNPCBuffers+2)*sizeof(QOABuffer*));
  fUZBuffers=(Char_t**)malloc((fNPCBuffers+2)*sizeof(Char_t*));
  memset(fUZQOAB,0,(fNPCBuffers+2)*sizeof(QOABuffer*));
  memset(fUZBuffers,0,(fNPCBuffers+2)*sizeof(Char_t*));
  ReadWriteBuffer();
}

void QOversizeArray::InitShMem()
{
  ClearShMem();

  bool fOwns=false;

  block_signals_init(nset,oset,throw 1);

  //Block all signals
  block_signals(&nset,&oset,throw 1);

  fShFDesc=shm_open("/QOversizeArray",O_RDWR|O_CREAT,S_IRUSR|S_IWUSR);

  if(fShFDesc==-1) {
    perror("QOversizeArray::shm_open");
    //Stop blocking signals.
    unblock_signals(&oset,);
    throw 1;
  }

  if(ftruncate(fShFDesc, sSHPAMSize)==-1) { 
    perror("ftruncate");
    //Stop blocking signals. Need to call terminatesmem to just handle the fShFDesc
    unblock_signals(&oset,);
    throw 1;
  }

  fShArSt=(struct sharst*)mmap(NULL,sSHPAMSize,PROT_READ|PROT_WRITE,MAP_SHARED,fShFDesc,0);

  if(fShArSt==MAP_FAILED) {
    perror("mmap");
    //Stop blocking signals. Need to call terminatesmem to handle the fShFDesc and the mmap
    sigprocmask(SIG_SETMASK,&oset,NULL);
    throw 1;
  }

  //q_store(&fShArSt->susers,0);
  q_add(&fShArSt->susers,(int32_t)1);
  //printf("pid is %u\n",getpid());
  //printf("nusers is %i\n",q_load(&fShArSt->susers));
  int i;

  for(i=QOA_MAXPROCS-1; i>=0; --i)
    if(!q_fetch_and_compare_and_set(&fShArSt->used[i],(bool)false,(bool)true)) {
      printf("ID is %i\n",i);
      fTotalMemSize=fShArSt->memory+i;
      q_store(fTotalMemSize,(Long64_t)0);
      break;
    }
  unblock_signals(&oset,throw 1);

  if(i<0) {
    fprintf(stderr,"QOversizeArray::InitShMem(): Error: Cannot find available room for a new process in the shared memory segment\n");
    throw 1;
  }
}

void QOversizeArray::Fill()
{
  CFuncDef(Fill,1);

  //If fWriteBuffer contains the maximum number of objects allowed by the currently allocated memory
  if(fNObjects-fWBFirstObjIdx == fWBNAllocObjs) {

    UInt_t lFillallocsize=0;
    //printstatus("Fill needs to allocate more space");

    if(fWBNAllocObjs<fNOPerBuffer) {
      fWBNAllocObjs+=fNOAllocBlock;

      if(fWBNAllocObjs>fNOPerBuffer) fWBNAllocObjs=fNOPerBuffer;
      lFillallocsize=fWBNAllocObjs*fObjectSize;

      fWriteBuffer->fBuffer=(char*)realloc(fWriteBuffer->fBuffer,lFillallocsize);
      q_add(fTotalMemSize,lFillallocsize-fWriteBuffer->fBufferSize);
      q_add(&fArrayMemSize,lFillallocsize-fWriteBuffer->fBufferSize);
      fWriteBuffer->fBufferSize=lFillallocsize;
      //CheckMemory();

      //If fWriteBuffer is full
    } else {
      //printstatus("Write buffer is full");
      pthread_mutex_unlock(&fBuffersMutex);
#ifndef QSFAST
      if(fOpenMode == kRead) {
	fprintf(stderr,"QOversizeArray::Fill(): Error: File '%s' is opened in read-only mode\n",fFilename.Data());
	throw 1;
      }
#endif

      UInt_t lFillnextbufidx=fWriteBuffer->fBufferIdx+1;

      //If switching from read mode
      pthread_mutex_lock(&fBuffersMutex);
      if(fCurRBIdx>=0) {
	fCurReadBuffer=NULL;
	fCurRBIdx=-1;

	if(q_load(&fQOAQ.Array)) {
	  pthread_mutex_unlock(&fBuffersMutex);
	  //Wait for confirmation
	  sem_wait(&fBLWSem);
	  pthread_mutex_lock(&fBuffersMutex);
	}
	ASSERT(q_load(&fQOAQ.Array)==NULL);
	fCurBLBuffer=NULL;

	if(fCurBLRBIdx!=-2) {   
	  fCurBLRBIdx=-2;       
	  CleanUZBuffers();     
	}
      }
      fWriteBuffer->fPreviousOAB=fLastReadBuffer;
      fWriteBuffer->fNextOAB=NULL;
      //if(!fWriteBuffer->fIsModified) printf("Modified state for buffer %p is set to kTRUE\n",fWriteBuffer);
      fWriteBuffer->fIsModified=kTRUE;

      //If this is not the first buffer in the structure
      if(fLastReadBuffer) {
	fLastReadBuffer->fNextOAB=fWriteBuffer;
      } else fFirstReadBuffer=fWriteBuffer;
      fLastReadBuffer=fWriteBuffer;
      ++fNReadBuffers;
      fWBFirstObjIdx=fNObjects;
      pthread_mutex_unlock(&fBuffersMutex);
      //Add the uncompressed size of the new read buffer to priority calculations
      fArrayIO+=fMaxBDataSize;
      pthread_mutex_lock(&fPriorityMutex);
      fAPriority=1./fArrayIO;
      pthread_mutex_unlock(&fPriorityMutex);

      pthread_mutex_lock(&fPBuffersMutex);
      if(fFirstParkedBuffer) {
	//printstatus("Using a parked buffer for the next write buffer");
	ASSERT(fFirstParkedBuffer!=fCurReadBuffer);
	fWriteBuffer=fFirstParkedBuffer;
	fFirstParkedBuffer=fFirstParkedBuffer->fNextOAB;
	pthread_mutex_unlock(&fPBuffersMutex);
	fWriteBuffer->fBufferIdx=lFillnextbufidx;
	fWriteBuffer->fIsCompressed=0;
	fWBNAllocObjs=fWriteBuffer->fBufferSize/fObjectSize;

	if(fWBNAllocObjs<fNOAllocBlock) {
	  free(fWriteBuffer->fBuffer);
	  fWBNAllocObjs=fNOAllocBlock;
	  lFillallocsize=fWBNAllocObjs*fObjectSize;
	  fWriteBuffer->fBuffer=(char*)malloc(lFillallocsize);
	  q_add(fTotalMemSize,lFillallocsize-fWriteBuffer->fBufferSize);
	  q_add(&fArrayMemSize,lFillallocsize-fWriteBuffer->fBufferSize);
	  fWriteBuffer->fBufferSize=lFillallocsize;
	  CheckMemory();
	}

      } else {
	//printstatus("Using a new buffer for the next write buffer");
	pthread_mutex_unlock(&fPBuffersMutex);
	fWBNAllocObjs=fNOAllocBlock;
	lFillallocsize=fWBNAllocObjs*fObjectSize;
	fWriteBuffer=new QOABuffer(lFillnextbufidx, lFillallocsize);
	q_add(fTotalMemSize,lFillallocsize+sizeof(QOABuffer));
	q_add(&fArrayMemSize,lFillallocsize+sizeof(QOABuffer));
	CheckMemory();
      }
      //printf("Next write buffer index is %i\n",lFillnextbufidx);
    }
    //printstatus("Fill: space allocated");
  }

  //Copy the content of the object buffer in fWriteBuffer
  //It is fine to access fWBFirstObjIdx from the main thread without locking fBuffersMutex, since its value can only be modified from that thread
  memcpy(fWriteBuffer->fBuffer+(fNObjects-fWBFirstObjIdx)*fObjectSize,fBuffer,fObjectSize);
  ++fNObjects;
  //printstatus("Exiting Fill");
}

void QOversizeArray::KillThreads()
{
  Int_t i;

  block_signals_once();

  if(fInstances.Count()) {
    pthread_cancel(fMMThread);
    pthread_join(fMMThread,NULL);

    for(i=fNLoaders-1; i>=0; --i) {
      pthread_cancel(fBLThreads[i]);
      pthread_join(fBLThreads[i],NULL);
    }
  if(fCThreshLevel<=fCritLevel) {
    pthread_cancel(fMCThread);
    pthread_join(fMCThread,NULL);
  }

    for(i=fInstances.Count()-1; i>=0; --i) {
      pthread_cancel(fInstances[i]->fMWThread);
      pthread_join(fInstances[i]->fMWThread,NULL);
    }
    fInstances.Clear();
  }
  unblock_signals_once();
}

void QOversizeArray::LoadEntry(const Long64_t &entry)
{
  //Do not need to apply locks for reading operations of variables that are only modified in the main thread
  CFuncDef(LoadEntry,1);
#ifndef QSFAST
  //If the entry is out of bound
  if(entry<0 || entry>=fNObjects) {
    fprintf(stderr,"QOversizeArray::LoadEntry: Error: Entry index is invalid\n");
    //printstatus("Exiting LoadEntry");
    return;
  }
#endif

  //If the entry is located in the write buffer. Do not need a lock since only the main thread access the write buffer
  if(entry>=fWBFirstObjIdx) {
    if(fCurRBIdx>=0) {
      //printf("Reading from the write buffer\n");
      pthread_mutex_lock(&fBuffersMutex);
      fCurReadBuffer=NULL;
      fCurRBIdx=-2;
      pthread_mutex_unlock(&fBuffersMutex);

      //If this is not in the B2L queue
      if(!q_fetch_and_compare_and_set(&fQOAQ.Array,NULL,this)) {
	pthread_mutex_lock(&fB2LMutex);
        //printf("Adding node %p (array %p) with priority %i\n",&fQOAQ,this,0);

	if(fLastB2Ls[0]) {

	  //printf("Surrounding nodes: %p %p %p \n",fLastB2Ls[0],&fQOAQ,fLastB2Ls[0]->Next);
	  fQOAQ.Next=fLastB2Ls[0]->Next;
	  fLastB2Ls[0]->Next=&fQOAQ;
	  fLastB2Ls[0]=&fQOAQ;

	} else {
	  //printf("Unique node: %p\n",&fQOAQ);
	  fQOAQ.Next=NULL;
	  fFirstB2L=&fQOAQ;

	  for(Int_t i=fMaxNPCBuffers; i>=0; --i) fLastB2Ls[i]=fFirstB2L;
	}
	pthread_mutex_unlock(&fB2LMutex);
	sem_post(&fB2LSem);
      }
    }

    memcpy(fBuffer,fWriteBuffer->fBuffer+(entry-fWBFirstObjIdx)*fObjectSize,fObjectSize);
    //printstatus("Exiting LoadEntry");
    return;
  }

  Int_t lLEibuf=entry/fNOPerBuffer;

  //If the read buffer for the current event is already loaded
  if(fCurReadBuffer && lLEibuf==fCurReadBuffer->fBufferIdx) {
    //Copy the memory
    //printf("Entry %lli\n",entry);
    memcpy(fBuffer,fCRBData+(entry-fCurReadBuffer->fBufferIdx*fNOPerBuffer)*fObjectSize,fObjectSize);

  } else {
    //printstatus("LoadEntry needs to load an entry from a new buffer");

    //fprintf(stderr,"%p\tCalling LoadEntry with entry==%lli (buffer index %i)\n",this,entry,lLEibuf);
    pthread_mutex_lock(&fBuffersMutex);
    fCurRBIdx=lLEibuf;

    if(fCurReadBuffer && fCurReadBuffer->fBufferIdx<=fCurRBIdx) {

      while(fCurReadBuffer->fNextOAB && fCurReadBuffer->fNextOAB->fBufferIdx<=fCurRBIdx) fCurReadBuffer=fCurReadBuffer->fNextOAB;
      ASSERT(2 && fCurReadBuffer->fBufferIdx<=fCurRBIdx);

    } else if(fFirstReadBuffer && fFirstReadBuffer->fBufferIdx<=fCurRBIdx) {
      fCurReadBuffer=fFirstReadBuffer;

      while(fCurReadBuffer->fNextOAB && fCurReadBuffer->fNextOAB->fBufferIdx<=fCurRBIdx) fCurReadBuffer=fCurReadBuffer->fNextOAB;
      ASSERT(3 && fCurReadBuffer->fBufferIdx<=fCurRBIdx);

    } else fCurReadBuffer=NULL;

    //printf("Looping on buffers with fCurReadBuffer==%p with buffer index %i\n",fCurReadBuffer,fCurReadBuffer?fCurReadBuffer->fBufferIdx:-1);

    //If the buffer is not loaded
    if(!fCurReadBuffer || fCurReadBuffer->fBufferIdx != fCurRBIdx) {
      pthread_mutex_unlock(&fBuffersMutex);

      //If this is not in the B2L queue
      if(!q_fetch_and_compare_and_set(&fQOAQ.Array,NULL,this)) {
	pthread_mutex_lock(&fB2LMutex);
        //printf("Adding node %p (array %p) with priority %i\n",&fQOAQ,this,0);

	if(fLastB2Ls[0]) {

	  //printf("Surrounding nodes: %p %p %p \n",fLastB2Ls[0],&fQOAQ,fLastB2Ls[0]->Next);
	  fQOAQ.Next=fLastB2Ls[0]->Next;
	  fLastB2Ls[0]->Next=&fQOAQ;
	  fLastB2Ls[0]=&fQOAQ;

	} else {
	  //printf("Unique node: %p\n",&fQOAQ);
	  fQOAQ.Next=NULL;
	  fFirstB2L=&fQOAQ;

	  for(Int_t i=fMaxNPCBuffers; i>=0; --i) fLastB2Ls[i]=fFirstB2L;
	}
	pthread_mutex_unlock(&fB2LMutex);
	sem_post(&fB2LSem);
      }
      sem_wait(&fBLCSem);
      //printf("%p\tLoadEntry received confirmation for buffer %i\n",this,fCurRBIdx);

      pthread_mutex_lock(&fBuffersMutex);
      if(!fCurReadBuffer) fCurReadBuffer=fFirstReadBuffer;
      while(fCurReadBuffer->fNextOAB && fCurReadBuffer->fNextOAB->fBufferIdx<=fCurRBIdx) fCurReadBuffer=fCurReadBuffer->fNextOAB;
      //Assertion: fCurReadBuffer->fBufferIdx==fCurRBIdx
      //ASSERT(fCurReadBuffer->fBufferIdx==fCurRBIdx);
#ifndef QSFAST
      if(!(fCurReadBuffer->fBufferIdx==fCurRBIdx)) {
	fprintf(stderr,"Error: LoadEntry: fCurReadBuffer index is %i and fCurRBIdx is %i\n",fCurReadBuffer->fBufferIdx,fCurRBIdx);
	throw 1;
      }
#endif

      pthread_mutex_unlock(&fBuffersMutex);

      //Else if the buffer is being uncompressed
    } else if(fCurReadBuffer->fIsCompressed != 0 && fCurReadBuffer->fIsCompressed != 4) {
	pthread_mutex_unlock(&fBuffersMutex);

      //If this is not in the B2L queue
      if(!q_fetch_and_compare_and_set(&fQOAQ.Array,NULL,this)) {
	pthread_mutex_lock(&fB2LMutex);
        //printf("Adding node %p (array %p) with priority %i\n",&fQOAQ,this,0);

	if(fLastB2Ls[0]) {

	  //printf("Surrounding nodes: %p %p %p \n",fLastB2Ls[0],&fQOAQ,fLastB2Ls[0]->Next);
	  fQOAQ.Next=fLastB2Ls[0]->Next;
	  fLastB2Ls[0]->Next=&fQOAQ;
	  fLastB2Ls[0]=&fQOAQ;

	} else {
	  //printf("Unique node: %p\n",&fQOAQ);
	  fQOAQ.Next=NULL;
	  fFirstB2L=&fQOAQ;

	  for(Int_t i=fMaxNPCBuffers; i>=0; --i) fLastB2Ls[i]=fFirstB2L;
	}
	pthread_mutex_unlock(&fB2LMutex);
	sem_post(&fB2LSem);
      }
      sem_wait(&fBLCSem);
      //printf("%p\tLoadEntry received confirmation for buffer %i\n",this,fCurRBIdx);

    } else {
	pthread_mutex_unlock(&fBuffersMutex);

      //If this is not in the B2L queue
      if(!q_fetch_and_compare_and_set(&fQOAQ.Array,NULL,this)) {
	pthread_mutex_lock(&fB2LMutex);
        //printf("Adding node %p (array %p) with priority %i\n",&fQOAQ,this,fNPCBuffers);

	if(fLastB2Ls[fNPCBuffers]) {

	  //printf("Surrounding nodes: %p %p %p \n",fLastB2Ls[fNPCBuffers],&fQOAQ,fLastB2Ls[fNPCBuffers]->Next);
	  fQOAQ.Next=fLastB2Ls[fNPCBuffers]->Next;
	  fLastB2Ls[fNPCBuffers]->Next=&fQOAQ;
	  fLastB2Ls[fNPCBuffers]=&fQOAQ;

	} else {
	  //printf("Unique node: %p\n",&fQOAQ);
	  fQOAQ.Next=NULL;
	  fFirstB2L=&fQOAQ;

	  for(Int_t i=fMaxNPCBuffers; i>=0; --i) fLastB2Ls[i]=fFirstB2L;
	}
	pthread_mutex_unlock(&fB2LMutex);
	sem_post(&fB2LSem);
      }
    }

    //If the buffer has never been compressed
    if(fCurReadBuffer->fIsCompressed==0) fCRBData=fCurReadBuffer->fBuffer;

    //Else if the buffer is uncompressed (fIsCompressed==4)
    else {
      pthread_mutex_lock(&fUZBMutex);

      //Find fCurReadBuffer in fUZQOAB
      for(Int_t lLEuzbidx=0;; ++lLEuzbidx) {

	if(fUZQOAB[lLEuzbidx]==fCurReadBuffer) {
	  //printf("LoadEntry: Uncompressed buffer %i==%i (%p) is found in fUZQOAB[%i]\n",fCurReadBuffer->fBufferIdx,fUZQOAB[lLEuzbidx]->fBufferIdx,fUZQOAB[lLEuzbidx],lLEuzbidx);
	  fCRBData=fUZBuffers[lLEuzbidx];
	  break;
	}
      }
      //ASSERT(lLEuzbidx<fNPCBuffers+2 && fUZQOAB[lLEuzbidx]->fBufferIdx==fCurReadBuffer->fBufferIdx);
      pthread_mutex_unlock(&fUZBMutex);
    }

    //Add the uncompressed size of the newly accessed read buffer to priority calculations
    fArrayIO+=fMaxBDataSize;
    pthread_mutex_lock(&fPriorityMutex);
    fAPriority=1./fArrayIO;
    pthread_mutex_unlock(&fPriorityMutex);

    //Copy the memory
    //printf("First Entry %lli from curbuffer %i with compression state %i\t",entry,fCurReadBuffer->fBufferIdx,fCurReadBuffer->fIsCompressed);
    memcpy(fBuffer,fCRBData+(entry-fCurReadBuffer->fBufferIdx*fNOPerBuffer)*fObjectSize,fObjectSize);
    //printf("...loaded\n");
    //printstatus("LoadEntry: New buffer is loaded");
  }   

  //printstatus("Exiting LoadEntry");
  return;
}

void QOversizeArray::OpenFile()
{
  CFuncDef(OpenFile,0);
  printstatus("void QOversizeArray::OpenFile()");
  if(fFDesc) CloseFile();

  switch(fOpenMode) {
    case kRead:
      fFDesc=open(fFilename,O_RDONLY);

      if(fFDesc<0) {
	perror("QOversizeArray");
	fprintf(stderr,"QOversizeArray::OpenFile: Error: file '%s' cannot be opened in read-only mode\n",fFilename.Data());
	throw 1;
      }
      ReadHeader();
      break;

    case kRW:
      fFDesc=open(fFilename,O_RDWR);

      if(fFDesc<0) {
	perror("QOversizeArray");
	fprintf(stderr,"QOversizeArray::OpenFile: Error: file '%s' cannot be opened in read-write mode\n",fFilename.Data());
	throw 1;
      }
      ReadHeader();
      break;

    case kRecreate:
      fFDesc=open(fFilename,O_RDWR|O_CREAT|O_TRUNC,S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH);

      if(fFDesc<0) {
	perror("QOversizeArray");
	fprintf(stderr,"QOversizeArray::OpenFile: Error: file '%s' cannot be recreated\n",fFilename.Data());
	throw 1;
      }

      if(fObjectSize==0) {
	fprintf(stderr,"QOversizeArray::OpenFile: Error: object size cannot be 0\n");
	throw 1;
      }

      if(fNOPerBuffer==0) {
	fprintf(stderr,"QOversizeArray::OpenFile: Error: number of objects per buffer cannot be 0\n");
	throw 1;
      }
      WriteHeader();
  }

  //Create memory writing thread
  if(fOpenMode!=kRead) pthread_create(&fMWThread, NULL, QOAMWThread, this);
  //Create buffer loading thread

  //printf("Block signals\n");
  block_signals_once();
  pthread_mutex_lock(&fILMutex);
  fInstances.Add(this);
  fICumulPriority.Add(0);

  if(fInstances.Count() == 1) {
    pthread_mutex_unlock(&fILMutex);
    //printf("Unblock signals\n");
    unblock_signals_once();

    InitShMem();
#ifdef WITH_LIBPROCINFO
    while (sysfreemem() < fMinMemSize) sleep(fMemUpdateInt);
#endif

    sem_init(&fB2LSem,0,0);
    fBLThreads=new pthread_t[fNLoaders];

    for(Int_t i=fNLoaders-1; i>=0; --i) {
      pthread_create(fBLThreads+i, NULL, QOABLThread, NULL);
    }
    pthread_create(&fMMThread, NULL, QOAMMThread, NULL);
    if(fCThreshLevel<=fCritLevel) pthread_create(&fMCThread, NULL, QOAMCThread, NULL);

  } else {
    pthread_mutex_unlock(&fILMutex);
    //printf("Unblock signals2\n");
    unblock_signals_once();
  }

  Init();
}

void QOversizeArray::PrintInfo() const
{
  printf("Array name: %s\n",fArrayName.Data());
  if(fObjectTypeName.Length()) printf("Array type name: %s\n",fObjectTypeName.Data());
  printf("Objects size: %i\n",fObjectSize);
  printf("Number of objects per buffer: %u\n",fNOPerBuffer);
  printf("Number of pre-cached buffers: %i\n",fNPCBuffers);
  printf("Number of objects per memory allocation block: %u\n",fNOAllocBlock);
  printf("Total number of objects: %lli\n",fNObjects);
  printf("Time stamp: ");fTStamp.Print();
}

void QOversizeArray::PrintPriorities()
{
  FuncDef(PrintPriorities,0);
  pthread_mutex_lock(&fILMutex);
  Int_t i;
  Float_t fbuf=0;
  pthread_mutex_lock(&fPriorityMutex);

  for(i=0; i<fInstances.Count(); ++i) fbuf+=fInstances.GetArray()[i]->fAPriority;

  for(i=0; i<fInstances.Count(); ++i) {
    printf("Array '%s': %7.5e\n",fInstances.GetArray()[i]->fFilename.Data(),fbuf?fInstances.GetArray()[i]->fAPriority*100./fbuf:-1.);
  }
  pthread_mutex_unlock(&fPriorityMutex);
  pthread_mutex_unlock(&fILMutex);
}

void QOversizeArray::ResetArray()
{
  CFuncDef(ResetArray,1);
  printstatus("QOversizeArray::ResetArray has been called");
  //Resets the array, without saving on disk and without freeing memory (buffers are converted into parked buffers). Should be the method to be called before refilling an array.
  pthread_mutex_lock(&fBuffersMutex);
  fCurReadBuffer=NULL;
  fCurRBIdx=-1;

  if(q_load(&fQOAQ.Array)) {
    //Wait for confirmation
    pthread_mutex_unlock(&fBuffersMutex);
    sem_wait(&fBLWSem);
    pthread_mutex_lock(&fBuffersMutex);
    ASSERT(q_load(&fQOAQ.Array)==NULL);

    if(fCurBLRBIdx!=-2) {   
      fCurBLRBIdx=-2;       
      CleanUZBuffers();     
    }
  }
  fCurBLBuffer=NULL;
  pthread_mutex_unlock(&fBuffersMutex);

  if(fOpenMode!=kRead) {
    pthread_mutex_lock(&fMWMutex);
    //Request a pause from memory writing thread
    fMWAction=1;
    //It is important to unlock fMWMutex after locking fMWCMutex to ensure getting the right condition from QOAMWThread
    pthread_mutex_lock(&fMWCMutex);
    pthread_mutex_unlock(&fMWMutex);
    pthread_cond_signal(&fMWCond);
    //Wait for pause confirmation
    pthread_cond_wait(&fMWPCond,&fMWCMutex);
    pthread_mutex_unlock(&fMWCMutex);
    printstatus("Memory writing thread confirmed to be in pausing condition");
  }

  if(fCThreshLevel<=fCritLevel) {
    //A Giant lock is used here to avoid conflicts when ResetArray for different QOA instances are called at the same time (for a multi-threaded application).
    pthread_mutex_lock(&fMCGMutex);
    pthread_mutex_lock(&fMCMutex);
    printstatus("fMCMutex is locked");
    //Request a pause from memory compression thread
    fMCAction=1;
    //It is important to unlock fMCMutex after locking fMCCMutex to ensure getting the right condition from QOAMCThread
    pthread_mutex_lock(&fMCCMutex);
    printstatus("fMCCMutex is locked");
    pthread_mutex_unlock(&fMCMutex);
    pthread_cond_signal(&fMCCond);
    //Wait for pause confirmation
    printstatus("Waiting signal from QOAMCThread");
    pthread_cond_wait(&fMCPCond,&fMCCMutex);
    printstatus("Signal received");
    pthread_mutex_unlock(&fMCCMutex);
    printstatus("Memory compression thread confirmed to be in pausing condition");
  }

  //Lock buffer structure.
  pthread_mutex_lock(&fBuffersMutex);

  if(fNUMBuffers) {
    free(fUMBuffers);
    fUMBuffers=NULL;
    fNUMBuffers=0;
  }

  if(fFirstReadBuffer) {

    if(!fFirstParkedBuffer) {
      fFirstParkedBuffer=fFirstReadBuffer;

    } else {
      fLastReadBuffer->fNextOAB=fFirstParkedBuffer;
      //Not necessary to set fPreviousOAB for parked buffers
      fFirstParkedBuffer=fFirstReadBuffer;
    }
    fFirstReadBuffer=NULL;
    fLastReadBuffer=NULL;
    fPTNRBObjects=fWBFirstObjIdx;
    fNReadBuffers=0;
  } else fPTNRBObjects=-1;
  fWBFirstObjIdx=0;
  if(fWriteBuffer) fWriteBuffer->fBufferIdx=0;
  fNObjects=0;

  if(fCThreshLevel<=fCritLevel) {
    //Remove pause condition on memory compression thread
    pthread_mutex_lock(&fMCMutex);
    fMCAction=0;
    //It is important to unlock fMCMutex after locking fMCCMutex to ensure getting the right condition from QOAMCThread
    pthread_mutex_lock(&fMCCMutex);
    pthread_mutex_unlock(&fMCMutex);
    printstatus("QOversizeArray::ResetArray: Removing pausing condition from memory compression thread");
    pthread_cond_signal(&fMCCond);
    pthread_mutex_unlock(&fMCCMutex);
    pthread_mutex_unlock(&fMCGMutex);
  }

  if(fOpenMode!=kRead) {
    //Remove pause condition on memory writing thread
    pthread_mutex_lock(&fMWMutex);
    fMWAction=0;
    //It is important to unlock fMWMutex after locking fMWCMutex to ensure getting the right condition from QOAMWThread
    pthread_mutex_lock(&fMWCMutex);
    pthread_mutex_unlock(&fMWMutex);
    printstatus("QOversizeArray::ResetArray: Removing pausing condition from memory writing thread");
    pthread_cond_signal(&fMWCond);
    pthread_mutex_unlock(&fMWCMutex);
  }

  //Unlock buffer structure
  pthread_mutex_unlock(&fBuffersMutex);
  printstatus("Done Reset\n");
}

void QOversizeArray::ResetPriorities()
{
  FuncDef(ResetPriorities,1);
  pthread_mutex_lock(&fPriorityMutex);
  pthread_mutex_lock(&fILMutex);

  for(Int_t i=fInstances.Count()-1; i>=0; --i) {
    fInstances.GetArray()[i]->fArrayIO=0;
    fInstances.GetArray()[i]->fAPriority=0;
  }
  pthread_mutex_unlock(&fILMutex);
  pthread_mutex_unlock(&fPriorityMutex);
}

void QOversizeArray::ReadHeader()
{
  CFuncDef(ReadHeader,1);
  printstatus("void QOversizeArray::ReadHeader()");
  UInt_t uibuf,uibuf2;
  char *strbuf;
  time_t sec;
  Int_t nsec;

  pthread_mutex_lock(&fFileMutex);

  if(lseek(fFDesc,0,SEEK_SET)==-1) {
    perror("QOversizeArray::ReadHeader: Error: ");
    throw 1;
  }

  if(read(fFDesc, &uibuf, sizeof(uibuf))!=sizeof(uibuf)) {
    perror("QOversizeArray::ReadHeader: Error: ");
    throw 1;
  }
  strbuf=(char*)malloc(uibuf+1);

  if((UInt_t)read(fFDesc, strbuf, uibuf)!=uibuf) {
    perror("QOversizeArray::ReadHeader: Error: ");
    throw 1;
  }
  strbuf[uibuf]=0;

  if(strcmp(strbuf,fArrayName)) {
    fprintf(stderr,"QOversizeArray::ReadHeader: Error: Array name saved in file '%s' does not match provided array name\n",fFilename.Data());
    throw 1;
  }

  uibuf2=strlen(strbuf)+1;

  if(fObjectTypeName.Length() && uibuf>uibuf2 && strcmp(fObjectTypeName,strbuf+uibuf2)) {
    fprintf(stderr,"QOversizeArray::ReadHeader: Error: Array type '%s' saved in file '%s' does not match the provided array type ('%s')\n",strbuf+uibuf2,fFilename.Data(),fObjectTypeName.Data());
    throw 1;
  }

  if(!fObjectTypeName.Length() && uibuf>uibuf2) fObjectTypeName=strbuf+uibuf2;
  free(strbuf);

  if(lseek(fFDesc,sizeof(uibuf)+256,SEEK_SET)==-1) {
    perror("QOversizeArray::ReadHeader: Error: ");
    throw 1;
  }
  if(read(fFDesc, &uibuf, sizeof(uibuf))!=sizeof(uibuf)) {
    perror("QOversizeArray::ReadHeader: Error: ");
    throw 1;
  }

  if(fObjectSize != 0 && fObjectSize != uibuf) {
    fprintf(stderr,"QOversizeArray::ReadHeader: Error: Objects size saved in file '%s' does not match provided objects size\n",fFilename.Data());
    throw 1;
  }
  fObjectSize=uibuf;

  if(read(fFDesc, &uibuf, sizeof(uibuf))!=sizeof(uibuf)) {
    perror("QOversizeArray::ReadHeader: Error: ");
    throw 1;
  }

  if(fNOPerBuffer != 0 && fNOPerBuffer != uibuf) {
    fprintf(stderr,"QOveriszeArray::ReadHeader: Error: Number of objects per buffer saved in file '%s' (%u) does not match provided value (%u)\n",fFilename.Data(),uibuf,fNOPerBuffer);
    throw 1;
  }
  fNOPerBuffer=uibuf;
  fMaxBDataSize=fObjectSize*fNOPerBuffer;
  fMaxBHBDataSize=fBufferHeaderSize+fMaxBDataSize;
  
  if(!fNOAllocBlock || fNOAllocBlock>fNOPerBuffer) fNOAllocBlock=fNOPerBuffer;

  if(read(fFDesc, &fNObjects, sizeof(fNObjects))!=sizeof(fNObjects)) {
    perror("QOversizeArray::ReadHeader: Error: ");
    throw 1;
  }

  if(read(fFDesc, &sec, sizeof(sec))!=sizeof(sec)) {
    perror("QOversizeArray::ReadHeader: Error: ");
    throw 1;
  }
  fTStamp.SetSec(sec);

  if(read(fFDesc, &nsec, sizeof(nsec))!=sizeof(nsec)) {
    perror("QOversizeArray::ReadHeader: Error: ");
    throw 1;
  }
  fTStamp.SetNanoSec(nsec);
  pthread_mutex_unlock(&fFileMutex);
}

void QOversizeArray::ReadBuffer(QOABuffer **buf, const UInt_t &bufferidx)
{
  CFuncDef(ReadBuffer,1);
  //printf("Read buffer %u at %u\n",bufferidx,bufferidx*fMaxBHBDataSize+fFirstDataByte);
  Int_t buffersize;

  pthread_mutex_lock(&fFileMutex);
  if(lseek(fFDesc,bufferidx*fMaxBHBDataSize+fFirstDataByte,SEEK_SET)==-1){
    perror("QOversizeArray::ReadBuffer: lseek Error: ");
    throw 1;
  }
  if(read(fFDesc, &buffersize, fBufferHeaderSize)!=fBufferHeaderSize) {
    perror("QOversizeArray::ReadBuffer: read Error: ");
    throw 1;
  }

  pthread_mutex_lock(&fPBuffersMutex);
  if(fFirstParkedBuffer) {
    ASSERT(fFirstParkedBuffer!=fCurReadBuffer);
    *buf=fFirstParkedBuffer;
    fFirstParkedBuffer=fFirstParkedBuffer->fNextOAB;
    pthread_mutex_unlock(&fPBuffersMutex);

    if((*buf)->fBufferSize != buffersize) {
      free((*buf)->fBuffer);
      (*buf)->fBuffer=(char*)malloc(buffersize);
      q_add(fTotalMemSize,buffersize-(*buf)->fBufferSize);
      q_add(&fArrayMemSize,buffersize-(*buf)->fBufferSize);

      if((*buf)->fBufferSize < buffersize) {
	(*buf)->fBufferSize=buffersize;

      } else {
	(*buf)->fBufferSize=buffersize;
      }	       
    }

    if(buffersize < fMaxBDataSize) (*buf)->fIsCompressed=1;
    else (*buf)->fIsCompressed=0;

  } else {
    pthread_mutex_unlock(&fPBuffersMutex);
    *buf=new QOABuffer(0,buffersize);
    if(buffersize < fMaxBDataSize) (*buf)->fIsCompressed=1;
    q_add(fTotalMemSize,buffersize+sizeof(QOABuffer));
    q_add(&fArrayMemSize,buffersize+sizeof(QOABuffer));
  }
  (*buf)->fBufferIdx=bufferidx;
  (*buf)->fIsModified=kFALSE;

  if(read(fFDesc, (*buf)->fBuffer, (*buf)->fBufferSize)!=(*buf)->fBufferSize) {
    perror("QOversizeArray::ReadBuffer: Error: ");
    throw 1;
  }
  pthread_mutex_unlock(&fFileMutex);
}

void QOversizeArray::ReadWriteBuffer()
{
  CFuncDef(ReadWriteBuffer,1);
  UInt_t bufferidx=(UInt_t)((fNObjects-1)/fNOPerBuffer);
  size_t numobjs=int(fNObjects-(Long64_t)bufferidx*fNOPerBuffer);
  Int_t buffersize;
  UInt_t allocsize;

  if(numobjs) {
    pthread_mutex_lock(&fFileWMutex);
    pthread_mutex_lock(&fFileMutex);
    if(lseek(fFDesc,bufferidx*fMaxBHBDataSize+fFirstDataByte,SEEK_SET)==-1){
      perror("QOversizeArray::ReadWriteBuffer: Error: ");
      throw 1;
    }
    if(read(fFDesc, &buffersize, fBufferHeaderSize)!=fBufferHeaderSize) {
      perror("QOversizeArray::ReadWriteBuffer: Error: ");
      throw 1;
    }

    if(!fWriteBuffer) {
      fWBNAllocObjs=numobjs;
      allocsize=fWBNAllocObjs*fObjectSize;
      fWriteBuffer=new QOABuffer(0, allocsize);
      q_add(fTotalMemSize,allocsize+sizeof(QOABuffer));
      q_add(&fArrayMemSize,allocsize+sizeof(QOABuffer));
      CheckMemory();

    } else if(fWBNAllocObjs<numobjs) {
      free(fWriteBuffer->fBuffer);
      fWBNAllocObjs=numobjs;
      allocsize=fWBNAllocObjs*fObjectSize;
      fWriteBuffer->fBuffer=(char*)malloc(allocsize);
      q_add(fTotalMemSize,allocsize-fWriteBuffer->fBufferSize);
      q_add(&fArrayMemSize,allocsize-fWriteBuffer->fBufferSize);
      fWriteBuffer->fBufferSize=allocsize;
    }

    if(read(fFDesc, fWriteBuffer->fBuffer, buffersize)!=buffersize) {
      perror("QOversizeArray::ReadWriteBuffer: Error: ");
      throw 1;
    }
    pthread_mutex_unlock(&fFileMutex);
    pthread_mutex_unlock(&fFileWMutex);

  } else {

    if(!fWriteBuffer) {
      fWBNAllocObjs=fNOAllocBlock;
      allocsize=fWBNAllocObjs*fObjectSize;
      fWriteBuffer=new QOABuffer(0, allocsize);
      q_add(fTotalMemSize,allocsize+sizeof(QOABuffer));
      q_add(&fArrayMemSize,allocsize+sizeof(QOABuffer));
      CheckMemory();
    } 
  }
  fWriteBuffer->fBufferIdx=bufferidx;
  pthread_mutex_lock(&fBuffersMutex);
  fWBFirstObjIdx=fNObjects-numobjs;
  pthread_mutex_unlock(&fBuffersMutex);
}

void QOversizeArray::Save(const Float_t &compfrac)
{
  //Save the array, while compressing a fraction compfrac of the modified buffers.
  CFuncDef(Save,1);
  printstatus("void QOversizeArray::Save()");
  if(!fFDesc) {
    fprintf(stderr,"QOversizeArray::Save: Error: There is no opened file\n");
    throw 1;
  }

  if(fOpenMode == kRead) {
    fprintf(stderr,"QOversizeArray::Save(): Error: File '%s' is opened in read-only mode\n",fFilename.Data());
    throw 1;
  }

  QOABuffer *buf;
  Char_t *tmpbuf;
  Int_t ibuf;

  pthread_mutex_lock(&fMWMutex);
  //Request a pause from memory writing thread
  fMWAction=1;
  //It is important to unlock fMWMutex after locking fMWCMutex to ensure getting the right condition from QOAMWThread
  pthread_mutex_lock(&fMWCMutex);
  pthread_mutex_unlock(&fMWMutex);
  pthread_cond_signal(&fMWCond);
  //Wait for pause confirmation
  pthread_cond_wait(&fMWPCond,&fMWCMutex);
  pthread_mutex_unlock(&fMWCMutex);
  printstatus("Memory writing thread confirmed to be in pausing condition");

  if(fCThreshLevel<=fCritLevel) {
    pthread_mutex_lock(&fMCGMutex);
    pthread_mutex_lock(&fMCMutex);
    //Request a pause from memory compression thread
    fMCAction=1;
    //It is important to unlock fMCMutex after locking fMCCMutex to ensure getting the right condition from QOAMCThread
    pthread_mutex_lock(&fMCCMutex);
    pthread_mutex_unlock(&fMCMutex);
    pthread_cond_signal(&fMCCond);
    //Wait for pause confirmation
    pthread_cond_wait(&fMCPCond,&fMCCMutex);
    pthread_mutex_unlock(&fMCCMutex);
    printstatus("Memory compression thread confirmed to be in pausing condition");
  }

  //Lock buffer structure.
  pthread_mutex_lock(&fBuffersMutex);

  buf=fFirstReadBuffer;

  if(!compfrac) {

    while(buf) {
      if(buf->fIsModified) {
	WriteBuffer(buf);
	buf->fIsModified=kFALSE;
      }
      buf=buf->fNextOAB;
    }

  } else if(compfrac==1) {

    while(buf) {
      if(buf->fIsModified) {
	tmpbuf=(char*)malloc(fMaxBDataSize);
	R__zip(1,&fMaxBDataSize,buf->fBuffer,&fMaxBDataSize,tmpbuf,&ibuf);

	//If compressed data is more compact than uncompressed data
	if(ibuf && ibuf<fMaxBDataSize) {
	  //printf("Compression %i/%i\n",ibuf,buf->fBufferSize);
	  free(buf->fBuffer);
	  tmpbuf=(char*)realloc(tmpbuf,ibuf);
	  buf->fBuffer=tmpbuf;
	  q_add(fTotalMemSize,-(buf->fBufferSize-ibuf));
	  q_add(&fArrayMemSize,-(buf->fBufferSize-ibuf));
	  buf->fBufferSize=ibuf;

	} else free(tmpbuf);
	buf->fIsCompressed=1;
	WriteBuffer(buf);
	buf->fIsModified=kFALSE;
      }
      buf=buf->fNextOAB;
    }

  } else {
    Float_t fbuf=0;

    while(buf) {
      if(buf->fIsModified) {
	fbuf+=compfrac;

	if(fbuf>=1) {
	  tmpbuf=(char*)malloc(fMaxBDataSize);
	  R__zip(1,&fMaxBDataSize,buf->fBuffer,&fMaxBDataSize,tmpbuf,&ibuf);

	  //If compressed data is more compact than uncompressed data
	  if(ibuf && ibuf<fMaxBDataSize) {
	    //printf("Compression %i/%i\n",ibuf,buf->fBufferSize);
	    free(buf->fBuffer);
	    tmpbuf=(char*)realloc(tmpbuf,ibuf);
	    buf->fBuffer=tmpbuf;
	    q_add(fTotalMemSize,-(buf->fBufferSize-ibuf));
	    q_add(&fArrayMemSize,-(buf->fBufferSize-ibuf));
	    buf->fBufferSize=ibuf;
	  } else free(tmpbuf);
	  --fbuf;
	}
	buf->fIsCompressed=1;
	WriteBuffer(buf);
	buf->fIsModified=kFALSE;
      }
      buf=buf->fNextOAB;
    }
  }

  WriteWriteBuffer();

  if(fCThreshLevel<=fCritLevel) {
    //Remove pause condition on memory compression thread
    pthread_mutex_lock(&fMCMutex);
    fMCAction=0;
    //It is important to unlock fMCMutex after locking fMCCMutex to ensure getting the right condition from QOAMCThread
    pthread_mutex_lock(&fMCCMutex);
    pthread_mutex_unlock(&fMCMutex);
    printstatus("QOversizeArray::Save: Removing pausing condition from memory compression thread");
    pthread_cond_signal(&fMCCond);
    pthread_mutex_unlock(&fMCCMutex);
    pthread_mutex_unlock(&fMCGMutex);
  }

  //Remove pause condition on memory writing thread
  pthread_mutex_lock(&fMWMutex);
  fMWAction=0;
  //It is important to unlock fMWMutex after locking fMWCMutex to ensure getting the right condition from QOAMWThread
  pthread_mutex_lock(&fMWCMutex);
  pthread_mutex_unlock(&fMWMutex);
  printstatus("QOversizeArray::Save: Removing pausing condition from memory writing thread");
  pthread_cond_signal(&fMWCond);
  pthread_mutex_unlock(&fMWCMutex);
  //Unlock buffer structure
  pthread_mutex_unlock(&fBuffersMutex);

  WriteHeader();
}

void QOversizeArray::SetMemConstraints(const Long64_t &minmemsize, Float_t critlevel, const Float_t &level1, const Float_t &level2, Float_t cthreshlevel)
{
  FuncDef(SetMemConstraints,1);

  if(critlevel>1) critlevel=1;
  if(cthreshlevel<0) cthreshlevel=2;

  pthread_mutex_lock(&fILMutex);
  if(fInstances.Count()>0) {
    pthread_mutex_unlock(&fILMutex);
    if(cthreshlevel<=critlevel && fCThreshLevel>fCritLevel) pthread_create(&fMCThread, NULL, QOAMCThread, NULL);
    else if(cthreshlevel>critlevel && fCThreshLevel<=fCritLevel) {
      //Send a signal to fMCThread to terminate and wait for termination
      pthread_mutex_lock(&fMCMutex);
      fMCAction=2;
      //It is important to unlock fMCMutex after locking fMCCMutex to ensure getting the right condition from QOAMCThread
      pthread_mutex_lock(&fMCCMutex);
      pthread_mutex_unlock(&fMCMutex);
      pthread_cond_signal(&fMCCond);
      pthread_mutex_unlock(&fMCCMutex);
      //printf("Waiting for MC thread %p to terminate...\n",this);
      pthread_join(fMCThread,NULL);
      //printf("MC thread %p has terminated\n",this);
    }

  } else pthread_mutex_unlock(&fILMutex);

  //pthread_mutex_lock(&fMSizeMutex);
  fMinMemSize=(minmemsize<0?0:minmemsize);
  fCritLevel=critlevel;
  fLevel1=level1;
  fLevel2=level2;
  fCThreshLevel=cthreshlevel;

  if(fCritLevel) {
    if(!fLevel2) fLevel2=(UInt_t)(0.95*fCritLevel);
    if(!fLevel1|| fLevel1> fLevel2) fLevel1=(UInt_t)(0.95*fLevel2);
  }

#ifdef WITH_LIBPROCINFO
  //while (sysfreemem()+(fTotalMemSize?q_load(fTotalMemSize):0) < fMinMemSize) sleep(fMemUpdateInt);
#else
    fLevel1MemSize=fLevel1*fMinMemSize;
    fLevel2MemSize=fLevel2*fMinMemSize;
    fCritMemSize=fCritLevel*fMinMemSize;
    fCThreshMemSize=fCThreshLevel*fMinMemSize;
#endif
  //pthread_mutex_unlock(&fMSizeMutex);
}

void QOversizeArray::ShowMemStats()
{
#ifdef WITH_LIBPROCINFO
  printf("Available memory: %lli\n",sysfreemem());
#endif

  printf("Memory used by the %u QOversizeArray processes:\n",q_load((uint32_t*)(&fShArSt->susers)));
  Long64_t llbuf;

  for(int i=QOA_MAXPROCS-1; i>=0; --i) {
    llbuf=q_load(fShArSt->memory+i);

    if(llbuf) printf("%3i: %lli\n",i,llbuf);
  }
}

void QOversizeArray::UpdateMemStats()
{
#ifdef WITH_LIBPROCINFO
  struct timeval now;
  gettimeofday(&now,NULL);

  if(now.tv_sec-fMemUpdateTime >= fMemUpdateInt) {
    long long int avail=sysfreemem();
    //printf("Free system memory: %lli\n",avail);

    for(int i=QOA_MAXPROCS-1; i>=0; --i) {
      avail+=q_load(fShArSt->memory+i);
    }
    //printf("Total available memory: %lli\n",avail);
    avail/=q_load(&fShArSt->susers);
    //printf("Available memory for this process: %lli\n",avail);

    if(avail<fMinMemSize) avail=fMinMemSize;

    fLevel1MemSize=fLevel1*avail; //Does not need an atomic operation since it is only read from QOAMMThread (i.e. the only thread calling UpdateMemStats
    q_store(&fLevel2MemSize,(Long64_t)(fLevel2*avail));
    q_store(&fCritMemSize,(Long64_t)(fCritLevel*avail));
    fCThreshMemSize=fCThreshLevel*avail; //Does not need an atomic operation since it is only read from QOAMMThread (i.e. the only thread calling UpdateMemStats
    //printf("fLevel1: %f\tfLevel1MemSize: %lli\n",fLevel1,fLevel1MemSize);
    //printf("fCritLevel: %f\tfCritLevelMemSize: %lli\n",fCritLevel,fCritMemSize);
    fMemUpdateTime=now.tv_sec;
  }
#endif

  if(q_load(&fCLReached) && q_load(fTotalMemSize) <= fCritMemSize) {
    //printstatus("***** Memory size is no longer critical");
    //printf("***** Memory size is no longer critical\n");
    q_store(&fCLReached,(Bool_t)kFALSE);
    //printstatus("Sending signal back to CheckMemory");
    pthread_mutex_lock(&fCMSCMutex);
    pthread_cond_broadcast(&fCMSCond);
    pthread_mutex_unlock(&fCMSCMutex);
  }
}

void QOversizeArray::Terminate()
{
  //MW and BL threads should be terminated before calling this function
  CFuncDef(Terminate,1);
  QOABuffer *buf, *nextbuf;
  printstatus("void QOversizeArray::Terminate()");

  //Lock buffer structure
  pthread_mutex_lock(&fBuffersMutex);

  //printf("FirstReadBuffer index: %i\tLastReadBuffer index: %i\n",fFirstReadBuffer?fFirstReadBuffer->fBufferIdx:-1,fLastReadBuffer?fLastReadBuffer->fBufferIdx:-1);

  if(fNUMBuffers) {
    free(fUMBuffers);
    fUMBuffers=NULL;
    fNUMBuffers=0;
  }

  buf=fFirstReadBuffer;

  while(buf) {
    nextbuf=buf->fNextOAB;
    q_add(fTotalMemSize,-(buf->fBufferSize+sizeof(QOABuffer)));
    q_add(&fArrayMemSize,-(buf->fBufferSize+sizeof(QOABuffer)));
    delete buf;
    buf=nextbuf;
  }

  pthread_mutex_lock(&fPBuffersMutex);
  buf=fFirstParkedBuffer;

  while(buf) {
    nextbuf=buf->fNextOAB;
    q_add(fTotalMemSize,-(buf->fBufferSize+sizeof(QOABuffer)));
    q_add(&fArrayMemSize,-(buf->fBufferSize+sizeof(QOABuffer)));
    delete buf;
    buf=nextbuf;
  }
  fFirstParkedBuffer=NULL;
  pthread_mutex_unlock(&fPBuffersMutex);

  if(fWriteBuffer) {
    q_add(fTotalMemSize,-(fWriteBuffer->fBufferSize+sizeof(QOABuffer)));
    q_add(&fArrayMemSize,-(fWriteBuffer->fBufferSize+sizeof(QOABuffer)));
    delete fWriteBuffer;
    fWriteBuffer=NULL;
  }

  free(fUZQOAB); fUZQOAB=NULL;
  free(fUZBuffers); fUZBuffers=NULL;

  fFirstReadBuffer=NULL;
  fLastReadBuffer=NULL;
  if(fNReadBuffers) fPTNRBObjects=fWBFirstObjIdx;
  else fPTNRBObjects=-1;
  fNReadBuffers=0;
  fWBFirstObjIdx=0;
  fNObjects=0;

  pthread_mutex_unlock(&fBuffersMutex);
}

void QOversizeArray::WriteHeader() const
{
  CFuncDef(WriteHeader,1);
  printstatus("QOversizeArray::WriteHeader()");
  UInt_t uibuf;
  Char_t* sbuf;

  if(fObjectTypeName.Length()) {
    uibuf=fArrayName.Length()+fObjectTypeName.Length()+2;
    sbuf=new Char_t[uibuf];
    strcpy(sbuf,fArrayName);
    strcpy(sbuf+fArrayName.Length()+1,fObjectTypeName);

  } else {
    uibuf=fArrayName.Length()+1;
    sbuf=new Char_t[uibuf];
    strcpy(sbuf,fArrayName);
  }

  if(uibuf>256) {
    fprintf(stderr,"QOversizeArray::WriteHeader: Error: Array description must not exceed 256 characters\n");
    throw 1;
  }

  time_t sec=fTStamp.GetSec();
  Int_t nsec=fTStamp.GetNanoSec();

  pthread_mutex_lock(&fFileWMutex);
  pthread_mutex_lock(&fFileMutex);
  if(lseek(fFDesc,0,SEEK_SET)==-1) {
    perror("QOversizeArray::WriteHeader: Error: ");
    throw 1;
  }
  if((UInt_t)write(fFDesc, &uibuf, sizeof(uibuf))!=sizeof(uibuf) || (UInt_t)write(fFDesc, sbuf, uibuf)!=uibuf) {
    perror("QOversizeArray::WriteHeader: Error: ");
    throw 1;
  }
  delete[] sbuf;
  if(lseek(fFDesc,sizeof(uibuf)+256,SEEK_SET)==-1) {
    perror("QOversizeArray::WriteHeader: Error: ");
    throw 1;
  }
  if(write(fFDesc, &fObjectSize, sizeof(fObjectSize))!=sizeof(fObjectSize) || write(fFDesc, &fNOPerBuffer, sizeof(fNOPerBuffer))!=sizeof(fNOPerBuffer) || write(fFDesc, &fNObjects, sizeof(fNObjects))!=sizeof(fNObjects) || write(fFDesc, &sec, sizeof(sec))!=sizeof(sec) || write(fFDesc, &nsec, sizeof(nsec))!=sizeof(nsec)) {
    perror("QOversizeArray::WriteHeader: Error: ");
    throw 1;
  }
  pthread_mutex_unlock(&fFileMutex);
  pthread_mutex_unlock(&fFileWMutex);
}

void QOversizeArray::WriteBuffer(const QOABuffer *buf) const
{
  CFuncDef(WriteBuffer,0);
  //printf("Write buffer %u at %u\n",buf->fBufferIdx,buf->fBufferIdx*fMaxBHBDataSize+fFirstDataByte);
  pthread_mutex_lock(&fFileWMutex);
  pthread_mutex_lock(&fFileMutex);
  if(lseek(fFDesc,buf->fBufferIdx*fMaxBHBDataSize+fFirstDataByte,SEEK_SET)==-1){
    perror("QOversizeArray::WriteBuffer: lseek Error: ");
    throw 1;
  }
  if(write(fFDesc, &buf->fBufferSize, fBufferHeaderSize)!=fBufferHeaderSize || write(fFDesc, buf->fBuffer, buf->fBufferSize)!=buf->fBufferSize) {
    perror("QOversizeArray::WriteBuffer: write Error: ");
    throw 1;
  }
  pthread_mutex_unlock(&fFileMutex);
  pthread_mutex_unlock(&fFileWMutex);
}

void QOversizeArray::WriteWriteBuffer() const
{
  size_t numobjs=int(fNObjects-((fNObjects-1)/fNOPerBuffer)*fNOPerBuffer);
  //printf("fNObjects=%i, fNOPerBuffer=%i, fBufferSize=%i\n",fNObjects,fNOPerBuffer,fWriteBuffer->fBufferSize);

  if(numobjs) {
    Int_t buffersize=fWriteBuffer->fBufferSize;
    fWriteBuffer->fBufferSize=numobjs*fObjectSize;
    WriteBuffer(fWriteBuffer);
    fWriteBuffer->fBufferSize=buffersize;
  }
}

void* QOversizeArray::QOAMWThread(void *array)
{
  FuncDef(QOAMWThread,1);
  pthread_block_signals_once(throw 1);
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS,NULL); //Thread will cancel right away if pthread_cancel is called
  QOversizeArray *qoa=(QOversizeArray*)array;
  QOABuffer *buf;
  //printstatus("Starting memory writing thread");

  for(;;) {
    pthread_mutex_lock(&qoa->fMWMutex);

    if(qoa->fMWAction) {

      if(qoa->fMWAction == 1) {
	//Pause
	//Important to lock fMWCMutex before unlocking fMWMutex to avoid a deadlock with threads that are waiting for a signal from QOAMWThread
	pthread_mutex_lock(&qoa->fMWCMutex);
	pthread_mutex_unlock(&qoa->fMWMutex);
	pthread_cond_signal(&qoa->fMWPCond);
	pthread_cond_wait(&qoa->fMWCond, &qoa->fMWCMutex);
	//printstatus("Memory writing thread got a signal in pausing condition");
	pthread_mutex_unlock(&qoa->fMWCMutex);
	pthread_mutex_lock(&qoa->fMWMutex);

	if(qoa->fMWAction==1) {
	  pthread_mutex_unlock(&qoa->fMWMutex);
	  continue;
	} else {
	  qoa->fMWBuffer=NULL;
	  pthread_mutex_unlock(&qoa->fMWMutex);
	}
      } else {
	//Stop
	//printstatus("Memory writing thread is stopping");
	//printf("Memory writing thread for array %p is stopping\n",qoa);
	qoa->fMWAction=0;
	qoa->fMWBuffer=NULL;
	pthread_mutex_unlock(&qoa->fMWMutex);
	return NULL;
      }

    } else if(!qoa->fMWBuffer) {
      //printstatus("Memory writing thread is waiting for a new buffer to write");
      //Important to lock fMWCMutex before unlocking fMWMutex to avoid a deadlock with threads that are waiting for a signal from QOAMWThread
      pthread_mutex_lock(&qoa->fMWCMutex);
      pthread_mutex_unlock(&qoa->fMWMutex);
      pthread_cond_signal(&qoa->fMWCCond);
      pthread_cond_wait(&qoa->fMWCond, &qoa->fMWCMutex);
      pthread_mutex_unlock(&qoa->fMWCMutex);
    } else {
      //printstatus("Looping in memory writing thread");

      //printf("Current buffer to be freed: %p\n",qoa->fMWBuffer);
      buf=qoa->fMWBuffer;
      qoa->fMWBuffer=NULL;
      qoa->fMWWBuffer=buf;
      //printf("Value of resetted buffer address: %p\n",qoa->fMWBuffer);
      //Important to lock fMWCMutex before unlocking fMWMutex to avoid a deadlock with threads that are waiting for a signal from QOAMWThread
      pthread_mutex_lock(&qoa->fMWCMutex);
      pthread_mutex_unlock(&qoa->fMWMutex); 
      pthread_cond_signal(&qoa->fMWCCond);
      pthread_mutex_unlock(&qoa->fMWCMutex);
      //printstatus("Memory writing thread just sent a confirmation");

      //Wait for the buffer to finish compressing if it is being compressed by QOAMCThread
      pthread_mutex_lock(&fMCDMutex);
      if(fMCCBuffer==buf) {
        //printf("Memory writing thread is waiting after buffer %p to compress\n",buf);
	pthread_cond_wait(&fMCDCond, &fMCDMutex);
        //printf("Memory writing thread is done waiting after buffer %p to compress\n",buf);
      }
      pthread_mutex_unlock(&fMCDMutex);

      //printf("Writing buffer %p\n",buf);
      qoa->WriteBuffer(buf);

      //Remove it from the linked list first
      pthread_mutex_lock(&qoa->fBuffersMutex);
      buf->fIsModified=kFALSE;
      //To put this block of code after the call to write buffer ensures that buffers removed from the linked list have been fully written on disk (important for reading thread).
      //If the array is in write mode or if the array is in read mode and the current buffer is neither the currently read buffer not a pre-cached buffer
      if(buf!=qoa->fCurReadBuffer && (qoa->fCurBLRBIdx==-2 || buf->fBufferIdx<qoa->fCurBLRBIdx || buf->fBufferIdx-qoa->fCurBLRBIdx>qoa->fNPCBuffers)) {
	if(buf->fPreviousOAB) buf->fPreviousOAB->fNextOAB=buf->fNextOAB;
	else qoa->fFirstReadBuffer=buf->fNextOAB;
	if(buf->fNextOAB) buf->fNextOAB->fPreviousOAB=buf->fPreviousOAB;
	else qoa->fLastReadBuffer=buf->fPreviousOAB;
	--(qoa->fNReadBuffers);

	pthread_mutex_lock(&qoa->fPBuffersMutex);
	if(qoa->fFirstParkedBuffer) {
	  pthread_mutex_unlock(&qoa->fPBuffersMutex);
	  //printf("Memory writing thread is deleting buffer %i\n",buf->fBufferIdx);
	  q_add(fTotalMemSize,-(buf->fBufferSize+sizeof(QOABuffer)));
	  q_add(&qoa->fArrayMemSize,-(buf->fBufferSize+sizeof(QOABuffer)));
	  //printf("QOAMWThread is deleting parked buffer %p\n",buf);
	  delete buf;

	} else {
	  //printf("Memory writing thread is parking buffer %i\n",buf->fBufferIdx);
	  ASSERT(qoa->fCurReadBuffer!=buf);
	  qoa->fFirstParkedBuffer=buf;
	  buf->fPreviousOAB=NULL;
	  buf->fNextOAB=NULL;
	  pthread_mutex_unlock(&qoa->fPBuffersMutex);
	}

	//Else if the buffer is required, add it to the list of unmodified buffers	
      } else {
	qoa->fUMBuffers=(QOABuffer**)realloc(qoa->fUMBuffers,++(qoa->fNUMBuffers)*sizeof(QOABuffer*));
	qoa->fUMBuffers[qoa->fNUMBuffers-1]=buf;
      }
      qoa->fMWWBuffer=NULL;
      //printf("Buffer %p is done writing\n",buf);

      if(qoa->fSigMMThread) {
	pthread_mutex_unlock(&qoa->fBuffersMutex);
	pthread_mutex_lock(&fMWCDMutex);
	//printf("Memory writing thread is signaling that it is done processing buffer %p\n",buf);
	pthread_cond_signal(&fMWCDCond);
	pthread_mutex_unlock(&fMWCDMutex);

      } else pthread_mutex_unlock(&qoa->fBuffersMutex);
    }
    //printstatus("Reached the bottom of the loop in memory writing thread");
  }
  //printstatus("Exiting from memory writing thread");
  return NULL;
}

void* QOversizeArray::QOABLThread(void*)
{
  FuncDef(QOABLThread,1);
  pthread_block_signals_once(throw 1);
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS,NULL); //Thread will cancel right away if pthread_cancel is called
  QOversizeArray *qoa=NULL;
  Int_t ibuf, ibuf2, ibuf3;
  QOABuffer *buf2, *buf3;
  Int_t curblbufidx;
  Int_t action;
  printstatus("Starting buffer loading thread");

  for(;;) {

    sem_wait(&fB2LSem);

    if(!fFirstB2L) break;
    pthread_mutex_lock(&fB2LMutex);

    for(ibuf2=fMaxNPCBuffers; ibuf2>=0; --ibuf2) {

      if(fLastB2Ls[ibuf2]==fFirstB2L) {
	//printf("fLastB2Ls[%i]: %p -> %p\n",ibuf2,fLastB2Ls[ibuf2],fFirstB2L->Next);
	fLastB2Ls[ibuf2]=fFirstB2L->Next;
      }
    }
    qoa=q_load(&fFirstB2L->Array);
    //printf("QOABLThread using array %p\n",qoa);
    fFirstB2L=fFirstB2L->Next;
    pthread_mutex_unlock(&fB2LMutex);

    pthread_mutex_lock(&qoa->fBuffersMutex);

    //If switching from write mode
    if(qoa->fCurBLRBIdx==-2) {
      //printf("Switching from write mode\n");
      pthread_mutex_lock(&qoa->fPBuffersMutex);

      //If there are some parked buffers
      if(qoa->fFirstParkedBuffer) {
	//Delete all parked buffers
	buf3=qoa->fFirstParkedBuffer;

	while(buf3) {
	  ASSERT(qoa->fCurReadBuffer!=buf3);
	  buf2=buf3->fNextOAB;
	  q_add(fTotalMemSize,-(buf3->fBufferSize+sizeof(QOABuffer)));
	  q_add(&qoa->fArrayMemSize,-(buf3->fBufferSize+sizeof(QOABuffer)));
	  delete buf3;
	  buf3=buf2;
	}
	qoa->fFirstParkedBuffer=NULL;

      }
      pthread_mutex_unlock(&qoa->fPBuffersMutex);

    }
    action=-2;

    if(qoa->fCurRBIdx>=0) {

      //Clear old unzipped buffers that are no longer necessary
      if(qoa->fCurRBIdx!=qoa->fCurBLRBIdx){
	qoa->fCurBLRBIdx=qoa->fCurRBIdx;
	qoa->CleanUZBuffers();
      }

      if(qoa->fCurBLBuffer && qoa->fCurBLBuffer->fBufferIdx<=qoa->fCurBLRBIdx) {

	while(qoa->fCurBLBuffer->fNextOAB && qoa->fCurBLBuffer->fNextOAB->fBufferIdx<=qoa->fCurBLRBIdx) {
	  ASSERT(qoa->fCurBLBuffer->fNextOAB->fBufferIdx>qoa->fCurBLBuffer->fBufferIdx);
	  qoa->fCurBLBuffer=qoa->fCurBLBuffer->fNextOAB;
	}
	ASSERT(qoa->fCurBLBuffer->fBufferIdx<=qoa->fCurBLRBIdx);
	curblbufidx=qoa->fCurBLBuffer->fBufferIdx;

      } else if(qoa->fCurReadBuffer) {
	ASSERT(1 && qoa->fCurBLRBIdx>=0);
	//ASSERT(qoa->fCurReadBuffer->fBufferIdx<=qoa->fCurBLRBIdx);
#ifndef QSFAST
	if(!(qoa->fCurReadBuffer->fBufferIdx<=qoa->fCurBLRBIdx)) {
	  fprintf(stderr,"QOABLThread: Error: fCurReadBuffer index=%i and fCurBLRBIdx=%i\n",qoa->fCurReadBuffer->fBufferIdx,qoa->fCurBLRBIdx);
	  throw 1;
	}
#endif
	qoa->fCurBLBuffer=qoa->fCurReadBuffer;
	while(qoa->fCurBLBuffer->fNextOAB && qoa->fCurBLBuffer->fNextOAB->fBufferIdx<=qoa->fCurBLRBIdx) {
#ifndef QSFAST
	  if(!(qoa->fCurBLBuffer->fNextOAB->fBufferIdx>qoa->fCurBLBuffer->fBufferIdx)) {
	    fprintf(stderr,"%p\tBuffer to load: %i, previous buffer: %i, next buffer: %i\n",qoa,qoa->fCurBLRBIdx,qoa->fCurBLBuffer->fBufferIdx,qoa->fCurBLBuffer->fNextOAB->fBufferIdx);
	    //ASSERT(qoa->fCurBLBuffer->fNextOAB->fBufferIdx>qoa->fCurBLBuffer->fBufferIdx);
	  }
#endif
	  qoa->fCurBLBuffer=qoa->fCurBLBuffer->fNextOAB;
	}
	ASSERT(1 && qoa->fCurBLBuffer->fBufferIdx<=qoa->fCurBLRBIdx);
	curblbufidx=qoa->fCurBLBuffer->fBufferIdx;

      } else if(qoa->fFirstReadBuffer && qoa->fFirstReadBuffer->fBufferIdx<=qoa->fCurBLRBIdx) {
	qoa->fCurBLBuffer=qoa->fFirstReadBuffer;
	while(qoa->fCurBLBuffer->fNextOAB && qoa->fCurBLBuffer->fNextOAB->fBufferIdx<=qoa->fCurBLRBIdx) {
#ifndef QSFAST
	  if(qoa->fCurBLBuffer->fNextOAB->fBufferIdx<=qoa->fCurBLBuffer->fBufferIdx) {
	    fprintf(stderr,"%p\tBuffer to load: %i, previous buffer: %i, next buffer: %i\n",qoa,qoa->fCurBLRBIdx,qoa->fCurBLBuffer->fBufferIdx,qoa->fCurBLBuffer->fNextOAB->fBufferIdx);
	    //ASSERT(qoa->fCurBLBuffer->fNextOAB->fBufferIdx>qoa->fCurBLBuffer->fBufferIdx);
	  }
#endif
	  qoa->fCurBLBuffer=qoa->fCurBLBuffer->fNextOAB;
	}
	ASSERT(11 && qoa->fCurBLBuffer->fBufferIdx<=qoa->fCurBLRBIdx);
	curblbufidx=qoa->fCurBLBuffer->fBufferIdx;

      } else {
	qoa->fCurBLBuffer=NULL;
	curblbufidx=-1;
      }
      //printf("fCurBLBuffer: %p\tfCurReadBuffer: %p\n",qoa->fCurBLBuffer,qoa->fCurReadBuffer);

      //Assertion: qoa->fCurBLBuffer (and fCurReadBuffer) contains a pointer to the loaded buffer located the closest by the left to fCurRBIdx (including fCurRBIdx), or if not possible qoa->fCurBLBuffer=fFirstReadBuffer

      //Trying to loop over the buffer that currently needs to be loaded and the pre-cached buffers
      ibuf=qoa->fCurBLRBIdx;
      for(;;) {
	//printf("%p: qoa->fCurBLBuffer idx: %i\treq idx: %i\n",qoa,qoa->fCurBLBuffer?qoa->fCurBLBuffer->fBufferIdx:-1,ibuf);
	ASSERT(action==-2);

	//If the required buffer is not loaded
	if(!qoa->fCurBLBuffer || qoa->fCurBLBuffer->fBufferIdx!=ibuf) {

#ifndef QSFAST
	  if(qoa->fCurBLBuffer && !(qoa->fCurBLBuffer->fBufferIdx<ibuf)) {
	    fprintf(stderr,"Error: QOABLThread: fCurBLBuffer index=%i and ibuf=%i (fCurBLRBIdx is %i, total number of read buffers is %lli)\n",qoa->fCurBLBuffer->fBufferIdx,ibuf,qoa->fCurBLRBIdx,qoa->fWBFirstObjIdx/qoa->fNOPerBuffer);
	    throw 1;
	  }
#endif
	  pthread_mutex_unlock(&qoa->fBuffersMutex);

	  //Load the required buffer from disk
	  //printf("Loading buffer %i\n",ibuf);
	  qoa->ReadBuffer(&buf2, ibuf);
	  CheckMemory();

	  //If it is compressed
	  if(buf2->fIsCompressed==1) {
	    //printf("Unzipping buffer %i\n",ibuf);
	    pthread_mutex_lock(&qoa->fUZBMutex);

	    //Find a free spot in fUZQOAB
	    for(ibuf2=0;;++ibuf2) {
	      if(!qoa->fUZQOAB[ibuf2]) {
		//printf("Storing zipped buffer %i (%p) in fUZQOAB[%i]\n",buf2->fBufferIdx,buf2,ibuf2);
		qoa->fUZQOAB[ibuf2]=buf2;
		break;
	      }
	    }

	    q_add(fTotalMemSize,qoa->fMaxBDataSize);
	    q_add(&qoa->fArrayMemSize,qoa->fMaxBDataSize);
	    //Allocate space for the unzipped buffer
	    qoa->fUZBuffers[ibuf2]=(Char_t*)malloc(qoa->fMaxBDataSize);
	    //printf("Allocating space for fUZBuffers[%i] at position %p\n",ibuf2,qoa->fUZBuffers[ibuf2]);
	    pthread_mutex_unlock(&qoa->fUZBMutex);
	    CheckMemory();
	    //Unzip the buffer
	    R__unzip(&buf2->fBufferSize, (UChar_t*)buf2->fBuffer, &qoa->fMaxBDataSize, qoa->fUZBuffers[ibuf2], &ibuf3);

	    if(!ibuf3) {
	      fprintf(stderr,"QOversizeArray::QOABLThread: Error: A buffer cannot be unzipped\n");
	      throw 1;
	    }
	    buf2->fIsCompressed=4;
	  }

	  pthread_mutex_lock(&qoa->fBuffersMutex);
	  ASSERT(ibuf>=qoa->fCurBLRBIdx);
	  ASSERT(ibuf<=qoa->fCurBLRBIdx+qoa->fNPCBuffers);
#ifndef QSFAST
	  if(qoa->fCurBLBuffer) {
	    ASSERT(3 && qoa->fCurBLBuffer->fBufferIdx<ibuf);
	    ASSERT(qoa->fCurBLBuffer->fBufferIdx<=qoa->fCurBLRBIdx+qoa->fNPCBuffers);
	  }
#endif

	  //Add the buffer to the structure
	  qoa->fUMBuffers=(QOABuffer**)realloc(qoa->fUMBuffers,++(qoa->fNUMBuffers)*sizeof(QOABuffer*));
	  qoa->fUMBuffers[qoa->fNUMBuffers-1]=buf2;

	  //printf("1: fCurBLBuffer: %p\tfCurReadBuffer: %p\n",qoa->fCurBLBuffer,qoa->fCurReadBuffer);

	  //If the index of the buffer that was pointed by fCurBLBuffer was not protected, retrieve the pointer again from the chain, since it can have been deleted while fBuffersMutex was unlocked
	  if(curblbufidx<qoa->fCurBLRBIdx) {

	    if(qoa->fCurReadBuffer && qoa->fCurReadBuffer->fBufferIdx<ibuf) {
	      qoa->fCurBLBuffer=qoa->fCurReadBuffer;
	      while(qoa->fCurBLBuffer->fNextOAB && qoa->fCurBLBuffer->fNextOAB->fBufferIdx<ibuf) qoa->fCurBLBuffer=qoa->fCurBLBuffer->fNextOAB;

	    } else if(qoa->fFirstReadBuffer && qoa->fFirstReadBuffer->fBufferIdx<ibuf) {
	      qoa->fCurBLBuffer=qoa->fFirstReadBuffer;
	      while(qoa->fCurBLBuffer->fNextOAB && qoa->fCurBLBuffer->fNextOAB->fBufferIdx<ibuf) qoa->fCurBLBuffer=qoa->fCurBLBuffer->fNextOAB;

	    } else {
	      qoa->fCurBLBuffer=NULL;
	    }
	  }

	  //printf("2: fCurBLBuffer: %p\tfCurReadBuffer: %p\n",qoa->fCurBLBuffer,qoa->fCurReadBuffer);

	  //If qoa->fCurBLBuffer==NULL (first buffer in read buffer structure)
	  if(!qoa->fCurBLBuffer) {
	    buf2->fPreviousOAB=NULL;

	    if(qoa->fFirstReadBuffer) {
	      ASSERT(qoa->fFirstReadBuffer->fBufferIdx>buf2->fBufferIdx);
	      qoa->fFirstReadBuffer->fPreviousOAB=buf2;
	      buf2->fNextOAB=qoa->fFirstReadBuffer;

	    } else {
	      //fprintf(stderr,"%p\tAdding first buffer\n",qoa);
	      ASSERT(!qoa->fFirstReadBuffer);
	      ASSERT(!qoa->fLastReadBuffer);
	      buf2->fNextOAB=NULL;
	      qoa->fLastReadBuffer=buf2;
	    }
	    qoa->fFirstReadBuffer=buf2;
	    qoa->fCurBLBuffer=buf2;

	    // else if qoa->fCurBLBuffer has a smaller buffer index (this can be the case when qoa->fCurBLBuffer==fCurReadBuffer!=NULL)
	  } else {
	    ASSERT(qoa->fCurBLBuffer->fBufferIdx<buf2->fBufferIdx);
	    //printf("Buffer index of qoa->fCurBLBuffer: %i\n",qoa->fCurBLBuffer->fBufferIdx);
#ifndef QSFAST
	    if(qoa->fCurBLBuffer->fNextOAB) {
	      //printf("qoa->fCurBLBuffer idx: %i\tqoa->fCurBLBuffer->fNextOAB idx: %i\n",qoa->fCurBLBuffer->fBufferIdx,qoa->fCurBLBuffer->fNextOAB->fBufferIdx);
	      //fprintf(stderr,"%p\tLoaded buffer: %i, previous buffer: %i, next buffer: %i curreadbuffer: %i\n",qoa,buf2->fBufferIdx,qoa->fCurBLBuffer->fBufferIdx,qoa->fCurBLBuffer->fNextOAB->fBufferIdx);
	      ASSERT(qoa->fCurBLBuffer->fNextOAB->fBufferIdx>buf2->fBufferIdx);
	    }
	    if(qoa->fCurBLBuffer->fPreviousOAB) {
	      //printf("qoa->fCurBLBuffer idx: %i\tqoa->fCurBLBuffer->fPreviousOAB idx: %i\n",qoa->fCurBLBuffer->fBufferIdx,qoa->fCurBLBuffer->fPreviousOAB->fBufferIdx);
	      //fprintf(stderr,"%p\tLoaded buffer: %i, 2nd previous buffer: %i, previous buffer: %i\n",qoa,buf2->fBufferIdx,qoa->fCurBLBuffer->fPreviousOAB->fBufferIdx,qoa->fCurBLBuffer->fBufferIdx);
	      ASSERT(qoa->fCurBLBuffer->fPreviousOAB->fBufferIdx<buf2->fBufferIdx);
	    }
#endif
	    buf2->fNextOAB=qoa->fCurBLBuffer->fNextOAB;
	    if(qoa->fCurBLBuffer->fNextOAB) qoa->fCurBLBuffer->fNextOAB->fPreviousOAB=buf2;
	    buf2->fPreviousOAB=qoa->fCurBLBuffer;
	    qoa->fCurBLBuffer->fNextOAB=buf2;
	    if(qoa->fCurBLBuffer==qoa->fLastReadBuffer) qoa->fLastReadBuffer=buf2;
	    qoa->fCurBLBuffer=buf2;
	  }
	  ++(qoa->fNReadBuffers);

	  if(qoa->fCurBLBuffer->fBufferIdx==qoa->fCurRBIdx) {
	    sem_post(&qoa->fBLCSem);
	    //printf("Buffer loading thread just sent a confirmation for buffer %i of array %p\n",qoa->fCurBLBuffer->fBufferIdx,qoa);
	  }
	  action=1;
	  //Else if the required buffer is loaded, but it is compressed or being compressed
	} else if(qoa->fCurBLBuffer->fIsCompressed>0 && qoa->fCurBLBuffer->fIsCompressed<3) {

	  //printf("Uncompressing buffer %i\n",ibuf);
	  //If the buffer is currently being compressed, wait for fMCDCond (use a check on fMCCBuffer value to make sure the compression does not end right after releasing fBuffersMutex).
	  if(qoa->fCurBLBuffer->fIsCompressed==2) {
	    pthread_mutex_unlock(&qoa->fBuffersMutex);
	    pthread_mutex_lock(&fMCDMutex);
	    if(fMCCBuffer==qoa->fCurBLBuffer) pthread_cond_wait(&fMCDCond, &fMCDMutex);
	    pthread_mutex_unlock(&fMCDMutex);

	    //Else if the buffer is compressed
	  } else {
	    pthread_mutex_unlock(&qoa->fBuffersMutex);
	  }

	  //printf("Unzipping buffer %i\n",qoa->fCurBLBuffer->fBufferIdx);
	  pthread_mutex_lock(&qoa->fUZBMutex);

	  //Find a free spot in fUZQOAB
	  for(ibuf2=0;;++ibuf2) {
	    if(!qoa->fUZQOAB[ibuf2]) {
	      //printf("Storing zipped buffer %i (%p) in fUZQOAB[%i]\n",qoa->fCurBLBuffer->fBufferIdx,qoa->fCurBLBuffer,ibuf2);
	      qoa->fUZQOAB[ibuf2]=qoa->fCurBLBuffer;
	      break;
	    }
	  }

	  q_add(fTotalMemSize,qoa->fMaxBDataSize);
	  q_add(&qoa->fArrayMemSize,qoa->fMaxBDataSize);
	  //Allocate space for the unzipped buffer
	  qoa->fUZBuffers[ibuf2]=(Char_t*)malloc(qoa->fMaxBDataSize);
	  //printf("Allocating space for fUZBuffers[%i] at position %p\n",ibuf2,qoa->fUZBuffers[ibuf2]);
	  pthread_mutex_unlock(&qoa->fUZBMutex);
	  CheckMemory();
	  //Unzip the buffer
	  R__unzip(&qoa->fCurBLBuffer->fBufferSize, (UChar_t*)qoa->fCurBLBuffer->fBuffer, &qoa->fMaxBDataSize, qoa->fUZBuffers[ibuf2], &ibuf2);

	  if(!ibuf2) {
	    fprintf(stderr,"QOversizeArray::QOABLThread: Error: A buffer cannot be unzipped\n");
	    throw 1;
	  }

	  pthread_mutex_lock(&qoa->fBuffersMutex);
	  qoa->fCurBLBuffer->fIsCompressed=4;

	  if(qoa->fCurBLBuffer->fBufferIdx==qoa->fCurRBIdx) {
	    sem_post(&qoa->fBLCSem);
	    //printf("Buffer loading thread just sent a confirmation for buffer %i of array %p\n",qoa->fCurBLBuffer->fBufferIdx,qoa);
	  }
	  action=1;
	}

	//Switching to write mode (-2)  or pause (-1), or no need to load additional buffers. Note the usage of both fCurRBIdx to get the real array state and fCurBLRBIdx to get the limit for the current loop.
	if(qoa->fCurRBIdx<0 || ibuf==qoa->fCurBLRBIdx+qoa->fNPCBuffers || (ibuf+2)*qoa->fNOPerBuffer>qoa->fWBFirstObjIdx) {
	  action=-1;
	  break;

	//fCurRBIdx has switched outside the current range, need to load a buffer in first priority
	} else if(ibuf>qoa->fCurRBIdx+qoa->fNPCBuffers || ibuf<qoa->fCurRBIdx) {
	  //printf("ibuf=%i, fCurRBIdx=%i\n",ibuf,qoa->fCurRBIdx);
	  action=0;
	  break;

	//Loaded buffer, and need to load another buffer with a computed priority
	} else if(action==1) {
	  action=ibuf+1-qoa->fCurRBIdx;
	  if(qoa->fCurBLBuffer->fNextOAB && qoa->fCurBLBuffer->fNextOAB->fBufferIdx<=ibuf+1) qoa->fCurBLBuffer=qoa->fCurBLBuffer->fNextOAB;
	  ASSERT(1 && qoa->fCurBLBuffer->fBufferIdx>=qoa->fCurBLRBIdx);
	  ASSERT(1 && qoa->fCurBLBuffer->fBufferIdx<=qoa->fCurBLRBIdx+qoa->fNPCBuffers);
	  break;

	//No buffer was loaded, stay in the loop and search for next buffer
	} else {
	  ++ibuf;
	  if(qoa->fCurBLBuffer->fNextOAB && qoa->fCurBLBuffer->fNextOAB->fBufferIdx<=ibuf) qoa->fCurBLBuffer=qoa->fCurBLBuffer->fNextOAB;
	  ASSERT(qoa->fCurBLBuffer->fBufferIdx<=ibuf);
	  ASSERT(11 && qoa->fCurBLBuffer->fBufferIdx>=qoa->fCurBLRBIdx);
	  ASSERT(11 && qoa->fCurBLBuffer->fBufferIdx<=qoa->fCurBLRBIdx+qoa->fNPCBuffers);
	}
      }
    }

    //If the current buffer index is not the last one that needs to be loaded
    if(action>=0) {
      pthread_mutex_lock(&fB2LMutex);
      //printf("Adding node %p (array %p)  with priority %i\n",&qoa->fQOAQ,qoa,action);

      if(fLastB2Ls[action]) {

	//printf("Surrounding nodes: %p %p %p \n",fLastB2Ls[action],&qoa->fQOAQ,fLastB2Ls[action]->Next);
	qoa->fQOAQ.Next=fLastB2Ls[action]->Next;
	fLastB2Ls[action]->Next=&qoa->fQOAQ;
	fLastB2Ls[action]=&qoa->fQOAQ;

      } else {
	//printf("Unique node: %p\n",&qoa->fQOAQ);
	qoa->fQOAQ.Next=NULL;
	fFirstB2L=&qoa->fQOAQ;

	for(Int_t i=fMaxNPCBuffers; i>=0; --i) fLastB2Ls[i]=fFirstB2L;
      }
      pthread_mutex_unlock(&fB2LMutex);
      sem_post(&fB2LSem);

      //Else if all the pre-cached buffers are loaded, if a waiting request is made of if the array is going inro write mode
    } else {
      //Do not add the array to the queue
      q_store(&qoa->fQOAQ.Array,NULL);

      if(qoa->fCurRBIdx==-1) {
	printstatus("Buffer loading thread is sending a waiting notification");
	//printf("Buffer loading thread is sending a waiting notification\n");
	sem_post(&qoa->fBLWSem);

	//Else if qoa->fCurRBIdx==-2, clear old unzipped buffers that are no longer necessary
      } else if(qoa->fCurRBIdx==-2 && qoa->fCurBLRBIdx!=-2){
	qoa->fCurBLBuffer=NULL;
	qoa->fCurBLRBIdx=-2;
	qoa->CleanUZBuffers();
      }
    }
    pthread_mutex_unlock(&qoa->fBuffersMutex);
    //printstatus("Reached the bottom of the loop in buffer loading thread");
  }
  printstatus("Exiting from buffer loading thread");
  //printf("Exiting from buffer loading thread\n");
  return NULL;
}

void* QOversizeArray::QOAMMThread(void *)
{
  FuncDef(QOAMMThread,1);
  pthread_block_signals_once(throw 1);
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS,NULL); //Thread will cancel right away if pthread_cancel is called
  Int_t i,j;
  Float_t fbuf, fbuf2;
  TRandom rnd;
  QOversizeArray *abuf;
  QOABuffer *bbuf, *bbuf2;
  Long64_t libuf,libuf2;
  Bool_t dwait;
  struct timespec waittime;
  waittime.tv_nsec=0;
  //printstatus("Starting memory management thread");

  for(;;) {
    pthread_mutex_lock(&fILMutex);

    //Exit thread if there is no instance left
    if(!fInstances.Count()) {
      pthread_mutex_unlock(&fILMutex);
      //printstatus("Memory management thread is stopping");
      break;

    } else {
      pthread_mutex_unlock(&fILMutex);
    }
    
#ifdef WITH_LIBPROCINFO
    waittime.tv_sec=fMemUpdateTime+fMemUpdateInt;
#endif

    //Wait for memory management condition
    pthread_mutex_lock(&fMMMutex);
#ifdef WITH_LIBPROCINFO
    pthread_cond_timedwait(&fMMCond, &fMMMutex, &waittime);
#else
    pthread_cond_wait(&fMMCond, &fMMMutex);
#endif
    pthread_mutex_unlock(&fMMMutex);

    UpdateMemStats();

    //While total memory size exceeds fLevel1MemSize
    while(fLevel1MemSize && q_load(fTotalMemSize) > fLevel1MemSize) {
      //printf("Looping in memory management thread...\n");
      //printf("TotalMemSize is %lli\n",fTotalMemSize);

      pthread_mutex_lock(&fILMutex);
      //Exit from thread if there is no instance left
      if(!fInstances.Count()) {
	pthread_mutex_unlock(&fILMutex);
	return NULL;
      }
      i=0;

      //If there is more than one instance
      if(fInstances.Count()>1) {
	pthread_mutex_lock(&fPriorityMutex);
	//Compute the cumulative priorities
	fICumulPriority.GetArray()[0]=fInstances.GetArray()[0]->fAPriority;

	for(i=1; i<fInstances.Count(); ++i) {
	  fICumulPriority.GetArray()[i]=fICumulPriority.GetArray()[i-1]+fInstances.GetArray()[i]->fAPriority;
	}
	pthread_mutex_unlock(&fPriorityMutex);
	//Select randomly an instance. The probability of selecting a given instance is proportional to its priority
	fbuf2=fICumulPriority.GetLast();
	fbuf=rnd.Rndm()*fbuf2;
	i=std::lower_bound(fICumulPriority.GetArray(),fICumulPriority.GetArray()+fICumulPriority.Count(),fbuf)-fICumulPriority.GetArray();

	if(i==0) fbuf=fICumulPriority.GetArray()[0];
	else fbuf=fICumulPriority.GetArray()[i]-fICumulPriority.GetArray()[i-1];
      } else {
	fbuf=1.;
	fbuf2=1.;
      }
      abuf=fInstances[i];

      //If there are some parked buffers, delete the first one
      pthread_mutex_lock(&abuf->fPBuffersMutex);
      if(abuf->fFirstParkedBuffer) {
	//printstatus("Deleting a parked buffer");
	bbuf=abuf->fFirstParkedBuffer;
	ASSERT(1 && abuf->fCurReadBuffer!=bbuf);
	//printf("Memory management thread will delete parked buffer %p\n",bbuf);
	abuf->fFirstParkedBuffer=abuf->fFirstParkedBuffer->fNextOAB;
	pthread_mutex_unlock(&abuf->fPBuffersMutex);
	q_add(fTotalMemSize,-(bbuf->fBufferSize+sizeof(QOABuffer)));
	q_add(&abuf->fArrayMemSize,-(bbuf->fBufferSize+sizeof(QOABuffer)));
	delete bbuf;

	//Else if we need to delete a read buffer		
      } else {
	pthread_mutex_unlock(&abuf->fPBuffersMutex);
	bbuf=NULL;

	pthread_mutex_lock(&abuf->fBuffersMutex);
	//printf("Number of unmodified buffers: %i\tNumber of read buffers: %i\n",abuf->fNUMBuffers,abuf->fNReadBuffers);
	//If the array is in write mode. Cannot use the condition on fCurRBIdx==-1 here because we want to make sure that CleanUZBuffers has been called before deleting an unmodified buffer
	if(abuf->fCurRBIdx<0 && abuf->fCurBLRBIdx==-2) {
	  //printstatus("Trying to select a buffer to delete in write mode");

	  //If there are some unmodified buffers
	  if(abuf->fNUMBuffers) {
	    //printstatus("There are unmodified buffers");
	    //Pick the last one that is not the one currently written on disk by QOAMWThread

	    for(i=abuf->fNUMBuffers-1; i>=0; --i) {
	      //printf("Unmodified buffer %i\n",abuf->fUMBuffers[i]->fBufferIdx);
	      if(abuf->fUMBuffers[i] != abuf->fMWWBuffer) {
		bbuf=abuf->fUMBuffers[i];
		break;
	      }
	    }
	  }

	  //If a valid unmodified buffer has been found
	  if(bbuf) {
	    //printf("Unmodified buffer %i will be deleted\n",bbuf->fBufferIdx);

	    for(;i<abuf->fNUMBuffers-1;++i) abuf->fUMBuffers[i]=abuf->fUMBuffers[i+1];
	    abuf->fUMBuffers=(QOABuffer**)realloc(abuf->fUMBuffers,--(abuf->fNUMBuffers)*sizeof(QOABuffer*));

	    //Else if some buffers can be freed
	  } else if(abuf->fNReadBuffers>0) {
	    //printstatus("Will delete a modified buffer");

	    //Get an estimate for the expected number of entries in the array (fPTNRBObjects if previously computed and not exceeeded or
	    //otherwise the current number of entries in read buffers (=fWBFirstObjIdx))
	    libuf=abuf->fPTNRBObjects<abuf->fWBFirstObjIdx?abuf->fWBFirstObjIdx:abuf->fPTNRBObjects;
	    bbuf2=abuf->fFirstReadBuffer;
	    libuf2=(Long64_t)((abuf->fNReadBuffers-abuf->fNReadBuffers*((Double_t)q_load(fTotalMemSize)-fLevel1MemSize)*fbuf/(fbuf2*q_load(&abuf->fArrayMemSize)))*abuf->fNOPerBuffer);
	    if(libuf2<=0) libuf2=abuf->fNOPerBuffer;
	    //printf("Targeted number of loaded buffers: %lli\n",libuf2/abuf->fNOPerBuffer);
	    //printf("Expected number of events: %lli\tTargeted number of loaded events: %lli\n",libuf,libuf2);

	    //Loop over all the fNReadBuffers read buffers (except the very first one) and pick the first one for which the buffer index / read buffer index < expected number of entries / targetted number of buffered entries (=(fNReadBuffers-1)*fNOPerBuffer or (fNReadBuffers-2)*fNOPerBuffer depending if there is another buffer being deleted or not)
	    i=0;
	    for(j=0; j<abuf->fNReadBuffers-1; ++j) {
	      bbuf2=bbuf2->fNextOAB;

	      if(bbuf2 != abuf->fMWWBuffer) {
		bbuf=bbuf2;
		//printf("bidx: %i\t%f\t%f\n",bbuf2->fBufferIdx,bbuf2->fBufferIdx/(i+1.),((Double_t)libuf)/libuf2);
		if(bbuf2->fBufferIdx*libuf2<libuf*(i+1)) break;
		++i;
	      }
	    }

	    //if(bbuf) printf("Modified buffer to be deleted: %i\n",bbuf->fBufferIdx);
	  }

	  //Else if the array is in read mode
	} else {
	  //printstatus("Trying to select a buffer to delete in read mode");

	  //If there are some unmodified buffers
	  if(abuf->fNUMBuffers) {
	    //printstatus("There are unmodified buffers");
	    //printf("fCurBLRIdx is %i\n",abuf->fCurBLRBIdx);
	    //Pick the first one that is neither the currently read buffer nor a pre-cached buffer

	    for(i=0; i<abuf->fNUMBuffers; ++i) {
	      //printf("Unmodified buffer %i\n",abuf->fUMBuffers[i]->fBufferIdx);
	      if(abuf->fUMBuffers[i]!=abuf->fCurReadBuffer && (abuf->fUMBuffers[i]->fBufferIdx<abuf->fCurBLRBIdx || abuf->fUMBuffers[i]->fBufferIdx-abuf->fCurBLRBIdx>abuf->fNPCBuffers) && abuf->fUMBuffers[i] != abuf->fMWWBuffer) {
		bbuf=abuf->fUMBuffers[i];
		//printf("Buffer %i can be deleted\n",bbuf->fBufferIdx);
		break;
	      }
	    }
	  }

	  //If a valid unmodified buffer has been found
	  if(bbuf) {
	    //printf("Unmodified buffer %i will be deleted\n",bbuf->fBufferIdx);

	    for(;i<abuf->fNUMBuffers-1;++i) abuf->fUMBuffers[i]=abuf->fUMBuffers[i+1];
	    abuf->fUMBuffers=(QOABuffer**)realloc(abuf->fUMBuffers,--(abuf->fNUMBuffers)*sizeof(QOABuffer*));

	  } else if(abuf->fNReadBuffers>0) {
	    //printstatus("Will delete a modified buffer");

	    //Get an estimate for the expected number of entries in the array (fPTNRBObjects if previously computed and not exceeeded or
	    //otherwise the current number of entries in read buffers (=fWBFirstObjIdx))
	    libuf=(abuf->fPTNRBObjects<abuf->fWBFirstObjIdx?abuf->fWBFirstObjIdx:abuf->fPTNRBObjects);
	    bbuf2=abuf->fFirstReadBuffer;
	    libuf2=(Long64_t)((abuf->fNReadBuffers-abuf->fNReadBuffers*((Double_t)q_load(fTotalMemSize)-fLevel1MemSize)*fbuf/(fbuf2*q_load(&abuf->fArrayMemSize)))*abuf->fNOPerBuffer);
	    if(libuf2<=0) libuf2=abuf->fNOPerBuffer;
	    //printf("Targeted number of loaded buffers: %lli\n",libuf2/abuf->fNOPerBuffer);

	    //Loop over all the fNReadBuffers read buffers (except the very first one) and pick the first one for which the buffer index / read buffer index < expected number of entries / targetted number of buffered entries (=(fNReadBuffers-1)*fNOPerBuffer or (fNReadBuffers-2)*fNOPerBuffer depending if there is another buffer being deleted or not)
	    i=0;
	    for(j=0; j<abuf->fNReadBuffers-1; ++j) {
	      bbuf2=bbuf2->fNextOAB;

	      if(bbuf2!=abuf->fCurReadBuffer && (bbuf2->fBufferIdx<abuf->fCurBLRBIdx || bbuf2->fBufferIdx-abuf->fCurBLRBIdx>abuf->fNPCBuffers) && bbuf2 != abuf->fMWWBuffer) {
		bbuf=bbuf2;
		//printf("bidx: %i\t%f<%f\n",bbuf2->fBufferIdx,bbuf2->fBufferIdx/(i+1.),((Double_t)libuf)/libuf2);
		if(bbuf2->fBufferIdx*libuf2<libuf*(i+1)) break;
		++i;
	      }
	    }

	    //if(bbuf) printf("Modified buffer to be deleted: %i\n",bbuf->fBufferIdx);
	  }
	}

	//If a read buffer has been selected and is different from the one currently taken care of by QOAMWThread
	if(bbuf) {
	  //printf("Selected buffer by QOAMMThread is %i\n",bbuf->fBufferIdx);
#ifndef QSFAST
	  if(!(bbuf!=abuf->fCurReadBuffer && (abuf->fCurBLRBIdx==-2 || bbuf->fBufferIdx<abuf->fCurBLRBIdx || bbuf->fBufferIdx>abuf->fCurBLRBIdx+abuf->fNPCBuffers))) {
	    fprintf(stderr,"Error: QOAMMThread: fCurBLRBIdx=%i, fBufferIdx=%i, fNPCBuffers=%i\n",abuf->fCurBLRBIdx,bbuf->fBufferIdx,abuf->fNPCBuffers);
	    throw 1;
	  }
#endif
	  //If the buffer has not been modified, delete it
	  if(!bbuf->fIsModified) {
	    //Remove it from the linked list first
	    if(bbuf->fPreviousOAB) bbuf->fPreviousOAB->fNextOAB=bbuf->fNextOAB;
	    else abuf->fFirstReadBuffer=bbuf->fNextOAB;
	    if(bbuf->fNextOAB) bbuf->fNextOAB->fPreviousOAB=bbuf->fPreviousOAB;
	    else abuf->fLastReadBuffer=bbuf->fPreviousOAB;
	    --(abuf->fNReadBuffers);

	    pthread_mutex_lock(&abuf->fPBuffersMutex);
	    if(abuf->fFirstParkedBuffer) {
	      pthread_mutex_unlock(&abuf->fPBuffersMutex);
	      //printf("Memory management thread is deleting buffer %i\n",bbuf->fBufferIdx);
	      q_add(fTotalMemSize,-(bbuf->fBufferSize+sizeof(QOABuffer)));
	      q_add(&abuf->fArrayMemSize,-(bbuf->fBufferSize+sizeof(QOABuffer)));
	      //printf("Memory management thread will delete parked buffer 2 %p\n",bbuf);
	      delete bbuf;

	    } else {
	      //printf("Memory management thread is parking buffer %i\n",bbuf->fBufferIdx);
	      ASSERT(2 && abuf->fCurReadBuffer!=bbuf);
	      abuf->fFirstParkedBuffer=bbuf;
	      bbuf->fPreviousOAB=NULL;
	      bbuf->fNextOAB=NULL;
	      pthread_mutex_unlock(&abuf->fPBuffersMutex);
	    }
	    pthread_mutex_unlock(&abuf->fBuffersMutex);

	    //Else if it was modified
	  } else {

	    //If the buffer is not compressed and the total memory size is >= compression memory size threshold, compress the buffer
	    if(!bbuf->fIsCompressed && q_load(fTotalMemSize)>=fCThreshMemSize) {
	      //printstatus("Memory management thread will compress of write a modified buffer");
	      //fMCMutex and abuf->fMWMutex need to be locked along with fBuffersMutex to check the buffer pointers

	      dwait=kFALSE;
	      //Do not need to lock any mutex to check these since we are just checking if they are non-zero and that fMWCDCond will not be sent by QOAMCThread or QOAMWThread before fBuffersMutex is released
	      //For this to work fMCCBuffer and fMWWBuffer must be set to NULL with fBuffersMutex locked
	      if(fMCCBuffer && abuf->fMWWBuffer) {
		//printstatus("Memory management thread will wait for the MC and MW threads");
		//fprintf(stderr,"Memory management thread will wait for the MC and MW threads before processing buffer %p\n",bbuf);
		//Do not need fMCMutex and abuf->fMWMutex to be locked anymore but abuf->fBuffersMutex should remain locked until fMWCDMutex gets locked and abuf->fSigMMThread gets set
		pthread_mutex_lock(&fMWCDMutex);
		abuf->fSigMMThread=kTRUE;
		pthread_mutex_unlock(&abuf->fBuffersMutex);
		dwait=kTRUE;
		pthread_cond_wait(&fMWCDCond, &fMWCDMutex);
		abuf->fSigMMThread=kFALSE;
		//A fMWCDCond should not be received by QOAMWThread here since this thread is also waiting for the compression to be over to send its signal
		pthread_mutex_unlock(&fMWCDMutex);
	      }

	      if(dwait) {
		pthread_mutex_lock(&abuf->fBuffersMutex);
		if (bbuf->fBufferIdx>=abuf->fCurBLRBIdx && bbuf->fBufferIdx-abuf->fCurBLRBIdx<=abuf->fNPCBuffers) {
		  pthread_mutex_unlock(&abuf->fBuffersMutex);
		  //fprintf(stderr,"Warning, the buffer %p is no longer available\n",bbuf);
		  goto skippoint;
		}
	      }

	      if(!abuf->fMWWBuffer) {
		//printstatus("Memory management thread will write the buffer");
		//printf("Memory management thread will write buffer %p\n",bbuf);

		pthread_mutex_lock(&abuf->fMWMutex);
		//printstatus("Memory management thread is asking to write buffer");
		pthread_mutex_unlock(&abuf->fBuffersMutex);
		//printf("Buffer address is %p\n",bbuf);
		abuf->fMWBuffer=bbuf;
                //It is important to unlock fMWMutex after locking fMWCMutex to ensure getting the right condition from QOAMWThread
		pthread_mutex_lock(&abuf->fMWCMutex);
		pthread_mutex_unlock(&abuf->fMWMutex);
		pthread_cond_signal(&abuf->fMWCond);
		//printstatus("Memory management thread is waiting for a confirmation");
		pthread_cond_wait(&abuf->fMWCCond, &abuf->fMWCMutex);
		pthread_mutex_unlock(&abuf->fMWCMutex);
		//printstatus("Memory management thread received a confirmation");

	      } else {
		//printstatus("Memory management thread will compress the buffer");
		//printf("Memory management thread will compress buffer %p\n",bbuf);

		pthread_mutex_lock(&fMCMutex);
		//printstatus("Memory management thread is asking to compress a buffer");
		//printf("Buffer address is %p\n",bbuf);
		fMCQOA=abuf;
		fMCBuffer=bbuf;
                bbuf->fIsCompressed=2;
		pthread_mutex_unlock(&abuf->fBuffersMutex);
                //It is important to unlock fMCMutex after locking fMCCMutex to ensure getting the right condition from QOAMCThread
		pthread_mutex_lock(&fMCCMutex);
		pthread_mutex_unlock(&fMCMutex);
		pthread_cond_signal(&fMCCond);
		//printstatus("Memory management thread is waiting for a confirmation");
		pthread_cond_wait(&fMCCCond, &fMCCMutex);
		pthread_mutex_unlock(&fMCCMutex);
		//printstatus("Memory management thread received a confirmation");
	      }
	      
	      //Else if the buffer is already compressed or total memory size < compression memory size threshold, write ask the memory writing thread to write it on disk and delete it
	    } else {

	      //printstatus("Memory management thread is asking to write a buffer");
	      //fMWMutex must be locked before unlocking fBuffersMutex to not remove QOAMWThread from pause condition
	      pthread_mutex_lock(&abuf->fMWMutex);
	      pthread_mutex_unlock(&abuf->fBuffersMutex);
	      //printf("Buffer address is %p\n",bbuf);
	      abuf->fMWBuffer=bbuf;
              //It is important to unlock fMWMutex after locking fMWCMutex to ensure getting the right condition from QOAMWThread
	      pthread_mutex_lock(&abuf->fMWCMutex);
	      pthread_mutex_unlock(&abuf->fMWMutex);
	      pthread_cond_signal(&abuf->fMWCond);
	      //printstatus("Memory management thread is waiting for a confirmation");
	      pthread_cond_wait(&abuf->fMWCCond, &abuf->fMWCMutex);
	      pthread_mutex_unlock(&abuf->fMWCMutex);
	      //printstatus("Memory management thread received a confirmation");
	    }
	  }

	} else {
	  pthread_mutex_unlock(&abuf->fBuffersMutex);
	}
      }

skippoint:
      pthread_mutex_unlock(&fILMutex);

      UpdateMemStats();
    }
  }

  //printstatus("Exiting from memory management thread");
  return NULL;
}

void* QOversizeArray::QOAMCThread(void *array)
{
  FuncDef(QOAMCThread,1);
  pthread_block_signals_once(throw 1);
  pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS,NULL); //Thread will cancel right away if pthread_cancel is called
  QOABuffer *buf;
  char* tmpbuf;
  Int_t ibuf;
  printstatus("Starting memory compression thread");

  for(;;) {
    pthread_mutex_lock(&fMCMutex);

    if(fMCAction) {

      if(fMCAction == 1) {
	//Pause
	//Important to lock fMCCMutex before unlocking fMCMutex to avoid a deadlock with threads that are waiting for a signal from QOAMCThread
	pthread_mutex_lock(&fMCCMutex);
	pthread_mutex_unlock(&fMCMutex);
	pthread_cond_signal(&fMCPCond);
	pthread_cond_wait(&fMCCond, &fMCCMutex);
	printstatus("Memory compression thread got a signal in pausing condition");
	pthread_mutex_unlock(&fMCCMutex);
	pthread_mutex_lock(&fMCMutex);

	if(fMCAction==1) {
	  pthread_mutex_unlock(&fMCMutex);
	  continue;
	} else {
	  fMCBuffer=NULL;
	  pthread_mutex_unlock(&fMCMutex);
	}
      } else {
	//Stop
	printstatus("Memory compression thread is stopping");
	fMCAction=0;
	fMCBuffer=NULL;
	pthread_mutex_unlock(&fMCMutex);
	return NULL;
      }

    } else if(!fMCBuffer) {
      printstatus("Memory compression thread is waiting for a new buffer to compress");
      //Important to lock fMCCMutex before unlocking fMCMutex to avoid a deadlock with threads that are waiting for a signal from QOAMCThread
      pthread_mutex_lock(&fMCCMutex);
      pthread_mutex_unlock(&fMCMutex);
      pthread_cond_signal(&fMCCCond);
      pthread_cond_wait(&fMCCond, &fMCCMutex);
      pthread_mutex_unlock(&fMCCMutex);

    } else {
      printstatus("Looping in memory compression thread");

      //printf("Current buffer to be freed: %p\n",fMCBuffer);
      buf=fMCBuffer;
      fMCBuffer=NULL;
      fMCCBuffer=buf;
      //printf("Value of resetted buffer address: %p\n",fMCBuffer);

      //Important to lock fMCCMutex before unlocking fMCMutex to avoid a deadlock with threads that are waiting for a signal from QOAMCThread
      pthread_mutex_lock(&fMCCMutex);
      pthread_mutex_unlock(&fMCMutex);
      pthread_cond_signal(&fMCCCond);
      pthread_mutex_unlock(&fMCCMutex);
      //printf("Memory compression thread will compress buffer %p\n",buf);
      printstatus("Memory compression thread just sent a confirmation");

      tmpbuf=(char*)malloc(fMCQOA->fMaxBDataSize);
      //printf("Compressing buffer %p\n",buf);
      R__zip(1,&fMCQOA->fMaxBDataSize,buf->fBuffer,&fMCQOA->fMaxBDataSize,tmpbuf,&ibuf);

      //If compressed data is more compact than uncompressed data
      if(ibuf && ibuf<fMCQOA->fMaxBDataSize) {
	//printf("Compression %i/%i\n",ibuf,buf->fBufferSize);
	free(buf->fBuffer);
	tmpbuf=(char*)realloc(tmpbuf,ibuf);
	buf->fBuffer=tmpbuf;
	q_add(fTotalMemSize,-(buf->fBufferSize-ibuf));
	q_add(&fMCQOA->fArrayMemSize,-(buf->fBufferSize-ibuf));
	buf->fBufferSize=ibuf;
      } else free(tmpbuf);
      //Since fBuffersMutex is released at this point and that fILMutex cannot be locked, the QOA could be deleted. However, since QOAMCThread only compresses modified buffers and that such buffers are written and deleted only after they are done compressing, it is safe to try accesing buf without further verifications
      pthread_mutex_lock(&fMCQOA->fBuffersMutex);
      buf->fIsCompressed=1;
      fMCCBuffer=NULL;
      pthread_mutex_unlock(&fMCQOA->fBuffersMutex);
      //printf("Memory compression thread is done using buffer %p\n",buf);

      pthread_mutex_lock(&fMCDMutex);
      pthread_cond_broadcast(&fMCDCond);
      pthread_mutex_unlock(&fMCDMutex);

      pthread_mutex_lock(&fMWCDMutex);
      //printf("Buffer %p is done compressing\n",buf);
      pthread_cond_signal(&fMWCDCond);
      pthread_mutex_unlock(&fMWCDMutex);
    }
    printstatus("Reached the bottom of the loop in memory compression thread");
  }
  printstatus("Exiting from memory compression thread");
  return NULL;
}
