#include "QOversizeArray.h"

ClassImp(QOversizeArray)

QList<QOversizeArray*> QOversizeArray::fInstances;
QList<Float_t> QOversizeArray::fICumulPriority;
Long64_t QOversizeArray::fLevel1MemSize=0;
Long64_t QOversizeArray::fLevel2MemSize=0;
Long64_t QOversizeArray::fCritMemSize=0;
Long64_t QOversizeArray::fCThreshMemSize=0;
Long64_t QOversizeArray::fTotalMemSize=0;
pthread_mutex_t QOversizeArray::fMSizeMutex=PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t QOversizeArray::fCMSCMutex=PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t QOversizeArray::fCMSCond=PTHREAD_COND_INITIALIZER;
Bool_t QOversizeArray::fCLReached=kFALSE;
pthread_t QOversizeArray::fMMThread;
pthread_mutex_t QOversizeArray::fMMMutex=PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t QOversizeArray::fMMCond=PTHREAD_COND_INITIALIZER;
pthread_mutex_t QOversizeArray::fILMutex=PTHREAD_MUTEX_INITIALIZER;
pthread_mutex_t QOversizeArray::fPriorityMutex=PTHREAD_MUTEX_INITIALIZER;

//#define FuncDef(a,b) const char* thefunc=#a; Bool_t show=b; TString indent;
//#define pthread_mutex_lock(a) {if(show) printf("\n%slocking %s in function %s...\n",indent.Data(),#a,thefunc); pthread_mutex_lock(a); if(show) printf("%s%s is now locked in function %s\n",indent.Data(),#a,thefunc); indent+="     ";}
//#define pthread_mutex_unlock(a) {indent=indent(0,indent.Length()-5); if(show) printf("\n%sunlocking %s in function %s...\n",indent.Data(),#a,thefunc); pthread_mutex_unlock(a); if(show) printf("%s%s is now unlocked in function %s\n",indent.Data(),#a,thefunc);}
//#define pthread_cond_signal(a) {if(show) printf("sending signal %s in function %s...\n",#a,thefunc); pthread_cond_signal(a);}
//#define pthread_cond_wait(a,b) {if(show) printf("waiting for signal %s with mutex %s in function %s...\n",#a,#b,thefunc); pthread_cond_wait(a,b);}
//#define ASSERT(a) if(!(a) && show) {fprintf(stderr,"Error in function %s: Assertion failed: %s\n",thefunc,#a); throw 1;}

#define FuncDef(a,b)
#define ASSERT(a)


/*void printstatus(const char* status)
  {
  printf("%s\n",status);
  }*/

void printstatus(const char*){}

QOversizeArray::QOversizeArray(const char *filename, const char *arraydescr, omode openmode, const UInt_t &objectsize, const UInt_t &nobjectsperbuffer, const Int_t &npcbuffers, const UInt_t &nobjectsallocblock): fFilename(filename), fArrayName(), fTStamp(), fFDesc(0), fFirstDataByte(sizeof(UInt_t)+256+sizeof(fObjectSize)+sizeof(fNOPerBuffer)+sizeof(fNObjects)+sizeof(time_t)+sizeof(Int_t)), fBufferHeaderSize(sizeof(UInt_t)), fOpenMode(openmode), fObjectSize(objectsize), fObjectTypeName(), fBuffer(NULL), fNOPerBuffer(nobjectsperbuffer), fNOAllocBlock(nobjectsallocblock),fMaxBDataSize(objectsize*nobjectsperbuffer), fMaxBHBDataSize(fBufferHeaderSize+fMaxBDataSize), fNObjects(0), fCurReadBuffer(NULL), fFirstReadBuffer(NULL), fLastReadBuffer(NULL), fWriteBuffer(NULL), fCRBData(NULL), fWBFirstObjIdx(0), fWBNAllocObjs(0), fCurRBIdx(-1), fCurBLRBIdx(-2), fNReadBuffers(0), fPTNRBObjects(-1), fUMBuffers(NULL), fNUMBuffers(0), fFirstParkedBuffer(NULL), fNPCBuffers(npcbuffers>0?npcbuffers:0), fArrayMemSize(0), fArrayIO(0), fAPriority(0), fFileMutex(), fBuffersMutex(), fPBuffersMutex(), fRBDIMutex(), fMWThread(), fMWMutex(), fMWCond(), fMWCMutex(), fMWPCond(), fMWCCond(), fMWAction(kFALSE), fMWBuffer(NULL), fMWWBuffer(NULL), fBLThread(), fBLMutex(), fBLCond(), fBLCMutex(), fBLCCond(), fBLWCond(), fBLAction(kFALSE), fUZQOAB(NULL), fUZBuffers(NULL), fUZBMutex()
{
  FuncDef(QOversizeArray,1);
  TString sbuf=arraydescr;
  Int_t k=sbuf.Last('/');

  if(k==-1) fArrayName=sbuf;
  else {
    fArrayName=sbuf(0,k);
    fObjectTypeName=sbuf(k+1,sbuf.Length()-k-1);
  }

  pthread_mutex_init(&fFileMutex,NULL);
  pthread_mutex_init(&fBuffersMutex,NULL);
  pthread_mutex_init(&fPBuffersMutex,NULL);
  pthread_mutex_init(&fRBDIMutex,NULL);
  pthread_mutex_init(&fMWMutex,NULL);
  pthread_cond_init(&fMWCond,NULL);
  pthread_mutex_init(&fMWCMutex,NULL);
  pthread_cond_init(&fMWPCond,NULL);
  pthread_cond_init(&fMWCCond,NULL);
  pthread_mutex_init(&fBLMutex,NULL);
  pthread_cond_init(&fBLCond,NULL);
  pthread_mutex_init(&fBLCMutex,NULL);
  pthread_cond_init(&fBLCCond,NULL);
  pthread_cond_init(&fBLWCond,NULL);
  pthread_mutex_init(&fUZBMutex,NULL);

  pthread_mutex_lock(&fMSizeMutex);
  //printf("%p\tTotal memory at array creation: %lli\n",this,fTotalMemSize);
  pthread_mutex_unlock(&fMSizeMutex);
  OpenFile();
}

QOversizeArray::~QOversizeArray()
{
  FuncDef(~QOversizeArray,1);
  printstatus("QOversizeArray::~QOversizeArray()");

  CloseFile();
  //printf("%p\tArray size at array destruction: %lli\n",this,fArrayMemSize);
  pthread_mutex_lock(&fMSizeMutex);
  //printf("%p\tTotal memory at array destruction: %lli\n",this,fTotalMemSize);
  pthread_mutex_unlock(&fMSizeMutex);
}

void QOversizeArray::CloseFile()
{
  FuncDef(CloseFile,1);
  printstatus("void QOversizeArray::CloseFile()");
  if(fFDesc) {
    pthread_mutex_lock(&fILMutex);

    Int_t idx=fInstances.FindFirst(this);
    fICumulPriority.Del(idx);
    fInstances.Del(idx);
    pthread_mutex_unlock(&fILMutex);
    //Send a signal to fMWThread to terminate and wait for termination
    pthread_mutex_lock(&fMWMutex);
    fMWAction=2;
    pthread_mutex_unlock(&fMWMutex);
    pthread_mutex_lock(&fMWCMutex);
    pthread_cond_signal(&fMWCond);
    pthread_mutex_unlock(&fMWCMutex);
    //printf("Waiting for MW thread %p to terminate...\n",this);
    pthread_join(fMWThread,NULL);
    //printf("MW thread %p has terminated\n",this);

    //Send a signal to fBLThread to terminate and wait for termination
    pthread_mutex_lock(&fBLMutex);
    fBLAction=2;
    pthread_mutex_unlock(&fBLMutex);
    pthread_mutex_lock(&fBLCMutex);
    pthread_cond_signal(&fBLCond);
    pthread_mutex_unlock(&fBLCMutex);
    //printf("Waiting for BL %p thread to terminate...\n",this);
    pthread_join(fBLThread,NULL);
    //printf("BL thread %p has terminated\n",this);

    if(close(fFDesc)) {
      perror("QOversizeArray::~QOversizeArray(): Error: ");
      throw 1;
    }
    fFDesc=0;
  }

  Terminate();
}

void QOversizeArray::CheckMemory()
{
  FuncDef(CheckMemory,1);
  pthread_mutex_lock(&fMSizeMutex);
  if(fLevel2MemSize && fTotalMemSize > fLevel2MemSize) {
    pthread_mutex_lock(&fMMMutex);
    pthread_cond_signal(&fMMCond);
    pthread_mutex_unlock(&fMMMutex);
  }
  pthread_mutex_lock(&fCMSCMutex);

  if(fCritMemSize && fTotalMemSize > fCritMemSize) {
    //Need to use this order for unlocking to avoid deadlock with MMThread due to wait on fC<SCond
    pthread_mutex_unlock(&fMSizeMutex);
    printstatus("***** Critical memory size has been reached");
    fCLReached=kTRUE;
    pthread_cond_wait(&fCMSCond, &fCMSCMutex);
    pthread_mutex_unlock(&fCMSCMutex);

  } else {
    pthread_mutex_unlock(&fMSizeMutex);
    pthread_mutex_unlock(&fCMSCMutex);
  }
}

void QOversizeArray::CleanUZBuffers()
{
  FuncDef(CleanUZBuffers,1);
  printstatus("QOversizeArray::CleanUZBuffers()");
  //****** Make sure fBuffersMutex is locked and that fCurBLRBIdx!=-2 when calling this function!
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
  pthread_mutex_lock(&fMSizeMutex);
  fTotalMemSize-=j*fMaxBDataSize;
  fArrayMemSize-=j*fMaxBDataSize;
  pthread_mutex_unlock(&fMSizeMutex);
  pthread_mutex_unlock(&fUZBMutex);
}

void QOversizeArray::Init()
{
  fUZQOAB=(QOABuffer**)malloc((fNPCBuffers+2)*sizeof(QOABuffer*));
  fUZBuffers=(Char_t**)malloc((fNPCBuffers+2)*sizeof(Char_t*));
  memset(fUZQOAB,0,(fNPCBuffers+2)*sizeof(QOABuffer*));
  memset(fUZBuffers,0,(fNPCBuffers+2)*sizeof(Char_t*));
  ReadWriteBuffer();
}

void QOversizeArray::Fill()
{
  FuncDef(Fill,1);

  //If fWriteBuffer contains the maximum number of objects allowed by the currently allocated memory
  if(fNObjects-fWBFirstObjIdx == fWBNAllocObjs) {
    static UInt_t allocsize;

    if(fWBNAllocObjs<fNOPerBuffer) {
      fWBNAllocObjs+=fNOAllocBlock;

      if(fWBNAllocObjs>fNOPerBuffer) fWBNAllocObjs=fNOPerBuffer;
      allocsize=fWBNAllocObjs*fObjectSize;

      fWriteBuffer->fBuffer=(char*)realloc(fWriteBuffer->fBuffer,allocsize);
      pthread_mutex_lock(&fMSizeMutex);
      fTotalMemSize+=allocsize-fWriteBuffer->fBufferSize;
      fArrayMemSize+=allocsize-fWriteBuffer->fBufferSize;
      pthread_mutex_unlock(&fMSizeMutex);
      fWriteBuffer->fBufferSize=allocsize;

      //If fWriteBuffer is full
    } else {
      printstatus("Write buffer is full");
#ifndef QSFAST
      if(fOpenMode == kRead) {
	fprintf(stderr,"QOversizeArray::Fill(): Error: File '%s' is opened in read-only mode\n",fFilename.Data());
	throw 1;
      }
#endif

      static UInt_t nextbufidx;
      nextbufidx=fWriteBuffer->fBufferIdx+1;

      //If switching from read mode
      pthread_mutex_lock(&fBuffersMutex);
      if(fCurReadBuffer) {
	fCurReadBuffer=NULL;
	fCurRBIdx=-1;

	pthread_mutex_unlock(&fBuffersMutex);

	//Request the buffer loading thread to go in waiting condition
	pthread_mutex_lock(&fBLCMutex);
	pthread_cond_signal(&fBLCond);
	//Wait for confirmation
	pthread_cond_wait(&fBLWCond,&fBLCMutex);
	pthread_mutex_unlock(&fBLCMutex);

	pthread_mutex_lock(&fBuffersMutex);
      }
      fWriteBuffer->fPreviousOAB=fLastReadBuffer;
      fWriteBuffer->fNextOAB=NULL;
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
	printstatus("Using a parked buffer for the next write buffer");
	fWriteBuffer=fFirstParkedBuffer;
	fFirstParkedBuffer=fFirstParkedBuffer->fNextOAB;
	pthread_mutex_unlock(&fPBuffersMutex);
	fWriteBuffer->fBufferIdx=nextbufidx;
	fWriteBuffer->fIsCompressed=0;
	fWBNAllocObjs=fWriteBuffer->fBufferSize/fObjectSize;

	if(fWBNAllocObjs<fNOAllocBlock) {
	  free(fWriteBuffer->fBuffer);
	  fWBNAllocObjs=fNOAllocBlock;
	  allocsize=fWBNAllocObjs*fObjectSize;
	  fWriteBuffer->fBuffer=(char*)malloc(allocsize);
	  pthread_mutex_lock(&fMSizeMutex);
	  fTotalMemSize+=allocsize-fWriteBuffer->fBufferSize;
	  fArrayMemSize+=allocsize-fWriteBuffer->fBufferSize;
	  pthread_mutex_unlock(&fMSizeMutex);
	  fWriteBuffer->fBufferSize=allocsize;
	}

      } else {
	printstatus("Using a new buffer for the next write buffer");
	pthread_mutex_unlock(&fPBuffersMutex);
	fWBNAllocObjs=fNOAllocBlock;
	allocsize=fWBNAllocObjs*fObjectSize;
	fWriteBuffer=new QOABuffer(nextbufidx, allocsize);
	pthread_mutex_lock(&fMSizeMutex);
	fTotalMemSize+=allocsize+sizeof(QOABuffer);
	fArrayMemSize+=allocsize+sizeof(QOABuffer);
	pthread_mutex_unlock(&fMSizeMutex);
	CheckMemory();
      }
      //printf("Next write buffer index is %i\n",nextbufidx);
    }
  }
  //Copy the content of the object buffer in fWriteBuffer
  //It is fine to access fWBFirstObjIdx from the main thread without locking fBuffersMutex, since its value can only be modified from that thread
  memcpy(fWriteBuffer->fBuffer+(fNObjects-fWBFirstObjIdx)*fObjectSize,fBuffer,fObjectSize);
  ++fNObjects;
}

void QOversizeArray::LoadEntry(const Long64_t &entry)
{
  //Do not need to apply locks for reading operations of variables that are only modified in the main thread
  FuncDef(LoadEntry,1);
  static Int_t uzbidx=-1;
  static Int_t ibuf;
#ifndef QSFAST
  //If the entry is out of bound
  if(entry<0 || entry>=fNObjects) {
    fprintf(stderr,"QOversizeArray::LoadEntry: Error: Entry index is invalid\n");
    return;
  }
#endif

  //If the entry is located in the write buffer. Do not need a lock since only the main thread access the write buffer
  if(entry>=fWBFirstObjIdx) {
    if(fCurRBIdx!=-1) {
      pthread_mutex_lock(&fBuffersMutex);
      fCurReadBuffer=NULL;
      fCurRBIdx=-1;
      pthread_mutex_unlock(&fBuffersMutex);
    }

    memcpy(fBuffer,fWriteBuffer->fBuffer+(entry-fWBFirstObjIdx)*fObjectSize,fObjectSize);
    return;
  }

  ibuf=entry/fNOPerBuffer;

  //If the read buffer for the current event is already loaded
  if(fCurReadBuffer && ibuf==fCurReadBuffer->fBufferIdx) {
    //Copy the memory
    //printf("Entry %lli\n",entry);
    memcpy(fBuffer,fCRBData+(entry-fCurReadBuffer->fBufferIdx*fNOPerBuffer)*fObjectSize,fObjectSize);

  } else {
    //printf("Calling LoadEntry with entry==%lli (buffer index %i)\n",entry,ibuf);
    pthread_mutex_lock(&fBuffersMutex);
    fCurRBIdx=ibuf;

    if(!fCurReadBuffer || fCurReadBuffer->fBufferIdx>fCurRBIdx) fCurReadBuffer=fFirstReadBuffer;

    if(fCurReadBuffer) while(fCurReadBuffer->fNextOAB && fCurReadBuffer->fNextOAB->fBufferIdx<=fCurRBIdx) fCurReadBuffer=fCurReadBuffer->fNextOAB;
    //printf("Looping on buffers with fCurReadBuffer==%p with buffer index %i\n",fCurReadBuffer,fCurReadBuffer?fCurReadBuffer->fBufferIdx:-1);

    //If the buffer is not loaded
    if(!fCurReadBuffer || fCurReadBuffer->fBufferIdx != fCurRBIdx) {
      pthread_mutex_unlock(&fBuffersMutex);
      pthread_mutex_lock(&fBLMutex);
      //printf("LoadEntry is waiting for buffer %i to load\n",fCurRBIdx);
      //printf("Buffer address is %p\n",bbuf);
      pthread_mutex_lock(&fBLCMutex);
      pthread_cond_signal(&fBLCond);
      pthread_mutex_unlock(&fBLMutex);
      printstatus("LoadEntry is waiting for a confirmation");
      pthread_cond_wait(&fBLCCond, &fBLCMutex);
      pthread_mutex_unlock(&fBLCMutex);
      printstatus("LoadEntry received a confirmation");

      pthread_mutex_lock(&fBuffersMutex);
      if(!fCurReadBuffer) fCurReadBuffer=fFirstReadBuffer;
      while(fCurReadBuffer->fNextOAB && fCurReadBuffer->fNextOAB->fBufferIdx<=fCurRBIdx) fCurReadBuffer=fCurReadBuffer->fNextOAB;
      //Assertion: fCurReadBuffer->fBufferIdx==fCurRBIdx
      ASSERT(fCurReadBuffer->fBufferIdx==fCurRBIdx);
      pthread_mutex_unlock(&fBuffersMutex);

      //Else if the buffer is being uncompressed
    } else if(fCurReadBuffer->fIsCompressed != 0 && fCurReadBuffer->fIsCompressed != 4) {
      pthread_mutex_unlock(&fBuffersMutex);
      pthread_mutex_lock(&fBLMutex);
      //printf("LoadEntry is waiting for buffer %i to unzip\n",fCurRBIdx);
      pthread_mutex_lock(&fBLCMutex);
      pthread_cond_signal(&fBLCond);
      pthread_mutex_unlock(&fBLMutex);
      printstatus("LoadEntry is waiting for a confirmation");
      pthread_cond_wait(&fBLCCond, &fBLCMutex);
      pthread_mutex_unlock(&fBLCMutex);
      printstatus("LoadEntry received a confirmation");

    } else {
      pthread_mutex_unlock(&fBuffersMutex);
      pthread_mutex_lock(&fBLCMutex);
      pthread_cond_signal(&fBLCond);
      pthread_mutex_unlock(&fBLCMutex);
    }

    //If the buffer has never been compressed
    if(fCurReadBuffer->fIsCompressed==0) fCRBData=fCurReadBuffer->fBuffer;

    //Else if the buffer is uncompressed (fIsCompressed==4)
    else {
      pthread_mutex_lock(&fUZBMutex);

      //Find fCurReadBuffer in fUZQOAB
      for(uzbidx=0;;++uzbidx) {

	if(fUZQOAB[uzbidx]==fCurReadBuffer) {
	  //printf("LoadEntry: Uncompressed buffer %i==%i (%p) is found in fUZQOAB[%i]\n",fCurReadBuffer->fBufferIdx,fUZQOAB[uzbidx]->fBufferIdx,fUZQOAB[uzbidx],uzbidx);
	  fCRBData=fUZBuffers[uzbidx];
	  break;
	}
      }
      ASSERT(uzbidx<fNPCBuffers+2 && fUZQOAB[uzbidx]->fBufferIdx==fCurReadBuffer->fBufferIdx);
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
  }   

  return;
}

void QOversizeArray::OpenFile()
{
  FuncDef(OpenFile,0);
  printstatus("void QOversizeArray::OpenFile()");
  if(fFDesc) CloseFile();

  switch(fOpenMode) {
    case kRead:
      fFDesc=open(fFilename,O_RDONLY);

      if(fFDesc<0) {
	fprintf(stderr,"QOversizeArray::OpenFile: Error: file '%s' cannot be opened in read-only mode\n",fFilename.Data());
	throw 1;
      }
      ReadHeader();
      break;

    case kRW:
      fFDesc=open(fFilename,O_RDWR);

      if(fFDesc<0) {
	fprintf(stderr,"QOversizeArray::OpenFile: Error: file '%s' cannot be opened in read-write mode\n",fFilename.Data());
	throw 1;
      }
      ReadHeader();
      break;

    case kRecreate:
      fFDesc=open(fFilename,O_RDWR|O_CREAT|O_TRUNC,S_IRUSR|S_IWUSR|S_IRGRP|S_IROTH);

      if(fFDesc<0) {
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
  pthread_create(&fMWThread, NULL, QOAMWThread, this);
  //Create buffer loading thread
  pthread_create(&fBLThread, NULL, QOABLThread, this);

  pthread_mutex_lock(&fILMutex);
  fInstances.Add(this);
  fICumulPriority.Add(0);

  if(fInstances.Count() == 1) {
    pthread_mutex_unlock(&fILMutex);
    pthread_create(&fMMThread, NULL, QOAMMThread, NULL);

  } else {
    pthread_mutex_unlock(&fILMutex);
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
  printstatus("QOversizeArray::ResetArray has been called");
  //Resets the array, without saving on disk and without freeing memory (buffers are converted into parked buffers). Should be the method to be called before refilling an array.
  FuncDef(ResetArray,1);
  pthread_mutex_lock(&fBuffersMutex);
  fCurReadBuffer=NULL;
  fCurRBIdx=-1;
  pthread_mutex_unlock(&fBuffersMutex);

  //Wait for the buffer loading thread to finish the current operation
  //Request the buffer loading thread to go in waiting condition
  pthread_mutex_lock(&fBLCMutex);
  pthread_cond_signal(&fBLCond);
  //Wait for confirmation
  pthread_cond_wait(&fBLWCond,&fBLCMutex);
  pthread_mutex_unlock(&fBLCMutex);

  pthread_mutex_lock(&fMWMutex);
  //Request a pause from memory writing thread
  fMWAction=1;
  pthread_mutex_unlock(&fMWMutex);
  pthread_mutex_lock(&fMWCMutex);
  pthread_cond_signal(&fMWCond);
  //Wait for pause confirmation
  pthread_cond_wait(&fMWPCond,&fMWCMutex);
  pthread_mutex_unlock(&fMWCMutex);
  printstatus("Memory writing thread confirmed to be in pausing condition");

  //Lock buffer structure. Lock fRBDIMutex must be applied after the lock on fBuffersMutex to ensure possible compression operations are over in QOAMMThread
  pthread_mutex_lock(&fBuffersMutex);
  pthread_mutex_lock(&fRBDIMutex);

  if(fNUMBuffers) {
    free(fUMBuffers);
    fUMBuffers=NULL;
    fNUMBuffers=0;
  }

  if(fFirstReadBuffer) {
    if(!fFirstParkedBuffer) fFirstParkedBuffer=fFirstReadBuffer;
    else {
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

  pthread_mutex_unlock(&fRBDIMutex);

  //Remove pause condition on memory writing thread
  pthread_mutex_lock(&fMWMutex);
  fMWAction=0;
  pthread_mutex_unlock(&fMWMutex);
  pthread_mutex_lock(&fMWCMutex);
  printstatus("QOversizeArray::ResetArray: Removing pausing condition from memory writing thread");
  pthread_cond_signal(&fMWCond);
  pthread_mutex_unlock(&fMWCMutex);
  //Unlock buffer structure
  pthread_mutex_unlock(&fBuffersMutex);
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
  FuncDef(ReadHeader,1);
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
  FuncDef(ReadBuffer,1);
  //printf("Read buffer %u at %u\n",buf->fBufferIdx,buf->fBufferIdx*fMaxBHBDataSize+fFirstDataByte);
  Int_t buffersize;

  pthread_mutex_lock(&fFileMutex);
  if(lseek(fFDesc,bufferidx*fMaxBHBDataSize+fFirstDataByte,SEEK_SET)==-1){
    perror("QOversizeArray::ReadBuffer: Error: ");
    throw 1;
  }
  if(read(fFDesc, &buffersize, fBufferHeaderSize)!=fBufferHeaderSize) {
    perror("QOversizeArray::ReadBuffer: Error: ");
    throw 1;
  }

  pthread_mutex_lock(&fPBuffersMutex);
  if(fFirstParkedBuffer) {
    *buf=fFirstParkedBuffer;
    fFirstParkedBuffer=fFirstParkedBuffer->fNextOAB;
    pthread_mutex_unlock(&fPBuffersMutex);

    if((*buf)->fBufferSize != buffersize) {
      free((*buf)->fBuffer);
      (*buf)->fBuffer=(char*)malloc(buffersize);
      pthread_mutex_lock(&fMSizeMutex);
      fTotalMemSize+=buffersize-(*buf)->fBufferSize;
      fArrayMemSize+=buffersize-(*buf)->fBufferSize;
      pthread_mutex_unlock(&fMSizeMutex);

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
    pthread_mutex_lock(&fMSizeMutex);
    fTotalMemSize+=buffersize+sizeof(QOABuffer);
    fArrayMemSize+=buffersize+sizeof(QOABuffer);
    pthread_mutex_unlock(&fMSizeMutex);
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
  FuncDef(ReadWriteBuffer,1);
  size_t numobjs=fNObjects%fNOPerBuffer;
  UInt_t bufferidx=fNObjects/fNOPerBuffer;
  Int_t buffersize;
  UInt_t allocsize;

  if(numobjs) {
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
      pthread_mutex_lock(&fMSizeMutex);
      fTotalMemSize+=allocsize+sizeof(QOABuffer);
      fArrayMemSize+=allocsize+sizeof(QOABuffer);
      pthread_mutex_unlock(&fMSizeMutex);
      CheckMemory();

    } else if(fWBNAllocObjs<numobjs) {
      free(fWriteBuffer->fBuffer);
      fWBNAllocObjs=numobjs;
      allocsize=fWBNAllocObjs*fObjectSize;
      fWriteBuffer->fBuffer=(char*)malloc(allocsize);
      pthread_mutex_lock(&fMSizeMutex);
      fTotalMemSize+=allocsize-fWriteBuffer->fBufferSize;
      fArrayMemSize+=allocsize-fWriteBuffer->fBufferSize;
      pthread_mutex_unlock(&fMSizeMutex);
      fWriteBuffer->fBufferSize=allocsize;
    }

    if(read(fFDesc, fWriteBuffer->fBuffer, buffersize)!=buffersize) {
      perror("QOversizeArray::ReadWriteBuffer: Error: ");
      throw 1;
    }
    pthread_mutex_unlock(&fFileMutex);

  } else {

    if(!fWriteBuffer) {
      fWBNAllocObjs=fNOAllocBlock;
      allocsize=fWBNAllocObjs*fObjectSize;
      fWriteBuffer=new QOABuffer(0, allocsize);
      pthread_mutex_lock(&fMSizeMutex);
      fTotalMemSize+=allocsize+sizeof(QOABuffer);
      fArrayMemSize+=allocsize+sizeof(QOABuffer);
      pthread_mutex_unlock(&fMSizeMutex);
      CheckMemory();
    } 
  }
  fWriteBuffer->fBufferIdx=bufferidx;
  pthread_mutex_lock(&fBuffersMutex);
  fWBFirstObjIdx=fNObjects-numobjs;
  pthread_mutex_unlock(&fBuffersMutex);
}

void QOversizeArray::Save()
{
  FuncDef(Save,1);
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

  pthread_mutex_lock(&fMWMutex);
  //Request a pause from memory writing thread
  fMWAction=1;
  pthread_mutex_unlock(&fMWMutex);
  pthread_mutex_lock(&fMWCMutex);
  pthread_cond_signal(&fMWCond);
  //Wait for pause confirmation
  pthread_cond_wait(&fMWPCond,&fMWCMutex);
  pthread_mutex_unlock(&fMWCMutex);
  printstatus("Memory writing thread confirmed to be in pausing condition");

  //Lock buffer structure. Lock fRBDIMutex must be applied after the lock on fBuffersMutex to ensure possible compression operations are over in QOAMMThread
  pthread_mutex_lock(&fBuffersMutex);
  pthread_mutex_lock(&fRBDIMutex);

  buf=fFirstReadBuffer;

  while(buf) {
    if(buf->fIsModified) {
      WriteBuffer(buf);
      buf->fIsModified=kFALSE;
    }
    buf=buf->fNextOAB;
  }

  WriteWriteBuffer();

  pthread_mutex_unlock(&fRBDIMutex);

  //Remove pause condition on memory writing thread
  pthread_mutex_lock(&fMWMutex);
  fMWAction=0;
  pthread_mutex_unlock(&fMWMutex);
  pthread_mutex_lock(&fMWCMutex);
  printstatus("QOversizeArray::Save: Removing pausing condition from memory writing thread");
  pthread_cond_signal(&fMWCond);
  pthread_mutex_unlock(&fMWCMutex);
  //Unlock buffer structure
  pthread_mutex_unlock(&fBuffersMutex);

  WriteHeader();
}

void QOversizeArray::SetMemConstraints(const Long64_t &critmemsize,const Long64_t &level1memsize,const Long64_t &level2memsize,const Long64_t &cthreshmemsize)
{
  FuncDef(SetMemConstraints,1);
  pthread_mutex_lock(&fMSizeMutex);
  fCritMemSize=critmemsize;
  fLevel1MemSize=level1memsize;
  fLevel2MemSize=level2memsize;
  fCThreshMemSize=cthreshmemsize>=0?cthreshmemsize:critmemsize;

  if(fCritMemSize) {
    if(!fLevel2MemSize) fLevel2MemSize=(UInt_t)(0.95*fCritMemSize);
    if(!fLevel1MemSize || fLevel1MemSize > fLevel2MemSize) fLevel1MemSize=(UInt_t)(0.95*fLevel2MemSize);
  }
  pthread_mutex_unlock(&fMSizeMutex);
}

void QOversizeArray::Terminate()
{
  //MW and BL threads should be terminated before calling this function
  FuncDef(Terminate,1);
  QOABuffer *buf, *nextbuf;
  printstatus("void QOversizeArray::Terminate()");

  //Lock buffer structure
  pthread_mutex_lock(&fBuffersMutex);
  pthread_mutex_lock(&fRBDIMutex);
  pthread_mutex_lock(&fMSizeMutex);

  //printf("FirstReadBuffer index: %i\tLastReadBuffer index: %i\n",fFirstReadBuffer?fFirstReadBuffer->fBufferIdx:-1,fLastReadBuffer?fLastReadBuffer->fBufferIdx:-1);

  if(fNUMBuffers) {
    free(fUMBuffers);
    fUMBuffers=NULL;
    fNUMBuffers=0;
  }

  buf=fFirstReadBuffer;

  while(buf) {
    nextbuf=buf->fNextOAB;
    fTotalMemSize-=buf->fBufferSize+sizeof(QOABuffer);
    fArrayMemSize-=buf->fBufferSize+sizeof(QOABuffer);
    delete buf;
    buf=nextbuf;
  }

  pthread_mutex_lock(&fPBuffersMutex);
  buf=fFirstParkedBuffer;

  while(buf) {
    nextbuf=buf->fNextOAB;
    fTotalMemSize-=buf->fBufferSize+sizeof(QOABuffer);
    fArrayMemSize-=buf->fBufferSize+sizeof(QOABuffer);
    delete buf;
    buf=nextbuf;
  }
  fFirstParkedBuffer=NULL;
  pthread_mutex_unlock(&fPBuffersMutex);

  if(fWriteBuffer) {
    fTotalMemSize-=fWriteBuffer->fBufferSize+sizeof(QOABuffer);
    fArrayMemSize-=fWriteBuffer->fBufferSize+sizeof(QOABuffer);
    delete fWriteBuffer;
    fWriteBuffer=NULL;
  }

  free(fUZQOAB); fUZQOAB=NULL;
  free(fUZBuffers); fUZBuffers=NULL;

  fFirstReadBuffer=NULL;
  fLastReadBuffer=NULL;
  fCurReadBuffer=NULL;
  fCurRBIdx=-1;
  if(fNReadBuffers) fPTNRBObjects=fWBFirstObjIdx;
  else fPTNRBObjects=-1;
  fNReadBuffers=0;
  fWBFirstObjIdx=0;
  fNObjects=0;

  pthread_mutex_unlock(&fMSizeMutex);
  pthread_mutex_unlock(&fRBDIMutex);
  pthread_mutex_unlock(&fBuffersMutex);
}

void QOversizeArray::WriteHeader() const
{
  FuncDef(WriteHeader,1);
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
}

void QOversizeArray::WriteBuffer(const QOABuffer *buf) const
{
  FuncDef(WriteBuffer,1);
  //printf("Write buffer %u at %u\n",buf->fBufferIdx,buf->fBufferIdx*fMaxBHBDataSize+fFirstDataByte);
  pthread_mutex_lock(&fFileMutex);
  if(lseek(fFDesc,buf->fBufferIdx*fMaxBHBDataSize+fFirstDataByte,SEEK_SET)==-1){
    perror("QOversizeArray::WriteBuffer: Error: ");
    throw 1;
  }
  if(write(fFDesc, &buf->fBufferSize, fBufferHeaderSize)!=fBufferHeaderSize || write(fFDesc, buf->fBuffer, buf->fBufferSize)!=buf->fBufferSize) {
    perror("QOversizeArray::WriteBuffer: Error: ");
    throw 1;
  }
  pthread_mutex_unlock(&fFileMutex);
}

void QOversizeArray::WriteWriteBuffer() const
{
  size_t numobjs=fNObjects%fNOPerBuffer;

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
  QOversizeArray *qoa=(QOversizeArray*)array;
  QOABuffer *buf;
  printstatus("Starting memory writing thread");

  for(;;) {
    pthread_mutex_lock(&qoa->fMWMutex);

    if(qoa->fMWAction) {

      if(qoa->fMWAction == 1) {
	//Pause
	pthread_mutex_lock(&qoa->fMWCMutex);
	pthread_cond_signal(&qoa->fMWPCond);
	pthread_mutex_unlock(&qoa->fMWCMutex);
	//printf("Memory writing thread %p is pausing\n",qoa);

	pthread_mutex_lock(&qoa->fMWCMutex);
	pthread_mutex_unlock(&qoa->fMWMutex);
	pthread_cond_wait(&qoa->fMWCond, &qoa->fMWCMutex);
	printstatus("Memory writing thread got a signal in pausing condition");
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
	printstatus("Memory writing thread is stoping");
	qoa->fMWAction=0;
	qoa->fMWBuffer=NULL;
	pthread_mutex_unlock(&qoa->fMWMutex);
	return NULL;
      }

    } else if(!qoa->fMWBuffer) {
      printstatus("Memory writing thread is waiting for a new buffer to write");
      pthread_mutex_unlock(&qoa->fMWMutex);
      pthread_mutex_lock(&qoa->fMWCMutex);
      pthread_cond_signal(&qoa->fMWCCond);
      pthread_cond_wait(&qoa->fMWCond, &qoa->fMWCMutex);
      pthread_mutex_unlock(&qoa->fMWCMutex);
    } else {
      printstatus("Looping in memory writing thread");

      //printf("Current buffer to be freed: %p\n",qoa->fMWBuffer);
      buf=qoa->fMWBuffer;
      qoa->fMWBuffer=NULL;
      qoa->fMWWBuffer=buf;
      //printf("Value of resetted buffer address: %p\n",qoa->fMWBuffer);
      pthread_mutex_unlock(&qoa->fMWMutex);

      pthread_mutex_lock(&qoa->fMWCMutex);
      pthread_cond_signal(&qoa->fMWCCond);
      pthread_mutex_unlock(&qoa->fMWCMutex);
      printstatus("Memory writing thread just sent a confirmation");

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
	  pthread_mutex_lock(&fMSizeMutex);
	  fTotalMemSize-=buf->fBufferSize+sizeof(QOABuffer);
	  qoa->fArrayMemSize-=buf->fBufferSize+sizeof(QOABuffer);
	  pthread_mutex_unlock(&fMSizeMutex);
	  delete buf;

	} else {
	  //printf("Memory writing thread is parking buffer %i\n",buf->fBufferIdx);
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
      pthread_mutex_unlock(&qoa->fBuffersMutex);
    }
    printstatus("Reached the bottom of the loop in memory writing thread");
  }
  printstatus("Exiting from memory writing thread");
  return NULL;
}

void* QOversizeArray::QOABLThread(void *array)
{
  FuncDef(QOABLThread,1);
  QOversizeArray *qoa=(QOversizeArray*)array;
  Int_t ibuf, ibuf2, ibuf3;
  Int_t lastbidx=-3;
  QOABuffer *buf, *buf2;
  printstatus("Starting buffer loading thread");

  for(;;) {
    pthread_mutex_lock(&qoa->fBLMutex);
    pthread_mutex_lock(&qoa->fBuffersMutex);

    if(qoa->fBLAction==2) {
      if(qoa->fCurBLRBIdx!=-2) {
	qoa->fCurBLRBIdx=-2;
	qoa->CleanUZBuffers();
      }
      pthread_mutex_unlock(&qoa->fBuffersMutex);

      //Stop
      printstatus("Buffer loading thread is stoping");
      qoa->fBLAction=0;
      pthread_mutex_unlock(&qoa->fBLMutex);
      return NULL;

    } else if(qoa->fCurRBIdx==-1) {
      if(qoa->fCurBLRBIdx!=-2) {
	qoa->fCurBLRBIdx=-2;
	qoa->CleanUZBuffers();
      }
      printstatus("Buffer loading thread is waiting for a new buffer to load");
      pthread_mutex_lock(&qoa->fBLCMutex);
      pthread_mutex_unlock(&qoa->fBuffersMutex);
      pthread_mutex_unlock(&qoa->fBLMutex);
      pthread_cond_signal(&qoa->fBLWCond);
      pthread_cond_wait(&qoa->fBLCond, &qoa->fBLCMutex);
      pthread_mutex_unlock(&qoa->fBLCMutex);

    } else if(qoa->fCurRBIdx==lastbidx) {
      printstatus("Buffer loading thread is waiting for a new buffer to load");
      pthread_mutex_lock(&qoa->fBLCMutex);
      pthread_mutex_unlock(&qoa->fBuffersMutex);
      pthread_mutex_unlock(&qoa->fBLMutex);
      pthread_cond_signal(&qoa->fBLWCond);
      pthread_cond_wait(&qoa->fBLCond, &qoa->fBLCMutex);
      pthread_mutex_unlock(&qoa->fBLCMutex);

    } else {
      pthread_mutex_unlock(&qoa->fBLMutex);
      printstatus("Looping in buffer loading thread");

      //If switching from write mode
      if(!qoa->fCurReadBuffer) {
	pthread_mutex_lock(&qoa->fPBuffersMutex);

	//If there are some parked buffers
	if(qoa->fFirstParkedBuffer) {
	  //Delete all parked buffers
	  buf=qoa->fFirstParkedBuffer;
	  pthread_mutex_lock(&fMSizeMutex);

	  while(buf) {
	    buf2=buf->fNextOAB;
	    fTotalMemSize-=buf->fBufferSize+sizeof(QOABuffer);
	    qoa->fArrayMemSize-=buf->fBufferSize+sizeof(QOABuffer);
	    delete buf;
	    buf=buf2;
	  }
	  pthread_mutex_unlock(&fMSizeMutex);
	  qoa->fFirstParkedBuffer=NULL;

	}
	pthread_mutex_unlock(&qoa->fPBuffersMutex);
	buf=NULL;

      } else {
	buf=qoa->fCurReadBuffer;
	while(buf->fNextOAB && buf->fNextOAB->fBufferIdx<=qoa->fCurRBIdx) buf=buf->fNextOAB;
      }

      //Assertion: buf (and fCurReadBuffer) contains a pointer to the loaded buffer located the closest by the left to fCurRBIdx (including fCurRBIdx), or if not possible buf=fFirstReadBuffer

      //Trying to loop over the buffer that currently needs to be loaded and the pre-cached buffers
      for(ibuf=qoa->fCurRBIdx; qoa->fCurRBIdx!=-1 && ibuf>=qoa->fCurRBIdx && ibuf<=qoa->fCurRBIdx+qoa->fNPCBuffers && ibuf*qoa->fNOPerBuffer<qoa->fWBFirstObjIdx; ++ibuf) {
	//Important to set fCurBLRBIdx only here to make sure it reflects the fact that the condition of the current loop is respected
	qoa->fCurBLRBIdx=qoa->fCurRBIdx;
	//printf("buf idx: %i\treq idx: %i\n",buf?buf->fBufferIdx:-1,ibuf);

	//If the required buffer is not loaded
	if(!buf || buf->fBufferIdx!=ibuf) {
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
		//printf("Storing unzipped buffer %i (%p) in fUZQOAB[%i]\n",buf2->fBufferIdx,buf2,ibuf2);
		qoa->fUZQOAB[ibuf2]=buf2;
		break;
	      }
	    }

	    pthread_mutex_lock(&fMSizeMutex);
	    fTotalMemSize+=qoa->fMaxBDataSize;
	    qoa->fArrayMemSize+=qoa->fMaxBDataSize;
	    pthread_mutex_unlock(&fMSizeMutex);
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

	  //Add the buffer to the structure
	  qoa->fUMBuffers=(QOABuffer**)realloc(qoa->fUMBuffers,++(qoa->fNUMBuffers)*sizeof(QOABuffer*));
	  qoa->fUMBuffers[qoa->fNUMBuffers-1]=buf2;

	  //If fCurReadBuffer has changed while the first buffer in the range was being loaded, scan for the previous buffer from fFirstReadBuffer (in case buf was deleted while fBuffersMutex was unlocked)
	  if(ibuf==qoa->fCurBLRBIdx &&  buf!=qoa->fCurReadBuffer) {
	    buf=qoa->fFirstReadBuffer;
	    while(buf->fNextOAB && buf->fNextOAB->fBufferIdx<=ibuf) buf=buf->fNextOAB;
	  }

	  //If buf==NULL (first buffer in read buffer structure)
	  if(!buf) {
	    ASSERT(!qoa->fFirstReadBuffer);
	    ASSERT(!qoa->fLastReadBuffer);
	    buf2->fPreviousOAB=NULL;
	    buf2->fNextOAB=NULL;
	    qoa->fFirstReadBuffer=buf2;
	    qoa->fLastReadBuffer=buf2;
	    buf=buf2;

	    // else if buf has a smaller buffer index (this can be the case when buf==fCurReadBuffer!=NULL)
	  } else {
	    //printf("Buffer index of buf: %i\n",buf->fBufferIdx);
	    if(buf->fNextOAB) {
	      //printf("buf idx: %i\tbuf->fNextOAB idx: %i\n",buf->fBufferIdx,buf->fNextOAB->fBufferIdx);
	      ASSERT(buf->fNextOAB->fBufferIdx>buf->fBufferIdx);
	    }
	    if(buf->fPreviousOAB) {
	      //printf("buf idx: %i\tbuf->fPreviousOAB idx: %i\n",buf->fBufferIdx,buf->fPreviousOAB->fBufferIdx);
	      ASSERT(buf->fPreviousOAB->fBufferIdx<buf->fBufferIdx);
	    }
	    buf2->fNextOAB=buf->fNextOAB;
	    if(buf->fNextOAB) buf->fNextOAB->fPreviousOAB=buf2;
	    buf2->fPreviousOAB=buf;
	    buf->fNextOAB=buf2;
	    if(buf==qoa->fLastReadBuffer) qoa->fLastReadBuffer=buf2;
	    buf=buf2;
	  }
	  ++(qoa->fNReadBuffers);

	  if(buf->fBufferIdx==qoa->fCurRBIdx) {
	    pthread_mutex_lock(&qoa->fBLCMutex);
	    pthread_cond_signal(&qoa->fBLCCond);
	    pthread_mutex_unlock(&qoa->fBLCMutex);
	    printstatus("Buffer loading thread just sent a confirmation");
	  }

	  //Else if the required buffer is loaded, but it is compressed or being compressed
	} else if(buf->fIsCompressed>0 && buf->fIsCompressed<3) {

	  //If the buffer is currently being compressed, wait using a lock on fRBDIMutex
	  if(buf->fIsCompressed==2) {
	    //pthread_mutex_unlock(&qoa->fBuffersMutex);
	    pthread_mutex_lock(&qoa->fRBDIMutex); pthread_mutex_unlock(&qoa->fRBDIMutex);
	    //pthread_mutex_lock(&qoa->fBuffersMutex);

	    //Else if the buffer is compressed
	  }

	  buf->fIsCompressed=3;
	  pthread_mutex_unlock(&qoa->fBuffersMutex);
	  //printf("Unzipping buffer %i\n",buf->fBufferIdx);
	  pthread_mutex_lock(&qoa->fUZBMutex);

	  //Find a free spot in fUZQOAB
	  for(ibuf2=0;;++ibuf2) {
	    if(!qoa->fUZQOAB[ibuf2]) {
	      //printf("Storing unzipped buffer %i (%p) in fUZQOAB[%i]\n",buf->fBufferIdx,buf,ibuf2);
	      qoa->fUZQOAB[ibuf2]=buf;
	      break;
	    }
	  }

	  pthread_mutex_lock(&fMSizeMutex);
	  fTotalMemSize+=qoa->fMaxBDataSize;
	  qoa->fArrayMemSize+=qoa->fMaxBDataSize;
	  pthread_mutex_unlock(&fMSizeMutex);
	  //Allocate space for the unzipped buffer
	  qoa->fUZBuffers[ibuf2]=(Char_t*)malloc(qoa->fMaxBDataSize);
	  //printf("Allocating space for fUZBuffers[%i] at position %p\n",ibuf2,qoa->fUZBuffers[ibuf2]);
	  pthread_mutex_unlock(&qoa->fUZBMutex);
	  CheckMemory();
	  //Unzip the buffer
	  R__unzip(&buf->fBufferSize, (UChar_t*)buf->fBuffer, &qoa->fMaxBDataSize, qoa->fUZBuffers[ibuf2], &ibuf2);

	  if(!ibuf2) {
	    fprintf(stderr,"QOversizeArray::QOABLThread: Error: A buffer cannot be unzipped\n");
	    throw 1;
	  }

	  pthread_mutex_lock(&qoa->fBuffersMutex);
	  buf->fIsCompressed=4;

	  if(buf->fBufferIdx==qoa->fCurRBIdx) {
	    pthread_mutex_lock(&qoa->fBLCMutex);
	    pthread_cond_signal(&qoa->fBLCCond);
	    pthread_mutex_unlock(&qoa->fBLCMutex);
	    printstatus("Buffer loading thread just sent a confirmation");
	  }
	}
	if(buf->fNextOAB && buf->fNextOAB->fBufferIdx<=ibuf+1) buf=buf->fNextOAB;

	//Clear old unzipped buffers that are no longer necessary
	if(qoa->fCurBLRBIdx!=lastbidx){
	  qoa->CleanUZBuffers();
	  lastbidx=qoa->fCurBLRBIdx;
	}
      }
      pthread_mutex_unlock(&qoa->fBuffersMutex);
    }
    printstatus("Reached the bottom of the loop in buffer loading thread");
  }
  printstatus("Exiting from buffer loading thread");
  return NULL;
}

void* QOversizeArray::QOAMMThread(void *)
{
  FuncDef(QOAMMThread,1);
  Int_t i,j;
  Float_t fbuf, fbuf2;
  TRandom rnd;
  QOversizeArray *abuf;
  QOABuffer *bbuf, *bbuf2;
  Int_t ibuf;
  Long64_t libuf,libuf2;
  char* tmpbuf;
  printstatus("Starting memory management thread");

  for(;;) {
    pthread_mutex_lock(&fILMutex);

    //Exit thread if there is no instance left
    if(!fInstances.Count()) {
      pthread_mutex_unlock(&fILMutex);
      break;

    } else {
      pthread_mutex_unlock(&fILMutex);
    }

    //Wait for memory management condition
    pthread_mutex_lock(&fMMMutex);
    pthread_cond_wait(&fMMCond, &fMMMutex);
    pthread_mutex_unlock(&fMMMutex);

    pthread_mutex_lock(&fMSizeMutex);

    //While total memory size exceeds fLevel1MemSize
    while(fLevel1MemSize && fTotalMemSize > fLevel1MemSize) {
      //printf("Looping in memory management thread...\n");
      //printf("TotalMemSize is %lli\n",fTotalMemSize);
      pthread_mutex_unlock(&fMSizeMutex);

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
	printstatus("Deleting a parked buffer");
	bbuf=abuf->fFirstParkedBuffer;
	abuf->fFirstParkedBuffer=abuf->fFirstParkedBuffer->fNextOAB;
	pthread_mutex_unlock(&abuf->fPBuffersMutex);
	pthread_mutex_lock(&fMSizeMutex);
	fTotalMemSize-=bbuf->fBufferSize+sizeof(QOABuffer);
	abuf->fArrayMemSize-=bbuf->fBufferSize+sizeof(QOABuffer);
	pthread_mutex_unlock(&fMSizeMutex);
	delete bbuf;

	//Else if we need to delete a read buffer		
      } else {
	pthread_mutex_unlock(&abuf->fPBuffersMutex);
	bbuf=NULL;

	pthread_mutex_lock(&abuf->fBuffersMutex);
	//printf("Number of unmodified buffers: %i\tNumber of read buffers: %i\n",abuf->fNUMBuffers,abuf->fNReadBuffers);
	//If the array is in write mode
	if(abuf->fCurRBIdx==-1 || abuf->fCurBLRBIdx==-2) {
	  printstatus("Trying to select a buffer to delete in write mode");

	  //If there are some unmodified buffers
	  if(abuf->fNUMBuffers) {
	    printstatus("There are unmodified buffers");
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
	    printstatus("Will delete a modified buffer");

	    //Get an estimate for the expected number of entries in the array (fPTNRBObjects if previously computed and not exceeeded or
	    //otherwise the current number of entries in read buffers (=fWBFirstObjIdx))
	    libuf=abuf->fPTNRBObjects<abuf->fWBFirstObjIdx?abuf->fWBFirstObjIdx:abuf->fPTNRBObjects;
	    bbuf2=abuf->fFirstReadBuffer;
	    pthread_mutex_lock(&fMSizeMutex);
	    libuf2=(Long64_t)((abuf->fNReadBuffers-abuf->fNReadBuffers*((Double_t)fTotalMemSize-fLevel1MemSize)*fbuf/(fbuf2*abuf->fArrayMemSize))*abuf->fNOPerBuffer);
	    pthread_mutex_unlock(&fMSizeMutex);
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
	  printstatus("Trying to select a buffer to delete in read mode");

	  //If there are some unmodified buffers
	  if(abuf->fNUMBuffers) {
	    printstatus("There are unmodified buffers");
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
	    printstatus("Will delete a modified buffer");

	    //Get an estimate for the expected number of entries in the array (fPTNRBObjects if previously computed and not exceeeded or
	    //otherwise the current number of entries in read buffers (=fWBFirstObjIdx))
	    libuf=(abuf->fPTNRBObjects<abuf->fWBFirstObjIdx?abuf->fWBFirstObjIdx:abuf->fPTNRBObjects);
	    bbuf2=abuf->fFirstReadBuffer;
	    pthread_mutex_lock(&fMSizeMutex);
	    libuf2=(Long64_t)((abuf->fNReadBuffers-abuf->fNReadBuffers*((Double_t)fTotalMemSize-fLevel1MemSize)*fbuf/(fbuf2*abuf->fArrayMemSize))*abuf->fNOPerBuffer);
	    pthread_mutex_unlock(&fMSizeMutex);
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
	      pthread_mutex_lock(&fMSizeMutex);
	      fTotalMemSize-=bbuf->fBufferSize+sizeof(QOABuffer);
	      abuf->fArrayMemSize-=bbuf->fBufferSize+sizeof(QOABuffer);
	      pthread_mutex_unlock(&fMSizeMutex);
	      delete bbuf;

	    } else {
	      //printf("Memory management thread is parking buffer %i\n",bbuf->fBufferIdx);
	      abuf->fFirstParkedBuffer=bbuf;
	      bbuf->fPreviousOAB=NULL;
	      bbuf->fNextOAB=NULL;
	      pthread_mutex_unlock(&abuf->fPBuffersMutex);
	    }
	    pthread_mutex_unlock(&abuf->fBuffersMutex);

	    //Else if it was modified
	  } else {
	    pthread_mutex_lock(&fMSizeMutex);

	    //If the buffer is not compressed and the total memory size is >= compression memory size threshold, compress the buffer
	    if(!bbuf->fIsCompressed && fTotalMemSize>=fCThreshMemSize) {
	      pthread_mutex_unlock(&fMSizeMutex);
	      // ***** Important to lock fRBDIMutex before unlocking fBuffersMutex for functions that pause memory writing in other threads
	      pthread_mutex_lock(&abuf->fRBDIMutex);
	      bbuf->fIsCompressed=2;
	      pthread_mutex_unlock(&abuf->fBuffersMutex);
	      tmpbuf=(char*)malloc(abuf->fMaxBDataSize);
	      R__zip(1,&abuf->fMaxBDataSize,bbuf->fBuffer,&abuf->fMaxBDataSize,tmpbuf,&ibuf);

	      //If compressed data is more compact than uncompressed data
	      if(ibuf && ibuf<abuf->fMaxBDataSize) {
		//printf("Compression %i/%i\n",ibuf,bbuf->fBufferSize);
		free(bbuf->fBuffer);
		tmpbuf=(char*)realloc(tmpbuf,ibuf);
		bbuf->fBuffer=tmpbuf;
		pthread_mutex_lock(&fMSizeMutex);
		fTotalMemSize-=bbuf->fBufferSize-ibuf;
		abuf->fArrayMemSize-=bbuf->fBufferSize-ibuf;
		pthread_mutex_unlock(&fMSizeMutex);
		bbuf->fBufferSize=ibuf;
	      }
	      //Need to unlock RBDIMutex first to avoid deadlock with Save and Terminate functions. Should not cause a problem with reading thread
	      pthread_mutex_unlock(&abuf->fRBDIMutex);
	      pthread_mutex_lock(&abuf->fBuffersMutex);
	      //The following if statement is required since the lock on fRBDIMutex has to be released before locking fBuffersMutex. The buffer can get deleted by the Terminate function or uncompressed by the buffer loading thread
	      if(abuf->fNReadBuffers && bbuf->fIsCompressed==2) bbuf->fIsCompressed=1;
	      pthread_mutex_unlock(&abuf->fBuffersMutex);

	      //Else if the buffer is already compressed or total memory size < compression memory size threshold, write ask the memory writing thread to write it on disk and delete it	
	    } else {
	      pthread_mutex_unlock(&fMSizeMutex);
	      pthread_mutex_lock(&abuf->fMWMutex);
	      pthread_mutex_unlock(&abuf->fBuffersMutex);
	      printstatus("Memory management thread is asking to write a buffer");
	      //printf("Buffer address is %p\n",bbuf);
	      abuf->fMWBuffer=bbuf;
	      pthread_mutex_lock(&abuf->fMWCMutex);
	      pthread_cond_signal(&abuf->fMWCond);
	      pthread_mutex_unlock(&abuf->fMWMutex);
	      printstatus("Memory management thread is waiting for a confirmation");
	      pthread_cond_wait(&abuf->fMWCCond, &abuf->fMWCMutex);
	      pthread_mutex_unlock(&abuf->fMWCMutex);
	      printstatus("Memory management thread received a confirmation");
	    }
	  }

	} else {
	  pthread_mutex_unlock(&abuf->fBuffersMutex);
	}
      }

      pthread_mutex_unlock(&fILMutex);

      //printf("CritMemSize: %lli\tTotalMemSize: %lli\n",fCritMemSize,fTotalMemSize);
      pthread_mutex_lock(&fMSizeMutex);
      pthread_mutex_lock(&fCMSCMutex);
      if(fCLReached && (!fCritMemSize || fTotalMemSize <= fCritMemSize)) {
	printstatus("***** Memory size is no longer critical");
	fCLReached=kFALSE;
	printstatus("Sending signal back to CheckMemory");
	pthread_cond_signal(&fCMSCond);
	pthread_mutex_unlock(&fCMSCMutex);
      } else {
	pthread_mutex_unlock(&fCMSCMutex);
      }
    }
    pthread_mutex_unlock(&fMSizeMutex);
  }

  printstatus("Exiting from memory management thread");
  return NULL;
}
