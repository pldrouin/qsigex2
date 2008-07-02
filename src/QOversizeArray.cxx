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

//#define FuncDef(a) const char* thefunc=#a; TString indent;
//#define pthread_mutex_lock(a) {printf("\n%slocking %s in function %s...\n",indent.Data(),#a,thefunc); pthread_mutex_lock(a); printf("%s%s is now locked in function %s\n",indent.Data(),#a,thefunc); indent+="     ";}
//#define pthread_mutex_unlock(a) {indent=indent(0,indent.Length()-5); printf("\n%sunlocking %s in function %s...\n",indent.Data(),#a,thefunc); pthread_mutex_unlock(a); printf("%s%s is now unlocked in function %s\n",indent.Data(),#a,thefunc);}
#define FuncDef(a)

/*void printstatus(const char* status)
{
    printf("%s\n",status);
}*/

void printstatus(const char*){}

QOversizeArray::QOversizeArray(const char *filename, const char *arrayname, omode openmode, const UInt_t &objectsize, const UInt_t &nobjectsperbuffer): fFilename(filename), fArrayName(arrayname), fFDesc(0), fFirstDataByte(sizeof(UInt_t)+256+sizeof(fObjectSize)+sizeof(fNOPerBuffer)+sizeof(fNObjects)), fBufferHeaderSize(sizeof(UInt_t)), fOpenMode(openmode), fObjectSize(objectsize), fBuffer(NULL), fNOPerBuffer(nobjectsperbuffer), fMaxBDataSize(objectsize*nobjectsperbuffer), fMaxBHBDataSize(fBufferHeaderSize+fMaxBDataSize), fNObjects(0), fCurReadBuffer(NULL), fFirstReadBuffer(NULL), fLastReadBuffer(NULL), fWriteBuffer(NULL), fWBFirstObjIdx(0), fNReadBuffers(0), fUMBuffers(NULL), fNUMBuffers(0), fFirstParkedBuffer(NULL), fNPCBuffers(3), fArrayIO(0), fAPriority(0), fFileMutex(PTHREAD_MUTEX_INITIALIZER), fBuffersMutex(PTHREAD_MUTEX_INITIALIZER), fRBDIMutex(PTHREAD_MUTEX_INITIALIZER), fMWThread(), fMWMutex(PTHREAD_MUTEX_INITIALIZER), fMWCond(PTHREAD_COND_INITIALIZER), fMWCMutex(PTHREAD_MUTEX_INITIALIZER), fMWPCond(PTHREAD_COND_INITIALIZER), fMWCCond(PTHREAD_COND_INITIALIZER), fMWAction(kFALSE), fMWBuffer(NULL), fMWWBuffer(NULL)
{
    pthread_mutex_lock(&fMSizeMutex);
    printf("%p\tTotal memory at array creation: %lli\n",this,fTotalMemSize);
    pthread_mutex_unlock(&fMSizeMutex);
    OpenFile();
}

QOversizeArray::~QOversizeArray()
{
    printstatus("QOversizeArray::~QOversizeArray()");
    CloseFile();
    pthread_mutex_lock(&fMSizeMutex);
    printf("%p\tTotal memory at array destruction: %lli\n",this,fTotalMemSize);
    pthread_mutex_unlock(&fMSizeMutex);
}

void QOversizeArray::CloseFile()
{
    FuncDef(CloseFile);
    printstatus("void QOversizeArray::CloseFile()");
    if(fFDesc) {
	if(fOpenMode != kRead)  Save();

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
	printstatus("Waiting for MW thread to terminate...");
	pthread_join(fMWThread,NULL);
	printstatus("MW thread has terminated");

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
    FuncDef(CheckMemory);
    pthread_mutex_lock(&fMMMutex);
    pthread_cond_signal(&fMMCond);
    pthread_mutex_unlock(&fMMMutex);
    pthread_mutex_lock(&fMSizeMutex);
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

void QOversizeArray::Init()
{
    FuncDef(Init);
    ReadWriteBuffer();
}

void QOversizeArray::Fill()
{
    FuncDef(Fill);
    //Copy the content of the object buffer in fWriteBuffer
    memcpy(fWriteBuffer->fBuffer+(fNObjects-fWBFirstObjIdx)*fObjectSize,fBuffer,fObjectSize);
    fNObjects++;

    //If fWriteBuffer is full
    if(fNObjects-fWBFirstObjIdx == fNOPerBuffer) {
	if(fOpenMode == kRead) {
	    fprintf(stderr,"QOversizeArray::Fill(): Error: File '%s' is opened in read-only mode\n",fFilename.Data());
	    throw 1;
	}

	static UInt_t nextbufidx;
	nextbufidx=fWriteBuffer->fBufferIdx+1;

	pthread_mutex_lock(&fBuffersMutex);
	if(fLastReadBuffer) {
	    fLastReadBuffer->fNextOAB=fWriteBuffer;
	}
	fWriteBuffer->fPreviousOAB=fLastReadBuffer;
	fWriteBuffer->fNextOAB=NULL;
	fWriteBuffer->fIsModified=kTRUE;
	fLastReadBuffer=fWriteBuffer;
	if(!fFirstReadBuffer) fFirstReadBuffer=fWriteBuffer;
	fNReadBuffers++;
	pthread_mutex_unlock(&fBuffersMutex);
	fArrayIO+=fMaxBDataSize;
	pthread_mutex_lock(&fPriorityMutex);
	fAPriority=1./fArrayIO;
	pthread_mutex_unlock(&fPriorityMutex);

	if(fFirstParkedBuffer) {
	    fWriteBuffer=fFirstParkedBuffer;
	    fWriteBuffer->fBufferIdx=nextbufidx;
	    fWriteBuffer->fIsCompressed=0;
	    fFirstParkedBuffer=fFirstParkedBuffer->fNextOAB;

	} else {
	    fWriteBuffer=new QOABuffer(nextbufidx, fMaxBDataSize);
	    pthread_mutex_lock(&fMSizeMutex);
	    fTotalMemSize+=fMaxBDataSize+sizeof(QOABuffer);
	    pthread_mutex_unlock(&fMSizeMutex);
	    CheckMemory();
	}
	fWBFirstObjIdx=fNObjects;
    }
}

void QOversizeArray::OpenFile()
{
    FuncDef(OpenFile);
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

void QOversizeArray::ResetPriorities()
{
    FuncDef(ResetPriorities);
    pthread_mutex_lock(&fPriorityMutex);
    pthread_mutex_lock(&fILMutex);

    for(Int_t i=0; i<fInstances.Count(); i++) {
	fInstances.GetArray()[i]->fArrayIO=0;
	fInstances.GetArray()[i]->fAPriority=0;
    }
    pthread_mutex_unlock(&fILMutex);
    pthread_mutex_unlock(&fPriorityMutex);
}

void QOversizeArray::ReadHeader()
{
    FuncDef(ReadHeader);
    printstatus("void QOversizeArray::ReadHeader()");
    UInt_t uibuf;
    char *strbuf;

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
	fprintf(stderr,"QOveriszeArray::ReadHeader: Error: Number of objects per buffer saved in file '%s' does not match provided value\n",fFilename.Data());
	throw 1;
    }
    fNOPerBuffer=uibuf;
    fMaxBDataSize=fObjectSize*fNOPerBuffer;
    fMaxBHBDataSize=fBufferHeaderSize+fMaxBDataSize;

    if(read(fFDesc, &fNObjects, sizeof(fNObjects))!=sizeof(fNObjects)) {
	perror("QOversizeArray::ReadHeader: Error: ");
	throw 1;
    }
    pthread_mutex_unlock(&fFileMutex);
}

void QOversizeArray::ReadBuffer(QOABuffer **buf, const UInt_t &bufferidx)
{
    FuncDef(ReadBuffer);
    //printf("Write buffer %u at %u\n",buf->fBufferIdx,buf->fBufferIdx*fMaxBHBDataSize+fFirstDataByte);
    static Int_t buffersize;

    pthread_mutex_lock(&fFileMutex);
    if(lseek(fFDesc,bufferidx*fMaxBHBDataSize+fFirstDataByte,SEEK_SET)==-1){
	perror("QOversizeArray::ReadBuffer: Error: ");
	throw 1;
    }
    if(read(fFDesc, &buffersize, fBufferHeaderSize)!=fBufferHeaderSize) {
	perror("QOversizeArray::ReadBuffer: Error: ");
	throw 1;
    }

    if(fFirstParkedBuffer) {
	*buf=fFirstParkedBuffer;
	fFirstParkedBuffer=fFirstParkedBuffer->fNextOAB;

	if((*buf)->fBufferSize != buffersize) {
	    (*buf)->fBuffer=(char*)realloc((*buf)->fBuffer,buffersize);
	    pthread_mutex_lock(&fMSizeMutex);
	    fTotalMemSize+=buffersize-(*buf)->fBufferSize;
	    pthread_mutex_unlock(&fMSizeMutex);

	    if((*buf)->fBufferSize < buffersize) {
		(*buf)->fBufferSize=buffersize;
		CheckMemory();

	    } else {
		(*buf)->fBufferSize=buffersize;
	    }	       
	}

	if(buffersize < fMaxBDataSize) (*buf)->fIsCompressed=1;
	else (*buf)->fIsCompressed=0;

    } else {
	*buf=new QOABuffer(0,buffersize);
	if(buffersize < fMaxBDataSize) (*buf)->fIsCompressed=1;
	pthread_mutex_lock(&fMSizeMutex);
	fTotalMemSize+=buffersize+sizeof(QOABuffer);
	pthread_mutex_unlock(&fMSizeMutex);
	CheckMemory();
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
    FuncDef(ReadWriteBuffer);
    size_t numobjs=fNObjects%fNOPerBuffer;
    UInt_t bufferidx=fNObjects/fNOPerBuffer;
    Int_t buffersize;

    if(!fWriteBuffer) {
	fWriteBuffer=new QOABuffer(0, fMaxBDataSize);
	pthread_mutex_lock(&fMSizeMutex);
	fTotalMemSize+=fMaxBDataSize+sizeof(QOABuffer);
	pthread_mutex_unlock(&fMSizeMutex);
	CheckMemory();
    } 
    fWriteBuffer->fBufferIdx=bufferidx;

    if(numobjs) {
	pthread_mutex_lock(&fFileMutex);
	if(lseek(fFDesc,bufferidx*fMaxBHBDataSize+fFirstDataByte,SEEK_SET)==-1){
	    perror("QOversizeArray::ReadBuffer: Error: ");
	    throw 1;
	}
	if(read(fFDesc, &buffersize, fBufferHeaderSize)!=fBufferHeaderSize) {
	    perror("QOversizeArray::ReadBuffer: Error: ");
	    throw 1;
	}

	if(read(fFDesc, fWriteBuffer->fBuffer, buffersize)!=buffersize) {
	    perror("QOversizeArray::ReadBuffer: Error: ");
	    throw 1;
	}
	pthread_mutex_unlock(&fFileMutex);
    }
    fWBFirstObjIdx=fNObjects-numobjs;
}

void QOversizeArray::Save()
{
    FuncDef(Save);
    printstatus("void QOversizeArray::Save()");
    if(!fFDesc) {
	fprintf(stderr,"QOversizeArray::Save: Error: There is no opened file\n");
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

    //Lock buffer structure
    pthread_mutex_lock(&fBuffersMutex);
    pthread_mutex_lock(&fRBDIMutex);

    buf=fFirstReadBuffer;

    while(buf) {
	if(buf->fIsModified) {
	    //Write(buf->fBuffer, fMaxBDataSize, 1, buf->fBufferIdx*fMaxBDataSize+fFirstDataByte);
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
    FuncDef(SetMemConstraints);
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
    FuncDef(Terminate);
    QOABuffer *buf, *nextbuf;
    printstatus("void QOversizeArray::Terminate()");

    if(fNUMBuffers) free(fUMBuffers);

    //Lock buffer structure
    pthread_mutex_lock(&fBuffersMutex);
    pthread_mutex_lock(&fRBDIMutex);
    pthread_mutex_lock(&fMSizeMutex);

    buf=fFirstReadBuffer;

    while(buf) {
	nextbuf=buf->fNextOAB;
        fTotalMemSize-=buf->fBufferSize+sizeof(QOABuffer);
	delete buf;
	buf=nextbuf;
    }

    fFirstReadBuffer=NULL;
    fLastReadBuffer=NULL;
    fCurReadBuffer=NULL;
    fNObjects=0;

    if(fWriteBuffer) {
	fTotalMemSize-=fWriteBuffer->fBufferSize+sizeof(QOABuffer);
	delete fWriteBuffer;
	fWriteBuffer=NULL;
    }

    pthread_mutex_unlock(&fMSizeMutex);
    pthread_mutex_unlock(&fRBDIMutex);
    pthread_mutex_unlock(&fBuffersMutex);
}

void QOversizeArray::WriteHeader() const
{
    FuncDef(WriteHeader);
    printstatus("QOversizeArray::WriteHeader()");
    UInt_t uibuf=fArrayName.Length();
    uibuf=(uibuf>256 ? 256 : uibuf);

    pthread_mutex_lock(&fFileMutex);
    if(lseek(fFDesc,0,SEEK_SET)==-1) {
	perror("QOversizeArray::WriteHeader: Error: ");
	throw 1;
    }
    if((UInt_t)write(fFDesc, &uibuf, sizeof(uibuf))!=sizeof(uibuf) || (UInt_t)write(fFDesc, fArrayName.Data(), uibuf)!=uibuf) {
	perror("QOversizeArray::WriteHeader: Error: ");
	throw 1;
    }
    if(lseek(fFDesc,sizeof(uibuf)+256,SEEK_SET)==-1) {
	perror("QOversizeArray::WriteHeader: Error: ");
	throw 1;
    }
    if(write(fFDesc, &fObjectSize, sizeof(fObjectSize))!=sizeof(fObjectSize) || write(fFDesc, &fNOPerBuffer, sizeof(fNOPerBuffer))!=sizeof(fNOPerBuffer) || write(fFDesc, &fNObjects, sizeof(fNObjects))!=sizeof(fNObjects)) {
	perror("QOversizeArray::WriteHeader: Error: ");
	throw 1;
    }
    pthread_mutex_unlock(&fFileMutex);
}

void QOversizeArray::WriteBuffer(const QOABuffer *buf) const
{
    FuncDef(WriteBuffer);
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
	fWriteBuffer->fBufferSize=numobjs;
	WriteBuffer(fWriteBuffer);
	fWriteBuffer->fBufferSize=fMaxBDataSize;
    }
}

void* QOversizeArray::QOAMWThread(void *array)
{
    FuncDef(QOAMWThread);
    QOversizeArray *qoa=(QOversizeArray*)array;
    QOABuffer *buf, *buf2;
    Bool_t safedelete;
    UInt_t ibuf;
    printstatus("Starting memory writing thread");

    for(;;) {
	pthread_mutex_lock(&qoa->fMWMutex);

	if(qoa->fMWAction) {

	    if(qoa->fMWAction == 1) {
		//Pause
		pthread_mutex_unlock(&qoa->fMWMutex);
	        pthread_mutex_lock(&qoa->fMWCMutex);
		pthread_cond_signal(&qoa->fMWPCond);
	        pthread_mutex_unlock(&qoa->fMWCMutex);
		printstatus("Memory writing thread is pausing");
	        pthread_mutex_lock(&qoa->fMWCMutex);
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

	    //qoa->Write(buf->fBuffer, qoa->fMaxBDataSize, 1, buf->fBufferIdx*qoa->fMaxBDataSize+qoa->fFirstDataByte);
	    qoa->WriteBuffer(buf);

	    //Remove it from the linked list first
	    pthread_mutex_lock(&qoa->fBuffersMutex);
	    buf->fIsModified=kFALSE;
	    //To put this block of code after the call to write buffer ensures that buffers removed from the linked list have been fully written on disk (important for reading thread).
	    safedelete=kTRUE;
	    if(qoa->fCurReadBuffer) {
		buf2=qoa->fCurReadBuffer;
		if(buf2==buf) safedelete=kFALSE;

		else {
		    ibuf=0;

		    while(ibuf<qoa->fNPCBuffers && buf2) {
			buf2=buf2->fNextOAB;

			if(buf2==buf) {
			    safedelete=kFALSE;
			    break;
			}
			ibuf++;
		    }
		}
	    }

	    if(safedelete) {
		if(buf->fPreviousOAB) buf->fPreviousOAB->fNextOAB=buf->fNextOAB;
		if(buf->fNextOAB) buf->fNextOAB->fPreviousOAB=buf->fPreviousOAB;
		if(qoa->fFirstReadBuffer == buf) qoa->fFirstReadBuffer=buf->fNextOAB;
		if(qoa->fLastReadBuffer == buf) qoa->fLastReadBuffer=buf->fPreviousOAB;
		qoa->fNReadBuffers--;

		printstatus("Memory writing thread is deleting a buffer");
		pthread_mutex_lock(&QOversizeArray::fMSizeMutex);
		QOversizeArray::fTotalMemSize-=buf->fBufferSize+sizeof(QOABuffer);
		pthread_mutex_unlock(&QOversizeArray::fMSizeMutex);
		delete buf;
	    }
	    qoa->fMWWBuffer=NULL;
	    pthread_mutex_unlock(&qoa->fBuffersMutex);
	}
	printstatus("Reached the bottom of the loop in memory writing thread");
    }
    printstatus("Exiting from memory writing thread");
    return NULL;
}

void* QOversizeArray::QOAReadThread(void *array)
{
    FuncDef(QOAReadThread);
    QOversizeArray *qoa=(QOversizeArray*)array;
    return NULL;
}

void* QOversizeArray::QOAMMThread(void *)
{
    FuncDef(QOAMMThread);
    Int_t i;
    Float_t fbuf;
    TRandom rnd;
    QOversizeArray *abuf;
    QOABuffer *bbuf;
    Int_t ibuf;
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

	//While total memory size exceeds fLevel2MemSize
	while(fLevel2MemSize && fTotalMemSize > fLevel2MemSize) {
	    pthread_mutex_unlock(&fMSizeMutex);
	    //printf("Looping in memory management thread...\n");

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

		for(i=1; i<fInstances.Count(); i++) {
		    fICumulPriority.GetArray()[i]=fICumulPriority.GetArray()[i-1]+fInstances.GetArray()[i]->fAPriority;
		}
		pthread_mutex_unlock(&fPriorityMutex);
		//Select randomly an instance. The probability of selecting a given instance is proportional to its priority
		fbuf=rnd.Rndm()*fICumulPriority.GetLast();
		i=0;

		while(fICumulPriority.GetArray()[i]<fbuf && i<fInstances.Count()) i++;
		if(i==fInstances.Count()) i--;
	    }
	    abuf=fInstances[i];
	    bbuf=NULL;
	    pthread_mutex_lock(&abuf->fBuffersMutex);

	    //If the array is in write mode
	    if(!abuf->fCurReadBuffer) {

		//Get the number of buffers that could be freed
		ibuf=abuf->fNReadBuffers-abuf->fNPCBuffers;

		//If some buffers can be freed
		if(ibuf>0) {

		    //If there is only one buffer available
		    if(ibuf==1) {
			i=0;

			//Else
		    } else {
			//Pickup a buffer randomly
			i=ceil(ibuf*rnd.Rndm())-1;
		    }
		    bbuf=abuf->fFirstReadBuffer;

		    ibuf=0;
		    //Get a pointer to the selected buffer
		    while((UInt_t)ibuf<i+abuf->fNPCBuffers) {
			bbuf=bbuf->fNextOAB;
			ibuf++;
		    }
		}

		//Else if the array is in read mode
	    } else {
	    }

	    if(bbuf && bbuf != abuf->fMWWBuffer) {
		//If the buffer has not been modified, delete it
		if(!bbuf->fIsModified) {
		    //Remove it from the linked list first
		    if(bbuf->fPreviousOAB) bbuf->fPreviousOAB->fNextOAB=bbuf->fNextOAB;
		    if(bbuf->fNextOAB) bbuf->fNextOAB->fPreviousOAB=bbuf->fPreviousOAB;
		    if(abuf->fFirstReadBuffer == bbuf) abuf->fFirstReadBuffer=bbuf->fNextOAB;
		    if(abuf->fLastReadBuffer == bbuf) abuf->fLastReadBuffer=bbuf->fPreviousOAB;
		    abuf->fNReadBuffers--;
		    pthread_mutex_lock(&QOversizeArray::fMSizeMutex);
		    QOversizeArray::fTotalMemSize-=bbuf->fBufferSize+sizeof(QOABuffer);
		    pthread_mutex_unlock(&QOversizeArray::fMSizeMutex);
		    pthread_mutex_unlock(&abuf->fBuffersMutex);
		    delete bbuf;

		    //Else if it was modified
		} else {
		    pthread_mutex_lock(&QOversizeArray::fMSizeMutex);

		    //If the buffer is not compressed and the total memory size is >= compression memory size threshold, compress the buffer
		    if(!bbuf->fIsCompressed && QOversizeArray::fTotalMemSize>=QOversizeArray::fCThreshMemSize) {
			pthread_mutex_unlock(&QOversizeArray::fMSizeMutex);
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
			    pthread_mutex_lock(&QOversizeArray::fMSizeMutex);
			    QOversizeArray::fTotalMemSize-=bbuf->fBufferSize-ibuf;
			    pthread_mutex_unlock(&QOversizeArray::fMSizeMutex);
			    bbuf->fBufferSize=ibuf;
			}
			//Need to unlock TBDIMutex first to avoid deadlock with Save and Terminate functions. Should not cause a problem with reading thread
			pthread_mutex_unlock(&abuf->fRBDIMutex);
			pthread_mutex_lock(&abuf->fBuffersMutex);
			bbuf->fIsCompressed=1;
			pthread_mutex_unlock(&abuf->fBuffersMutex);

			//Else if the buffer is already compressed or total memory size < compression memory size threshold, write ask the memory writing thread to write it on disk and delete it	
		    } else {
			pthread_mutex_unlock(&QOversizeArray::fMSizeMutex);
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
