#include "QOversizeArray.h"

ClassImp(QOversizeArray)

QList<QOversizeArray*> QOversizeArray::fInstances;
QList<Float_t> QOversizeArray::fICumulPriority;
Long64_t QOversizeArray::fLevel1MemSize=0;
Long64_t QOversizeArray::fLevel2MemSize=0;
Long64_t QOversizeArray::fCritMemSize=0;
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

Int_t mystamp=0;
pthread_mutex_t stampmutex=PTHREAD_MUTEX_INITIALIZER;

/*void printstatus(const char* status)
{
    pthread_mutex_lock(&stampmutex);
    mystamp++;
    printf("%i\t%s\n",mystamp,status);
    pthread_mutex_unlock(&stampmutex);
}*/

void printstatus(const char* status){}

QOversizeArray::QOversizeArray(const char *filename, const char *arrayname, omode openmode, const UInt_t &objectsize, const UInt_t &nobjectsperbuffer): fFilename(filename), fArrayName(arrayname), fFDesc(0), fFirstDataByte(sizeof(UInt_t)+256+sizeof(fObjectSize)+sizeof(fNOPerBuffer)+sizeof(fNObjects)), fBufferHeaderSize(sizeof(UInt_t)), fOpenMode(openmode), fObjectSize(objectsize), fBuffer(NULL), fNOPerBuffer(nobjectsperbuffer), fMaxBDataSize(objectsize*nobjectsperbuffer), fMaxBHBDataSize(fBufferHeaderSize+fMaxBDataSize), fNObjects(0), fCurReadBuffer(NULL), fFirstReadBuffer(NULL), fLastReadBuffer(NULL), fWriteBuffer(NULL), fWBFirstObjIdx(0), fNReadBuffers(0), fUMBuffers(NULL), fNUMBuffers(0), fFirstParkedBuffer(NULL), fNPCBuffers(3), fArrayIO(0), fAPriority(0), fFileMutex(PTHREAD_MUTEX_INITIALIZER), fBuffersMutex(PTHREAD_MUTEX_INITIALIZER), fMFThread(), fMFMutex(PTHREAD_MUTEX_INITIALIZER), fMFCond(PTHREAD_COND_INITIALIZER), fMFCMutex(PTHREAD_MUTEX_INITIALIZER), fMFPCond(PTHREAD_COND_INITIALIZER), fMFCCond(PTHREAD_COND_INITIALIZER), fMFAction(kFALSE), fMFBuffer(NULL)
{
    OpenFile();
}

QOversizeArray::~QOversizeArray()
{
    printstatus("QOversizeArray::~QOversizeArray()");
    CloseFile();
}

void QOversizeArray::CloseFile()
{
    printstatus("void QOversizeArray::CloseFile()");
    if(fFDesc) {
	if(fOpenMode != kRead)  Save();

	pthread_mutex_lock(&fILMutex);
	Int_t idx=fInstances.FindFirst(this);
	fICumulPriority.Del(idx);
	fInstances.Del(idx);
	pthread_mutex_unlock(&fILMutex);
	//Send a signal to fMFThread to terminate and wait for termination
	pthread_mutex_lock(&fMFMutex);
	fMFAction=2;
	pthread_mutex_unlock(&fMFMutex);
	pthread_mutex_lock(&fMFCMutex);
	pthread_cond_signal(&fMFCond);
	pthread_mutex_unlock(&fMFCMutex);
	printstatus("Waiting for MF thread to terminate...");
	pthread_join(fMFThread,NULL);
	printstatus("MF thread has terminated");

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
	pthread_mutex_lock(&fMMMutex);
	pthread_cond_signal(&fMMCond);
	pthread_mutex_unlock(&fMMMutex);
	pthread_mutex_lock(&fMSizeMutex);

	if(fCritMemSize && fTotalMemSize > fCritMemSize) {
	    pthread_mutex_unlock(&fMSizeMutex);
	    pthread_mutex_lock(&fCMSCMutex);
	    printstatus("***** Critical memory size has been reached");
	    fCLReached=kTRUE;
	    pthread_cond_wait(&fCMSCond, &fCMSCMutex);
	    pthread_mutex_unlock(&fCMSCMutex);

	} else {
	    pthread_mutex_unlock(&fMSizeMutex);
	}
}

void QOversizeArray::Init()
{
    if(!fWriteBuffer) {
	fWriteBuffer=new QOABuffer(0, fMaxBDataSize);
	pthread_mutex_lock(&fMSizeMutex);
	fTotalMemSize+=fMaxBDataSize+sizeof(QOABuffer);
	pthread_mutex_unlock(&fMSizeMutex);
	CheckMemory();
    } 
    ReadWriteBuffer();
}

void QOversizeArray::Fill()
{
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

    //Create memory freeing thread
    pthread_create(&fMFThread, NULL, QOAMFThread, this);

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

void QOversizeArray::ReadBuffer(QOABuffer *buf, const UInt_t &bufferidx)
{
    //printf("Write buffer %u at %u\n",buf->fBufferIdx,buf->fBufferIdx*fMaxBHBDataSize+fFirstDataByte);
    buf->fBufferIdx=bufferidx;
    pthread_mutex_lock(&fFileMutex);
    if(lseek(fFDesc,buf->fBufferIdx*fMaxBHBDataSize+fFirstDataByte,SEEK_SET)==-1){
	perror("QOversizeArray::ReadBuffer: Error: ");
	throw 1;
    }
    if((UInt_t)read(fFDesc, &buf->fBufferSize, fBufferHeaderSize)!=fBufferHeaderSize) {
	perror("QOversizeArray::ReadBuffer: Error: ");
	throw 1;
    }

    if((UInt_t)read(fFDesc, buf->fBuffer, buf->fBufferSize)!=buf->fBufferSize) {
	perror("QOversizeArray::ReadBuffer: Error: ");
	throw 1;
    }
    pthread_mutex_unlock(&fFileMutex);
}

void QOversizeArray::ReadWriteBuffer()
{
    size_t numobjs=fNObjects%fNOPerBuffer;
    UInt_t bufferidx=fNObjects/fNOPerBuffer;

    fWriteBuffer->fBufferIdx=bufferidx;

    if(numobjs) {
	ReadBuffer(fWriteBuffer,bufferidx);
    }
    fWBFirstObjIdx=fNObjects-numobjs;
}

void QOversizeArray::Save()
{
    printstatus("void QOversizeArray::Save()");
    if(!fFDesc) {
	fprintf(stderr,"QOversizeArray::Save: Error: There is no opened file\n");
	throw 1;
    }

    QOABuffer *buf;

    pthread_mutex_lock(&fMFMutex);
    //Request a pause from memory freeing thread
    fMFAction=1;
    pthread_mutex_unlock(&fMFMutex);
    pthread_mutex_lock(&fMFCMutex);
    pthread_cond_signal(&fMFCond);
    //Wait for pause confirmation
    pthread_cond_wait(&fMFPCond,&fMFCMutex);
    pthread_mutex_unlock(&fMFCMutex);
    printstatus("Memory freeing thread confirmed to be in pausing condition");

    //Lock buffer structure
    pthread_mutex_lock(&fBuffersMutex);

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

    //Remove pause condition on memory freeing thread
    pthread_mutex_lock(&fMFMutex);
    fMFAction=0;
    pthread_mutex_unlock(&fMFMutex);
    pthread_mutex_lock(&fMFCMutex);
    printstatus("QOversizeArray::Save: Removing pausing condition from memory freeing thread");
    pthread_cond_signal(&fMFCond);
    pthread_mutex_unlock(&fMFCMutex);
    //Unlock buffer structure
    pthread_mutex_unlock(&fBuffersMutex);

    WriteHeader();
}

void QOversizeArray::SetMemConstraints(const Long64_t &critmemsize,const Long64_t &level1memsize,const Long64_t &level2memsize)
{
    pthread_mutex_lock(&fMSizeMutex);
    fCritMemSize=critmemsize;
    fLevel1MemSize=level1memsize;
    fLevel2MemSize=level2memsize;

    if(fCritMemSize) {
	if(!fLevel2MemSize) fLevel2MemSize=(UInt_t)(0.95*fCritMemSize);
	if(!fLevel1MemSize || fLevel1MemSize > fLevel2MemSize) fLevel1MemSize=(UInt_t)(0.95*fLevel2MemSize);
    }
    pthread_mutex_unlock(&fMSizeMutex);
}

void QOversizeArray::Terminate()
{
    QOABuffer *buf, *nextbuf;
    printstatus("void QOversizeArray::Terminate()");
    if(fWriteBuffer) {
	delete fWriteBuffer;
	fWriteBuffer=NULL;
    }

    if(fNUMBuffers) free(fUMBuffers);

    //Lock buffer structure
    pthread_mutex_lock(&fBuffersMutex);

    buf=fFirstReadBuffer;

    while(buf) {
	nextbuf=buf->fNextOAB;
	delete buf;
	buf=nextbuf;
    }

    fFirstReadBuffer=NULL;
    fLastReadBuffer=NULL;
    fCurReadBuffer=NULL;
    fNObjects=0;

    pthread_mutex_lock(&fMSizeMutex);
    fTotalMemSize-=(fNObjects+1)*(fMaxBDataSize+sizeof(QOABuffer));
    pthread_mutex_unlock(&fMSizeMutex);
    pthread_mutex_unlock(&fBuffersMutex);
}

void QOversizeArray::WriteHeader() const
{
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
    //printf("Write buffer %u at %u\n",buf->fBufferIdx,buf->fBufferIdx*fMaxBHBDataSize+fFirstDataByte);
    pthread_mutex_lock(&fFileMutex);
    if(lseek(fFDesc,buf->fBufferIdx*fMaxBHBDataSize+fFirstDataByte,SEEK_SET)==-1){
	perror("QOversizeArray::WriteBuffer: Error: ");
	throw 1;
    }
    if((UInt_t)write(fFDesc, &buf->fBufferSize, fBufferHeaderSize)!=fBufferHeaderSize || (UInt_t)write(fFDesc, buf->fBuffer, buf->fBufferSize)!=buf->fBufferSize) {
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
    }
}

void* QOversizeArray::QOAMFThread(void *array)
{
    QOversizeArray *qoa=(QOversizeArray*)array;
    QOABuffer *buf;
    Bool_t safedelete;
    printstatus("Starting memory freeing thread");

    for(;;) {
	pthread_mutex_lock(&qoa->fMFMutex);

	if(qoa->fMFAction) {

	    if(qoa->fMFAction == 1) {
		//Pause
		pthread_mutex_unlock(&qoa->fMFMutex);
	        pthread_mutex_lock(&qoa->fMFCMutex);
		pthread_cond_signal(&qoa->fMFPCond);
	        pthread_mutex_unlock(&qoa->fMFCMutex);
		printstatus("Memory freeing thread is pausing");
	        pthread_mutex_lock(&qoa->fMFCMutex);
		pthread_cond_wait(&qoa->fMFCond, &qoa->fMFCMutex);
		printstatus("Memory freeing thread got a signal in pausing condition");
	        pthread_mutex_unlock(&qoa->fMFCMutex);
	        pthread_mutex_lock(&qoa->fMFMutex);

		if(qoa->fMFAction==1) {
		    pthread_mutex_unlock(&qoa->fMFMutex);
		    continue;
		} else {
		    qoa->fMFBuffer=NULL;
		    pthread_mutex_unlock(&qoa->fMFMutex);
		}
	    } else {
		//Stop
		printstatus("Memory freeing thread is stoping");
		qoa->fMFAction=0;
		qoa->fMFBuffer=NULL;
		pthread_mutex_unlock(&qoa->fMFMutex);
		return NULL;
	    }

	} else if(!qoa->fMFBuffer) {
	    printstatus("Memory freeing thread is waiting for a new buffer to free");
	    pthread_mutex_unlock(&qoa->fMFMutex);
	    pthread_mutex_lock(&qoa->fMFCMutex);
	    pthread_cond_signal(&qoa->fMFCCond);
	    pthread_cond_wait(&qoa->fMFCond, &qoa->fMFCMutex);
	    pthread_mutex_unlock(&qoa->fMFCMutex);
	} else {
	    printstatus("Looping in memory freeing thread");

	    //printf("Current buffer to be freed: %p\n",qoa->fMFBuffer);
	    buf=qoa->fMFBuffer;
	    qoa->fMFBuffer=NULL;
	    //printf("Value of resetted buffer address: %p\n",qoa->fMFBuffer);
	    pthread_mutex_unlock(&qoa->fMFMutex);
	    safedelete=kFALSE;

	    pthread_mutex_lock(&qoa->fBuffersMutex);
	    if(qoa->fCurReadBuffer != buf) {
		safedelete=kTRUE;
		if(buf->fPreviousOAB) buf->fPreviousOAB->fNextOAB=buf->fNextOAB;
		if(buf->fNextOAB) buf->fNextOAB->fPreviousOAB=buf->fPreviousOAB;
		if(qoa->fFirstReadBuffer == buf) qoa->fFirstReadBuffer=buf->fNextOAB;
		if(qoa->fLastReadBuffer == buf) qoa->fLastReadBuffer=buf->fPreviousOAB;
		qoa->fNReadBuffers--;
	    }
	    pthread_mutex_unlock(&qoa->fBuffersMutex);

	    pthread_mutex_lock(&qoa->fMFCMutex);
	    pthread_cond_signal(&qoa->fMFCCond);
	    pthread_mutex_unlock(&qoa->fMFCMutex);
	    printstatus("Memory freeing thread just sent a confirmation");

	    if(buf->fIsModified) {
		//qoa->Write(buf->fBuffer, qoa->fMaxBDataSize, 1, buf->fBufferIdx*qoa->fMaxBDataSize+qoa->fFirstDataByte);
		qoa->WriteBuffer(buf);
		buf->fIsModified=kFALSE;
	    }

	    if(safedelete) {
		printstatus("Memory freeing thread is deleting a buffer");
		delete buf;
		pthread_mutex_lock(&QOversizeArray::fMSizeMutex);
		QOversizeArray::fTotalMemSize-=qoa->fMaxBDataSize+sizeof(QOABuffer);
		pthread_mutex_unlock(&QOversizeArray::fMSizeMutex);
	    }
	}
	printstatus("Reached the bottom of the loop in memory freeing thread");
    }
    printstatus("Exiting from memory freeing thread");
    return NULL;
}

void* QOversizeArray::QOAReadThread(void *array)
{
    QOversizeArray *qoa=(QOversizeArray*)array;
    return NULL;
}

void* QOversizeArray::QOAMMThread(void *)
{
    Int_t i;
    Float_t fbuf;
    TRandom rnd;
    QOversizeArray *abuf;
    QOABuffer *bbuf;
    Int_t ibuf;
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

	//While total memory size exceeds fLevelsMemSize
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

	    if(bbuf) {
		pthread_mutex_lock(&abuf->fMFMutex);
		pthread_mutex_unlock(&abuf->fBuffersMutex);
		printstatus("Memory management thread is asking to delete a buffer");
		//printf("Buffer address is %p\n",bbuf);
		abuf->fMFBuffer=bbuf;
		pthread_mutex_lock(&abuf->fMFCMutex);
		pthread_cond_signal(&abuf->fMFCond);
		pthread_mutex_unlock(&abuf->fMFMutex);
		printstatus("Memory management thread is waiting for a confirmation");
		pthread_cond_wait(&abuf->fMFCCond, &abuf->fMFCMutex);
		pthread_mutex_unlock(&abuf->fMFCMutex);
		printstatus("Memory management thread received a confirmation");

	    } else {
		pthread_mutex_unlock(&abuf->fBuffersMutex);
	    }

	    pthread_mutex_unlock(&fILMutex);
	    pthread_mutex_lock(&fMSizeMutex);

	    //printf("CritMemSize: %lli\tTotalMemSize: %lli\n",fCritMemSize,fTotalMemSize);
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
