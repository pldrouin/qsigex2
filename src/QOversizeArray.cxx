#include "QOversizeArray.h"

ClassImp(QOversizeArray)

QList<QOversizeArray*> QOversizeArray::fInstances;
Long64_t QOversizeArray::fMaxMemSize=0;
Long64_t QOversizeArray::fMaxUnsavedMemSize=0;
Long64_t QOversizeArray::fCritMemSize=0;
Long64_t QOversizeArray::fTotalMemSize=0;

QOversizeArray::QOversizeArray(const char *filename, const char *arrayname, omode openmode, const UInt_t &objectsize, const UInt_t &nobjectsperbuffer): fFilename(filename), fArrayName(arrayname), fPtr(NULL), fFirstDataByte(sizeof(UInt_t)+256+sizeof(Long64_t)), fOpenMode(openmode), fObjectsSize(objectsize), fBuffer(NULL), fNOPerBuffer(nobjectsperbuffer), fNObjects(0), fCurReadBuffer(NULL), fFirstReadBuffer(NULL), fLastReadBuffer(NULL), fWriteBuffer(NULL), fNReadBuffers(0), fFirstUnsavedBuffer(NULL), fFirstParkedBuffer(NULL)
{
    fInstances.Add(this);
    OpenFile();
}

QOversizeArray::~QOversizeArray()
{
    fInstances.Del(fInstances.FindFirst(this));
    CloseFile();
}

void QOversizeArray::CloseFile()
{
    if(fPtr) {
	if(fOpenMode != kRead)  Save();

	if(fclose(fPtr) == EOF) {
	    perror("QOversizeArray::~QOversizeArray(): Error: ");
	    throw 1;
	}
	fPtr=NULL;
    }

    Terminate();
}

void QOversizeArray::CheckMemory()
{
}

void QOversizeArray::Init()
{
    size_t numobjs=fNObjects%fNOPerBuffer;
    Long64_t firstobjidx=fNObjects-numobjs;

    if(!fWriteBuffer) {
	fWriteBuffer=new QOABuffer(firstobjidx, fObjectsSize*fNOPerBuffer);
	fTotalMemSize+=fObjectsSize*fNOPerBuffer+sizeof(QOABuffer);
    } 

    if(numobjs) {
	Read(fWriteBuffer->GetBuffer(), fObjectsSize, numobjs, fFirstDataByte+firstobjidx);
    }
}

void QOversizeArray::Fill()
{
    //Copy the content of the object buffer in fWriteBuffer
    memcpy(fWriteBuffer->GetBuffer()+(fNObjects-fWriteBuffer->GetFirstObjIdx())*fObjectsSize,fBuffer,fObjectsSize);
    fNObjects++;

    //If fWriteBuffer is full
    if(fNObjects-fWriteBuffer->GetFirstObjIdx() == fNOPerBuffer) {

	if(fLastReadBuffer) {
	    fLastReadBuffer->SetNextOAB(fWriteBuffer);
	}
	fWriteBuffer->SetPreviousOAB(fLastReadBuffer);
	fWriteBuffer->SetNextOAB(NULL);
	fLastReadBuffer=fWriteBuffer;
	fNReadBuffers++;

	if(!fFirstUnsavedBuffer) fFirstUnsavedBuffer=fWriteBuffer;

	if(fFirstParkedBuffer) {
	    fWriteBuffer=fFirstParkedBuffer;
	    fFirstParkedBuffer=fFirstParkedBuffer->GetNextOAB();

	} else {
	    fWriteBuffer=new QOABuffer(fNObjects, fObjectsSize*fNOPerBuffer);
	    fTotalMemSize+=fObjectsSize*fNOPerBuffer+sizeof(QOABuffer);
	}
    }
    CheckMemory();
}

void QOversizeArray::OpenFile()
{
    if(fPtr) CloseFile();

    switch(fOpenMode) {
	case kRead:
	    fPtr=fopen(fFilename,"rb");

	    if(!fPtr) {
		fprintf(stderr,"QOversizeArray::QOversizeArray: Error: file '%s' cannot be opened in read-only mode\n",fFilename.Data());
		throw 1;
	    }
	    ReadHeader();
	    break;

	case kRW:
	    fPtr=fopen(fFilename,"rb+");

	    if(!fPtr) {
		fprintf(stderr,"QOversizeArray::QOversizeArray: Error: file '%s' cannot be opened in read-write mode\n",fFilename.Data());
		throw 1;
	    }
	    ReadHeader();
	    break;

	case kRecreate:
	    fPtr=fopen(fFilename,"wb+");

	    if(!fPtr) {
		fprintf(stderr,"QOversizeArray::QOversizeArray: Error: file '%s' cannot be recreated\n",fFilename.Data());
		throw 1;
	    }

	    if(fObjectsSize==0) {
		fprintf(stderr,"QOversizeArray::QOversizeArray: Error: objectssize cannot be 0\n");
		throw 1;
	    }
	    WriteHeader();
    }

    Init();
}

void QOversizeArray::Read(void *buf, const size_t &size, const size_t &num, const Long64_t &pos) const
{
    size_t ret;

    if(pos != -1) {

	if(fseek(fPtr,pos,SEEK_SET)) {
	    perror("QOversizeArray::Read: Error: ");
	    throw 1;
	}
    }
    
    if((ret=fread(buf,size,num,fPtr)) != num) {

	if(feof(fPtr)) {
	    fprintf(stderr,"QOversizeArray::Read: Error: End of file has been reached\n");
	    throw 1;
	}
	perror("QOversizeArray::Read: Error: ");
	throw 1;
    }
}

void QOversizeArray::ReadHeader()
{
    try {
	UInt_t uibuf;
	char *strbuf;

	Read(&uibuf, sizeof(uibuf), 1, 0);
	strbuf=(char*)malloc(uibuf+1);
	Read(strbuf, 1, uibuf);
	strbuf[uibuf]=0;

	if(strcmp(strbuf,fArrayName)) {
	    fprintf(stderr,"QOversizeArray::ReadHeader: Error: Array name saved in file '%s' does not match provided array name\n",fFilename.Data());
	    throw 1;
	}
	free(strbuf);
	Read(&uibuf, sizeof(uibuf), 1, sizeof(uibuf)+256);

	if(fObjectsSize != 0 && fObjectsSize != uibuf) {
	    fprintf(stderr,"QOversizeArray::ReadHeader: Error: Objects size saved in file '%s' does not march provided objects size\n",fFilename.Data());
	    throw 1;
	}
	fObjectsSize=uibuf;
	Read(&fNObjects, sizeof(fNObjects), 1);
	
    } catch (Int_t e) {
	fprintf(stderr,"Exception handled by QOversizeArray::ReadHeader\n");
	throw e;
    }
}

void QOversizeArray::Save()
{
    if(!fPtr) {
	fprintf(stderr,"QOversizeArray::Save: Error: There is no opened file\n");
	throw 1;
    }

    WriteHeader();
}

void QOversizeArray::Terminate()
{
    if(fWriteBuffer) {
	delete fWriteBuffer;
	fWriteBuffer=NULL;
    }
}

void QOversizeArray::Write(const void *buf, const size_t &size, const size_t &num, const Long64_t &pos) const
{
    size_t ret;

    if(pos != -1) {

	if(fseek(fPtr,pos,SEEK_SET)) {
	    perror("QOversizeArray::Write: Error: ");
	    throw 1;
	}
    }
    
    if((ret=fwrite(buf,size,num,fPtr)) != num) {

	perror("QOversizeArray::Write: Error: ");
	throw 1;
    }
}

void QOversizeArray::WriteHeader() const
{
    try {
	UInt_t uibuf=fArrayName.Length();
	uibuf=(uibuf>256 ? 256 : uibuf);

	Write(&uibuf, sizeof(uibuf), 1, 0);
	Write(fArrayName.Data(), 1, uibuf);
	Write(&fObjectsSize, sizeof(fObjectsSize), 1, sizeof(uibuf)+256);
	Write(&fNObjects, sizeof(fNObjects), 1);
	
    } catch (Int_t e) {
	fprintf(stderr,"Exception handled by QOversizeArray::WriteHeader\n");
	throw e;
    }
}
