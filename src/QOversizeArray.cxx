#include "QOversizeArray.h"

ClassImp(QOversizeArray)

UInt_t QOversizeArray::fNInstances=0;
Long64_t QOversizeArray::fAAMaxSize=-1;

QOversizeArray::QOversizeArray(const char *filename, const char *arrayname, omode openmode, UInt_t objectsize, UInt_t nobjectsperbuffer, Long64_t arraymaxsize, Long64_t allarraysmaxsize): fFilename(filename), fArrayName(arrayname), fPtr(NULL), fFirstDataByte(sizeof(UInt_t)+256+sizeof(Long64_t)), fOpenMode(openmode), fObjectsSize(objectsize), fBuffer(NULL), fNOPerBuffer(nobjectsperbuffer), fNObjects(0), fArrayMaxSize(arraymaxsize)
{
    fNInstances++;
    if(allarraysmaxsize != -2) fAAMaxSize=allarraysmaxsize;
    OpenFile();
}

QOversizeArray::~QOversizeArray()
{
    fNInstances--;
    CloseFile();
}

void QOversizeArray::CloseFile()
{
    if(fPtr) {

	if(fOpenMode != kRead) {
	    Write(&fNObjects, sizeof(fNObjects), 1, fFirstDataByte-sizeof(fNObjects));
	}

	if(fclose(fPtr) == EOF) {
	    perror("QOversizeArray::~QOversizeArray(): Error: ");
	    throw 1;
	}
	fPtr=NULL;
    }
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
}

void QOversizeArray::Read(void *buf, size_t size, size_t num, Long64_t pos)
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

void QOversizeArray::Write(const void *buf, size_t size, size_t num, Long64_t pos) const
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
