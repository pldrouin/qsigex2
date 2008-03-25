#include "QMyDir.h"

#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QMyDir)

QMyDir::QMyDir(const char *name, const char *title, TDirectory *initMotherDir)
{
  fName = name;
  fTitle = title;
  const char *classname="QMyDir";

  if (initMotherDir==0) initMotherDir = gDirectory;

  if (strchr(name,'/')) {
    ::Error("QMyDir","directory name (%s) cannot contain a slash", name);
    gDirectory = 0;
    return;
  }
  if (strlen(GetName()) == 0) {
    ::Error("QMyDir","directory name cannot be \"\"");
    gDirectory = 0;
    return;
  }

  Build(initMotherDir ? initMotherDir->GetFile() : 0, initMotherDir);

  TDirectory* motherdir = GetMotherDir();
  TFile* f = GetFile();

  if ((motherdir==0) || (f==0)) return;
  if (!f->IsWritable()) return; //*-* in case of a directory in memory
  if (motherdir->GetKey(name)) {
    Error("QMyDir","An object with name %s exists already", name);
    return;
  }
  TClass *cl = IsA();
  if (strlen(classname) != 0) cl = TClass::GetClass(classname);

  if (!cl) {
    Error("QMyDir","Invalid class name: %s",classname);
    return;
  }
  fBufferSize  = 0;
  fWritable    = kTRUE;

  if (f->IsBinary()) {
    fSeekParent  = f->GetSeekDir();
    Int_t nbytes = Sizeof();
    TKey *key    = new TKey(fName,fTitle,cl,nbytes,motherdir);
    
//    TKey *key    = new TKey(this,fName,100,initMotherDir);
    fNbytesName  = key->GetKeylen();
    fSeekDir     = key->GetSeekKey();
    if (fSeekDir == 0) return;
    char *buffer = key->GetBuffer();
    printf("QMyDir::QMyDir: Calling FillBuffer\n");
    FillBuffer(buffer);
    
    Int_t cycle = motherdir->AppendKey(key);
    key->WriteFile(cycle);
  } else {
    fSeekParent  = 0;
    fNbytesName  = 0;
//    fSeekDir     = f->DirCreateEntry(this);
    if (fSeekDir == 0) return;
  }

  fModified = kFALSE;
  R__LOCKGUARD2(gROOTMutex);
  gROOT->GetUUIDs()->AddUUID(fUUID,this);
}

void QMyDir::Streamer(TBuffer &b)
{
  PRINTF2(this,"\tvoid QMyDir::Streamer(TBuffer &b)")
  Version_t v,version;
  const UInt_t kIsBigFile = BIT(16);
  if (b.IsReading()) {
    PRINTF2(this,"\tReading from ")
    Build((TFile*)b.GetParent(), 0);
    if (fFile && fFile->IsWritable()) fWritable = kTRUE;

    if (fFile && !fFile->IsBinary()) {
      printf("non-binary file\n");
      Version_t R__v = b.ReadVersion(0, 0);

      TClass* dirclass = QMyDir::Class();

      b.ClassBegin(dirclass, R__v);

      TString sbuf;

      b.ClassMember("CreateTime","TString");
      sbuf.Streamer(b);
      TDatime timeC(sbuf.Data());
      fDatimeC = timeC;

      b.ClassMember("ModifyTime","TString");
      sbuf.Streamer(b);
      TDatime timeM(sbuf.Data());
      fDatimeM = timeM;

      b.ClassMember("UUID","TString");
      sbuf.Streamer(b);
      TUUID id(sbuf.Data());
      fUUID = id;

      b.ClassEnd(dirclass);

      fSeekKeys = 0; // read keys later in the TKeySQL class
    } else {
      printf("binary file\n");
      b >> version;
      fDatimeC.Streamer(b);
      fDatimeM.Streamer(b);
      b >> fNbytesKeys;
      b >> fNbytesName;
      if (version > 1000) {
	SetBit(kIsBigFile);
	b >> fSeekDir;
	b >> fSeekParent;
	b >> fSeekKeys;
      } else {
	Int_t sdir,sparent,skeys;
	b >> sdir;    fSeekDir    = (Long64_t)sdir;
	b >> sparent; fSeekParent = (Long64_t)sparent;
	b >> skeys;   fSeekKeys   = (Long64_t)skeys;
      }
      v = version%1000;
      if (v == 2) {
	fUUID.StreamerV1(b);
      } else if (v > 2) {
	fUUID.Streamer(b);
      }
    }
    R__LOCKGUARD2(gROOTMutex);
    gROOT->GetUUIDs()->AddUUID(fUUID,this);
    if (fSeekKeys) ReadKeys();
  } else {
    PRINTF2(this,"\tWriting to file...\n")
    if (fFile && !fFile->IsBinary()) {
      b.WriteVersion(QMyDir::Class());

      TString sbuf;

      b.ClassBegin(QMyDir::Class());

      b.ClassMember("CreateTime","TString");
      sbuf = fDatimeC.AsSQLString();
      sbuf.Streamer(b);

      b.ClassMember("ModifyTime","TString");
      fDatimeM.Set();
      sbuf = fDatimeM.AsSQLString();
      sbuf.Streamer(b);

      b.ClassMember("UUID","TString");
      sbuf = fUUID.AsString();
      sbuf.Streamer(b);

      b.ClassEnd(QMyDir::Class());
    } else {
      version = QMyDir::Class_Version();
      if (fFile && fFile->GetEND() > TFile::kStartBigFile) version += 1000;
      b << version;
      fDatimeC.Streamer(b);
      fDatimeM.Streamer(b);
      b << fNbytesKeys;
      b << fNbytesName;
      if (version > 1000) {
	b << fSeekDir;
	b << fSeekParent;
	b << fSeekKeys;
      } else {
	b << (Int_t)fSeekDir;
	b << (Int_t)fSeekParent;
	b << (Int_t)fSeekKeys;
      }
      fUUID.Streamer(b);
      if (version <=1000) for (Int_t i=0;i<3;i++) b << Int_t(0);
    }
  }
}

void QMyDir::FillBuffer(char *&buffer)
{
  char* ibuf=buffer;
  printf("QMyDir::FillBuffer\n");
  Version_t version = QMyDir::Class_Version();
  if (fSeekKeys > TFile::kStartBigFile) version += 1000;
  tobuf(buffer, version);
  fDatimeC.FillBuffer(buffer);
  fDatimeM.FillBuffer(buffer);
  tobuf(buffer, fNbytesKeys);
  tobuf(buffer, fNbytesName);
  if (version > 1000) {
    tobuf(buffer, fSeekDir);
    tobuf(buffer, fSeekParent);
    tobuf(buffer, fSeekKeys);
  } else {
    tobuf(buffer, (Int_t)fSeekDir);
    tobuf(buffer, (Int_t)fSeekParent);
    tobuf(buffer, (Int_t)fSeekKeys);
  }
  fUUID.FillBuffer(buffer);
  if (fFile && fFile->GetVersion() < 40000){
    printf("Number of bytes written: %i\n",buffer-ibuf);
    return;
  }
  if (version <=1000) for (Int_t i=0;i<3;i++) tobuf(buffer,Int_t(0));
  printf("Number of bytes written: %i\n",buffer-ibuf);
}


void QMyDir::WriteDirHeader()
{
  PRINTF2(this,"\tQMyDir::WriteDirHeader()\n");

  TFile* f = GetFile();
  if (f==0) return;

  if (!f->IsBinary()) {
    fDatimeM.Set();
//    f->DirWriteHeader(this);
    return;
  }

  Int_t nbytes  = TDirectoryFile::Sizeof();  //Warning ! TFile has a Sizeof()
  char * header = new char[nbytes];
  char * buffer = header;
  fDatimeM.Set();
  FillBuffer(buffer);
  Long64_t pointer = fSeekDir + fNbytesName; // do not overwrite the name/title part
  fModified     = kFALSE;
  f->Seek(pointer);
  f->WriteBuffer(header, nbytes);
  if (f->MustFlush()) f->Flush();
  delete [] header;
}

#include "debugger.h"
