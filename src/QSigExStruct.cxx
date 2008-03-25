#include "QSigExStruct.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QSigExStruct)

void QSigExStruct::Delete(const char* namecycle)
{
  if(fQArray){
    Short_t cycle;
    char name[2048];
    DecodeNameCycle(namecycle, name, cycle);
    fQArray->Remove(TDirectoryFile::FindObject(name));
  }
  TDirectoryFile::Delete(namecycle);
}

void QSigExStruct::Delete(Int_t idx)
{
  TObject *obj;
  if(fQArray && (obj=fQArray->At(idx))){

    if(dynamic_cast<TDirectoryFile*>(obj))
      dynamic_cast<TDirectoryFile*>(obj)->Delete("T*;*");
    GetList()->Remove(obj);
    fQArray->RemoveAt(idx);
    TKey *keybuf;

    while((keybuf=GetKey(obj->GetName()))){
      //TKey::Delete deletes the TKey from the file but does not free the memory
      keybuf->Delete();
//      Commented out because this is already done in TKey::Delete
//      GetListOfKeys()->Remove(keybuf);
      delete keybuf;
    }
    delete obj;
  }
}

void QSigExStruct::Init(const char *name, const char *title, TDirectory *initMotherDir)
{
  fName = name;
  fTitle = title;

  if (initMotherDir==0) initMotherDir = gDirectory;

  if (strchr(name,'/')) {
    ::Error("QSigExStruct","directory name (%s) cannot contain a slash", name);
    gDirectory = 0;
    return;
  }
  if (strlen(GetName()) == 0) {
    ::Error("QSigExStruct","directory name cannot be \"\"");
    gDirectory = 0;
    return;
  }

  Build(initMotherDir ? initMotherDir->GetFile() : 0, initMotherDir);

  TDirectory* motherdir = GetMotherDir();
  TFile* f = GetFile();

  if ((motherdir==0) || (f==0)) return;
  if (!f->IsWritable()) return; //*-* in case of a directory in memory
  if (motherdir->GetKey(name)) {
    Error("QSigExStruct","An object with name %s exists already", name);
    return;
  }
  TClass *cl = QSigExStruct::Class();

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
    printf("QSigExStruct::QSigExStruct: Calling FillBuffer\n");
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

TDirectory* QSigExStruct::mkdir(const char *name, const char *title, Int_t idx)
{
  // Create a sub-directory and return a pointer to the created directory.
  // Returns 0 in case of error.
  // Returns 0 if a directory with the same name already exists.
  // Note that the directory name cannot contain slashes.

  PRINTF7("TDirectory* QSigExStruct::mkdir(const char *name<",name,">, const char *title<",title,">, Int_t idx<",idx,">)\n")

  if (!name || !title || !strlen(name)) return 0;
  if (!strlen(title)) title = name;
  if (strchr(name,'/')) {
    Error("QSigExStruct::mkdir","directory name (%s) cannot contain a slash", name);
    return NULL;
  }
  if (GetKey(name)) {
    Error("QSigExStruct::mkdir","An object with name %s exists already",name);
    return NULL;
  }

  TDirectory::TContext ctxt(this);

  if(idx==-1) return new QSigExStruct(name, title, this);
  else return new QSigExStruct(name,title,this,idx);
}

void QSigExStruct::FillBuffer(char *&buffer)
{
  char* ibuf=buffer;
  printf("QSigExStruct::FillBuffer\n");
  Version_t version = QSigExStruct::Class_Version();
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

Int_t TDirectoryFile::Sizeof() const
{
  //*-*-*-*-*-*-*Return the size in bytes of the directory header*-*-*-*-*-*-*
  //*-*          ================================================
  //Int_t nbytes = sizeof(Version_t);    2
  //nbytes     += fDatimeC.Sizeof();
  //nbytes     += fDatimeM.Sizeof();
  //nbytes     += sizeof fNbytesKeys;    4
  //nbytes     += sizeof fNbytesName;    4
  //nbytes     += sizeof fSeekDir;       4 or 8
  //nbytes     += sizeof fSeekParent;    4 or 8
  //nbytes     += sizeof fSeekKeys;      4 or 8
  //nbytes     += fUUID.Sizeof();
  //nbytes     += sizeof Int_t;
  //nbytes     += fKeys->GetSize()*sizeof(Int_t);
  Int_t nbytes = 22;

  nbytes     += fDatimeC.Sizeof();
  nbytes     += fDatimeM.Sizeof();
  nbytes     += fUUID.Sizeof();
  //assume that the file may be above 2 Gbytes if file version is > 4
  if (fFile && fFile->GetVersion() >= 40000) nbytes += 12;
  return nbytes;
}

void QSigExStruct::Streamer(TBuffer &b)
{
  PRINTF2(this,"\tvoid QSigExStruct::Streamer(TBuffer &b)")
  Version_t v,version;
  const UInt_t kIsBigFile = BIT(16);
  if (b.IsReading()) {
    PRINTF2(this,"\tReading from ")
    Build((TFile*)b.GetParent(), 0);
    if (fFile && fFile->IsWritable()) fWritable = kTRUE;

    if (fFile && !fFile->IsBinary()) {
      printf("non-binary file\n");
      Version_t R__v = b.ReadVersion(0, 0);

      TClass* dirclass = QSigExStruct::Class();

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
      b.WriteVersion(QSigExStruct::Class());

      TString sbuf;

      b.ClassBegin(QSigExStruct::Class());

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

      b.ClassEnd(QSigExStruct::Class());
    } else {
      version = QSigExStruct::Class_Version();
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

void QSigExStruct::WriteDirHeader()
{
  PRINTF2(this,"\tQSigExStruct::WriteDirHeader()\n");

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

void QSigExStruct::WriteKeys()
{
  TFile* f = GetFile();
  if (f==0) return;

  if (!f->IsBinary()) {
    fprintf(stderr,"QSigExStruct::WriteKeys(): Warning: Function not implemented for non-binary files\n");
    return;
  }

  //*-* Delete the old keys structure if it exists
  if (fSeekKeys != 0) {
    f->MakeFree(fSeekKeys, fSeekKeys + fNbytesKeys -1);
  }
  //*-* Write new keys record
  TIter next(fKeys);
  TKey *key;
  Int_t nkeys  = fKeys->GetSize();
  Int_t nobjs  = fList->GetSize();
  Int_t nbytes = sizeof nkeys;          //*-* Compute size of all keys
  if (f->GetEND() > TFile::kStartBigFile) nbytes += 8;
  while ((key = (TKey*)next())) {
    nbytes += key->Sizeof();
  }
  //nbytes += nobjs * sizeof(nobjs); //Allocate space for QSigExStruct list
  TKey *headerkey  = new TKey(fName,fTitle,IsA(),nbytes,this);
  if (headerkey->GetSeekKey() == 0) {
    delete headerkey;
    return;
  }
  char *buffer = headerkey->GetBuffer();
  next.Reset();
  tobuf(buffer, nkeys);
  while ((key = (TKey*)next())) {
    key->FillBuffer(buffer);
  }

  /*if(fQArray){
    for(Int_t i=0; i<fList->GetSize(); i++) tobuf(buffer, fQArray->IndexOf(fList->At(i)));

  } else {
    for(Int_t i=0; i<fList->GetSize(); i++) tobuf(buffer, -1);
  }*/

  fSeekKeys     = headerkey->GetSeekKey();
  fNbytesKeys   = headerkey->GetNbytes();
  headerkey->WriteFile();
  delete headerkey;
}

#include "debugger.h"
