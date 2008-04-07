#include "QSigExStruct.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

const Int_t  kMaxLen = 2048;

ClassImp(QSigExStruct)

void QSigExStruct::Delete()
{
  TDirectory::TContext ctxt(gDirectory, this);

  TClass *cl;

  //*-*---------------------Case of Object in memory---------------------
  //                        ========================
  TNamed *idcur;
  TObjLink *nlnk;
  TObjLink *lnk=fList->FirstLink();
  while (lnk) {
    nlnk=lnk->Next();
    idcur=(TNamed*)lnk->GetObject();

    if (idcur->IsA()->InheritsFrom(TDirectoryFile::Class())) {
      // read subdirectories to correctly delete them
      ((TDirectory*) idcur)->ReadAll("dirs");
      idcur->Delete();
      delete idcur;

    } else
      idcur->Delete();
    lnk=nlnk;
  }
  //*-*---------------------Case of Key---------------------
  //                        ===========
  if (IsWritable()) {
    TKey *key;
    TIter nextkey(GetListOfKeys());

    while ((key = (TKey *) nextkey())) {
      cl=TClass::GetClass(key->GetClassName());

      if (cl->InheritsFrom(TDirectoryFile::Class())) {
	// read directory with subdirectories to correctly delete and free key structure
	TDirectory* dir = GetDirectory(key->GetName(), kTRUE, "Delete");
	if (dir!=0) {
	  dir->Delete();
	  delete dir;
	}
      }

      key->Delete();
      fKeys->Remove(key);
      fModified = kTRUE;
      delete key;
    }
    fKeys->Clear("nodelete");
  }
  fList->Clear("nodelete");

  if (IsWritable()) {
    TFile* f = GetFile();
    if (fModified && (f!=0)) {
      WriteKeys();            //*-* Write new keys structure
      WriteDirHeader();       //*-* Write new directory header
      f->WriteFree();     //*-* Write new free segments list
      f->WriteHeader();   //*-* Write new file header
    }
  }
}

void QSigExStruct::Delete(const char* namecycle)
{
  //*-*-*-*-*-*-*-* Delete Objects or/and keys in a directory *-*-*-*-*-*-*-*
  //*-*             =========================================
  //   namecycle has the format name;cycle
  //   namecycle = "" same as namecycle ="T*"
  //   name  = * means all
  //   cycle = * means all cycles (memory and keys)
  //   cycle = "" or cycle = 9999 ==> apply to a memory object
  //   When name=* use T* to delete subdirectories also
  //
  //   To delete one directory, you must specify the directory cycle,
  //      eg.  file.Delete("dir1;1");
  //
  //   examples:
  //     foo   : delete object named foo in memory
  //     foo*  : delete all objects with a name starting with foo
  //     foo;1 : delete cycle 1 of foo on file
  //     foo;* : delete all cycles of foo on file and also from memory
  //     *;2   : delete all objects on file having the cycle 2
  //     *;*   : delete all objects from memory and file
  //
  //

  PRINTF4(this,"\tQSigExStruct::Delete(const char* namecycle<'",namecycle,"'>)\n")
  
  TDirectory::TContext ctxt(gDirectory, this);
  Short_t  cycle;
  char     name[kMaxLen];
  DecodeNameCycle(namecycle, name, cycle);

  Int_t deleteall    = 0;
  if(strlen(namecycle) == 0 || strcmp(name,"*") == 0)   deleteall = 1;
  TRegexp re(name,kTRUE);
  TString s;
  Int_t deleteOK = 0;
  TClass *cl;

  //*-*---------------------Case of Object in memory---------------------
  //                        ========================
  if (cycle >= 9999 ) {
    TNamed *idcur;
    TObjLink *nlnk;
    TObjLink *lnk=fList->FirstLink();
    while (lnk) {
      nlnk=lnk->Next();
      idcur=(TNamed*)lnk->GetObject();
      deleteOK = 0;
      s = idcur->GetName();
      if (deleteall || s.Index(re) != kNPOS) {
	deleteOK = 1;
	if (idcur->IsA()->InheritsFrom(TDirectoryFile::Class())) {
	  deleteOK = 2;
	}
      }
      if (deleteOK != 0) {
	fList->Remove(lnk);
	if (deleteOK==2) {
	  // read subdirectories to correctly delete them
	  ((TDirectory*) idcur)->ReadAll("dirs");
	  idcur->Delete();
	  delete idcur;
	} else
//	  idcur->Delete(name);
	  idcur->Delete();
      }
      lnk=nlnk;
    }
  }
  //*-*---------------------Case of Key---------------------
  //                        ===========
  if (cycle != 9999 ) {
    if (IsWritable()) {
      TKey *key;
      TIter nextkey(GetListOfKeys());
      while ((key = (TKey *) nextkey())) {
	deleteOK = 0;
	s = key->GetName();
	if (deleteall || s.Index(re) != kNPOS) {
	  if (cycle == key->GetCycle()) deleteOK = 1;
	  if (cycle > 9999) deleteOK = 1;
	  cl=TClass::GetClass(key->GetClassName());
	  if (cl->InheritsFrom(TDirectoryFile::Class())) 
	    deleteOK = 2;
	}
	if (deleteOK) {
	  if (deleteOK==2) {
	    // read directory with subdirectories to correctly delete and free key structure
	    TDirectory* dir = GetDirectory(key->GetName(), kTRUE, "Delete");
	    if (dir!=0) {
	      dir->Delete();
	      fList->Remove(dir);
	      delete dir;
	    }
	  }

	  key->Delete();
	  fKeys->Remove(key);
	  fModified = kTRUE;
	  delete key;
	}
      }
      TFile* f = GetFile();
      if (fModified && (f!=0)) {
	WriteKeys();            //*-* Write new keys structure
	WriteDirHeader();       //*-* Write new directory header
	f->WriteFree();     //*-* Write new free segments list
	f->WriteHeader();   //*-* Write new file header
      }
    }
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

TDirectory* QSigExStruct::mkdir(const char *name, const char *title)
{
  // Create a sub-directory and return a pointer to the created directory.
  // Returns 0 in case of error.
  // Returns 0 if a directory with the same name already exists.
  // Note that the directory name cannot contain slashes.

  PRINTF5("TDirectory* QSigExStruct::mkdir(const char *name<",name,">, const char *title<",title,">)\n")

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

  return new QSigExStruct(name, title, this);
}

void QSigExStruct::FillBuffer(char *&buffer)
{
  //char* ibuf=buffer;
  PRINTF2(this,"\tQSigExStruct::FillBuffer\n")
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
    //printf("Number of bytes written: %i\n",buffer-ibuf);
    return;
  }
  if (version <=1000) for (Int_t i=0;i<3;i++) tobuf(buffer,Int_t(0));
  //printf("Number of bytes written: %i\n",buffer-ibuf);
}

void QSigExStruct::ReadAll(Option_t* opt)
{
  // Read objects from a ROOT db file directory into memory.
  // If an object is already in memory, the memory copy is deleted
  // and the object is again read from the file.
  // If opt=="dirs", only subdirectories will be read
  // If opt=="dirs*" complete directory tree will be read

  TDirectory::TContext ctxt(this);

  TKey *key;
  TIter next(GetListOfKeys());

  Bool_t readdirs = ((opt!=0) && ((strcmp(opt,"dirs")==0) || (strcmp(opt,"dirs*")==0)));
  TClass *cl;

  if (readdirs)
    while ((key = (TKey *) next())) {

      //if (strcmp(key->GetClassName(),"TDirectory")!=0) continue;
      //if (strstr(key->GetClassName(),"TDirectory")==0) continue;
      cl = TClass::GetClass(key->GetClassName());
      if(!(cl->InheritsFrom(TDirectoryFile::Class()))) continue;

      TDirectory *dir = GetDirectory(key->GetName(), kTRUE, "ReadAll");

      if ((dir!=0) && (strcmp(opt,"dirs*")==0)) dir->ReadAll("dirs*");
    }
  else
    while ((key = (TKey *) next())) {
      TObject *thing = GetList()->FindObject(key->GetName());
      if (thing) { delete thing; }
      thing = key->ReadObj();
    }
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

#include "debugger.h"
