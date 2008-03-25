// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QSigExIO.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

////////////////////////////////////////////////////////////////////////
//                                                                    //
// QSigExIO                                                           //
//                                                                    //
// This class is responsible or reading and writing ROOT objects in   //
// ROOT files. It can be used only with objects which are independent //
// of the TFile once they have been loaded by their streamer. It      //
// means, for example, that a TTree object cannot be loaded in memory //
// using QSigExIO.                                                    //
//                                                                    //
////////////////////////////////////////////////////////////////////////

ClassImp(QSigExIO)

void QSigExIO::Load(const Char_t* filename, const Char_t* objectname)
{
  //This function loads an object from a ROOT file which filename is filename.
  //The objectname argument can be a simple object name, if the object is
  //located in the root directory of the file, or it can be the path to the
  //object (ex: "mydir/myobject").

  PRINTF6(this,"\tvoid QSigExIO::Load(const Char_t* filename<",filename,">, const Char_t* objectname<",objectname,">)\n")
  Clear();
  TFile f(filename,"READ");
  fObject=f.Get(objectname);

  if(!fObject){
    cout << "QSigExIO::Load: Object '" << objectname << "' in file '" << filename << "' doesn't exist\n"; 
    throw 1;
  }

  TryCast(fObject,fClassName);

  if(dynamic_cast<TChain*>(fObject)){
    dynamic_cast<TChain*>(fObject)->SetDirectory(0);
  } else if(dynamic_cast<TEventList*>(fObject)){
    dynamic_cast<TEventList*>(fObject)->SetDirectory(0);
  } else if(dynamic_cast<TH1*>(fObject)){
    dynamic_cast<TH1*>(fObject)->SetDirectory(0);
  } else if(dynamic_cast<TH2*>(fObject)){
    dynamic_cast<TH2*>(fObject)->SetDirectory(0);
  }
  
  f.Close();
  fFilename=filename;
  Int_t lasts=strrchr(objectname,'/')-objectname;
  Int_t lastbs=strrchr(objectname,'\\')-objectname;
  if(lastbs>lasts) lasts=lastbs;
  if(lasts>0) fDirectory.Append(objectname,lasts);
  else      fDirectory=(Char_t*)NULL;  
}

void QSigExIO::Save() const
{
  //This function saves the object to the file and directory where it has been
  //read/saved the last time.

  PRINTF2(this,"\tvoid QSigExIO::Save() const\n")

  if(!fObject){
    cout << "QSigExIO::Save/SaveAs: Cannot save since no object is loaded\n";
    throw 1;
  }
  if(fFilename.IsNull()){
    cout << "QSigExIO::Save: Cannot save since no filename has been associated. Use SaveAs instead\n";
    throw 1;
  }
  TFile f(fFilename,"Update");
  if(!fDirectory.IsNull()){
    if(!f.cd(fDirectory)){
      cout << "QSigExIO::Save: The directory '" << fDirectory << "' doesn't exist in file '" << fFilename << "'\n";
      throw 1; 
    }
  }
  fObject->Write();
  f.Close();
}

void QSigExIO::SaveAs(const Char_t* filename, const Char_t* directory)
{
  //This function saves the object in directory directory of ROOT file which
  //filename is filename. If the second argument is not provided, the object
  //will be saved in the root directory of the file.

  PRINTF6(this,"\tvoid QSigExIO::SaveAs(const Char_t* filename<",filename,">, const Char_t* directory<",directory,">)\n")

  fFilename=filename;
  fDirectory=directory;
  Save();
}

const QSigExIO& QSigExIO::operator=(const QSigExIO& newqio)
{
  //This function copies by value the content of object QSigExIO to this.

  PRINTF2(this,"\tconst QSigExIO& QSigExIO::operator=(const QSigExIO& newqio)\n")

  Clear();
  fObject=TryCast((const_cast<QSigExIO& >(newqio).fObject)->Clone(),fClassName);
  if(newqio.fFilename){
    fFilename=newqio.fFilename;
    fDirectory=newqio.fDirectory;
  }
  return *this;
}

TObject* QSigExIO::GetObject() const
{
  //This function returns a pointer to the object.

  PRINTF2(this,"\tTObject* QSigExIO::GetObject() const\n")

  return fObject;
};

void QSigExIO::Clear()
{
  //This function resets the current object.

  PRINTF2(this,"\tvoid QSigExIO::Clear()\n")

  fFilename=(Char_t*)NULL;
  fDirectory=(Char_t*)NULL;
  delete fObject;
  fObject=NULL;
}

TObject* QSigExIO::TryCast(const TObject* uptr, const Char_t* classname) const
{
  //This function checks if the TObject which pointer is uptr belong to or is
  //derived from class classname.

  PRINTF4(this,"\tTObject* QSigExIOAbstract::TryCast(const TObject* uptr<",uptr,">) const\n")

  TObject* tobuf=uptr->IsA()->GetBaseClass(classname);
  if(!tobuf){
    cout << "ERROR:'" << uptr->ClassName() << "' is not derived from '" << classname << "'\n";
    throw 1;
  }
  return tobuf;
}

void QSigExIO::Streamer(TBuffer &R__b)
{
  // Stream an object of class QSigExIO.

  UInt_t R__s, R__c;
  if (R__b.IsReading()) {
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
    fFilename.Streamer(R__b);
    fDirectory.Streamer(R__b);
    fClassName.Streamer(R__b);
    R__b >> fObject;
    dynamic_cast<TH1*>(fObject)->SetDirectory(0);
    R__b.CheckByteCount(R__s, R__c, QSigExIO::IsA());
  } else {
    R__c = R__b.WriteVersion(QSigExIO::IsA(), kTRUE);
    fFilename.Streamer(R__b);
    fDirectory.Streamer(R__b);
    fClassName.Streamer(R__b);
    R__b << fObject;
    R__b.SetByteCount(R__c, kTRUE);
  }
}

#include "debugger.h"







