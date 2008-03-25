// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QSigExDirHandler.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"



//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExDirHandler                                                     //
//                                                                      //
// This class is an abstract base class that provides a basic interface //
// for all the classes that need to handle the TDirectory instance in   //
// which QSigEx stores its information. A new handler can be created by //
// deriving a new class from QSigExDirHandler.                          //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
                                                                      
ClassImp(QSigExDirHandler)

void QSigExDirHandler::GetObjs(TList* list, TDirectory* dir)
{
  //This function loads all the non-TDirectory derived objects of dir into
  //memory and fills the list with pointers of all derived objects. The
  //kCanDelete bit is set for all the objects that have been loaded into memory
  //by the function.

  PRINTF6(this,"\tconst QSigExDirHandler::TList* GetObjs(TList* list<",list,
      	">, TDirectory* dir<",dir,">)\n")

  try{
    //Unset the ownership of list on its objects
    list->SetOwner(kFALSE);
    //Get the list of keys in dir
    TList* dirkeylist=dir->GetListOfKeys();

    TObject* objbuf; //TObject buffer
    
    Int_t i;  //Iterator
    
    //Loop on the list of objects (loaded in memory) in dir
    for(i=0;i<dir->GetList()->GetSize();i++) 
      //If the object is not derived from TDirectory
      if(!dynamic_cast<TDirectory*>(dir->GetList()->At(i))){
	//Add it to the list
	list->AddLast(dir->GetList()->At(i));
      }
    
    //Loop on the list of keys in dir
    for(i=0; i< dirkeylist->GetSize();i++){
      //If there's no object in list having the same name than the current
      //key and if the class of the key is not derived from TDirectory
      if(!(list->FindObject(dirkeylist->At(i)->GetName())) &&
	 !gROOT->GetClass(dynamic_cast<TKey*>(dirkeylist->At(i))->
		 GetClassName())->GetBaseClass("TDirectory")
	 && !(dir->FindObject(dirkeylist->At(i)->GetName()))){
	//Load the object corresponding to key in memory
	objbuf=dir->Get(dirkeylist->At(i)->GetName());
	//If the object is not derived from TDirectory
	if(!dynamic_cast<TDirectory*>(objbuf)){
	  //Allow list to delete the object when TList::Clear is called
	  objbuf->SetBit(kCanDelete);
	  //Add the new object to the list
	  list->AddLast(objbuf);
	  //Else if the object is derived from TDirectory
	}else{
	  //delete it properly
	  dir->RecursiveRemove(objbuf);
	  //RecursiveRemove does not delete the object in memory!
	  delete objbuf;
	}
      }
    }
  }catch(Int_t e){
    cout << "Exception handled by QSigExDirHandler::GetObjs\n";
    throw e;
  }
}

void QSigExDirHandler::GetDirs(TList* list, TDirectory* dir)
{
  //This function loads all the TDirectory derived objects of dir into memory
  //and fills the list with pointers of all TDirectory derived objects.

  PRINTF6(this,"\tconst QSigExDirHandler::TList* GetDirs(TList* list<",list,
		  ">, TDirectory* dir<",dir,">)\n")

  try{
    //Unset the ownership of list on its objects
    list->SetOwner(kFALSE);  
    //Get the list of keys in dir
    TList* dirkeylist=dir->GetListOfKeys();

    TObject* objbuf;  //TObject buffer
    
    Int_t i; //iterator
    
    //Loop on the list of objects (loaded in memory) in dir
    for(i=0;i<dir->GetList()->GetSize();i++)
      //If the object is derived from TDirectory
      if(dynamic_cast<TDirectory*>(dir->GetList()->At(i))){
	//Add it to list
	list->AddLast(dir->GetList()->At(i));
      }
    
    //Loop on the list of keys in dir
    for(i=0; i< dirkeylist->GetSize();i++){
      //If there's no object in list and in dir having the same name than the
      //current key and if the class of the key is not derived from TDirectory
      if(!(list->FindObject(dirkeylist->At(i)->GetName())) &&
	 gROOT->GetClass(dynamic_cast<TKey*>(dirkeylist->At(i))->
		 GetClassName())->GetBaseClass("TDirectory")
	 && !(dir->FindObject(dirkeylist->At(i)->GetName()))){
	//Load the object corresponding to key in memory
	objbuf=dir->Get(dirkeylist->At(i)->GetName());
	//If the object is derived from TDirectory
	if(dynamic_cast<TDirectory*>(objbuf)){
	  //Add it to the list
	  list->AddLast(objbuf);
	  //else delete it
	}else{
	  delete objbuf;
	}
      }
    }
  }catch(Int_t e){
    cout << "Exception handled by QSigExDirHandler::GetDirs\n";
    throw e;
  }
}

void QSigExDirHandler::GetListOfObjsKeys(TList* list, TDirectory* dir)
{
  //This function returns a list that is the concatenation of lists
  //TDirectory::GetList() and TDirectory::GetListOfKeys()
 
  Int_t i;

  for(i=0;i<dir->GetList()->GetSize();i++)
    list->AddLast(dir->GetList()->At(i));
  for(i=0;i<dir->GetListOfKeys()->GetSize();i++)
    list->AddLast(dir->GetListOfKeys()->At(i));

}

Int_t QSigExDirHandler::DelObjsKeys(const Char_t* name, TDirectory* dir)
{
  //This function recursively deletes the object instance and/or all the TKeys
  //that have the name name. It returns the number of deleted objects in the
  //first level (count 1 for 1 deleted folder in the first level)

  Int_t ret=0;

  TKey* keybuf; //TKwy buffer
  //If there's an object with name name in dir
  Int_t i;
  TObject *objbuf;
  //Loop over the object in dir directory
  for(i=0;i<dir->GetList()->GetSize();i++){
    //Get a pointer to the current object
    objbuf=dir->GetList()->At(i);
    //If the current object name is name
    if(!strcmp(objbuf->GetName(),name)){
      //delete it properly
      dir->RecursiveRemove(objbuf);
      //RecursiveRemove does not delete the object in memory!
      delete objbuf;
      //Increment the number of deleted objects
      ret++;
    }
  }
  TString strbuf;
  //Loop over the keys in dir with name name
  while((keybuf=dir->FindKey(name))){
    strbuf=keybuf->GetName();
    strbuf+=";";
    strbuf+=keybuf->GetCycle();
    dir->Delete(strbuf);
    //Increment the number of deleted objects
    ret++;
  }

  //Return the number of deleted objects
  return ret;
}

Bool_t QSigExDirHandler::FindObjKey(const Char_t* name, const TDirectory* dir) const
{
  //This function return kTRUE if dir contains a TKey or TObject with name
  //name

  if(dir->FindKey(name)) return kTRUE;
  if(dir->FindObject(name)) return kTRUE;
  Int_t i;
  TObject *objbuf;
  for(i=0;i<dir->GetList()->GetSize();i++){
    //Get a pointer to the current object
    objbuf=dir->GetList()->At(i);
    //If the current object name is name
    if(!strcmp(objbuf->GetName(),name)){
      return kTRUE;
    }
  }
  return kFALSE;
}

TDirectory* QSigExDirHandler::CopyDir(TDirectory* fromdir, TDirectory* todir)
{
  //This function is intended to copy recursively TDirectory objects
  //It's not implemented at the moment

  /*  TList* listbuf=dynamic_cast<TList*>(fromdir->GetListOfKeys());
  TKey* keybuf;
  TObject* objbuf;
  Int_t i;

  if(listbuf){
    for(i=0;i<listbuf->GetSize();i++){
      keybuf=dynamic_cast<TKey*>(listbuf->At(I));
      objbuf=keybuf->ReadObj();
    }
    }*/

  fromdir=NULL;

  return todir;
}

TDirectory* QSigExDirHandler::SetDirectory(TDirectory* dir, TObject* obj)
{
  //This function allow to change the owner TDirectory of ROOT objects that have
  //an implemented SetDirectory function or their derived classes . The function
  //does nothing with other objects. It returns dir

  if(dynamic_cast<TChain*>(obj)){
    dynamic_cast<TChain*>(obj)->SetDirectory(dir);
  } else if(dynamic_cast<TEventList*>(obj)){
    dynamic_cast<TEventList*>(obj)->SetDirectory(dir);
  } else if(dynamic_cast<TH1*>(obj)){
    dynamic_cast<TH1*>(obj)->SetDirectory(dir);
  } else if(dynamic_cast<TH2*>(obj)){
    dynamic_cast<TH2*>(obj)->SetDirectory(dir);
  } else if(dynamic_cast<TTree*>(obj)){
    dynamic_cast<TTree*>(obj)->SetDirectory(dir);
  } else dir->Add(obj);

  return dir;
}

void QSigExDirHandler::CheckCardNFields(Int_t nfields, Int_t min, Int_t max)
{
  //This function throw an exception if the following expression is not true:
  //min<=nfields<=max
  if(max==-1) max=nfields;
  if(nfields<min || nfields>max){
    cout << "Error in card file: wrong number of fields\n";
    throw 1;
  }
}

#include "debugger.h"
