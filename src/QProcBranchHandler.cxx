#include "QProcBranchHandler.h"

ClassImp(QProcBranchHandler)

QList<TObject*> QProcBranchHandler::fIFiles;
QList<Int_t> QProcBranchHandler::fNObjReqIFiles;
QList<TObject*> QProcBranchHandler::fOFiles;
QList<Int_t> QProcBranchHandler::fNObjReqOFiles;
QList<TObject*> QProcBranchHandler::fTBObjs;
QList<TObject*> QProcBranchHandler::fQPTBWObjs;
QList<Int_t> QProcBranchHandler::fNObjReqTB;
Bool_t QProcBranchHandler::fSaveOutputs=kTRUE;

QProcArray* QProcBranchHandler::LoadBranch(const char *treelocation, const char *adesc, Bool_t isoutput, Bool_t incrdeps)
{
  TDirectory *dbuf;
  QList<TString> donbuf=QFileUtils::DecodeObjName(treelocation);
  QList<TString> dpn;
  TString bname;
  Int_t btype=GetNameTypeID(adesc,&bname);
  Int_t i;
  TTree *tbuf;
  TBranch *bbuf;
  QProcArray *qabuf;

  if(!donbuf.Count()) {
    fprintf(stderr,"QProcBranchHandler::LoadBranch: Error: Invalid tree location: '%s'\n",treelocation);
    throw 1;
  }

  switch (isoutput) {
    case kTRUE:

      if(btype==-1) btype=kDouble;

      //If the tree is located in a file
      if(donbuf.Count() == 2) {

	//If the file is already opened
	if((dbuf=gDirectory->GetDirectory(donbuf[1]+":/"))) {
	  dbuf->cd();

	  if(!gDirectory->IsWritable()) {
	    fprintf(stderr,"QProcBranchHandler::LoadBranch: Error: File '%s' is not writable\n",donbuf[1].Data());
	    throw 1;
	  }

	  //If the file has been opened previously by QProcBranchHandler in rw mode
	  if((i=fOFiles.FindFirst(dbuf)) != -1) {
	    //Increment the number of output branches that require that file
	    if(incrdeps) fNObjReqOFiles[i]++;
	  }

	  //Else if the file is not opened
	} else {
	  if((gSystem->AccessPathName(gSystem->DirName(donbuf[1]),kWritePermission) || gSystem->AccessPathName(gSystem->DirName(donbuf[1]),kExecutePermission))
	      && !gSystem->AccessPathName(donbuf[1],kFileExists) && gSystem->AccessPathName(donbuf[1],kWritePermission)) {
	    fprintf(stderr,"QProcBranchHandler::LoadBranch: Error: File '%s' cannot be opened for writing\n",donbuf[1].Data());
	    throw 1;
	  }

	  //Open the file
	  fOFiles.Add(new TFile(donbuf[1],"update"));
	  if(incrdeps) fNObjReqOFiles.Add(1);
	  else fNObjReqOFiles.Add(0);
	}
      }
      //Decode the path to the object
      dpn=QFileUtils::DecodePathName(donbuf[0]);

      //Access the directory where the tree has to be created and create directories if necessary
      for(i=0; i<dpn.Count()-1; i++) {

	if(!(dbuf=gDirectory->GetDirectory(dpn[i]))) {
	  dbuf=gDirectory->mkdir(dpn[i]);

	  if(!dbuf) {
	    fprintf(stderr,"QProcBranchHandler::LoadBranch: Error: Directory '%s' cannot be created in location '%s'\n",dpn[i].Data(),gDirectory->GetPath());
	    throw 1;
	  }
	}
	dbuf->cd();
      }

      //If the tree does not exist
      if(!(tbuf=dynamic_cast<QProcTree*>(gDirectory->Get(dpn.GetLast())))) {
	//Create the output tree
	tbuf=new QProcTree(dpn.GetLast(),dpn.GetLast());
      }
      dpn.Clear();

      //If the branch already exists
      if((bbuf=tbuf->GetBranch(bname))) {

	//If the branch is a not QProcBranch
	if(!(qabuf=dynamic_cast<QProcBranch*>(bbuf))) {

	  //If there is no wrapper that have been created for that branch, create one
	  if((i=fTBObjs.FindFirst(bbuf)) == -1) {
	    fTBObjs.Add(bbuf);
	    qabuf=new QProcTBranchWrapper(bbuf);
	    fQPTBWObjs.Add((TObject*)qabuf);
	    fNObjReqTB.Add(0);
	    i=fQPTBWObjs.Count()-1;
	  }
	  //Return a pointer to the wrapper
	  fNObjReqTB[i]++;
	}

	if(qabuf->GetBTypeID()!=btype) {
	  fprintf(stderr,"QProcBranchHandler::LoadBranch(): Error: Existing branch '%s' in tree '%s:%s' has type '%s' instead of '%s'\n",bname.Data(),donbuf[1].Data(),donbuf[0].Data(),GetTypeName(qabuf->GetBTypeID()),GetTypeName(btype));
	  throw 1;
	}
	return qabuf;
      }

      //Create the branch
      return dynamic_cast<QProcBranch*>(tbuf->Branch(bname,NULL,bname+"/"+GetTypeSName(btype)));
      break;

    case kFALSE:

      //If the tree is located in a file
      if(donbuf.Count() ==2) {

	//If the file is already opened
	if((dbuf=gDirectory->GetDirectory(donbuf[1]+":/"))) {
	  dbuf->cd();

	  //If the file has been opened previously by QProcBranchHandler in rw mode
	  if((i=fOFiles.FindFirst(dbuf)) != -1) {
	    //Increment the number of output branches that require that file
	    if(incrdeps) fNObjReqOFiles[i]++;
	  }

	  //If the file has been opened previously by QProcBranchHandler in read-only mode
	  if((i=fIFiles.FindFirst(dbuf)) != -1) {
	    //Increment the number of input branches that require that file
	    if(incrdeps) fNObjReqIFiles[i]++;
	  }

	  //Else if the file is not opened
	} else {

	  if(gSystem->AccessPathName(donbuf[1],kReadPermission)) {
	    fprintf(stderr,"QProcBranchHandler::LoadBranch: Error: File '%s' cannot be read\n",donbuf[1].Data());
	    throw 1;
	  }

	  //Open the file
	  fIFiles.Add(new TFile(donbuf[1],"read"));
	  if(incrdeps) fNObjReqIFiles.Add(1);
	  else fNObjReqIFiles.Add(0);
	}
      }
      //Decode the path to the object
      dpn=QFileUtils::DecodePathName(donbuf[0]);

      //Access the directory from where the tree has to be read
      for(i=0; i<dpn.Count()-1; i++) {

	if(!(dbuf=gDirectory->GetDirectory(dpn[i]))) {
	  fprintf(stderr,"QProcBranchHandler::LoadBranch: Directory '%s' does not exist in location '%s'\n",dpn[i].Data(),gDirectory->GetPath());
	  throw 1;
	}
	dbuf->cd();
      }

      //Load the input tree
      if(!(tbuf=dynamic_cast<TTree*>(gDirectory->Get(dpn.GetLast())))) {
	fprintf(stderr,"QProcBranchHandler::LoadBranch: Tree '%s:%s' does not exist\n",donbuf[1].Data(),donbuf[0].Data());
	throw 1;
      }
      dpn.Clear();

      //Get a pointer to the branch
      if(!(bbuf=tbuf->GetBranch(bname))) {
	fprintf(stderr,"QProcBranchHandler::LoadBranch(): Error: Branch '%s' does not exist in tree '%s:%s'\n",bname.Data(),donbuf[1].Data(),donbuf[0].Data());
	throw 1;
      }

      //If the branch is not a QProcBranch
      if(!(qabuf=dynamic_cast<QProcBranch*>(bbuf))) {

	//If there is no wrapper that have been created for that branch, create one
	if((i=fTBObjs.FindFirst(bbuf)) == -1) {
	  fTBObjs.Add(bbuf);
	  qabuf=new QProcTBranchWrapper(bbuf);
	  fQPTBWObjs.Add((TObject*)qabuf);
	  fNObjReqTB.Add(0);
	  i=fQPTBWObjs.Count()-1;
	}
	//Return a pointer to the wrapper
	fNObjReqTB[i]++;
      }

      if(btype!=-1 && qabuf->GetBTypeID()!=btype) {
	fprintf(stderr,"QProcBranchHandler::LoadBranch(): Error: Existing branch '%s' in tree '%s:%s' has type '%s' instead of '%s'\n",bname.Data(),donbuf[1].Data(),donbuf[0].Data(),GetTypeName(qabuf->GetBTypeID()),GetTypeName(btype));
	throw 1;
      }

      return qabuf;
      break;

    default:
      return NULL;
  }
}

void QProcBranchHandler::UnloadBranch(TBranch *branch)
{
//  printf("void QProcBranchHandler::UnloadBranch(TBranch *branch<'%s'>)\n",branch->GetName());
  //Get a pointer to the file were is stored the tree associated to the branch
  TFile *fbuf=branch->GetTree()->GetCurrentFile();
  Int_t i;

  //If the branch is a TBranch with a wrapper object
  if((i=fTBObjs.FindFirst((TObject*)branch)) != -1){
//    printf("The branch is a TBranch with a wrapper object\n");
    fNObjReqTB[i]--;

    if(!fNObjReqTB[i]) {
//      printf("Deleting the wrapper object...\t");
      delete (QProcTBranchWrapper*)fQPTBWObjs[i];
      fTBObjs.Del(i);
      fQPTBWObjs.Del(i);
      fNObjReqTB.Del(i);
//      printf("Done\n");
    }
  }

  //If the file is an input file loaded by this class
  if((i=fIFiles.FindFirst((TObject*)fbuf)) != -1) {
//    printf("Associated file is an input file loaded by this class\n");
    //Decrement the number of objects requiring this file
    fNObjReqIFiles[i]--;

    //If no object requires this files anymore
    if(!fNObjReqIFiles[i]) {
//      printf("Closing and deleting the input file...\t");
      fbuf->Close();
      delete fbuf;
      fIFiles.Del(i);
      fNObjReqIFiles.Del(i);
//      printf("Done\n");
    }
  }

  //If the file is an output file loaded by this class
  if((i=fOFiles.FindFirst((TObject*)fbuf)) != -1) {
//    printf("Associated file is an output file loaded by this class\n");
    //Decrement the number of objects requiring this file
    fNObjReqOFiles[i]--;

    //If no object requires this files anymore
    if(!fNObjReqOFiles[i]) {
//      printf("Saving, closing and deleting the output file...\t");
      if(fSaveOutputs) fbuf->Write();
      fbuf->Close();
      delete fbuf;
      fOFiles.Del(i);
      fNObjReqOFiles.Del(i);
//      printf("Done\n");
    }
  }
  //printf("Status of QProcBranchHandler:\n");
  //printf("Number of input files: %i\n",fIFiles.Count());
  //printf("Number of output files: %i\n",fOFiles.Count());
  //printf("Number of TBranch objects: %i\n",fTBObjs.Count());
}
