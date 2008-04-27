#include "QTTreeProcessor.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QTTreeProcessor)

QTTreeProcessor::~QTTreeProcessor()
{
  PRINTF2(this,"\tQTTreeProcessor::~QTTreeProcessor()\n")
  delete fProcs;
  fProcs=NULL;
  delete fParams;
  fParams=NULL;
  delete fLastParams;
  fLastParams=NULL;
  delete fParamsNames;
  fParamsNames=NULL;
  delete fITNames;
  fITNames=NULL;
  delete fOTNames;
  fOTNames=NULL;
  delete fIBNames;
  fIBNames=NULL;
  delete fOBNames;
  fOBNames=NULL;
  delete fBuNames;
  fBuNames=NULL;
  delete fIFiles;
  fIFiles=NULL;
  delete fOFiles;
  fOFiles=NULL;
  delete fIBranches;
  fIBranches=NULL;
  delete fOBranches;
  fOBranches=NULL;
  delete fProcsDepends;
  fProcsDepends=NULL;
  delete fITRequired;
  fITRequired=NULL;
  delete fIBRequired;
  fIBRequired=NULL;
  delete fOTDepends;
  fOTDepends=NULL;
  delete fOBDepends;
  fOBDepends=NULL;
  delete fIBBuffers;
  fIBBuffers=NULL;
  delete fOBBuffers;
  fOBBuffers=NULL;
  delete fIBCBuffers;
  fIBCBuffers=NULL;
  delete fIBCBTypes;
  fIBCBTypes=NULL;
  delete fBuffers;
  fBuffers=NULL;
}

void QTTreeProcessor::AddParam(const char *parname, Int_t index)
{
  fParamsNames->Add(parname,index);
  fParams->Add(0.0,index);
}

void QTTreeProcessor::AddProc(const char *name, const char *title, Int_t index)
{
  PRINTF8(this,"\tQTTreeProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, Int_t index<",index,"<)\n")
  fProcs->RedimList(fProcs->Count()+1,index);
  (*fProcs)[index].SetNameTitle(name,title);
  fProcsDepends->RedimList(fProcsDepends->Count()+1,index);
}

void QTTreeProcessor::AddProc(const char *name, const char *title, void (*proc)(Double_t**, Double_t**, Double_t**),const char *procname, Int_t index)
{
  PRINTF12(this,"\tQTTreeProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, void (*proc)(Double_t**, Double_t**, Double_t**)<",proc,">, const char *procname<'",procname,"'>, Int_t index<",index,"<)\n")
  AddProc(name,title,index);
  (*fProcs)[index].SetProc(proc,procname);
  fProcsDepends->RedimList(fProcsDepends->Count()+1,index);
}

void QTTreeProcessor::AddProc(const char *name, const char *title, const char *procname, Int_t index)
{
  PRINTF10(this,"\tQTTreeProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, const char *procname<'",procname,"'>, Int_t index<",index,"<)\n")
  AddProc(name,title,index);
  (*fProcs)[index].SetProc(procname);
  fProcsDepends->RedimList(fProcsDepends->Count()+1,index);
}

void QTTreeProcessor::AddProc(const char *name, const char *title, void *proc, const char *procname, Int_t index)
{
  PRINTF12(this,"\tQTTreeProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, void *proc<",proc,">, const char *procname<'",procname,"'>, Int_t index<",index,"<)\n")
  AddProc(name,title,index);
  (*fProcs)[index].SetProc(proc,procname);
  fProcsDepends->RedimList(fProcsDepends->Count()+1,index);
}

Int_t QTTreeProcessor::Analyze()
{
  Int_t i,j;
  QNamedProc *proc;
  QList<TString> params; //Parameters
  QList<QMask>   bdepends; //Dependencies of output buffers on parameters
  TString sbuf;
  QList<TString> donbuf;       //Decoded object name buffer

  fAnalysisDir=gDirectory->GetPath();

  Int_t nprocs=fProcs->Count();
  Int_t ninputs, noutputs, nparams;
  Int_t pidx, iidx, oidx, ibidx, obidx;

  QList<QMask> procsddepends; //Direct dependencies of processes

  fITNames->Clear();
  fOTNames->Clear();
  fIBNames->Clear();
  fOBNames->Clear();
  fITRequired->Clear();
  fIBRequired->Clear();
  fOTDepends->Clear();
  fOBDepends->Clear();

  procsddepends.RedimList(fProcsDepends->Count());

  //Loop over the processes
  for(i=0; i<nprocs; i++) {
    proc=&((*fProcs)[i]);
    //printf("Process '%s'\n",proc->GetName());
    ninputs=proc->GetNInputs();
    noutputs=proc->GetNOutputs();
    nparams=proc->GetNParams();

    //Loop over the parameters for the current process
    for(j=0; j<nparams; j++) {
      sbuf=proc->GetParam(j).GetName();

      //If the current parameter is not listed in the list of parameters
      pidx=fParamsNames->FindFirst(sbuf);

      if(pidx == -1) {
	fprintf(stderr,"QTTreeProcessor::Analyze(): Error with process '%s': Parameter '%s' does not exist\n",proc->GetName(),sbuf.Data());
	return -1;
      }
      //Turn on the bit of the dependency mask for the current parameter
      procsddepends[i].SetBit(pidx,1);
    }

    (*fProcsDepends)[i]=procsddepends[i];

    //Loop over the inputs for the current process
    for(j=0; j<ninputs; j++) {
      sbuf=proc->GetInput(j);
      //printf("Encoded Input: '%s'\t%s\n",sbuf.Data(),proc->GetInput(j).GetName());

      //If the input is from a tree
      if(sbuf.Length()) {
	//printf("Input from a tree\n");
	donbuf=QFileUtils::DecodeObjName(sbuf);

	if(!donbuf.Count()) {
	  fprintf(stderr,"QTTreeProcessor::Analyze(): Error: Invalid input object name: '%s'\n",sbuf.Data());
	  return -1;
	}

	//printf("Decoded Input: %s",donbuf[0].Data());
	//if(donbuf.Count() ==2) printf("\t%s",donbuf[1].Data());
	//printf("\t%s\n",proc->GetInput(j).GetName());

	//Ensure the required tree is in the list
	iidx=fITNames->AddUnique(donbuf);

	//If the tree is not already listed in the list of existing trees
	if(iidx == -1) {
	  //printf("New input tree\n");
	  //Add the name of the required branch
	  fIBNames->RedimList(fITNames->Count());
	  fIBNames->GetLast().Add(proc->GetInput(j).GetName());
	  iidx=fIBRequired->Count();
	  fIBRequired->RedimList(fIBNames->Count());
	  fIBRequired->GetLast().RedimList(1);
	  ibidx=0;

	  //Else if the tree is already listed
	} else {
	  //printf("Existing input tree\n");
	  //Ensure the required branch is in the list
	  ibidx=(*fIBNames)[iidx].AddUnique(proc->GetInput(j).GetName());

	  //If the branch for the current input was not in the list
	  if(ibidx == -1) {
	    ibidx=(*fIBRequired)[iidx].Count();
	    (*fIBRequired)[iidx].RedimList((*fIBNames)[iidx].Count());
	  }
	}

	oidx=fOTNames->FindFirst(donbuf);

	//If the tree has been generated by a previous process
	if(oidx != -1) {
	  //printf("Intermediary input\n");
	  obidx=(*fOBNames)[oidx].FindFirst(proc->GetInput(j).GetName());

	  //If the branch has not been listed in the list of generated branches
	  if(obidx == -1) {
	    fprintf(stderr,"QTTreeProcessor::Analyze(): Error with process '%s': Input branch '%s' for tree '%s' was not generated by a previous process\n",proc->GetName(),proc->GetInput(j).GetName(),((*fOTNames)[oidx][1]+"/"+(*fOTNames)[oidx][0]).Data());
	    return -1;

	  } else {
	    //Add the dependencies of the input branch to the dependency mask for the current process
	    (*fProcsDepends)[i]|=(*fOBDepends)[oidx][obidx];
	    //Set the mask for the current input branch to the mask for direct dependencies of the current process minus the mask for the corresponding output branch.
	    //This ensures the branch value is not read if is is generated by another process
	    (*fIBRequired)[iidx][ibidx]|=procsddepends[i]&~(*fOBDepends)[oidx][obidx];
	  }

	  //Else if the tree is not also an output tree
	} else {
	  //Set the mask for the current input branch to the mask for direct dependencies of the current process
	  (*fIBRequired)[iidx][ibidx]|=procsddepends[i];
	}

	//Else if the input is from a buffer in memory
      } else {
	//printf("Input from a memory buffer\n");
	iidx=fBuNames->FindFirst(proc->GetInput(j).GetName());

	//If the buffer has not been listed as an output for a previous process
	if(iidx == -1) {
	  fprintf(stderr,"QTTreeProcessor: Analyze(): Error with process '%s': Input buffer '%s' was not generated by a previous process\n",proc->GetName(),proc->GetInput(j).GetName());
	  return -1;

	} else {
	  //Add the dependencies of the last recorded buffer value to the dependency mask for the current process
	  (*fProcsDepends)[i]|=bdepends[iidx];
	}
      }
    }
    donbuf.Clear();

    //Loop over the outputs for the current process
    for(j=0; j<noutputs; j++) {
      sbuf=proc->GetOutput(j);
      //printf("Encoded Output: '%s'\t%s\n",sbuf.Data(),proc->GetOutput(j).GetName());

      //If the output is in a tree
      if(sbuf.Length()) {
	//printf("Output to a tree\n");
	donbuf=QFileUtils::DecodeObjName(sbuf);

	if(!donbuf.Count()) {
	  fprintf(stderr,"QTTreeProcessor::Analyze(): Error: Invalid output object name: '%s'\n",sbuf.Data());
	  return -1;
	}

	//printf("Decoded Output: %s",donbuf[0].Data());
	//if(donbuf.Count() ==2) printf("\t%s",donbuf[1].Data());
	//printf("\t%s\n",proc->GetOutput(j).GetName());

	oidx=fOTNames->AddUnique(donbuf);

	//If this tree has not been generated by a previous process
	if(oidx == -1) {

	  if(fITNames->FindFirst(donbuf) != -1) {
	    fprintf(stderr,"QTTreeProcessor: Analyze(): Error with process '%s': Branch '%s' for tree '%s' cannot be overwritten\n",proc->GetName(),proc->GetInput(j).GetName(),(donbuf[1]+"/"+donbuf[0]).Data());
	    return -1;
	  }
	  //printf("New output tree\n");
	  //Add the name of the output branch
	  fOBNames->RedimList(fOTNames->Count());
	  fOBNames->GetLast().Add(proc->GetOutput(j).GetName());
	  //Add a mask for the output branch and set it to the mask value for the current process
	  fOBDepends->RedimList(fOTNames->Count());
	  fOBDepends->GetLast().Add((*fProcsDepends)[i]);

	  //Else if the tree is already listed
	} else {
	  //printf("Existing output tree\n");
	  //Ensure the output branch is in the list
	  obidx=(*fOBNames)[oidx].AddUnique(proc->GetOutput(j).GetName());
	  //Add an extra dependency mask if this is a new output branch
	  //Set the dependency mask value to the mask value for the current process
	  if(obidx==-1) (*fOBDepends)[oidx].Add((*fProcsDepends)[i]);
	  else (*fOBDepends)[oidx][obidx]=(*fProcsDepends)[i];
	}

	//Else if the output is to a buffer in memory
      } else {
	//printf("Output to a memory buffer\n");
	//Ensure the output memory buffer is in the list
	oidx=fBuNames->AddUnique(proc->GetOutput(j).GetName());
	//Add an extra dependency mask if this is a new output branch
	//Set the dependency mask value to the mask value for the current process
	if(oidx==-1) bdepends.Add((*fProcsDepends)[i]);
	else bdepends[oidx]=(*fProcsDepends)[i];
      }
    }
  }

  //Loop over the input trees
  for(i=0; i<fITNames->Count(); i++) {

    //If the input tree is listed as an output tree
    if((oidx=fOTNames->FindFirst((*fITNames)[i])) != -1) {

      //Loop over the input branches
      for(j=0; j<(*fIBNames)[i].Count(); j++) {

	//Get the index of the corresponding output branch
	obidx=(*fOBNames)[oidx].FindFirst((*fIBNames)[i][j]);

	//Remove the mask for the output from the mask for the input (we don't want to read the value from the tree if it is being generated by a previous process
//	(*fIBRequired)[i][j]&=~(*fOBDepends)[oidx][obidx];
      }
    }
  }

  bdepends.Clear(); //! bdepends values are not valid outside of the previous loop

  //Loop over the processes
  for(i=0; i<nprocs; i++) {
    proc=&((*fProcs)[i]);

    //Loop over the input parameters
    for(j=0; j<proc->GetNInputs(); j++) {
      sbuf=proc->GetInput(j);
      donbuf.Clear();

      //If the input is not from a memory buffer
      if(sbuf.Length()) {
	donbuf=QFileUtils::DecodeObjName(sbuf);

	//If this is an  PLD
      }
    }
  }

 fITRequired->RedimList(fITNames->Count());

  //Loop over the input trees
  for(i=0; i<fITRequired->Count(); i++) {

    //Set the dependency mask for the tree to be the combination of the masks for its branches
    for(j=0; j<(*fIBRequired)[i].Count(); j++) (*fITRequired)[i]|=(*fIBRequired)[i][j];
  }

  fOTDepends->RedimList(fOTNames->Count());

  //Loop over the output trees
  for(i=0; i<fOTDepends->Count(); i++) {

    //Set the dependency mask for the tree to be the combination of the masks for its branches
    for(j=0; j<(*fOBDepends)[i].Count(); j++) (*fOTDepends)[i]|=(*fOBDepends)[i][j];
  }

  return 0;
}

void QTTreeProcessor::DelParam(const char *paramname)
{
  Int_t i;
  if((i=FindParamIndex(paramname))!=-1) DelParam(i);
}

void QTTreeProcessor::DelProc(const char *procname)
{
  Int_t i;
  if((i=FindProcIndex(procname))!=-1) {
    fProcs->Del(i);
    fProcsDepends->Del(i);
  }
}

void QTTreeProcessor::Exec()
{
  QMask pardiffs;
  Int_t i;

  if(fLastParams->Count() == fParams->Count()) {

    for(i=0; i<fParams->Count(); i++) {

      if((*fParams)[i] != (*fLastParams)[i]) pardiffs.SetBit(i,1);
    }
  } else {
    pardiffs.FillMask(fParams->Count());
  }

  printf("Mask for the current parameters: ");
  pardiffs.Print();
}

Int_t QTTreeProcessor::FindParamIndex(const char *paramname) const
{
  for(Int_t i=0; i<fParams->Count(); i++){
    if(!strcmp((*fParamsNames)[i],paramname)) return i;
  }
  return -1;
}

Int_t QTTreeProcessor::FindProcIndex(const char *procname) const
{
  for(Int_t i=0; i<fProcs->Count(); i++){
    if(!strcmp((*fProcs)[i].GetName(),procname)) return i;
  }
  return -1;
}

QNamedProc& QTTreeProcessor::GetProc(const char *procname) const
{
  Int_t i;
  if((i=FindProcIndex(procname))!=-1){
    return (*fProcs)[i];
  }
  fprintf(stderr,"QTTreeProcessor::GetProc: Procedure '%s' does not exist\n",procname);
  throw 1;
  return (*fProcs)[0];
}

Int_t QTTreeProcessor::InitProcess()
{
  TDirectory *curdir=gDirectory;
  TDirectory *dbuf;
  Int_t i,j,k;
  QList<TString> dpn;
  TTree *tbuf;
  Int_t tidx;

  fIFiles->Clear();
  fOFiles->Clear();
  fIBranches->Clear();
  fOBranches->Clear();

  fIBranches->RedimList(fITNames->Count());
  fOBranches->RedimList(fOTNames->Count());
  fIBBuffers->RedimList(fITNames->Count());
  fOBBuffers->RedimList(fOTNames->Count());
  fBuffers->RedimList(fBuNames->Count());

  if(!(dbuf=gDirectory->GetDirectory(fAnalysisDir))) {
    fprintf(stderr,"QTTreeProcessor::InitProcess(): Error: Directory '%s' does not exist\n",fAnalysisDir.Data());
    return 1;
  }
  dbuf->cd();

  //Loop over the output trees
  for(i=0; i<fOTNames->Count(); i++) {

    //If the tree is located in a file
    if((*fOTNames)[i].Count() == 2) {

      //If the file is already opened
      if((dbuf=gDirectory->GetDirectory((*fOTNames)[i][1]+":/"))) {
	dbuf->cd();

	if(!gDirectory->IsWritable()) {
	  fprintf(stderr,"QTTreeProcessor::InitProcess(): Error: File '%s' is not writable\n",(*fOTNames)[i][1].Data());
	  return 1;
	}

	//Else if the file is not opened
      } else {
	if((gSystem->AccessPathName(gSystem->DirName((*fOTNames)[i][1]),kWritePermission) || gSystem->AccessPathName(gSystem->DirName((*fOTNames)[i][1]),kExecutePermission))
	    && !gSystem->AccessPathName((*fOTNames)[i][1],kFileExists) && gSystem->AccessPathName((*fOTNames)[i][1],kWritePermission)) {
	  fprintf(stderr,"QTTreeProcessor::InitProcess(): Error: File '%s' cannot be opened for writing\n",(*fOTNames)[i][1].Data());
	  return 1;
	}

	//Open the file
	fOFiles->Add(new TFile((*fOTNames)[i][1],"update"));
      }
    }
    //Decode the path to the object
    dpn=QFileUtils::DecodePathName((*fOTNames)[i][0]);

    //Access the directory where the tree has to be created and create directories if necessary
    for(j=0; j<dpn.Count()-1; j++) {

      if(!(dbuf=gDirectory->GetDirectory(dpn[j]))) {
	dbuf=gDirectory->mkdir(dpn[j]);

	if(!dbuf) {
	  fprintf(stderr,"QTTreeProcessor::InitProcess(): Directory '%s' cannot be created in location '%s'\n",dpn[j].Data(),gDirectory->GetPath());
	  return 1;
	}
      }
      dbuf->cd();
    }
    //Create the output tree
    tbuf=new TTree(dpn.GetLast(),dpn.GetLast());
    dpn.Clear();
    (*fOBranches)[i].RedimList((*fOBNames)[i].Count());
    (*fOBBuffers)[i].RedimList((*fOBNames)[i].Count());

    //Create the branches for this output tree
    for(j=0; j<(*fOBNames)[i].Count(); j++) {
      (*fOBranches)[i][j]=tbuf->Branch((*fOBNames)[i][j],&((*fOBBuffers)[i][j]),(*fOBNames)[i][j]+"/D");
    }
  }

  if(!(dbuf=gDirectory->GetDirectory(fAnalysisDir))) {
    fprintf(stderr,"QTTreeProcessor::InitProcess(): Error: Directory '%s' does not exist\n",fAnalysisDir.Data());
    return 1;
  }
  dbuf->cd();

  TLeaf *lbuf;
  const char* cabuf;
  ClearIBCBuffers();

  //Loop over the input trees
  for(i=0; i<fITNames->Count(); i++) {

    //If the input tree is not also an output tree
    if((tidx=fOTNames->FindFirst((*fITNames)[i])) == -1) {

      //If the tree is located in a file
      if((*fITNames)[i].Count() ==2) {

	//If the file is already opened
	if((dbuf=gDirectory->GetDirectory((*fITNames)[i][1]+":/"))) {
	  dbuf->cd();

	  //Else if the file is not opened
	} else {

	  if(gSystem->AccessPathName((*fITNames)[i][1],kReadPermission)) {
	    fprintf(stderr,"QTTreeProcessor::InitProcess(): Error: File '%s' cannot be read\n",(*fITNames)[i][1].Data());
	    return 1;
	  }

	  //Open the file
	  fIFiles->Add(new TFile((*fITNames)[i][1],"read"));
	}
      }
      //Decode the path to the object
      dpn=QFileUtils::DecodePathName((*fITNames)[i][0]);

      //Access the directory from where the tree has to be read
      for(j=0; j<dpn.Count()-1; j++) {

	if((dbuf=gDirectory->GetDirectory(dpn[j]))) {
	  fprintf(stderr,"QTTreeProcessor::InitProcess(): Directory '%s' does not exist in location '%s'\n",dpn[j].Data(),gDirectory->GetPath());
	  return 1;
	}
	dbuf->cd();
      }

      //Load the input tree
      if(!(tbuf=dynamic_cast<TTree*>(gDirectory->Get(dpn.GetLast())))) {
	fprintf(stderr,"QTTreeProcessor::InitProcess(): Tree '%s:/%s' does not exist\n",(*fITNames)[i][1].Data(),(*fITNames)[i][0].Data());
	return 1;
      }
      dpn.Clear();
      (*fIBranches)[i].RedimList((*fIBNames)[i].Count());
      (*fIBBuffers)[i].RedimList((*fIBNames)[i].Count());

      //Loop over the required branches for this input tree
      for(j=0; j<(*fIBNames)[i].Count(); j++) {

	if(!((*fIBranches)[i][j]=tbuf->GetBranch((*fIBNames)[i][j]))) {
	  fprintf(stderr,"QTTreeProcessor::InitProcess(): Error: Branch '%s' does not exist in tree '%s:/%s'\n",(*fIBNames)[i][j].Data(),(*fITNames)[i][1].Data(),(*fITNames)[i][0].Data());
	  return 1;
	}

	//Get a pointer to the leaf named according to the branch name
	if(!(lbuf=((TBranch*)(*fIBranches)[i][j])->GetLeaf((*fIBNames)[i][j]))) {
	  fprintf(stderr,"QTTreeProcessor::InitProcess(): Error: There is no leaf '%s' contained in branch '%s' from tree '%s:/%s'\n",(*fIBNames)[i][j].Data(),(*fIBNames)[i][j].Data(),(*fITNames)[i][1].Data(),(*fITNames)[i][0].Data());
	  return 1;
	}

	//Get the data type for the current branch from the tree
	cabuf=lbuf->GetTypeName();

	//If the data type is a Double_t, the buffer is assigned directly to the branch
	if(!strcmp(cabuf,"Double_t")) {
	  ((TBranch*)(*fIBranches)[i][j])->SetAddress(&((*fIBBuffers)[i][j]));

	  //Else if the input branch has a different basic data type, assign a different temporary buffer
	} else {

	  //Redim lists if this is the first input tree having branches with a different data type
	  if(!fIBCBuffers->Count()) {
	    fIBCBuffers->RedimList(fITNames->Count());
	    fIBCBTypes->RedimList(fITNames->Count());
	  }

	  //Redim lists if this is the first branch of the input tree having a  different data type
	  if(!(*fIBCBuffers)[i].Count()) {
	    (*fIBCBuffers)[i].RedimList((*fIBNames)[i].Count(),-1,NULL);
	    (*fIBCBTypes)[i].RedimList((*fIBNames)[i].Count(),-1,0);
	  }

	  if(!strcmp(cabuf,"Float_t")) {
	    (*fIBCBuffers)[i][j]=malloc(sizeof(Float_t));
	    (*fIBCBTypes)[i][j]=kFloat_t;

	  } else if(!strcmp(cabuf,"UInt_t")) {
	    (*fIBCBuffers)[i][j]=malloc(sizeof(UInt_t));
	    (*fIBCBTypes)[i][j]=kUInt_t;

	  } else if(!strcmp(cabuf,"Int_t")) {
	    (*fIBCBuffers)[i][j]=malloc(sizeof(Int_t));
	    (*fIBCBTypes)[i][j]=kInt_t;

	  } else if(!strcmp(cabuf,"UShort_t")) {
	    (*fIBCBuffers)[i][j]=malloc(sizeof(UShort_t));
	    (*fIBCBTypes)[i][j]=kUShort_t;

	  } else if(!strcmp(cabuf,"Short_t")) {
	    (*fIBCBuffers)[i][j]=malloc(sizeof(Short_t));
	    (*fIBCBTypes)[i][j]=kShort_t;

	  } else if(!strcmp(cabuf,"UChar_t")) {
	    (*fIBCBuffers)[i][j]=malloc(sizeof(UChar_t));
	    (*fIBCBTypes)[i][j]=kUChar_t;

	  } else if(!strcmp(cabuf,"Char_t")) {
	    (*fIBCBuffers)[i][j]=malloc(sizeof(Char_t));
	    (*fIBCBTypes)[i][j]=kChar_t;

	  } else if(!strcmp(cabuf,"Bool_t")) {
	    (*fIBCBuffers)[i][j]=malloc(sizeof(Bool_t));
	    (*fIBCBTypes)[i][j]=kBool_t;

	  } else {
	    fprintf(stderr,"QTTreeProcessor::InitProcess(): Error: The data type '%s' contained in branch '%s' from tree '%s:/%s' is not supported\n",cabuf,(*fIBNames)[i][j].Data(),(*fITNames)[i][1].Data(),(*fITNames)[i][0].Data());
	    return 1;
	  }

	  ((TBranch*)(*fIBranches)[i][j])->SetAddress((*fIBCBuffers)[i][j]);
	}
      }

      //Else if the input tree is also an output tree
    } else {
      (*fIBranches)[i].RedimList((*fIBNames)[i].Count());

      //Loop over the required branches for this input tree
      for(j=0; j<(*fIBNames)[i].Count(); j++) {

	//Set the branch address using the output tree list
	(*fIBranches)[i][j]=(*fOBranches)[tidx][(*fOBNames)[tidx].FindFirst((*fIBNames)[i][j])];
      }
    }
  }

  //Create output buffers
  fBuffers->RedimList(fBuNames->Count());

  Int_t nprocs=fProcs->Count();
  QNamedProc *proc;
  TString sbuf;
  QList<TString> donbuf;

  //Loop over the processes
  for(i=0; i<nprocs; i++) {
    proc=&((*fProcs)[i]);

    //Loop over the parameters for the current process
    for(j=0; j<proc->GetNParams(); j++) {
      //Set the address to the assign buffer for this parameter
      proc->SetParamBuf(j,&((*fParams)[fParamsNames->FindFirst(proc->GetParam(j).GetName())]));
    }

    //Loop over the inputs for the current process
    for(j=0; j<proc->GetNInputs(); j++) {
      sbuf=proc->GetInput(j);

      //If the input is not a memory buffer
      if(sbuf.Length()) {
	//Decode the object name
	donbuf=QFileUtils::DecodeObjName(sbuf);
	//Find the object in the list of inputs
	k=fITNames->FindFirst(donbuf); //Tree index

	//If there are input buffers for this tree (i.e. the tree is not also listed as an output tree)
	if((*fIBBuffers)[k].Count()) {
	//Get the buffer address from the input branches buffers list
	proc->SetInputBuf(j,&((*fIBBuffers)[k][(*fIBNames)[k].FindFirst(proc->GetInput(j).GetName())]));

	//Else if there are no input buffers
	} else {
	  //Find the object in the list of outputs
	  k=fOTNames->FindFirst(donbuf);
	  //Get the buffer address from the output branches buffers list
	  proc->SetInputBuf(j,&((*fOBBuffers)[k][(*fIBNames)[k].FindFirst(proc->GetInput(j).GetName())]));
	}

	//Else if the input is a memory buffer
      } else {
	//Get the buffer address from the memory buffers list
	proc->SetInputBuf(j,&((*fBuffers)[fBuNames->FindFirst(proc->GetInput(j).GetName())]));
      }
    }

    //Loop over the output for the current process
    for(j=0; j<proc->GetNOutputs(); j++) {
      sbuf=proc->GetOutput(j);

      //If the output is not a memory buffer
      if(sbuf.Length()) {
	//Get the buffer address from the output branches buffers list
	k=fOTNames->FindFirst(QFileUtils::DecodeObjName(sbuf)); //Tree index
	proc->SetOutputBuf(j,&((*fOBBuffers)[k][(*fOBNames)[k].FindFirst(proc->GetOutput(j).GetName())]));

	//Else if the output is a memory buffer
      } else {
	//Get the buffer address from the memory buffers list
	proc->SetOutputBuf(j,&((*fBuffers)[fBuNames->FindFirst(proc->GetOutput(j).GetName())]));
      }
    }
  }

  //Erase last parameters
  fLastParams->Clear();

  curdir->cd();
  return 0;
}

const QTTreeProcessor& QTTreeProcessor::operator=(const QTTreeProcessor &rhs)
{
  *fProcs=*rhs.fProcs;
  *fParams=*rhs.fParams;
  *fParamsNames=*rhs.fParamsNames;
  fAnalysisDir=rhs.fAnalysisDir;
  *fITNames=*rhs.fITNames;
  *fOTNames=*rhs.fOTNames;
  *fIBNames=*rhs.fIBNames;
  *fOBNames=*rhs.fOBNames;
  *fBuNames=*rhs.fBuNames;
  *fProcsDepends=*rhs.fProcsDepends;
  *fITRequired=*rhs.fITRequired;
  *fIBRequired=*rhs.fIBRequired;
  *fOTDepends=*rhs.fOTDepends;
  *fOBDepends=*rhs.fOBDepends;
  return *this;
}

void QTTreeProcessor::PrintAnalysisResults() const
{
  TDirectory *curdir=gDirectory;

  if(!gDirectory->cd(fAnalysisDir)) {
    fprintf(stderr,"QTTreeProcessor::PrintAnalysisResults(): Error: Directory %s does not exist\n",fAnalysisDir.Data());
    return;
  }

  printf("Analysis Directory: %s\n",fAnalysisDir.Data());

  Int_t i,j;
  printf("\nParameters:\n");
  for(i=0; i<GetNParams(); i++) {
    printf("%3i:\t%s\n",i,GetParamName(i));
  }

  Int_t nprocs=fProcs->Count();
  QNamedProc *proc;
  TString sbuf;
  QList<TString> donbuf;

  for(i=0; i<nprocs; i++) {
    proc=&((*fProcs)[i]);
    printf("\n%03i Process '%s'\n",i,proc->GetName());

    printf("Parameters:\n");
    for(j=0; j<proc->GetNParams(); j++) {
      printf("%3i:\t%s\n",j,proc->GetParam(j).GetName());
    }

    printf("\nInputs:\n");
    for(j=0; j<proc->GetNInputs(); j++) {
      sbuf=proc->GetInput(j);
      donbuf.Clear();
      if(sbuf.Length()) donbuf=QFileUtils::DecodeObjName(sbuf);
      printf("%3i",j);
      if(donbuf.Count()>0) printf("\t%s",donbuf[0].Data());
      if(donbuf.Count()==2) printf("\t%s",donbuf[1].Data());
      printf("\t%s\n",proc->GetInput(j).GetName());
    }

    printf("\nOutputs:\n");
    for(j=0; j<proc->GetNOutputs(); j++) {
      sbuf=proc->GetOutput(j);
      donbuf.Clear();
      if(sbuf.Length()) donbuf=QFileUtils::DecodeObjName(sbuf);
      printf("%3i",j);
      if(donbuf.Count()>0) printf("\t%s",donbuf[0].Data());
      if(donbuf.Count()==2) printf("\t%s",donbuf[1].Data());
      printf("\t%s\n",proc->GetOutput(j).GetName());
    }

    printf("\nDependencies:\n");
    (*fProcsDepends)[i].Print();
    printf("\n");
  }

  printf("All Input Branches:\n");
  for(i=0; i<fITNames->Count(); i++) {
    printf("%3i Tree %s",i,(*fITNames)[i][0].Data());
    if((*fITNames)[i].Count() ==2) printf(" %s",(*fITNames)[i][1].Data());
    printf("\t");
    (*fITRequired)[i].Print();

    for(j=0; j<(*fIBNames)[i].Count(); j++) {
      printf("\t%3i %s\t",j,(*fIBNames)[i][j].Data());
      (*fIBRequired)[i][j].Print();
    }
    printf("\n");
  }

  printf("All Output Branches:\n");
  for(i=0; i<fOTNames->Count(); i++) {
    printf("%3i Tree %s",i,(*fOTNames)[i][0].Data());
    if((*fOTNames)[i].Count() ==2) printf(" %s",(*fOTNames)[i][1].Data());
    printf("\t");
    (*fOTDepends)[i].Print();

    for(j=0; j<(*fOBNames)[i].Count(); j++) {
      printf("\t%3i %s\t",j,(*fOBNames)[i][j].Data());
      (*fOBDepends)[i][j].Print();
    }
    printf("\n");
  }
  curdir->cd();
}

void QTTreeProcessor::SetParam(const char *paramname, Double_t value)
{
  Int_t i;
  if((i=FindParamIndex(paramname))!=-1) SetParam(i,value);
}

void QTTreeProcessor::SetParams(Double_t *params)
{
  memcpy(fParams->GetArray(),params,fParams->Count()*sizeof(Double_t));
}

void QTTreeProcessor::TerminateProcess()
{
  Int_t i;

  for(i=0; i<fIFiles->Count(); i++) {
    ((TFile*)(*fIFiles)[i])->Close();
    delete ((TFile*)(*fIFiles)[i]);
  }
  fIFiles->Clear();
  fIBranches->Clear();

  for(i=0; i<fOFiles->Count(); i++) {
    ((TFile*)(*fOFiles)[i])->Write();
    ((TFile*)(*fOFiles)[i])->Close();
    delete ((TFile*)(*fOFiles)[i]);
  }
  fOFiles->Clear();
  fOBranches->Clear();
  ClearIBCBuffers();
}

void QTTreeProcessor::ClearIBCBuffers()
{
  fIBCBTypes->Clear();
  Int_t i,j;

  for(i=0; i<fIBCBuffers->Count(); i++) {

    for(j=0; j<(*fIBCBuffers)[i].Count(); j++) {
      free((*fIBCBuffers)[i][j]);
    }
  }
  fIBCBuffers->Clear();
}

#include "debugger.h"
