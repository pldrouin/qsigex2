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
  QList<QList< QList<TString> > > allitrees, allotrees;  //trees information for all processes
  QList<QMask>   bdepends; //Dependencies of output buffers on parameters
  TString sbuf;
  QList<TString> donbuf;       //Decoded object name buffer

  fAnalysisDir=gDirectory->GetPath();

  Int_t nprocs=fProcs->Count();
  Int_t ninputs, noutputs, nparams;
  Int_t pidx, iidx, oidx, bidx;

  allitrees.RedimList(nprocs);
  allotrees.RedimList(nprocs);

  fITNames->Clear();
  fOTNames->Clear();
  fIBNames->Clear();
  fOBNames->Clear();
  fOTDepends->Clear();
  fOBDepends->Clear();

  //Loop over the processes
  for(i=0; i<nprocs; i++) {
    proc=&((*fProcs)[i]);
    //printf("Process '%s'\n",proc->GetName());
    ninputs=proc->GetNInputs();
    noutputs=proc->GetNOutputs();
    nparams=proc->GetNParams();
    allitrees[i].RedimList(ninputs);
    allotrees[i].RedimList(noutputs);
    (*fProcsDepends)[i].Clear(); //Set the mask to 0 for the current process

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
      (*fProcsDepends)[i].SetBit(pidx,1);
    }

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

	allitrees[i][j]=donbuf;

	oidx=fOTNames->FindFirst(donbuf);

	//If this tree has not been generated by a previous process
	if(oidx == -1) {
	  //printf("Absolute input\n");
	  iidx=fITNames->AddUnique(donbuf);

	  //If the tree is not already listed in the list of existing trees
	  if(iidx == -1) {
	    //printf("New input tree\n");
	    //Add the name of the required branch
	    fIBNames->RedimList(fITNames->Count());
	    fIBNames->GetLast().Add(proc->GetInput(j).GetName());

	    //Else if the tree is already listed
	  } else {
	    //printf("Existing input tree\n");
	    //Ensure the required branch is in the list
	    (*fIBNames)[iidx].AddUnique(proc->GetInput(j).GetName());
	  }

	  //Else if the tree has been generated by a previous process
	} else {
	  //printf("Intermediary input\n");
	  bidx=(*fOBNames)[oidx].FindFirst(proc->GetInput(j).GetName());

	  //If the branch has not been listed in the list of generated branches
	  if(bidx == -1) {
	    fprintf(stderr,"QTTreeProcessor::Analyze(): Error with process '%s': Input branch '%s' for tree '%s' was not generated by a previous process\n",proc->GetName(),proc->GetInput(j).GetName(),((*fOTNames)[oidx][1]+"/"+(*fOTNames)[oidx][0]).Data());
	    return -1;

	  } else {
	    //Add the dependencies of the input branch to the dependency mask for the current process
	    (*fProcsDepends)[i]|=(*fOBDepends)[oidx][bidx];
	  }
	}
	donbuf.Clear();

	//Else if the input is from a buffer in memory
      } else {
	//printf("Input from a memory buffer\n");
	bidx=fBuNames->FindFirst(proc->GetInput(j).GetName());

	//If the buffer has not been listed as an output for a previous process
	if(bidx == -1) {
	  fprintf(stderr,"QTTreeProcessor: Analyze(): Error with process '%s': Input buffer '%s' was not generated by a previous process\n",proc->GetName(),proc->GetInput(j).GetName());
	  return -1;

	} else {
	  //Add the dependencies of the last recorded buffer value to the dependency mask for the current process
	  (*fProcsDepends)[i]|=bdepends[bidx];
	}
      }
    }

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

	if(fITNames->FindFirst(donbuf) != -1) {
	  fprintf(stderr,"QTTreeProcessor: Analyze(): Error with process '%s': Branch '%s' for tree '%s' cannot be overwritten\n",proc->GetName(),proc->GetInput(j).GetName(),(donbuf[1]+"/"+donbuf[0]).Data());
	  return -1;
	}

	oidx=fOTNames->AddUnique(donbuf);

	//If this tree has not been generated by a previous process
	if(oidx == -1) {
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
	  bidx=(*fOBNames)[oidx].AddUnique(proc->GetOutput(j).GetName());
	  //Add an extra dependency mask if this is a new output branch
	  //Set the dependency mask value to the mask value for the current process
	  if(bidx==-1) (*fOBDepends)[oidx].Add((*fProcsDepends)[i]);
	  else (*fOBDepends)[oidx][bidx]=(*fProcsDepends)[i];
	}
	allotrees[i][j]=donbuf;

	//Else if the output is to a buffer in memory
      } else {
	//printf("Output to a memory buffer\n");
	//Ensure the output memory buffer is in the list
	bidx=fBuNames->AddUnique(proc->GetOutput(j).GetName());
	//Add an extra dependency mask if this is a new output branch
	//Set the dependency mask value to the mask value for the current process
	if(bidx==-1) bdepends.Add((*fProcsDepends)[i]);
	else bdepends[bidx]=(*fProcsDepends)[i];
      }
    }
  }

  bdepends.Clear();

  fOTDepends->RedimList(fOTNames->Count());

  //Loop over the output trees
  for(i=0; i<fOTDepends->Count(); i++) {

    //If there is at least one output branch
    if((*fOBDepends)[i].Count() > 0) {
      //Set the dependency mask for the tree to be the combination of the masks for its branches
      (*fOTDepends)[i]=(*fOBDepends)[i][0];

      for(j=1; j<(*fOBDepends)[i].Count(); j++) (*fOTDepends)[i]|=(*fOBDepends)[i][j];
    }
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
  Int_t i,j;
  QList<TString> dpn;
  TTree *tbuf;

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

  //Loop over the input branches
  for(i=0; i<fITNames->Count(); i++) {

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

      } else if(!strcmp(cabuf,"Float_t")) {
      
      } else if(!strcmp(cabuf,"UInt_t")) {

      } else if(!strcmp(cabuf,"Int_t")) {

      } else if(!strcmp(cabuf,"UShort_t")) {

      } else if(!strcmp(cabuf,"Short_t")) {

      } else if(!strcmp(cabuf,"UChar_t")) {

      } else if(!strcmp(cabuf,"Char_t")) {

      } else if(!strcmp(cabuf,"Bool_t")) {

      } else {
	fprintf(stderr,"QTTreeProcessor::InitProcess(): Error: The data type '%s' contained in branch '%s' from tree '%s:/%s' is not supported\n",cabuf,(*fIBNames)[i][j].Data(),(*fITNames)[i][1].Data(),(*fITNames)[i][0].Data());
	return 1;
      }
    }
  }

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
    printf(":\n");

    for(j=0; j<(*fIBNames)[i].Count(); j++) {
      printf("\t%3i %s\n",j,(*fIBNames)[i][j].Data());
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
}

#include "debugger.h"
