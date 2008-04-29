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
  delete fIBRequired;
  fIBRequired=NULL;
  delete fIBDepends;
  fIBDepends=NULL;
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
  fIBRequired->Clear();
  fIBDepends->Clear();
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

	  //Else if the tree is already listed
	} else {
	  //printf("Existing input tree\n");
	  //Ensure the required branch is in the list
	  ibidx=(*fIBNames)[iidx].AddUnique(proc->GetInput(j).GetName());
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
	  }
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

	  //Else if the tree is already listed
	} else {
	  //printf("Existing output tree\n");
	  //Ensure the output branch is in the list
	  obidx=(*fOBNames)[oidx].AddUnique(proc->GetOutput(j).GetName());

	  //If the output branch was already listed
	  if(obidx != -1) {
	    //Add the dependencies of the previously recorded mask for the branch to the existing dependencies for the current process.
	    //This ensures the recorded value for a given output branch is always generated by the last process that can possibly outputs to that branch.
	    (*fProcsDepends)[i]|=(*fOBDepends)[oidx][obidx];
	  }
	}

	//Else if the output is to a buffer in memory
      } else {
	//printf("Output to a memory buffer\n");
	//Ensure the output memory buffer is in the list
	oidx=fBuNames->AddUnique(proc->GetOutput(j).GetName());

	//If the output buffer was already in the list
	if(oidx != -1) {
	  //Add the dependencies of the previously recorded mask for the memory buffer to the existing dependencies for the current process.
	  //This ensures the recorded value for a given memory buffer is always generated by the last process that can possibly record that buffer.
	  (*fProcsDepends)[i]|=bdepends[oidx];
	}
      }
    }

    fIBRequired->RedimList(fITNames->Count());
    fIBDepends->RedimList(fITNames->Count());

    //Loop over the required inputs for this process for a second time
    for(j=0; j<ninputs; j++) {
      sbuf=proc->GetInput(j);

      //If the input is from a tree
      if(sbuf.Length()) {
	donbuf=QFileUtils::DecodeObjName(sbuf);

	//Get the tree index from from the inputs list
	iidx=fITNames->FindFirst(donbuf);

	//Get the branch index from the inputs list
	ibidx=(*fIBNames)[iidx].FindFirst(proc->GetInput(j).GetName());

	//Ensure there are sufficient elements for all the masks for all the branches of the input tree
	(*fIBRequired)[iidx].RedimList((*fIBNames)[iidx].Count());
	(*fIBDepends)[iidx].RedimList((*fIBNames)[iidx].Count());

	//Add a mask for the current branch corresponding to the all the direct and indirect dependencies for the current process
	(*fIBRequired)[iidx][ibidx].Add((*fProcsDepends)[i]);

	//Try to retrieve the tree index from the outputs list
	oidx=fOTNames->FindFirst(donbuf);

	//If the tree has been generated by a previous process
	if(oidx != -1) {
	  //Retrieve the branch index from the outputs list
	  obidx=(*fOBNames)[oidx].FindFirst(proc->GetInput(j).GetName());

	  //Add a mask for the current branch corresponding to all the direct and indirect dependencies for the current process minus the dependencies of the previous process generating that branch
	  (*fIBDepends)[iidx][ibidx].Add((*fOBDepends)[oidx][obidx]);
	}
      }
    }
    
    fOBDepends->RedimList(fOTNames->Count());
    bdepends.RedimList(fBuNames->Count());

    //Loop over the outputs for the current process for a second time
    for(j=0; j<noutputs; j++) {
      sbuf=proc->GetOutput(j);

      //If the output is in a tree
      if(sbuf.Length()) {
	donbuf=QFileUtils::DecodeObjName(sbuf);

	//Get the tree index of the output from the outputs list
	oidx=fOTNames->FindFirst(donbuf);

	//Get the branch index of the output from the outputs list
	obidx=(*fOBNames)[oidx].FindFirst(proc->GetOutput(j).GetName());

	(*fOBDepends)[oidx].RedimList((*fOBNames)[oidx].Count());

	//Set the dependency mask value to the mask value for the current process
	(*fOBDepends)[oidx][obidx]=(*fProcsDepends)[i];

	//Else if the output is to a buffer in memory
      } else {
	oidx=fBuNames->FindFirst(proc->GetOutput(j).GetName());

	//Set the dependency mask value to the mask value for the current process
	bdepends[oidx]=(*fProcsDepends)[i];
      }
    }
  }

  bdepends.Clear(); //! bdepends values are not valid outside of the previous loop

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
  static QMask pardiffs;
  pardiffs.Clear();
  static Int_t i;

  if(fLastParams->Count() == fParams->Count()) {

    for(i=0; i<fParams->Count(); i++) {

      if((*fParams)[i] != (*fLastParams)[i]) pardiffs.SetBit(i,1);
    }
  } else {
    pardiffs.FillMask(fParams->Count());
  }

  printf("Mask for the current parameters: ");
  pardiffs.Print();

  //If at least one of the parameters has changed
  if(pardiffs) {
    static QList<TObject*> ibranches; //List for needed input branches
    static QList<TObject*> obranches; //List for output branches needing update
    static QList<TObject*> procs;     //List of needed processes
    static Bool_t bbuf, bbuf2;
    static QList<Double_t*> ibbuffers; //Double_t buffers for input branches containing a different data type
    static QList<void*>    ibcbuffers; //buffers for input branches containing a different data type
    static QList<Char_t>   ibcbtypes; //data type id of input branches containing a different data type
    static TEntryList*     elist;     //Pointer to the eventlist for the current TTree
    static QList<TObject*> ibhassel;  //list of TEntryList objects for input branches
    static TTree *tbuf;               //Tree buffer
    static Int_t nentries;            //Number of selected entries for the current input tree
    static Int_t nentrieslast;        //Number of selected entries for the previous input tree

    static Int_t j,k;
    static Int_t nj,nk;

    ibranches.Clear();
    obranches.Clear();
    procs.Clear();
    nentrieslast=-1;
    
    //If one of the input trees uses a different data type
    if(fIBCBuffers->Count()) {
      ibbuffers.Clear();
      ibcbuffers.Clear();
      ibcbtypes.Clear();
    }

    //Loop over all input trees
    for(i=0; i<fIBRequired->Count(); i++) {
      bbuf2=kFALSE;
      nj=(*fIBRequired)[i].Count();

      //Get a pointer to the tree (assertion: the input tree contains at least one branch since its mask is non-zero)
      tbuf=((TBranch*)(*fIBranches)[i][0])->GetTree();

      //Get a pointer to the event list associated to the tree
      //If there is an event list
      if((elist=tbuf->GetEntryList())) {
	//Set the ownership to the tree (TTree::GetEntryList sets the bit to kFALSE)
	elist->SetBit(kCanDelete, kTRUE);
      }

      //Loop over the branches of this input tree
      for(j=0; j<nj; j++) {
	bbuf=kFALSE;
	nk=(*fIBRequired)[i][j].Count();

	//If the branch is not also an output branch
	if(!(*fIBDepends)[i][j].Count()) {

	  //Loop over the processes that require that branch
	  for(k=0; k<nk; k++) {

	    //If the process requiring the current input branch is triggered by the parameters mask
	    if((*fIBRequired)[i][j][k] && pardiffs) {
              //The input branch needs to be loaded from the tree
	      bbuf=kTRUE;
	      break;
	    }
	  }

	  //Else if the branch is also an output branch
	} else {

	  //Loop over the processes that require that branch
	  for(k=0; k<nk; k++) {

	    //If the process requiring the current input branch is triggered by the parameters mask
	    //and the previous process generating this branch is not
	    if(!((*fIBDepends)[i][j][k] && pardiffs) && ((*fIBRequired)[i][j][k] && pardiffs)) {
	      //The input branch needs to be loaded from the tree
	      bbuf=kTRUE;
	      break;
	    }
	  }
	}

	//If the branch needs to be loaded from the tree
	if(bbuf) {

	  //If the current input branch uses a different data type
	  if(fIBCBuffers->Count() && (*fIBCBuffers)[i].Count() && (*fIBCBuffers)[i][j]) {
	    //Add the buffer addresses and the data type id
	    ibbuffers.Add(&(*fIBBuffers)[i][j]);
	    ibcbuffers.Add((*fIBCBuffers)[i][j]);
	    ibcbtypes.Add((*fIBCBTypes)[i][j]);
	  }

	  //Add it to the list of needed input branches
	  ibranches.Add((*fIBranches)[i][j]);
	  ibhassel.Add(elist);

	  bbuf2=kTRUE;
	}
      }

      //If at least one branch of the current input tree has to be read from the tree
      if(bbuf2) {
	//Get the number of selected entries for the current tree
	nentries=tbuf->GetEntries();

	//If the number of select entries for the current input tree does not match the number of selected entries for the previous triggered input tree
	if(nentries != nentrieslast && nentrieslast != -1) {
	  fprintf(stderr,"QTTreeProcessor::Exec(): Error: The number of selected entries in tree %s:%s does not match the number of entries for the previously triggered input tree\n",(*fITNames)[i][1].Data(),(*fITNames)[i][0].Data());
	}
	nentrieslast=nentries;
      }
    }

    //Loop over all output trees
    for(i=0; i<fOTDepends->Count(); i++) {

      //If the current output tree is triggered by the parameters mask
      if((*fOTDepends)[i] && pardiffs) {
	nj=(*fOBDepends)[i].Count();

	//Loop over the branches of this output tree
	for(j=0; j<nj; j++) {

	  //If the current output branch is triggered by the parameters mask
	  if((*fOBDepends)[i][j] && pardiffs) {
	    //Delete the baskets for the output branch
	    ((TBranch*)(*fOBranches)[i][j])->DeleteBaskets("all");
	    //Add it to the list of needed output branches
	    obranches.Add((*fOBranches)[i][j]);
	  }
	}
      }
    }

    //Loop over all processes
    for(i=0; i<fProcsDepends->Count(); i++) {

      //If the current process is triggered by the parameters mask
      if((*fProcsDepends)[i] && pardiffs) {
	//Add it to the list of needed processes
	procs.Add(&(*fProcs)[i]);
      }
    }

    //If at least one tree has to be read
    if(nentrieslast != -1) {

      QProgress progress(nentries);
      //Loop over the entries
      for(i=0; i<nentries; i++) {

	//Load all triggered input branches
	for(j=0; j<ibranches.Count(); j++) {
	  ((TBranch*)ibranches.GetArray()[j])->GetEntry((elist=(TEntryList*)ibhassel.GetArray()[j]) ? elist->GetEntry(i) : i);
	}

	//Convert all buffers for branches containing a different data type
	for(j=0; j<ibbuffers.Count(); j++) {

	  switch(ibcbtypes.GetArray()[j]) {
	    case kFloat_t:
	      *(ibbuffers.GetArray()[j])=(Double_t)*((Float_t*)ibcbuffers.GetArray()[j]);
	      break;
	    case kUInt_t:
	      *(ibbuffers.GetArray()[j])=(Double_t)*((UInt_t*)ibcbuffers.GetArray()[j]);
	      break;
	    case kInt_t:
	      *(ibbuffers.GetArray()[j])=(Double_t)*((Int_t*)ibcbuffers.GetArray()[j]);
	      break;
	    case kUShort_t:
	      *(ibbuffers.GetArray()[j])=(Double_t)*((UShort_t*)ibcbuffers.GetArray()[j]);
	      break;
	    case kShort_t:
	      *(ibbuffers.GetArray()[j])=(Double_t)*((Short_t*)ibcbuffers.GetArray()[j]);
	      break;
	    case kUChar_t:
	      *(ibbuffers.GetArray()[j])=(Double_t)*((UChar_t*)ibcbuffers.GetArray()[j]);
	      break;
	    case kChar_t:
	      *(ibbuffers.GetArray()[j])=(Double_t)*((Char_t*)ibcbuffers.GetArray()[j]);
	      break;
	    case kBool_t:
	      *(ibbuffers.GetArray()[j])=(Double_t)*((Bool_t*)ibcbuffers.GetArray()[j]);
	  }
	}

	//Call all triggered processes
	for(j=0; j<procs.Count(); j++) {
	  ((QNamedProc*)procs.GetArray()[j])->Exec();
	}

	//Save all triggered output branches
	for(j=0; j<obranches.Count(); j++) {
	  ((TBranch*)obranches.GetArray()[j])->Fill();
	}

	progress(i+1);
      }
      progress(i,kTRUE);
      printf("\n");

      //Set the number of entries at the TTree level for each output branch
      for(i=0; i<obranches.Count(); i++) {
	((TBranch*)obranches.GetArray()[i])->GetTree()->SetEntries(nentries);
      }
    }

    //Save the parameters
    (*fLastParams)=(*fParams);
  }
}

Int_t QTTreeProcessor::FindParamIndex(const char *paramname) const
{
  Int_t ret=(*fParamsNames).FindFirst(paramname);

  if(ret == -1) fprintf(stderr,"QTTreeProcessor::FindParamIndex: Error: parameter '%s' not found\n",paramname);
  return ret;
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

	if(!(dbuf=gDirectory->GetDirectory(dpn[j]))) {
	  fprintf(stderr,"QTTreeProcessor::InitProcess(): Directory '%s' does not exist in location '%s'\n",dpn[j].Data(),gDirectory->GetPath());
	  return 1;
	}
	dbuf->cd();
      }

      //Load the input tree
      if(!(tbuf=dynamic_cast<TTree*>(gDirectory->Get(dpn.GetLast())))) {
	fprintf(stderr,"QTTreeProcessor::InitProcess(): Tree '%s:%s' does not exist\n",(*fITNames)[i][1].Data(),(*fITNames)[i][0].Data());
	return 1;
      }
      dpn.Clear();
      (*fIBranches)[i].RedimList((*fIBNames)[i].Count());
      (*fIBBuffers)[i].RedimList((*fIBNames)[i].Count());

      //Loop over the required branches for this input tree
      for(j=0; j<(*fIBNames)[i].Count(); j++) {

	if(!((*fIBranches)[i][j]=tbuf->GetBranch((*fIBNames)[i][j]))) {
	  fprintf(stderr,"QTTreeProcessor::InitProcess(): Error: Branch '%s' does not exist in tree '%s:%s'\n",(*fIBNames)[i][j].Data(),(*fITNames)[i][1].Data(),(*fITNames)[i][0].Data());
	  return 1;
	}

	//Get a pointer to the leaf named according to the branch name
	if(!(lbuf=((TBranch*)(*fIBranches)[i][j])->GetLeaf((*fIBNames)[i][j]))) {
	  fprintf(stderr,"QTTreeProcessor::InitProcess(): Error: There is no leaf '%s' contained in branch '%s' from tree '%s:%s'\n",(*fIBNames)[i][j].Data(),(*fIBNames)[i][j].Data(),(*fITNames)[i][1].Data(),(*fITNames)[i][0].Data());
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
	    fprintf(stderr,"QTTreeProcessor::InitProcess(): Error: The data type '%s' contained in branch '%s' from tree '%s:%s' is not supported\n",cabuf,(*fIBNames)[i][j].Data(),(*fITNames)[i][1].Data(),(*fITNames)[i][0].Data());
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
  *fIBRequired=*rhs.fIBRequired;
  *fIBDepends=*rhs.fIBDepends;
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

  Int_t i,j,k;
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
    printf("\n");

    for(j=0; j<(*fIBNames)[i].Count(); j++) {
      printf("\t%3i %s\n",j,(*fIBNames)[i][j].Data());

      if((*fIBRequired)[i][j].Count()) {
	printf("\t\tRequired masks:\n");

	for(k=0; k<(*fIBRequired)[i][j].Count(); k++) {
	  printf("\t\t\t");
	  (*fIBRequired)[i][j][k].Print();
	}
      }

      if((*fIBDepends)[i][j].Count()) {
	printf("\t\tDependencies masks:\n");

	for(k=0; k<(*fIBDepends)[i][j].Count(); k++) {
	  printf("\t\t\t");
	  (*fIBDepends)[i][j][k].Print();
	}
      }
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
