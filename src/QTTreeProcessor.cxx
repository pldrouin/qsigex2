#include "QTTreeProcessor.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QTTreeProcessor)

Int_t QTTreeProcessor::QDependentProcs::fInitIdx=-1;
QList<void*> QTTreeProcessor::QDependentProcs::fQPDObjs;
QList<Int_t> QTTreeProcessor::QDependentProcs::fPCalled;

QTTreeProcessor::~QTTreeProcessor()
{
  PRINTF2(this,"\tQTTreeProcessor::~QTTreeProcessor()\n")
  delete fProcs;
  fProcs=NULL;
  delete fSelProcs;
  fSelProcs=NULL;
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
  delete fITIndices;
  fITIndices=NULL;
  delete fIBIndices;
  fIBIndices=NULL;
  delete fOTIndices;
  fOTIndices=NULL;
  delete fOBIndices;
  fOBIndices=NULL;
  delete fAITrees;
  fAITrees=NULL;
  delete fIFiles;
  fIFiles=NULL;
  delete fOFiles;
  fOFiles=NULL;
  delete fIBranches;
  fIBranches=NULL;
  delete fOBranches;
  fOBranches=NULL;
  delete fSelDepProcs;
  fSelDepProcs=NULL;
  delete fITSProc;
  fITSProc=NULL;
  delete fOTSProc;
  fOTSProc=NULL;
  delete fProcsParDepends;
  fProcsParDepends=NULL;
  delete fProcsTDepends;
  fProcsTDepends=NULL;
  delete fProcsBDepends;
  fProcsBDepends=NULL;
  delete fIBBuffers;
  fIBBuffers=NULL;
  delete fOwnsIBBuffers;
  fOwnsIBBuffers=NULL;
  delete fOBBuffers;
  fOBBuffers=NULL;
  delete fIBCBuffers;
  fIBCBuffers=NULL;
  delete fOwnsIBCBuffers;
  fOwnsIBCBuffers=NULL;
  delete fIBCBTypes;
  fIBCBTypes=NULL;
  delete fBuffers;
  fBuffers=NULL;
}

void QTTreeProcessor::AddParam(const char *parname, Double_t value, Int_t index)
{
  if(fParamsNames->FindFirst(parname) != -1) {
    fprintf(stderr,"QTTreeProcessor::AddParam: Error: Parameter '%s' already exists\n",parname);
    throw 1;
  }
  fParamsNames->Add(parname,index);
  fParams->Add(value,index);
}

void QTTreeProcessor::AddProc(const char *name, const char *title, Bool_t selector, Int_t index)
{
  PRINTF10(this,"\tQTTreeProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, Bool_t selector<",selector,">, Int_t index<",index,">)\n")

  index=AEProcIndexToIndex(index);
  fNAEProcs++;
  fProcs->RedimList(fProcs->Count()+1,index);
  fSelProcs->Add(selector,index);
  (*fProcs)[index].SetNameTitle(name,title);
}

void QTTreeProcessor::AddProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname, Bool_t selector, Int_t index)
{
  PRINTF14(this,"\tQTTreeProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, Bool_t (*proc)(QProcArgs&)<",proc,">, const char *procname<'",procname,"'>, Bool_t selector<",selector,">, Int_t index<",index,">)\n")

  index=AEProcIndexToIndex(index);
  AddProc(name,title,selector,index);
  (*fProcs)[index].SetProc(proc,procname);
}

void QTTreeProcessor::AddProc(const char *name, const char *title, const char *procname, Bool_t selector, Int_t index)
{
  PRINTF12(this,"\tQTTreeProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, const char *procname<'",procname,"'>, Bool_t selector<",selector,">, Int_t index<",index,">)\n")

  index=AEProcIndexToIndex(index);
  AddProc(name,title,selector,index);
  (*fProcs)[index].SetProc(procname);
}

void QTTreeProcessor::AddProc(const char *name, const char *title, void *proc, const char *procname, Bool_t selector, Int_t index)
{
  PRINTF14(this,"\tQTTreeProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, void *proc<",proc,">, const char *procname<'",procname,"'>, Bool_t selector<",selector,">, Int_t index<",index,">)\n")

  index=AEProcIndexToIndex(index);
  AddProc(name,title,selector,index);
  (*fProcs)[index].SetProc(proc,procname);
}

void QTTreeProcessor::AddPSProc(const char *name, const char *title, Int_t index)
{
  PRINTF8(this,"\tQTTreeProcessor::AddPSProc(const char *name<'",name,"'>, const char *title<'",title,"'>, Int_t index<",index,">)\n")

  index=PSProcIndexToIndex(index);
  fProcs->RedimList(fProcs->Count()+1,index);
  fSelProcs->Add(kFALSE,index);
  (*fProcs)[index].SetNameTitle(name,title);
}

void QTTreeProcessor::AddPSProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname, Int_t index)
{
  PRINTF12(this,"\tQTTreeProcessor::AddPSProc(const char *name<'",name,"'>, const char *title<'",title,"'>, Bool_t (*proc)(QProcArgs&)<",proc,">, const char *procname<'",procname,"'>, Int_t index<",index,">)\n")

  index=PSProcIndexToIndex(index);
  AddPSProc(name,title,index);
  (*fProcs)[index].SetProc(proc,procname);
}

void QTTreeProcessor::AddPSProc(const char *name, const char *title, const char *procname, Int_t index)
{
  PRINTF10(this,"\tQTTreeProcessor::AddPSProc(const char *name<'",name,"'>, const char *title<'",title,"'>, const char *procname<'",procname,"'>, Int_t index<",index,">)\n")

  index=PSProcIndexToIndex(index);
  AddPSProc(name,title,index);
  (*fProcs)[index].SetProc(procname);
}

void QTTreeProcessor::AddPSProc(const char *name, const char *title, void *proc, const char *procname, Int_t index)
{
  PRINTF12(this,"\tQTTreeProcessor::AddPSProc(const char *name<'",name,"'>, const char *title<'",title,"'>, void *proc<",proc,">, const char *procname<'",procname,"'>, Int_t index<",index,">)\n")

  index=PSProcIndexToIndex(index);
  AddPSProc(name,title,index);
  (*fProcs)[index].SetProc(proc,procname);
}

void QTTreeProcessor::Analyze()
{
  Int_t i,j,k,l;
  QNamedProc *proc;
  QList<TString> params; //Parameters
  TString sbuf;
  QList<TString> donbuf; //Decoded object name buffer

  fAnalysisDir=gDirectory->GetPath();

  Int_t nprocs=fProcs->Count();
  Int_t ninputs, noutputs, nparams;
  Int_t pidx, iidx, oidx, ibidx, obidx;

  fITNames->Clear();
  fOTNames->Clear();
  fIBNames->Clear();
  fOBNames->Clear();
  fITIndices->Clear();
  fIBIndices->Clear();
  fOTIndices->Clear();
  fOBIndices->Clear();

  fITIndices->RedimList(nprocs);
  fIBIndices->RedimList(nprocs);
  fOTIndices->RedimList(nprocs);
  fOBIndices->RedimList(nprocs);
  QDependentProcs *depprocs=new QDependentProcs[nprocs]; //QDependentProcs objects contain a list of processes that should be triggered
  QList<QList<Int_t> > oblastproc; //Index of last process having recorded its output in a given branch
  QList<Int_t> blastproc;          //Index of last process having recorded its output in a given memory buffer
  fProcsParDepends->RedimList(nprocs);
  fSelDepProcs->RedimList(nprocs);

  //Section 1: First pass over all processes. Setting as many inputs, outputs and dependencies as possible and doing some checks
  //Loop over the processes
  for(i=0; i<nprocs; i++) {
    proc=&((*fProcs)[i]);
    //printf("Process '%s'\n",proc->GetName());
    ninputs=proc->GetNIVars();
    noutputs=proc->GetNOVars();
    nparams=proc->GetNParams();

    (*fProcsParDepends)[i].Clear();
    (*fSelDepProcs)[i]=kFALSE;

    //Loop over the parameters for the current process
    for(j=0; j<nparams; j++) {
      sbuf=proc->GetParam(j).GetName();

      //If the current parameter is not listed in the list of parameters
      pidx=fParamsNames->FindFirst(sbuf);

      if(pidx == -1) {
	fprintf(stderr,"QTTreeProcessor::Analyze(): Error with process '%s': Parameter '%s' does not exist\n",proc->GetName(),sbuf.Data());
	throw 1;
      }
      //Turn on the bit of the dependency mask for the current parameter
      (*fProcsParDepends)[i].SetBit(pidx,1);
    }

    //Loop over the inputs for the current process
    k=0;
    for(j=0; j<ninputs; j++) {
      sbuf=proc->GetIVarNameTitle(j);
      //printf("Encoded Input: '%s'\t%s\n",sbuf.Data(),proc->GetIVarNameTitle(j).GetName());

      //If the input is from a tree
      if(sbuf.Length()) {
	//printf("Input from a tree\n");
	donbuf=QFileUtils::DecodeObjName(sbuf);

	if(!donbuf.Count()) {
	  fprintf(stderr,"QTTreeProcessor::Analyze(): Error: Invalid input object name: '%s'\n",sbuf.Data());
	  throw 1;
	}

	//printf("Decoded Input: %s",donbuf[0].Data());
	//if(donbuf.Count() ==2) printf("\t%s",donbuf[1].Data());
	//printf("\t%s\n",proc->GetIVarNameTitle(j).GetName());

	//Ensure the required tree is in the list
	iidx=fITNames->AddUnique(donbuf);

	//If the tree is not already listed in the list of existing trees
	if(iidx == -1) {
	  //printf("New input tree\n");
	  //Add the name of the required branch
	  fIBNames->RedimList(fITNames->Count());
	  fIBNames->GetLast().Add(proc->GetIVarNameTitle(j).GetName());
	  iidx=fITNames->Count()-1;
	  ibidx=0;

	  //Else if the tree is already listed
	} else {
	  //printf("Existing input tree\n");
	  //Ensure the required branch is in the list
	  ibidx=(*fIBNames)[iidx].AddUnique(proc->GetIVarNameTitle(j).GetName());

	  if(ibidx == -1) ibidx=(*fIBNames)[iidx].Count()-1;
	}
	(*fITIndices)[i].Add(iidx);
	(*fIBIndices)[i].Add(ibidx);

	oidx=fOTNames->FindFirst(donbuf);

	//If the tree has been generated by a previous process
	if(oidx != -1) {
	  //printf("Intermediary input\n");
	  obidx=(*fOBNames)[oidx].FindFirst(proc->GetIVarNameTitle(j).GetName());

	  //If the branch has not been listed in the list of generated branches
	  if(obidx == -1) {
	    fprintf(stderr,"QTTreeProcessor::Analyze(): Error with process '%s': Input branch '%s' for tree '%s' was not generated by a previous process\n",proc->GetName(),proc->GetIVarNameTitle(j).GetName(),(*fOTNames)[oidx][0].Data());
	    throw 1;
	  }

	  //Add the index of the current process to the dependent processes list of the last process that has saved a value in the current branch
	  depprocs[oblastproc[oidx][obidx]].AddDepend(i);

          //If this is not the first input and the dependency of the current input on selector processes is different than for the previous input
	  if(k!=0 && (*fSelDepProcs)[oblastproc[oidx][obidx]] != (*fSelDepProcs)[i]) {
	    fprintf(stderr,"QTTreeProcessor::Analyze(): Error with process '%s': Input branch '%s' for tree '%s' does not have the same dependency on selector processes than the previous intermediary input for this process\n",proc->GetName(),proc->GetIVarNameTitle(j).GetName(),(*fOTNames)[oidx][0].Data());
	    throw 1;
	  }
	  //Add the dependencies on selector processes of the last process that has saved a value in the current branch to the dependencies for the current process
	  (*fSelDepProcs)[i]|=(*fSelDepProcs)[oblastproc[oidx][obidx]];
	  k++;
	}

	//Else if the input is from a buffer in memory
      } else {
	//printf("Input from a memory buffer\n");
	iidx=fBuNames->FindFirst(proc->GetIVarNameTitle(j).GetName());

	//If the buffer has not been listed as an output for a previous process
	if(iidx == -1) {
	  fprintf(stderr,"QTTreeProcessor: Analyze(): Error with process '%s': Input buffer '%s' was not generated by a previous process\n",proc->GetName(),proc->GetIVarNameTitle(j).GetName());
	  throw 1;
	}

	//Add the index of the current process to the dependent processes list of the last process that has saved a value in the current memory buffer
	depprocs[blastproc[iidx]].AddDepend(i);
	//Add the index of the last process that has saved a value in the current memory buffer to the dependent processes list of the current process
	depprocs[i].AddDepend(blastproc[iidx]);
	//Add the dependencies on selector processes of the last process that has saved a value in the current memory buffer to the dependencies for the current process
	(*fSelDepProcs)[i]|=(*fSelDepProcs)[blastproc[iidx]];
	k++;
      }
    }
    //If the current process is a selector process, adds it to fSelDepProcs
    (*fSelDepProcs)[i]|=(*fSelProcs)[i];

    //Loop over the outputs for the current process
    for(j=0; j<noutputs; j++) {

      sbuf=proc->GetOVarNameTitle(j);
      //printf("Encoded Output: '%s'\t%s\n",sbuf.Data(),proc->GetOVarNameTitle(j).GetName());

      //If the output is in a tree
      if(sbuf.Length()) {
	//printf("Output to a tree\n");
	donbuf=QFileUtils::DecodeObjName(sbuf);

	if(!donbuf.Count()) {
	  fprintf(stderr,"QTTreeProcessor::Analyze(): Error: Invalid output object name: '%s'\n",sbuf.Data());
	  throw 1;
	}

	oidx=fOTNames->AddUnique(donbuf);

	//If this tree has not been generated by a previous process
	if(oidx == -1) {

	  if(fITNames->FindFirst(donbuf) != -1) {
	    fprintf(stderr,"QTTreeProcessor: Analyze(): Error with process '%s': Branch '%s' for tree '%s/%s' cannot be overwritten\n",proc->GetName(),proc->GetOVarNameTitle(j).GetName(),donbuf[1].Data(),donbuf[0].Data());
	    throw 1;
	  }
	  //printf("New output tree\n");
	  //Add the name of the output branch
	  fOBNames->RedimList(fOTNames->Count());
	  fOBNames->GetLast().Add(proc->GetOVarNameTitle(j).GetName());
	  oidx=fOTNames->Count()-1;
	  obidx=0;
	  oblastproc.RedimList(fOTNames->Count());
	  oblastproc.GetLast().Add(i);

	  //Else if the tree is already listed
	} else {
	  //printf("Existing output tree\n");
	  //Ensure the output branch is in the list
	  obidx=(*fOBNames)[oidx].AddUnique(proc->GetOVarNameTitle(j).GetName());

	  //If the branch was not already in the list
	  if(obidx == -1) {
	    oblastproc[oidx].Add(i);
	    obidx=(*fOBNames)[oidx].Count()-1;

	    //Else if the branch was already in the list
	  } else {
	    oblastproc[oidx][obidx]=i;
	  }
	}
	(*fOTIndices)[i].Add(oidx);
	(*fOBIndices)[i].Add(obidx);

	//Else if the output is to a buffer in memory
      } else {
	//printf("Output to a memory buffer\n");
	//Ensure the output memory buffer is in the list
	oidx=fBuNames->AddUnique(proc->GetOVarNameTitle(j).GetName());

	//If the buffer was not already listed
	if(oidx == -1) {
	  blastproc.Add(i);

	  //Else if the buffer was already in the list
	} else {
	  blastproc[oidx]=i;
	}
      }
    }
  }

  oblastproc.Clear();
  blastproc.Clear();

  //Section 2: Tag output branches that depend directly or indirectly on selector processes
  QList<Bool_t> babuf;
  babuf.RedimList(fOTNames->Count(),-1,kFALSE);
  fOTSProc->RedimList(fOTNames->Count());

  //Loop over the regular processes
  for(i=0; i<nprocs; i++) {

    //Loop over the output branches for the current process
    for(j=0; j<(*fOTIndices)[i].Count(); j++) {

      //If this is not the first time that the tree associated to the current output branch is encountered and that its saved dependency is different from the one of the current process
      if(babuf[(*fOTIndices)[i][j]] && (*fOTSProc)[(*fOTIndices)[i][j]] != (*fSelDepProcs)[i]) {
	fprintf(stderr,"QTTreeProcessor: Analyze(): Error: Some branch(es) of the output tree '%s/%s' depend on a selector process while other branch(es) do not\n",(*fOTNames)[(*fOTIndices)[i][j]][1].Data(),(*fOTNames)[(*fOTIndices)[i][j]][0].Data());
	throw 1;
      } else {
	babuf[(*fOTIndices)[i][j]]=kTRUE;
	(*fOTSProc)[(*fOTIndices)[i][j]]=(*fSelDepProcs)[i];
      }
    }
  }
  babuf.Clear();

  //Section 3: Tag absolute input trees and input trees that depend directly or indirectly on selector processes
  fAITrees->RedimList(fITNames->Count());
  fITSProc->RedimList(fITNames->Count());
  memset(fITSProc->GetArray(),0,fITNames->Count()*sizeof(Bool_t));

  //Loop over input trees
  for(i=0; i<fITNames->Count(); i++) {

    //If the current input tree is not also an output tree, tag is as an absolute input tree
    if((oidx=fOTNames->FindFirst((*fITNames)[i])) == -1) (*fAITrees)[i]=kTRUE;
    //Else if the current input tree is also an output tree
    else {
      (*fAITrees)[i]=kFALSE;
      (*fITSProc)[i]=(*fOTSProc)[oidx];
    }
  }

  //Section 4: All selector processes should be called if a given selector process is called. For this reason it is better if selector processes don't have any output (reduce I/O)
  //Loop over all processses that can be selector processes
  for(i=0; i<fNAEProcs; i++) {

    //If the current process is a selector process
    if((*fSelProcs)[i]) {

      //Loop over all processes that can be selector processes
      for(j=0; j<fNAEProcs; j++) {

	//If the process j is also a selector process, add it to the list of dependent processes for selector process i
	if((*fSelProcs)[j]) depprocs[i].AddDepend(j);
      }
    }
  }

  //Section 5: Updating parameter dependencies to take into account indirect dependencies
  QList<Int_t> dpidx; //Indices of dependent processes
  Bool_t haschanged;
  QMask mbuf;

  do {
    haschanged=kFALSE;
    //printf("Looping...\n");
 
    //Loop over the processes
    for(i=0; i<nprocs; i++) {
      proc=&((*fProcs)[i]);
      //printf("%03i Process '%s'\n",i,proc->GetName());
      dpidx=depprocs[i].GetAllDepends();

      //Loop over the dependent processes
      for(j=0; j<dpidx.Count(); j++) {

	//If some dependencies are not taken into account
	if((mbuf=(*fProcsParDepends)[i]|(*fProcsParDepends)[dpidx[j]]) != (*fProcsParDepends)[dpidx[j]]) {
	  //Add the parameter dependencies of the current process to the parameters dependencies of the triggered process
	  (*fProcsParDepends)[dpidx[j]]=mbuf;

	  if(dpidx[j]<i) haschanged=kTRUE;
	}
      }
    }

    //While changes are made to the parameters dependencies of a process with a lower index (only occurs when memory buffers are used)
  } while(haschanged);
  mbuf.Clear();

  //Section 6: Setting direct and indirect branch dependencies
  *fProcsTDepends=*fITIndices;
  *fProcsBDepends=*fIBIndices;
  Bool_t inputfound;

  do {
    haschanged=kFALSE;
    //printf("Looping2...\n");

    //Loop over the processes
    for(i=0; i<nprocs; i++) {
      proc=&((*fProcs)[i]);
      //printf("%03i Process '%s'\n",i,proc->GetName());
      dpidx=depprocs[i].GetAllDepends();

      //Loop over the dependent processes
      for(j=0; j<dpidx.Count(); j++) {
	inputfound=kFALSE;

	//Loop over the input branches the current process depends on
	for(k=0; k<(*fProcsTDepends)[i].Count(); k++) {

	  //Loop over the input branches the dependent process depends on
	  for(l=0; l<(*fProcsTDepends)[dpidx[j]].Count(); l++) {

	    //If the input branch of the current process is found among the branches of the dependent process
	    if((*fProcsTDepends)[i][k] == (*fProcsTDepends)[dpidx[j]][l] && (*fProcsBDepends)[i][k] == (*fProcsBDepends)[dpidx[j]][l]) {
	      inputfound=kTRUE;
	      break;
	    }
	  }

	  //If the input has not been found
	  if(!inputfound) {
	    //Add it to the list of branches the dependent process depends on
	    (*fProcsTDepends)[dpidx[j]].Add((*fProcsTDepends)[i][k]);
	    (*fProcsBDepends)[dpidx[j]].Add((*fProcsBDepends)[i][k]);

	    //If the process index of the dependent process is smaller than the current process index
	    if(dpidx[j]<i) haschanged=kTRUE;
	  }
	} 
      }
    }

  } while(haschanged);

  dpidx.Clear();

  delete[] depprocs;
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
    DelProc(i);
  }
}

void QTTreeProcessor::Exec()
{
  static QMask pardiffs;
  static Bool_t firstrun;
  pardiffs.Clear();
  static QList<Bool_t>         neededit; //Needed input trees 
  static QList<QList<Bool_t> > neededib; //Needed input branches 
  static QList<Bool_t>         neededot; //Needed output trees
  static QList<QList<Bool_t> > neededob; //Needed output branches
  static Int_t i;

  if(fLastParams->Count() == fParams->Count()) {

    for(i=0; i<fParams->Count(); i++) {

      if((*fParams)[i] != (*fLastParams)[i]) pardiffs.SetBit(i,1);
    }
    firstrun=kFALSE;
  } else {
    pardiffs.FillMask(fParams->Count());

    neededit.RedimList(fITNames->Count());
    neededib.RedimList(fITNames->Count());

    //Loop over the input trees
    for(i=0; i<neededib.Count(); i++) {
      neededib[i].RedimList((*fIBNames)[i].Count());
    }

    neededot.RedimList(fOTNames->Count());
    neededob.RedimList(fOTNames->Count());

    //Loop over the output trees
    for(i=0; i<neededob.Count(); i++) {
      neededob[i].RedimList((*fOBNames)[i].Count());
    }
    firstrun=kTRUE;
  }

  printf("Mask for the current parameters: ");
  pardiffs.Print();

  //If at least one of the parameters has changed
  if(pardiffs) {
    static QList<TObject*> ibranches; //List for needed input branches
    static QList<TObject*> obranches; //List for output branches needing update
    static QList<Bool_t>   obsproc;   //Indicate if output branches needing update are dependent or not on a selector process
    static QList<TObject*> otrees;    //List for output trees needing update
    static QList<TObject*> procs;     //List of needed processes
    static Int_t           naeprocs;  //Number of processes that process all entries in the tree
    static QList<Bool_t> selprocs;    //Indicate if the needed processes is a selector process or not
    static QList<Bool_t> seldepprocs; //Indicate if the needed processes depends on a selector process or not
    static Bool_t doselection;
    static Bool_t bbuf;
    static QList<Double_t*> ibbuffers; //Double_t buffers for input branches containing a different data type
    static QList<void*>    ibcbuffers; //buffers for input branches containing a different data type
    static QList<Char_t>   ibcbtypes; //data type id of input branches containing a different data type
    static TTree *tbuf;               //Tree buffer
    static Int_t neaet;               //Number of entries for trees that should contain all events
    static Int_t neaetlast;           //Number of entries for the previous input tree that should contain all events
    static Int_t neset;               //Number of entries for trees that should contain only selected events
    static Int_t nesetlast;           //Number of entries for the previous input tree that should contain only selected events
    static Int_t nentries;            //Number of entries used in the events loop
    static Bool_t eventselected;

    static Int_t j,k;
    static Int_t nj;

    ibranches.Clear();
    obranches.Clear();
    obsproc.Clear();
    otrees.Clear();
    procs.Clear();
    naeprocs=0;
    selprocs.Clear();
    seldepprocs.Clear();
    doselection=kFALSE;
    neaetlast=-1;
    nesetlast=-1;

    //If one of the input trees uses a different data type
    if(fIBCBuffers->Count()) {
      ibbuffers.Clear();
      ibcbuffers.Clear();
      ibcbtypes.Clear();
    }

    memset(neededit.GetArray(),0,neededit.Count()*sizeof(Bool_t));

    //Loop over the input trees
    for(i=0; i<neededib.Count(); i++) {
      memset(neededib[i].GetArray(),0,neededib[i].Count()*sizeof(Bool_t));
    }

    memset(neededot.GetArray(),0,neededot.Count()*sizeof(Bool_t));

    //Loop over the input trees
    for(i=0; i<neededob.Count(); i++) {
      memset(neededob[i].GetArray(),0,neededob[i].Count()*sizeof(Bool_t));
    }

    //Loop over all processes
    for(i=0; i<fProcs->Count(); i++) {

      //If the current process has never been run or if it is triggered by the parameters mask
      if(firstrun || ((*fProcsParDepends)[i] && pardiffs)) {
	printf("Process '%s' will be called\n",(*fProcs)[i].GetName());
	//Add it to the list of needed processes
	procs.Add(&(*fProcs)[i]);
	selprocs.Add((*fSelProcs)[i]);
	if((*fSelProcs)[i]) doselection=kTRUE;
	seldepprocs.Add((*fSelDepProcs)[i]);

	//If the current process is processing all events
	if(i < fNAEProcs) {
	  //Increment the number of triggered processes that process all events
	  naeprocs++;
	}

	//Loop over the output branches of the current process
	nj=(*fOTIndices)[i].Count();
	for(j=0; j<nj; j++) {
	  //Add the tree of the current output branch to the list of needed output trees
	  k=(*fOTIndices)[i][j];
	  neededot[k]=kTRUE;
	  //Add the current output branch to the list of needed output branches
	  neededob[k][(*fOBIndices)[i][j]]=kTRUE;
	}

	//Loop over the input branches of the current process
	nj=(*fITIndices)[i].Count();
	for(j=0; j<nj; j++) {
	  //Add the tree of the current input branch to the list of needed input trees
	  k=(*fITIndices)[i][j];
	  neededit[k]=kTRUE;
	  //Add the current input branch to the list of needed input branches
	  neededib[k][(*fIBIndices)[i][j]]=kTRUE;
	}
      }
    }

    //Loop over all output trees
    for(i=0; i<neededob.Count(); i++) {

      //If the current output tree is needed
      if(neededot[i]) {
	nj=neededob[i].Count();

	//Get a pointer to the tree
	tbuf=((TBranch*)(*fOBranches)[i][0])->GetTree();

	//Add it to the list of needed output trees
	otrees.Add(tbuf);

	//Loop over the branches of this output tree
	for(j=0; j<nj; j++) {

	  //If the current output branch is needed
	  if(neededob[i][j]) {
	    printf("Branch '%s' (%p) from tree '%s%s' will be filled\n",((TBranch*)(*fOBranches)[i][j])->GetName(),(*fOBranches)[i][j],tbuf->GetDirectory()->GetPath(),tbuf->GetName());
	    //Delete the baskets for the output branch
	    ((TBranch*)(*fOBranches)[i][j])->DeleteBaskets("all");
	    //Add it to the list of needed output branches
	    obranches.Add((*fOBranches)[i][j]);
	    obsproc.Add((*fOTSProc)[i]);
	  }
	}
      }
    }

    //Loop over all input trees
    for(i=0; i<neededib.Count(); i++) {
      bbuf=kFALSE;

      //If the current input tree is needed
      if(neededit[i]) {
	nj=neededib[i].Count();

	//Get a pointer to the tree
	tbuf=((TBranch*)(*fIBranches)[i][0])->GetTree();

	//Loop over the branches of this input tree
	for(j=0; j<nj; j++) {

	  //If the branch is needed and it is not also an output branch
	  if(neededib[i][j] && obranches.FindFirst((TBranch*)(*fIBranches)[i][j]) == -1) {
	    printf("Branch '%s' (%p) from tree '%s%s' needs to be loaded\n",((TBranch*)(*fIBranches)[i][j])->GetName(),(*fIBranches)[i][j],tbuf->GetDirectory()->GetPath(),tbuf->GetName());

	    //If the current input branch uses a different data type
	    if(fIBCBuffers->Count() && (*fIBCBuffers)[i].Count() && (*fIBCBuffers)[i][j]) {
	      //Add the buffer addresses and the data type id
	      ibbuffers.Add((*fIBBuffers)[i][j]);
	      ibcbuffers.Add((*fIBCBuffers)[i][j]);
	      ibcbtypes.Add((*fIBCBTypes)[i][j]);
	    }

	    //Add it to the list of needed input branches
	    ibranches.Add((*fIBranches)[i][j]);

	    bbuf=kTRUE;
	  }
	}

	//If at least one branch of the current input tree has to be read from the tree
	if(bbuf) {

	  //If the current tree should contain all events
	  if(!(*fITSProc)[i]) {
	    //Get the number of entries for the current tree
	    neaet=tbuf->GetEntries();
	    printf("Number of entries in tree '%s%s' that should contain all events: %i\n",tbuf->GetDirectory()->GetPath(),tbuf->GetName(),neaet);

	    //If the number of entries for the current input tree does not match the number of entries for the previous triggered input tree
	    if(neaet != neaetlast && neaetlast != -1) {
	      fprintf(stderr,"QTTreeProcessor::Exec(): Error: The number of entries in tree '%s%s' does not match the number of entries for the previously triggered input tree\n",tbuf->GetDirectory()->GetPath(),tbuf->GetName());
	    }
	    neaetlast=neaet;

	    //Else if the current tree should contain only selected events
	  } else {
	    //Get the number of entries for the current tree
	    neset=tbuf->GetEntries();
	    printf("Number of entries in tree '%s%s' that should contain selected events: %i\n",tbuf->GetDirectory()->GetPath(),tbuf->GetName(),neset);

	    //If the number of entries for the current input tree does not match the number of entries for the previous triggered input tree
	    if(neset != nesetlast && nesetlast != -1) {
	      fprintf(stderr,"QTTreeProcessor::Exec(): Error: The number of entries in tree '%s%s' does not match the number of entries for the previously triggered input tree\n",tbuf->GetDirectory()->GetPath(),tbuf->GetName());
	    }
	    nesetlast=neset;
	  }
	}
      }
    }

    nentries=neaetlast>nesetlast?neaetlast:nesetlast;

    //If at least one tree containing all events has to be read
    if(nentries != -1) {

      QProgress progress(nentries);
      //Loop over the entries
      for(i=0; i<nentries; i++) {
	//printf("Entry %i/%i\n",i,nentries);

	//Load all triggered input branches
	for(j=0; j<ibranches.Count(); j++) {
	  //printf("\tInput branch %i/%i\n",j,ibranches.Count());
	  ((TBranch*)ibranches.GetArray()[j])->GetEntry(i);
	}

	//Convert all buffers for branches containing a different data type
	for(j=0; j<ibbuffers.Count(); j++) {
	  //printf("\tBuffer conversion %i/%i\n",j,ibbuffers.Count());

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
	eventselected=kTRUE;

	//Loop over all triggered all events processes
	for(j=0; j<naeprocs; j++) {
	  //Exec the process. If it is a selector process, use the output to check if the entry is selected or not
	  if(selprocs[j]) eventselected&=((QNamedProc*)procs.GetArray()[j])->Exec();
	  else ((QNamedProc*)procs.GetArray()[j])->Exec();
	}

	//Assertion: Post-selection processes that depend on a selected events tree are called only for selected events
	//Loop over all triggered post-selection processes
	for(j=naeprocs; j<procs.Count(); j++) {
	  //If the process does not depend on a selector process or if the entry is selected, execute it
	  if(!seldepprocs[j] || (eventselected && (doselection || i < nesetlast))) ((QNamedProc*)procs.GetArray()[j])->Exec();
	}
	
	//Assertion: Output branches are filled when it does not depend directly or indirectly on a selector process or
	//when an event is selected or when loading an entry from an existing tree containing only selected events.
	//Save all triggered output branches
	for(j=0; j<obranches.Count(); j++) {
	  //printf("\tOutput branch %i/%i\n",j,obranches.Count());
	  //printf("%i\t",(Int_t)((TBranch*)obranches.GetArray()[j])->GetEntries());
	  if(!obsproc[j] || (eventselected && (doselection || i < nesetlast))) ((TBranch*)obranches.GetArray()[j])->Fill();
	}
	//printf("\n");

	progress(i+1);
      }
      progress(i,kTRUE);
      printf("\n");

      //Loop over all output trees
      for(i=0; i<otrees.Count(); i++) {
	//Get a pointer to the current tree
	tbuf=(TTree*)otrees.GetArray()[i];
	//Set the number of entries for the tree to the number of entries for the first branch
	tbuf->SetEntries(((TBranch*)tbuf->GetListOfBranches()->First())->GetEntries());
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
    return GetProc(i);
  }
  fprintf(stderr,"QTTreeProcessor::GetProc: Procedure '%s' does not exist\n",procname);
  throw 1;
  return GetProc(0);
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

  ClearIBCBuffers();
  ClearIBBuffers();

  fIBranches->RedimList(fITNames->Count());
  fOBranches->RedimList(fOTNames->Count());
  fIBBuffers->RedimList(fITNames->Count());
  fOwnsIBBuffers->RedimList(fITNames->Count());
  fOBBuffers->RedimList(fOTNames->Count());
  fBuffers->RedimList(fBuNames->Count());

  //Loop over the output trees
  for(i=0; i<fOTNames->Count(); i++) {

    if(!(dbuf=gDirectory->GetDirectory(fAnalysisDir))) {
      fprintf(stderr,"QTTreeProcessor::InitProcess(): Error: Directory '%s' does not exist\n",fAnalysisDir.Data());
      return 1;
    }
    dbuf->cd();

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

  TLeaf *lbuf;
  const char* cabuf;

  //Loop over the input trees
  for(i=0; i<fITNames->Count(); i++) {

    if(!(dbuf=gDirectory->GetDirectory(fAnalysisDir))) {
      fprintf(stderr,"QTTreeProcessor::InitProcess(): Error: Directory '%s' does not exist\n",fAnalysisDir.Data());
      return 1;
    }
    dbuf->cd();

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
      (*fIBBuffers)[i].RedimList((*fIBNames)[i].Count(),-1,NULL);
      (*fOwnsIBBuffers)[i].RedimList((*fIBNames)[i].Count(),-1,kFALSE);

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

	  //If there is no buffer assigned to the branch
	  if(!((TBranch*)(*fIBranches)[i][j])->GetAddress()) {
	    //Create a new buffer
	    (*fIBBuffers)[i][j]=new Double_t;
	    (*fOwnsIBBuffers)[i][j]=kTRUE;
	    ((TBranch*)(*fIBranches)[i][j])->SetAddress((*fIBBuffers)[i][j]);

	    //Else if there is already a buffer assigned to the branch
	  } else {
	    //Get the buffer address from the branch
	    (*fIBBuffers)[i][j]=(Double_t*)((TBranch*)(*fIBranches)[i][j])->GetAddress();
	  }

	  //Else if the input branch has a different basic data type, assign a different temporary buffer
	} else {
	  //Create a new Double_t buffer for the branch
	  (*fIBBuffers)[i][j]=new Double_t;
	  (*fOwnsIBBuffers)[i][j]=kTRUE;

	  //Redim lists if this is the first input tree having branches with a different data type
	  if(!fIBCBuffers->Count()) {
	    fIBCBuffers->RedimList(fITNames->Count());
	    fOwnsIBCBuffers->RedimList(fITNames->Count());
	    fIBCBTypes->RedimList(fITNames->Count());
	  }

	  //Redim lists if this is the first branch of the input tree having a  different data type
	  if(!(*fIBCBuffers)[i].Count()) {
	    (*fIBCBuffers)[i].RedimList((*fIBNames)[i].Count(),-1,NULL);
	    (*fOwnsIBCBuffers)[i].RedimList((*fIBNames)[i].Count(),-1,kFALSE);
	    (*fIBCBTypes)[i].RedimList((*fIBNames)[i].Count(),-1,0);
	  }

	  if(!strcmp(cabuf,"Float_t")) {
	    (*fIBCBTypes)[i][j]=kFloat_t;

	  } else if(!strcmp(cabuf,"UInt_t")) {
	    (*fIBCBTypes)[i][j]=kUInt_t;

	  } else if(!strcmp(cabuf,"Int_t")) {
	    (*fIBCBTypes)[i][j]=kInt_t;

	  } else if(!strcmp(cabuf,"UShort_t")) {
	    (*fIBCBTypes)[i][j]=kUShort_t;

	  } else if(!strcmp(cabuf,"Short_t")) {
	    (*fIBCBTypes)[i][j]=kShort_t;

	  } else if(!strcmp(cabuf,"UChar_t")) {
	    (*fIBCBTypes)[i][j]=kUChar_t;

	  } else if(!strcmp(cabuf,"Char_t")) {
	    (*fIBCBTypes)[i][j]=kChar_t;

	  } else if(!strcmp(cabuf,"Bool_t")) {
	    (*fIBCBTypes)[i][j]=kBool_t;

	  } else {
	    fprintf(stderr,"QTTreeProcessor::InitProcess(): Error: The data type '%s' contained in branch '%s' from tree '%s:%s' is not supported\n",cabuf,(*fIBNames)[i][j].Data(),(*fITNames)[i][1].Data(),(*fITNames)[i][0].Data());
	    return 1;
	  }

	  //If there is no buffer associated to that branch
	  if(!((TBranch*)(*fIBranches)[i][j])->GetAddress()) {

	    //Create a new buffer having the proper size and assign it to the branch
	    switch((*fIBCBTypes)[i][j]) {
	      case kFloat_t:
		(*fIBCBuffers)[i][j]=malloc(sizeof(Float_t));
		break;
	      case kUInt_t:
		(*fIBCBuffers)[i][j]=malloc(sizeof(UInt_t));
		break;
	      case kInt_t:
		(*fIBCBuffers)[i][j]=malloc(sizeof(Int_t));
		break;
	      case kUShort_t:
		(*fIBCBuffers)[i][j]=malloc(sizeof(UShort_t));
		break;
	      case kShort_t:
		(*fIBCBuffers)[i][j]=malloc(sizeof(Short_t));
		break;
	      case kUChar_t:
		(*fIBCBuffers)[i][j]=malloc(sizeof(UChar_t));
		break;
	      case kChar_t:
		(*fIBCBuffers)[i][j]=malloc(sizeof(Char_t));
		break;
	      case kBool_t:
		(*fIBCBuffers)[i][j]=malloc(sizeof(Bool_t));
	    }
	    ((TBranch*)(*fIBranches)[i][j])->SetAddress((*fIBCBuffers)[i][j]);
	    (*fOwnsIBCBuffers)[i][j]=kTRUE;

	    //Else if there is already a buffer for this branch
	  } else {
	    (*fIBCBuffers)[i][j]=((TBranch*)(*fIBranches)[i][j])->GetAddress();
	  }
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
      proc->SetParamPtr(j,&((*fParams)[fParamsNames->FindFirst(proc->GetParam(j).GetName())]));
    }

    //Loop over the inputs for the current process
    for(j=0; j<proc->GetNIVars(); j++) {
      sbuf=proc->GetIVarNameTitle(j);

      //If the input is not a memory buffer
      if(sbuf.Length()) {
	//Decode the object name
	donbuf=QFileUtils::DecodeObjName(sbuf);
	//Find the object in the list of inputs
	k=fITNames->FindFirst(donbuf); //Tree index

	//If there are input buffers for this tree (i.e. the tree is not also listed as an output tree)
	if((*fIBBuffers)[k].Count()) {
	//Get the buffer address from the input branches buffers list
	proc->SetIVarPtr(j,(*fIBBuffers)[k][(*fIBNames)[k].FindFirst(proc->GetIVarNameTitle(j).GetName())]);

	//Else if there are no input buffers
	} else {
	  //Find the object in the list of outputs
	  k=fOTNames->FindFirst(donbuf);
	  //Get the buffer address from the output branches buffers list
	  proc->SetIVarPtr(j,&((*fOBBuffers)[k][(*fOBNames)[k].FindFirst(proc->GetIVarNameTitle(j).GetName())]));
	}

	//Else if the input is a memory buffer
      } else {
	//Get the buffer address from the memory buffers list
	proc->SetIVarPtr(j,&((*fBuffers)[fBuNames->FindFirst(proc->GetIVarNameTitle(j).GetName())]));
      }
    }

    //Loop over the output for the current process
    for(j=0; j<proc->GetNOVars(); j++) {

      sbuf=proc->GetOVarNameTitle(j);

      //If the output is not a memory buffer
      if(sbuf.Length()) {
	//Get the buffer address from the output branches buffers list
	k=fOTNames->FindFirst(QFileUtils::DecodeObjName(sbuf)); //Tree index
	proc->SetOVarPtr(j,&((*fOBBuffers)[k][(*fOBNames)[k].FindFirst(proc->GetOVarNameTitle(j).GetName())]));

	//Else if the output is a memory buffer
      } else {
	//Get the buffer address from the memory buffers list
	proc->SetOVarPtr(j,&((*fBuffers)[fBuNames->FindFirst(proc->GetOVarNameTitle(j).GetName())]));
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
  *fSelProcs=*rhs.fSelProcs;
  fNAEProcs=rhs.fNAEProcs;
  *fParams=*rhs.fParams;
  *fParamsNames=*rhs.fParamsNames;
  fAnalysisDir=rhs.fAnalysisDir;
  *fITNames=*rhs.fITNames;
  *fOTNames=*rhs.fOTNames;
  *fIBNames=*rhs.fIBNames;
  *fOBNames=*rhs.fOBNames;
  *fBuNames=*rhs.fBuNames;
  *(fITIndices)=*rhs.fITIndices;
  *(fIBIndices)=*rhs.fIBIndices;
  *(fOTIndices)=*rhs.fOTIndices;
  *(fOBIndices)=*rhs.fOBIndices;
  *(fAITrees)=*rhs.fAITrees;
  *fSelDepProcs=*rhs.fSelDepProcs;
  *fITSProc=*rhs.fITSProc;
  *fOTSProc=*rhs.fOTSProc;
  *fProcsParDepends=*rhs.fProcsParDepends;
  *fProcsTDepends=*rhs.fProcsTDepends;
  *fProcsBDepends=*rhs.fProcsBDepends;
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

    if(i<fNAEProcs) {
      printf("\n%03i All events process '%s'\n",i,proc->GetName());

    } else {
      printf("\n%03i Selected events process '%s'\n",i,proc->GetName());
    }

    printf("Parameters:\n");
    for(j=0; j<proc->GetNParams(); j++) {
      printf("%3i:\t%s\n",j,proc->GetParam(j).GetName());
    }

    printf("\nInputs:\n");
    k=0;
    for(j=0; j<proc->GetNIVars(); j++) {
      sbuf=proc->GetIVarNameTitle(j);
      donbuf.Clear();
      if(sbuf.Length()) donbuf=QFileUtils::DecodeObjName(sbuf);
      printf("%3i",j);
      if(donbuf.Count()>0) printf("\t%s",donbuf[0].Data());
      if(donbuf.Count()==2) printf("\t%s",donbuf[1].Data());
      printf("\t%s",proc->GetIVarNameTitle(j).GetName());
      if(donbuf.Count()>0) {printf(" (%i.%i)",(*fITIndices)[i][k],(*fIBIndices)[i][k]); k++;}
      printf("\n");
    }

    printf("\nOutputs:\n");
    k=0;
    for(j=0; j<proc->GetNOVars(); j++) {
      sbuf=proc->GetOVarNameTitle(j);
      donbuf.Clear();
      if(sbuf.Length()) donbuf=QFileUtils::DecodeObjName(sbuf);
      printf("%3i",j);
      if(donbuf.Count()>0) printf("\t%s",donbuf[0].Data());
      if(donbuf.Count()==2) printf("\t%s",donbuf[1].Data());
      printf("\t%s",proc->GetOVarNameTitle(j).GetName());
      if(donbuf.Count()>0) {printf(" (%i.%i)",(*fOTIndices)[i][k],(*fOBIndices)[i][k]); k++;}
      printf("\n");
    }

    printf("\nDependencies:\n");
    (*fProcsParDepends)[i].Print();

    if((*fSelProcs)[i]) printf("\nIs a selector process\n");
    else printf("\nIs not a selector process\n");

    if((*fSelDepProcs)[i]) printf("Depends on a selector process\n");
    else printf("Does not depend on a selector process\n");
  }

  printf("\nAll Input Branches:\n");
  for(i=0; i<fITNames->Count(); i++) {
    printf("%3i Tree %s",i,(*fITNames)[i][0].Data());
    if((*fITNames)[i].Count() ==2) printf(" %s",(*fITNames)[i][1].Data());
    printf("\n");

    for(j=0; j<(*fIBNames)[i].Count(); j++) {
      printf("\t%3i %s\n",j,(*fIBNames)[i][j].Data());
    }
    printf("\n");
  }

  printf("All Output Branches:\n");
  for(i=0; i<fOTNames->Count(); i++) {
    printf("%3i Tree %s",i,(*fOTNames)[i][0].Data());
    if((*fOTNames)[i].Count() ==2) printf(" %s",(*fOTNames)[i][1].Data());
    printf("\n");

    for(j=0; j<(*fOBNames)[i].Count(); j++) {
      printf("\t%3i %s\t",j,(*fOBNames)[i][j].Data());
      if((*fOTSProc)[i]) printf("Depends on a selector process\n");
      else printf("Does not depend on a selector process\n");
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

void QTTreeProcessor::Browse(TBrowser *b)
{   
  if(b) { 
    b->Add(fProcs,"Processes");
  } 
}

void QTTreeProcessor::ClearIBBuffers()
{
  Int_t i,j;

  for(i=0; i<fIBBuffers->Count(); i++) {

    for(j=0; j<(*fIBBuffers)[i].Count(); j++) {
      if((*fOwnsIBBuffers)[i][j]) free((*fIBBuffers)[i][j]);
    }
  }
  fIBBuffers->Clear();
  fOwnsIBBuffers->Clear();
}

void QTTreeProcessor::ClearIBCBuffers()
{
  fIBCBTypes->Clear();
  Int_t i,j;

  for(i=0; i<fIBCBuffers->Count(); i++) {

    for(j=0; j<(*fIBCBuffers)[i].Count(); j++) {
      if((*fOwnsIBCBuffers)[i][j]) free((*fIBCBuffers)[i][j]);
    }
  }
  fIBCBuffers->Clear();
  fOwnsIBCBuffers->Clear();
}

Int_t QTTreeProcessor::AEProcIndexToIndex(Int_t index){
  if(index == -1) return fNAEProcs;

  if(index > fNAEProcs) {
    fprintf(stderr,"QTTreeProcessor::AEProcIndexToIndex: Error: Index %i is invalid\n",index);
    throw 1;
  }

  return index;
}

Int_t QTTreeProcessor::PSProcIndexToIndex(Int_t index){
  if(index == -1) return fProcs->Count();

  if(index < fNAEProcs) {
    fprintf(stderr,"QTTreeProcessor::PSProcIndexToIndex: Error: Index %i is invalid\n",index);
    throw 1;
  }

  return index;
}

QList<Int_t> QTTreeProcessor::QDependentProcs::GetAllDepends() const
{
  QList<Int_t> ret;

  if(fInitIdx == -1) {
    fInitIdx=fIdx;

  } else {
    ret.Add(fIdx);
  }

  for(Int_t i=0; i<fDepends.Count(); i++) {
    if(fDepends[i] != fInitIdx && fPCalled.FindFirst(fDepends[i]) == -1) {
      fPCalled.Add(fDepends[i]);
      ret.Add(((QDependentProcs*)fQPDObjs[fDepends[i]])->GetAllDepends());
    }
  }

  if(fInitIdx == fIdx) {
    fPCalled.Clear();
    fInitIdx=-1;
  }

  return ret;
}

#include "debugger.h"
