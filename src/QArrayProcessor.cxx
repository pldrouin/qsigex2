#include "QArrayProcessor.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QArrayProcessor)

QArrayProcessor::~QArrayProcessor()
{
  PRINTF2(this,"\tQArrayProcessor::~QArrayProcessor()\n")
  delete fSelProcs;
  fSelProcs=NULL;
  delete fTrigAEProcs;
  fTrigAEProcs=NULL;
  delete fIANames;
  fIANames=NULL;
  delete fOANames;
  fOANames=NULL;
  delete fBuNames;
  fBuNames=NULL;
  delete fIAIndices;
  fIAIndices=NULL;
  delete fIOIndices;
  fIOIndices=NULL;
  delete fOAIndices;
  fOAIndices=NULL;
  delete fOOIndices;
  fOOIndices=NULL;
  delete fIArrays;
  fIArrays=NULL;
  delete fIObjects;
  fIObjects=NULL;
  delete fOArrays;
  fOArrays=NULL;
  delete fOObjects;
  fOObjects=NULL;
  delete fSelDepProcs;
  fSelDepProcs=NULL;
  delete fIASProc;
  fIASProc=NULL;
  delete fOASProc;
  fOASProc=NULL;
  delete fAPDepends;
  fAPDepends=NULL;
  delete fObjsPDepends;
  fObjsPDepends=NULL;
  delete fBuffers;
  fBuffers=NULL;
}

void QArrayProcessor::AddProc(const char *name, const char *title, Bool_t selector, Int_t index)
{
  PRINTF10(this,"\tQArrayProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, Bool_t selector<",selector,">, Int_t index<",index,">)\n")

  index=AEProcIndexToIndex(index);
  fNAEProcs++;
  fProcs->RedimList(fProcs->Count()+1,index);
  fSelProcs->Add(selector,index);
  (*fProcs)[index].SetNameTitle(name,title);
}

void QArrayProcessor::AddProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname, Bool_t selector, Int_t index)
{
  PRINTF14(this,"\tQArrayProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, Bool_t (*proc)(QProcArgs&)<",proc,">, const char *procname<'",procname,"'>, Bool_t selector<",selector,">, Int_t index<",index,">)\n")

  index=AEProcIndexToIndex(index);
  AddProc(name,title,selector,index);
  (*fProcs)[index].SetProc(proc,procname);
}

void QArrayProcessor::AddProc(const char *name, const char *title, const char *procname, Bool_t selector, Int_t index)
{
  PRINTF12(this,"\tQArrayProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, const char *procname<'",procname,"'>, Bool_t selector<",selector,">, Int_t index<",index,">)\n")

  index=AEProcIndexToIndex(index);
  AddProc(name,title,selector,index);
  (*fProcs)[index].SetProc(procname);
}

void QArrayProcessor::AddProc(const char *name, const char *title, void *proc, const char *procname, Bool_t selector, Int_t index)
{
  PRINTF14(this,"\tQArrayProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, void *proc<",proc,">, const char *procname<'",procname,"'>, Bool_t selector<",selector,">, Int_t index<",index,">)\n")

  index=AEProcIndexToIndex(index);
  AddProc(name,title,selector,index);
  (*fProcs)[index].SetProc(proc,procname);
}

void QArrayProcessor::AddPSProc(const char *name, const char *title, Bool_t selectedonly, Int_t index)
{
  PRINTF10(this,"\tQArrayProcessor::AddPSProc(const char *name<'",name,"'>, const char *title<'",title,"'>, Bool_t selectedonly<",selectedonly,">, Int_t index<",index,">)\n")

  index=PSProcIndexToIndex(index);
  fProcs->RedimList(fProcs->Count()+1,index);
  fSelProcs->Add(selectedonly,index);
  (*fProcs)[index].SetNameTitle(name,title);
}

void QArrayProcessor::AddPSProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname, Bool_t selectedonly, Int_t index)
{
  PRINTF14(this,"\tQArrayProcessor::AddPSProc(const char *name<'",name,"'>, const char *title<'",title,"'>, Bool_t (*proc)(QProcArgs&)<",proc,">, const char *procname<'",procname,"'>, Bool_t selectedonly<",selectedonly,">, Int_t index<",index,">)\n")

  index=PSProcIndexToIndex(index);
  AddPSProc(name,title,selectedonly,index);
  (*fProcs)[index].SetProc(proc,procname);
}

void QArrayProcessor::AddPSProc(const char *name, const char *title, const char *procname, Bool_t selectedonly, Int_t index)
{
  PRINTF12(this,"\tQArrayProcessor::AddPSProc(const char *name<'",name,"'>, const char *title<'",title,"'>, const char *procname<'",procname,"'>, Bool_t selectedonly<",selectedonly,">, Int_t index<",index,">)\n")

  index=PSProcIndexToIndex(index);
  AddPSProc(name,title,selectedonly,index);
  (*fProcs)[index].SetProc(procname);
}

void QArrayProcessor::AddPSProc(const char *name, const char *title, void *proc, const char *procname, Bool_t selectedonly, Int_t index)
{
  PRINTF14(this,"\tQArrayProcessor::AddPSProc(const char *name<'",name,"'>, const char *title<'",title,"'>, void *proc<",proc,">, const char *procname<'",procname,"'>, Bool_t selectedonly<",selectedonly,">, Int_t index<",index,">)\n")

  index=PSProcIndexToIndex(index);
  AddPSProc(name,title,selectedonly,index);
  (*fProcs)[index].SetProc(proc,procname);
}

void QArrayProcessor::Analyze()
{
  QStdProcessor::Analyze();
  Int_t i,j,k;
  QNamedProc *proc;
  QList<TString> params; //Parameters
  TString sbuf;
  QList<TString> qlsbuf;
  qlsbuf.RedimList(2);

  fAnalysisDir=gDirectory->GetPath();

  Int_t nprocs=fProcs->Count();
  Int_t nivars, niobjs, novars, noobjs, nparams;
  Int_t iidx, oidx;

  fIANames->Clear();
  fOANames->Clear();
  fAPDepends->Clear();
  fObjsPDepends->Clear();
  fIAIndices->Clear();
  fIOIndices->Clear();
  fOAIndices->Clear();
  fOOIndices->Clear();
  fIObjects->Clear();
  fOObjects->Clear();

  fIAIndices->RedimList(nprocs);
  fIOIndices->RedimList(nprocs);
  fOAIndices->RedimList(nprocs);
  fOOIndices->RedimList(nprocs);
  QDependentProcs *depprocs=new QDependentProcs[nprocs]; //QDependentProcs objects contain a list of processes that should be triggered
  QList<Int_t> oalastproc; //Index of last process having recorded its output in a given array
  QList<Int_t> oolastproc; //Index of last process having recorded its output in a given object
  QList<Int_t> blastproc;  //Index of last process having recorded its output in a given memory buffer
  fTrigAEProcs->RedimList(nprocs);
  fProcsParDepends->RedimList(nprocs);
  fSelDepProcs->RedimList(nprocs);

  //Section 1: First pass over all processes. Setting as many inputs, outputs and dependencies as possible and doing some checks
  //Loop over the processes
  for(i=0; i<nprocs; i++) {
    proc=&((*fProcs)[i]);
    //printf("Process '%s'\n",proc->GetName());
    nivars=proc->GetNIVars();
    niobjs=proc->GetNIObjs();
    novars=proc->GetNOVars();
    noobjs=proc->GetNOObjs();
    nparams=proc->GetNParams();

    (*fTrigAEProcs)[i]=kFALSE;
    (*fProcsParDepends)[i].Clear();
    (*fSelDepProcs)[i]=kFALSE;

    //Loop over the parameters for the current process
    for(j=0; j<nparams; j++) {
      //Turn on the bit of the dependency mask for the current parameter
      (*fProcsParDepends)[i].SetBit(fParamsNames->FindFirst(proc->GetParam(j).GetName()),1);
    }

    //Loop over the input variables for the current process
    k=0;
    for(j=0; j<nivars; j++) {
      sbuf=proc->GetIVarNameTitle(j).GetTitle();
      qlsbuf[0]=proc->GetIVarNameTitle(j).GetName();
      if(sbuf.Length()) {
	qlsbuf.RedimList(2);
	qlsbuf[1]=sbuf;
      } else qlsbuf.RedimList(1);

      //If the input is from an array
      if(qlsbuf.Count()==2) {
	  //printf("Input from array '%s\t%s'\n",qlsbuf[0].Data(),qlsbuf[1].Data());

	//Ensure the required array is in the list
	iidx=fIANames->AddUnique(qlsbuf);

	//If the array is not already listed in the list of existing arrays
	if(iidx == -1) {
	  //printf("New input array\n");
	  //Add the current process to the list of processes that depend on the current input array
	  fAPDepends->RedimList(fIANames->Count());
	  iidx=fIANames->Count()-1;
	}
	(*fAPDepends)[iidx].SetBit(i,kTRUE);
	(*fIAIndices)[i].Add(iidx);

	oidx=fOANames->FindFirst(qlsbuf);

	//If the array has been generated by a previous process
	if(oidx != -1) {
	  //printf("Intermediary input\n");

	  //Add the index of the current process to the dependent processes list of the last process that has saved a value in the current branch
	  depprocs[oalastproc[oidx]].AddDepend(i);

	  //If this is not the first input and the dependency of the current input on selector processes is different than for the previous input
	  if(k!=0 && (*fSelDepProcs)[oalastproc[oidx]] != (*fSelDepProcs)[i]) {
	    (*fTrigAEProcs)[i]=kTRUE;
	  }
	  //Add the dependencies on selector processes of the last process that has saved a value in the current branch to the dependencies for the current process
	  (*fSelDepProcs)[i]|=(*fSelDepProcs)[oalastproc[oidx]];
	  k++;
	}

	//Else if the input is from a buffer in memory
      } else {
	//printf("Input from a memory buffer\n");
	iidx=fBuNames->FindFirst(qlsbuf[0]);

	//If the buffer has not been listed as an output for a previous process
	if(iidx == -1) {
	  fprintf(stderr,"QArrayProcessor: Analyze(): Error with process '%s': Input buffer '%s' was not generated by a previous process\n",proc->GetName(),qlsbuf[0].Data());
	  throw 1;
	}

	//Add the index of the current process to the dependent processes list of the last process that has saved a value in the current memory buffer
	depprocs[blastproc[iidx]].AddDepend(i);
	//Add the index of the last process that has saved a value in the current memory buffer to the dependent processes list of the current process
	depprocs[i].AddDepend(blastproc[iidx]);

	//If this is not the first input and the dependency of the current input on selector processes is different than for the previous input
	if(k!=0 && (*fSelDepProcs)[blastproc[iidx]] != (*fSelDepProcs)[i]) {
	  (*fTrigAEProcs)[i]=kTRUE;
	}
	//Add the dependencies on selector processes of the last process that has saved a value in the current memory buffer to the dependencies for the current process
	(*fSelDepProcs)[i]|=(*fSelDepProcs)[blastproc[iidx]];
	k++;
      }
    }
    //If the current process is a selector process, adds it to fSelDepProcs
    (*fSelDepProcs)[i]|=(*fSelProcs)[i];

    //Loop over input objects for the current process
    (*fIOIndices)[i].RedimList(niobjs);
    for(j=0; j<niobjs; j++) {
      //Add the object
      iidx=fIObjects->AddUnique(const_cast<QProcObj*>(proc->IObj(j)));

      if(iidx == -1) {
	iidx=fIObjects->Count()-1;
	fObjsPDepends->RedimList(fObjsPDepends->Count()+1);
      }
      (*fIOIndices)[i][j]=iidx;
      (*fObjsPDepends)[iidx].SetBit(i,kTRUE);

      oidx=fOObjects->FindFirst(const_cast<QProcObj*>(proc->IObj(j)));

      //If the object has been updated by a previous process
      if(oidx != -1) {
	//Add the index of the current process to the dependent process list of the last process that updated the current input object
	depprocs[oolastproc[oidx]].AddDepend(i);
      }
    }

    //Loop over the output variables for the current process
    for(j=0; j<novars; j++) {
      sbuf=proc->GetOVarNameTitle(j).GetTitle();
      qlsbuf[0]=proc->GetOVarNameTitle(j).GetName();
      if(sbuf.Length()) {
	qlsbuf.RedimList(2);
	qlsbuf[1]=sbuf;
      } else qlsbuf.RedimList(1);

      //If the output is in an array
      if(qlsbuf.Count() == 2) {

	//Try to find the output in previous input and output lists
	iidx=fIANames->FindFirst(qlsbuf);
	oidx=fOANames->FindFirst(qlsbuf);

	if(oidx==-1 && iidx!=-1) {
	  fprintf(stderr,"QArrayProcessor: Analyze(): Error with process '%s': Array '%s\t%s' cannot be overwritten\n",proc->GetName(),qlsbuf[0].Data(),qlsbuf[1].Data());
	  throw 1;
	}

	//If this is not the first time that the processor outputs to that variable and the variable was also an input for a previous process
	if(oidx!=-1 && iidx!=-1) {
	  //Add the index of the last process that has saved a value to the current output variable to the dependent processes list of the current process
	  depprocs[i].AddDepend(oalastproc[oidx]);
	}

	//If this array has not been generated by a previous process
	if(oidx == -1) {
	  //printf("New output array\n");
	  //Add the name of the output branch
	  fOANames->Add(qlsbuf);
	  oidx=fOANames->Count()-1;
	  oalastproc.RedimList(fOANames->Count());
	}
	oalastproc[oidx]=i;
	(*fOAIndices)[i].Add(oidx);

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

    //If the current process has output objects, is not a post-selection process and depends directly or indirectly on a selector process
    if(noobjs && i<fNAEProcs && (*fSelDepProcs)[i]) {
      (*fTrigAEProcs)[i]=kTRUE;
    }

    //Loop over output objects for the current process
    (*fOOIndices)[i].RedimList(noobjs);
    for(j=0; j<noobjs; j++) {

      //Try to find the output object in previous input and output lists
      iidx=fIObjects->FindFirst(proc->OObj(j));
      oidx=fOObjects->FindFirst(proc->OObj(j));

      if(oidx==-1 && iidx!=-1) {
	fprintf(stderr,"QArrayProcessor: Analyze(): Error with process '%s': Object '%p' cannot be overwritten\n",proc->GetName(),proc->OObj(j));
	throw 1;
      }

      //If this is not the first time that the processor outputs to the object and the object was also an input for a previous process
      if(oidx!=-1 && iidx!=-1) {
	//Add the index of the last process that has saved a value to the current output object to the dependent processes list of the current process
	depprocs[i].AddDepend(oolastproc[oidx]);
      }

      //Add the object if it has not been generated by a previous process
      if(oidx==-1) {
	fOObjects->Add(proc->OObj(j));
	oidx=fOObjects->Count()-1;
	oolastproc.RedimList(fOObjects->Count());
      }
      oolastproc[oidx]=i;
      (*fOOIndices)[i][j]=oidx;
    }
  }

  oalastproc.Clear();
  blastproc.Clear();

  //Section 2: Tag output arrays that depend directly or indirectly on selector processes
  fOASProc->RedimList(fOANames->Count());

  //Loop over the regular processes
  for(i=0; i<nprocs; i++) {

    //Loop over the output arrays for the current process
    for(j=0; j<(*fOAIndices)[i].Count(); j++) {
      (*fOASProc)[(*fOAIndices)[i][j]]=(*fSelDepProcs)[i];
    }
  }

  //Section 3: Tag input arrays that depend directly or indirectly on selector processes
  fIASProc->RedimList(fIANames->Count());
  memset(fIASProc->GetArray(),0,fIANames->Count()*sizeof(Bool_t));

  //Loop over input array
  for(i=0; i<fIANames->Count(); i++) {

    //If the current input array is also an output array
    if((oidx=fOANames->FindFirst((*fIANames)[i])) != -1) {
      (*fIASProc)[i]=(*fOASProc)[oidx];
    }
  }

  //Section 4: All selector processes (and post-selection processes that explicitely process only selected events) should be called if a given selector process is called. For this reason it is better if selector processes don't have any output (reduce I/O)
  //Loop over all processses
  for(i=0; i<nprocs; i++) {

    //If the current process is a selector process or if it is a post-selection process that explicitely processes only selected events or if it triggers the selectors
    if((*fSelProcs)[i] || (*fTrigAEProcs)[i]) {

      //Loop over all processes
      for(j=0; j<nprocs; j++) {

	//If the process j is a selector process or if it is a post-selection process that explicitely processes only selected events, add it to the list of dependent processes for process i
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

  //Section 6: Updating input arrays and input objects dependencies to take into account indirect dependencies

  //Loop over all processes
  for(i=0; i<nprocs; i++) {
    proc=&((*fProcs)[i]);
    //printf("%03i Process '%s'\n",i,proc->GetName());
    dpidx=depprocs[i].GetAllDepends();

    //Loop over all input arrays
    for(j=0; j<fIANames->Count(); j++) {

      //If the current process is triggered by the input branch
      if((*fAPDepends)[j].GetBit(i)) {

	//Loop over dependent processes
	for(k=0; k<dpidx.Count(); k++) {
	  //Add the dependent process to the dependencies list of the current branch
	  (*fAPDepends)[j].SetBit(dpidx[k],kTRUE);
	}
      }
    }

    //Loop over all input objects
    for(j=0; j<fObjsPDepends->Count(); j++) {

      //If the current process is triggered by the input object
      if((*fObjsPDepends)[j].GetBit(i)) {

	//Loop over dependent processes
	for(k=0; k<dpidx.Count(); k++) {
	  (*fObjsPDepends)[j].SetBit(dpidx[k],kTRUE);
	}
      }
    }
  }
  dpidx.Clear();

  delete[] depprocs;
}

void QArrayProcessor::DelProc(const char *procname)
{
  Int_t i;
  if((i=FindProcIndex(procname))!=-1) {
    DelProc(i);
  }
}

void QArrayProcessor::Exec(const Bool_t &forceall) const
{
  static QMask pardiffs; //Modified parameters since the last call
  static QMask depmods;  //Required processes due to modified input arrays or input objects
  static Bool_t runall;
  pardiffs.Clear();
  depmods.Clear();
  static Int_t i,j;

  //fLastParams gets cleared by the function Analyze, so this is how the first run is identified
  if(forceall || fLastExec.GetSec() != 0) {

    //Loop over parameters
    for(i=0; i<fParams->Count(); i++) {

      //If the current parameter value has changed, set the correspongind bit in the parameter mask
      if(*(fParams->GetArray()[i]) != fLastParams->GetArray()[i]) {
	pardiffs.SetBit(i,1);
      }
    }

    //Loop over all input arrays
    for(i=0; i<fIArrays->Count(); i++) {

      //If the current input array has been modified after the last run, add its mask to the mask of required processes.
      if((*fIArrays)[i]->NewerThan(fLastExec)) {
	depmods|=(*fAPDepends)[i];
      }
    }

    //Loop over all input objects
    for(i=0; i<fIObjects->Count(); i++) {

      //If the current input object has been modified after the last run, add its mask to the mask of required processes
      if((*fIObjects)[i]->NewerThan(fLastExec)) depmods|=(*fObjsPDepends)[i];
    }

    runall=forceall;
  } else {
    fNeededIA.RedimList(fIANames->Count());
    fNeededOA.RedimList(fOANames->Count());
    fNeededOO.RedimList(fOObjects->Count());
    runall=kTRUE;
  }

  //If at least one of the parameters has changed
  if(pardiffs || depmods || runall) {

    if(GetVerbosity()&QProcessor::kShowExec) printf("QArrayProcessor('%s')::Exec()\n",GetName());
    //printf("runall: %i\n",runall);
    //printf("Parameter mask for the current parameters: ");
    //pardiffs.Print();
    //printf("Object mask for the current parameters: ");
    //depmods.Print();

    static QList<QProcArray*> iaarrays; //List for needed input all events arrays
    static QList<QProcArray*> isarrays; //List for needed input selected events arrays
    static QList<QProcArray*> oarrays; //List for output arrays needing update
    static QList<QProcObj*> oobjects; //List for output objects needing update
    static QList<Bool_t>   oasproc;   //Indicate if output array needing update are dependent or not on a selector process
    static QList<TObject*> procs;     //List of needed processes
    static Int_t           naeprocs;  //Number of processes that process all entries in the array
    static QList<Bool_t> selprocs;    //Indicate if the needed processes is a selector process or not
    static QList<Bool_t> seldepprocs; //Indicate if the needed processes depends on a selector process or not
    static Bool_t doselection;
    static Long64_t neaea;               //Number of entries for arrays that should contain all events
    static Long64_t neaealast;           //Number of entries for the previous input array that should contain all events
    static Long64_t nesea;               //Number of entries for arrays that should contain only selected events
    static Long64_t nesealast;           //Number of entries for the previous input array that should contain only selected events
    static Bool_t eventselected;

    static Int_t nj;

    iaarrays.Clear();
    isarrays.Clear();
    oarrays.Clear();
    oobjects.Clear();
    oasproc.Clear();
    procs.Clear();
    naeprocs=0;
    selprocs.Clear();
    seldepprocs.Clear();
    doselection=kFALSE;
    neaealast=-1;
    nesealast=-1;

    memset(fNeededIA.GetArray(),0,fNeededIA.Count()*sizeof(Bool_t));
    memset(fNeededOA.GetArray(),0,fNeededOA.Count()*sizeof(Bool_t));
    memset(fNeededOO.GetArray(),0,fNeededOO.Count()*sizeof(Bool_t));

    //Loop over all processes
    for(i=0; i<fProcs->Count(); i++) {

      //If the current process has never been run or if it is triggered by the parameters mask
      if(((*fProcsParDepends)[i] && pardiffs) || depmods.GetBit(i) || runall) {

	if(GetVerbosity()&QProcessor::kShowExec) printf("\tProcess '%s' will be called\n",(*fProcs)[i].GetName());
	//Add it to the list of needed processes
	procs.Add(&(*fProcs)[i]);
	selprocs.Add((*fSelProcs)[i]);
	seldepprocs.Add((*fSelDepProcs)[i]);

	//If the current process is processing all events
	if(i < fNAEProcs) {
	  if((*fSelProcs)[i]) doselection=kTRUE;
	  //Increment the number of triggered processes that process all events
	  naeprocs++;
	}

	//Loop over the output arrays of the current process
	nj=(*fOAIndices)[i].Count();
	for(j=0; j<nj; j++) {
	  //Add the current output array to the list of needed output arrays
	  fNeededOA[(*fOAIndices)[i][j]]=kTRUE;
	}

	//Loop over output objets of the current process
	nj=(*fOOIndices)[i].Count();
	for(j=0; j<nj; j++) {
	  //Add the current object to the list of needed output objects
	  fNeededOO[(*fOOIndices)[i][j]]=kTRUE;
	}

	//Loop over input arrays of the current process
	nj=(*fIAIndices)[i].Count();
	for(j=0; j<nj; j++) {
	  //Add the current input array to the list of needed input arrays
	  fNeededIA[(*fIAIndices)[i][j]]=kTRUE;
	}
      }
    }

    //Loop over all output arrays
    for(i=0; i<fNeededOA.Count(); i++) {

      //If the current output array is needed
      if(fNeededOA[i]) {
	//Reset the array
	(*fOArrays)[i]->ResetArray();
	//Add it to the list of needed output arrays
	oarrays.Add((*fOArrays)[i]);
	oasproc.Add((*fOASProc)[i]);
      }
    }

    //Loop over all output objects
    for(i=0; i<fNeededOO.Count(); i++) {

      //If the current output object is needed
      if(fNeededOO[i]) {
	//Add it to the list of needed output objects
	oobjects.Add((*fOObjects)[i]);
      }
    }

    //Loop over all input arrays
    for(i=0; i<fNeededIA.Count(); i++) {

      //If the current input array is needed and it is not also an output array
      if(fNeededIA[i] && oarrays.FindFirst((*fIArrays)[i]) == -1) {

	//If the current array should contain all events
	if(!(*fIASProc)[i]) {
	  //Add it to the list of needed input all events arrays
	  iaarrays.Add((*fIArrays)[i]);

	  //Get the number of entries for the current array
	  neaea=(*fIArrays)[i]->GetEntries();

	  //If the number of entries for the current input array does not match the number of entries for the previous triggered input array
	  if(neaea != neaealast && neaealast != -1) {
	    fprintf(stderr,"QArrayProcessor::Exec(): Error: The number of entries in array '%s\t%s' (%lli) does not match the number of entries for the previously triggered input array (%lli) \n",(*fIANames)[i][0].Data(),(*fIANames)[i][1].Data(),neaea,neaealast);
	    throw 1;
	  }
	  neaealast=neaea;

	  //Else if the current array should contain only selected events
	} else {
	  //Add it to the list of needed input selected events arrays
	  isarrays.Add((*fIArrays)[i]);

	  //Get the number of entries for the current array
	  nesea=(*fIArrays)[i]->GetEntries();

	  //If the number of entries for the current input array does not match the number of entries for the previous triggered input array
	  if(nesea != nesealast && nesealast != -1) {
	    fprintf(stderr,"QArrayProcessor::Exec(): Error: The number of entries in array '%s\t%s' (%lli) does not match the number of entries for the previously triggered input array (%lli)\n",(*fIANames)[i][0].Data(),(*fIANames)[i][1].Data(),neaea,neaealast);
	    throw 1;
	  }
	  nesealast=nesea;
	}
      }
    }

    //Loop over needed output objects
    for(i=0; i<oobjects.Count(); i++) {
      //Initialize the object
      oobjects[i]->InitProcObj();
    }

    //If all absolute input arrays contain selected events
    if(neaealast==-1) {

      //Loop over the selected events
      for(i=0; i<nesealast; i++) {
	//printf("Entry %i/%i\n",i,nesealast);

	//Load all triggered input arrays
	for(j=0; j<isarrays.Count(); j++) {
	  //printf("\tInput array %i/%i\n",j,isarrays.Count());
	  isarrays.GetArray()[j]->GetEntry(i);
	}

	//Loop over all triggered processes
	for(j=0; j<procs.Count(); j++) {
	  //Exec the process
	  ((QNamedProc*)procs.GetArray()[j])->Exec();
	}

	//Save all triggered output arrays
	for(j=0; j<oarrays.Count(); j++) {
	  oarrays.GetArray()[j]->Fill();
	}
      }

      //Else if all input arrays contain all events
    } else if(nesealast==-1) {

      //Loop over all events
      for(i=0; i<neaealast; i++) {
	//printf("Entry %i/%i\n",i,neaealast);

	//Load all triggered input arrays
	for(j=0; j<iaarrays.Count(); j++) {
	  //printf("\tInput array %i/%i\n",j,iaarrays.Count());
	  iaarrays.GetArray()[j]->GetEntry(i);
	}
	eventselected=kTRUE;

	//Loop over all triggered non-post-selection processes
	for(j=0; j<naeprocs; j++) {
	  //Exec the process. If it is a selector process, use the output to check if the entry is selected or not
	  if(selprocs[j]) eventselected&=((QNamedProc*)procs.GetArray()[j])->Exec();
	  else ((QNamedProc*)procs.GetArray()[j])->Exec();
	}

	//Assertion: Post-selection processes that depend on a selected events array are called only for selected events
	//Loop over all triggered post-selection processes
	for(j=naeprocs; j<procs.Count(); j++) {
	  //If the process does not depend on a selector process or if the entry is selected, execute it
	  if(!seldepprocs[j] || eventselected) ((QNamedProc*)procs.GetArray()[j])->Exec();
	}

	//Assertion: Output arrays are filled when they do not depend directly or indirectly on a selector process or
	//when an event is selected.
	//Save all triggered output arrays
	for(j=0; j<oarrays.Count(); j++) {
	  //printf("\tOutput array %i/%i\n",j,oarrays.Count());
	  if(!oasproc[j] || eventselected) oarrays.GetArray()[j]->Fill();
	}
      }

      //Else if dealing with selected and all events absolute input arrays (the selection does not need to be done)
    } else {

      //Loop over selected events
      for(i=0; i<nesealast; i++) {
	//printf("Entry %i/%i\n",i,nesealast);

	//Load all triggered input selected events arrays
	for(j=0; j<isarrays.Count(); j++) {
	  //printf("\tInput array %i/%i\n",j,isarrays.Count());
	  isarrays.GetArray()[j]->GetEntry(i);
	}

	//Loop over all triggered non-post-selection events processes
	for(j=0; j<naeprocs; j++) {
	  //Exec the process if it depends on a selected events array
	  if(seldepprocs[j]) ((QNamedProc*)procs.GetArray()[j])->Exec();
	}

	//Assertion: Post-selection processes that depend on a selected events array are called only for selected events
	//Loop over all triggered post-selection processes
	for(j=naeprocs; j<procs.Count(); j++) {
	  //If the process depends on a selector process
	  if(seldepprocs[j]) ((QNamedProc*)procs.GetArray()[j])->Exec();
	}

	//Assertion: Output arrays are filled when it depends directly or indirectly on a selector process.
	//Save all triggered output arrays
	for(j=0; j<oarrays.Count(); j++) {
	  //printf("\tOutput array %i/%i\n",j,oarrays.Count());
	  if(oasproc[j]) oarrays.GetArray()[j]->Fill();
	}
      }

      //Loop over all events
      for(i=0; i<neaealast; i++) {
	//printf("Entry %i/%i\n",i,neaealast);

	//Load all triggered input all events arrays
	for(j=0; j<iaarrays.Count(); j++) {
	  //printf("\tInput array %i/%i\n",j,iaarrays.Count());
	  iaarrays.GetArray()[j]->GetEntry(i);
	}

	//Loop over all triggered non-post-selection events processes
	for(j=0; j<naeprocs; j++) {
	  //Exec the process if it depends on a selected events array
	  if(!seldepprocs[j]) ((QNamedProc*)procs.GetArray()[j])->Exec();
	}

	//Assertion: Post-selection processes that depend on a selected events array are called only for selected events
	//Loop over all triggered post-selection processes
	for(j=naeprocs; j<procs.Count(); j++) {
	  //If the process does not depend on a selector process, execute it
	  if(!seldepprocs[j]) ((QNamedProc*)procs.GetArray()[j])->Exec();
	}

	//Assertion: Output arrays are filled when it does not depend directly or indirectly on a selector process.
	//Save all triggered output arrays
	for(j=0; j<oarrays.Count(); j++) {
	  //printf("\tOutput array %i/%i\n",j,oarrays.Count());
	  if(!oasproc[j]) oarrays.GetArray()[j]->Fill();
	}
      }
    }

    //Loop over needed output objects
    for(i=0; i<oobjects.Count(); i++) {
      //Terminate the object
      oobjects[i]->TerminateProcObj();
      //Update the modification time for the object
      oobjects[i]->UpdateModTime();
    }

    //Loop over needed output arrays
    for(i=0; i<oarrays.Count(); i++) {
      //Terminate the output array
      oarrays[i]->TerminateProcObj();
    }

    //Save the parameters
    for(i=0; i<fParams->Count(); i++) fLastParams->GetArray()[i]=*(fParams->GetArray()[i]);
    fLastExec.Set();

  } else {

    if(GetVerbosity()&QProcessor::kShowExec2) printf("QArrayProcessor('%s')::Exec()\n",GetName());
  }
}

void QArrayProcessor::InitProcess(Bool_t allocateparammem)
{
  TerminateProcess();
  TDirectory *curdir=gDirectory;
  TDirectory *dbuf;
  Int_t i,j;
  TString proto;

  fIArrays->Clear();
  fOArrays->Clear();

  fBuffers->RedimList(fBuNames->Count());

  //Loop over the output arrays
  for(i=0; i<fOANames->Count(); i++) {

    if(!(dbuf=gDirectory->GetDirectory(fAnalysisDir))) {
      fprintf(stderr,"QArrayProcessor::InitProcess(): Error: Directory '%s' does not exist\n",fAnalysisDir.Data());
      throw 1;
    }
    dbuf->cd();
    j=(*fOANames)[i][1].Index("://");

    if(j==-1) {
      fprintf(stderr,"QArrayProcessor::InitProcess(): Error: Array type is not specified\n");
      throw 1;
    }
    proto=(*fOANames)[i][1](0,j);

    if(!strcmp(proto,"tree")) {
      //Create the output array and store the pointer
      (*fOArrays).Add(QProcBranchHandler::LoadBranch((TString)(*fOANames)[i][1](j+3,(*fOANames)[i][1].Length()-j-3),(*fOANames)[i][0], kTRUE));

    } else if(!strcmp(proto,"qoa")) {
      //Create the output array and store the pointer
      (*fOArrays).Add(QProcQOAHandler::LoadQOA((TString)(*fOANames)[i][1](j+3,(*fOANames)[i][1].Length()-j-3),(*fOANames)[i][0], kTRUE));

    } else {
      fprintf(stderr,"QArrayProcessor::InitProcess(): Error: Array type '%s' is unknown\n",proto.Data());
      throw 1;
    }

  }

  //Loop over the input arrays
  for(i=0; i<fIANames->Count(); i++) {

    if(!(dbuf=gDirectory->GetDirectory(fAnalysisDir))) {
      fprintf(stderr,"QArrayProcessor::InitProcess(): Error: Directory '%s' does not exist\n",fAnalysisDir.Data());
      throw 1;
    }
    dbuf->cd();
    j=(*fIANames)[i][1].Index("://");

    if(j==-1) {
      fprintf(stderr,"QArrayProcessor::InitProcess(): Error: Array type is not specified\n");
      throw 1;
    }
    proto=(*fIANames)[i][1](0,j);

    if(!strcmp(proto,"tree")) {
      //Create the input array and store the pointer
      (*fIArrays).Add(QProcBranchHandler::LoadBranch((TString)(*fIANames)[i][1](j+3,(*fIANames)[i][1].Length()-j-3),(*fIANames)[i][0], kFALSE));

    } else if(!strcmp(proto,"qoa")) {
      //Create the input array and store the pointer
      (*fIArrays).Add(QProcQOAHandler::LoadQOA((TString)(*fIANames)[i][1](j+3,(*fIANames)[i][1].Length()-j-3),(*fIANames)[i][0], kFALSE));

    } else {
      fprintf(stderr,"QArrayProcessor::InitProcess(): Error: Array type '%s' is unknown\n",proto.Data());
      throw 1;
    }
  }

  //Create output buffers
  fBuffers->RedimList(fBuNames->Count());

  QStdProcessor::InitProcess(allocateparammem);

  Int_t nprocs=fProcs->Count();
  QNamedProc *proc;
  TString sbuf;
  QList<TString> qlsbuf;
  qlsbuf.RedimList(2);

  //Loop over the processes
  for(i=0; i<nprocs; i++) {
    proc=&((*fProcs)[i]);

    //Loop over the inputs for the current process
    for(j=0; j<proc->GetNIVars(); j++) {
      sbuf=proc->GetIVarNameTitle(j).GetTitle();
      qlsbuf[0]=proc->GetIVarNameTitle(j).GetName();
      if(sbuf.Length()) {
	qlsbuf.RedimList(2);
	qlsbuf[1]=sbuf;
      } else qlsbuf.RedimList(1);

      //If the input is not a memory buffer
      if(qlsbuf.Count() == 2) {
	//Set the address for the QNamedProc input buffer using the buffer address of the array object
	proc->SetIVarPtr(j,(Double_t*)(*fIArrays)[fIANames->FindFirst(qlsbuf)]->GetBuffer());

	//Else if the input is a memory buffer
      } else {
	//Get the buffer address from the memory buffers list
	proc->SetIVarPtr(j,&((*fBuffers)[fBuNames->FindFirst(proc->GetIVarNameTitle(j).GetName())]));
      }
    }

    //Loop over the output for the current process
    for(j=0; j<proc->GetNOVars(); j++) {
      sbuf=proc->GetOVarNameTitle(j).GetTitle();
      qlsbuf[0]=proc->GetOVarNameTitle(j).GetName();
      if(sbuf.Length()) {
	qlsbuf.RedimList(2);
	qlsbuf[1]=sbuf;
      } else qlsbuf.RedimList(1);

      //If the output is not a memory buffer
      if(qlsbuf.Count() == 2) {
	//Set the address for the QNamedProc input buffer using the buffer address of the array object
	proc->SetOVarPtr(j,(Double_t*)(*fOArrays)[fOANames->FindFirst(qlsbuf)]->GetBuffer());

	//Else if the output is a memory buffer
      } else {
	//Get the buffer address from the memory buffers list
	proc->SetOVarPtr(j,&((*fBuffers)[fBuNames->FindFirst(proc->GetOVarNameTitle(j).GetName())]));
      }
    }
  }

  //Erase last parameters
  fLastParams->Clear();
  fLastParams->RedimList(fParams->Count(),-1,0.);
  fLastExec.SetSec(0);

  curdir->cd();
}

const QArrayProcessor& QArrayProcessor::operator=(const QArrayProcessor &rhs)
{
  QStdProcessor::operator=(rhs);
  *fSelProcs=*rhs.fSelProcs;
  *fTrigAEProcs=*rhs.fTrigAEProcs;
  fNAEProcs=rhs.fNAEProcs;
  fAnalysisDir=rhs.fAnalysisDir;
  *fIANames=*rhs.fIANames;
  *fOANames=*rhs.fOANames;
  *fBuNames=*rhs.fBuNames;
  *fIAIndices=*rhs.fIAIndices;
  *fIOIndices=*rhs.fIOIndices;
  *fOAIndices=*rhs.fOAIndices;
  *fOOIndices=*rhs.fOOIndices;
  *fIObjects=*rhs.fIObjects;
  *fOObjects=*rhs.fIObjects;
  *fSelDepProcs=*rhs.fSelDepProcs;
  *fIASProc=*rhs.fIASProc;
  *fOASProc=*rhs.fOASProc;
  *fAPDepends=*rhs.fAPDepends;
  *fObjsPDepends=*rhs.fObjsPDepends;
  return *this;
}

void QArrayProcessor::PrintAnalysisResults() const
{
  TDirectory *curdir=gDirectory;

  if(!gDirectory->cd(fAnalysisDir)) {
    fprintf(stderr,"QArrayProcessor::PrintAnalysisResults(): Error: Directory %s does not exist\n",fAnalysisDir.Data());
    throw 1;
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

    printf("\nInput Variables:\n");
    k=0;
    for(j=0; j<proc->GetNIVars(); j++) {
      sbuf=proc->GetIVarNameTitle(j).GetTitle();
      printf("%3i",j);
      printf("\t%s",proc->GetIVarNameTitle(j).GetName());
      if(sbuf.Length()) {printf("\t%s (%i)",sbuf.Data(),(*fIAIndices)[i][k]); k++;}
      printf("\n");
    }

    printf("\nInput Objects:\n");
    for(j=0; j<(*fIOIndices)[i].Count(); j++) {
      printf("%3i\t",j);
      if(dynamic_cast<TObject*>((*fIObjects)[(*fIOIndices)[i][j]])) printf("%s (%p)",dynamic_cast<TObject*>((*fIObjects)[(*fIOIndices)[i][j]])->GetName(),(*fIObjects)[(*fIOIndices)[i][j]]);
      else printf("%p",(*fIObjects)[(*fIOIndices)[i][j]]);
      printf(" (%i)\n",(*fIOIndices)[i][j]);
    }

    printf("\nOutput Variables:\n");
    k=0;
    for(j=0; j<proc->GetNOVars(); j++) {
      sbuf=proc->GetOVarNameTitle(j).GetTitle();
      printf("%3i",j);
      printf("\t%s",proc->GetOVarNameTitle(j).GetName());
      if(sbuf.Length()) {printf("\t%s (%i)",sbuf.Data(),(*fOAIndices)[i][k]); k++;}
      printf("\n");
    }

    printf("\nOutput Objects:\n");
    for(j=0; j<(*fOOIndices)[i].Count(); j++) {
      printf("%3i\t",j);
      if(dynamic_cast<TObject*>((*fOObjects)[(*fOOIndices)[i][j]])) printf("%s (%p)",dynamic_cast<TObject*>((*fOObjects)[(*fOOIndices)[i][j]])->GetName(),(*fOObjects)[(*fOOIndices)[i][j]]);
      else printf("%p",(*fOObjects)[(*fOOIndices)[i][j]]);
      printf(" (%i)\n",(*fOOIndices)[i][j]);
    }

    printf("\nDependencies:\n");
    (*fProcsParDepends)[i].Print();

    if(i<fNAEProcs) {
      if((*fSelProcs)[i]) printf("\nIs a selector process\n");
      else printf("\nIs not a selector process\n");
    } else {
      if((*fSelProcs)[i]) printf("\nProcesses only selected events\n");
      else printf("\nProcesses all events\n");
    }
    if((*fTrigAEProcs)[i]) printf("Triggers the selectors\n");

    if((*fSelDepProcs)[i]) printf("Depends on a selector process\n");
    else printf("Does not depend on a selector process\n");
  }

  printf("\nAll Input Arrays:\n");
  for(i=0; i<fIANames->Count(); i++) {
    printf("%3i Array '%s\t%s'\t",i,(*fIANames)[i][0].Data(),(*fIANames)[i][1].Data());
    (*fAPDepends)[i].Print();
  }

  printf("\nAll Input Objects:\n");
  for(i=0; i<fIObjects->Count(); i++) {
    printf("%3i\t",i);
    if(dynamic_cast<TObject*>((*fIObjects)[i])) printf("%s (%p)\t",dynamic_cast<TObject*>((*fIObjects)[i])->GetName(),(*fIObjects)[i]);
    else printf("%p\t",(*fIObjects)[i]);
    (*fObjsPDepends)[i].Print();
  }

  printf("\nAll Output Arrays:\n");
  for(i=0; i<fOANames->Count(); i++) {
    printf("%3i Array '%s\t%s'\n",i,(*fOANames)[i][0].Data(),(*fOANames)[i][1].Data());

    if((*fOASProc)[i]) printf("\tDepends on a selector process\n");
    else printf("\tDoes not depend on a selector process\n");
  }

  printf("\nAll Output Objects:\n");
  for(i=0; i<fOObjects->Count(); i++) {
    printf("%3i\t",i);
    if(dynamic_cast<TObject*>((*fOObjects)[i])) printf("%s (%p)\n",dynamic_cast<TObject*>((*fOObjects)[i])->GetName(),(*fOObjects)[i]);
    else printf("%p\n",(*fOObjects)[i]);
  }
  curdir->cd();
}

void QArrayProcessor::PrintProcesses(UInt_t level) const
{
  Int_t i;
  Int_t nprocs=fProcs->Count();
  QNamedProc *proc;

  for(i=0; i<nprocs; i++) {
    proc=&((*fProcs)[i]);

    if(i<fNAEProcs) {
      printf("%*s%03i All events process '%s'\n",level*3,"",i,proc->GetName());

    } else {
      printf("%*s%03i Selected events process '%s'\n",level*3,"",i,proc->GetName());
    }
  }
}

void QArrayProcessor::TerminateProcess()
{
  Int_t i;

  for(i=0; i<fIArrays->Count(); i++) (*fIArrays)[i]->UnloadArray();
  for(i=0; i<fOArrays->Count(); i++) (*fOArrays)[i]->UnloadArray();
}

void QArrayProcessor::Browse(TBrowser *b)
{   
  if(b) { 
    b->Add(fProcs,"Processes");
  } 
}

Int_t QArrayProcessor::AEProcIndexToIndex(Int_t index){
  if(index == -1) return fNAEProcs;

  if(index > fNAEProcs) {
    fprintf(stderr,"QArrayProcessor::AEProcIndexToIndex: Error: Index %i is invalid\n",index);
    throw 1;
  }

  return index;
}

Int_t QArrayProcessor::PSProcIndexToIndex(Int_t index){
  if(index == -1) return fProcs->Count();

  if(index < fNAEProcs) {
    fprintf(stderr,"QArrayProcessor::PSProcIndexToIndex: Error: Index %i is invalid\n",index);
    throw 1;
  }

  return index;
}

#include "debugger.h"
