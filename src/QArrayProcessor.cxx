// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>

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
  delete fAIANames;
  fAIANames=NULL;
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

  if(fAAIArrays!=fAIArrays) {
      delete fAAIArrays;
      fAAIArrays=NULL;
  }
  delete fAIArrays;
  fAIArrays=NULL;
  delete fIObjects;
  fIObjects=NULL;

  if(fAAIObjects!=fAIObjects) {
      delete fAAIObjects;
      fAAIObjects=NULL;
  }
  delete fAIObjects;
  fAIObjects=NULL;
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

  if(fAAPDepends!=fAPDepends) {
      delete fAAPDepends;
      fAAPDepends=NULL;
  }
  delete fAPDepends;
  fAPDepends=NULL;

  if(fAObjsPDepends!=fObjsPDepends) {
      delete fAObjsPDepends;
      fAObjsPDepends=NULL;
  }
  delete fObjsPDepends;
  fObjsPDepends=NULL;
  delete fBuffers;
  fBuffers=NULL;

  if(fActiveAIO) {
      delete fActiveAIO;
      fActiveAIO=NULL;
  }

  if(fLastActiveAIO) {
      delete fLastActiveAIO;
      fLastActiveAIO=NULL;
  }

  if(fAllIObjects) {
    delete fAllIObjects;
    delete fAllOObjects;
  }
}

Int_t QArrayProcessor::AddProc(const char *name, const char *title, Bool_t selector, Int_t index)
{
  PRINTF10(this,"\tQArrayProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, Bool_t selector<",selector,">, Int_t index<",index,">)\n")

  index=AEProcIndexToIndex(index);
  fNAEProcs++;
  fProcs->RedimList(fProcs->Count()+1,index);
  fSelProcs->Add(selector,index);
  (*fProcs)[index].SetNameTitle(name,title);
  return index;
}

Int_t QArrayProcessor::AddProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname, Bool_t selector, Int_t index)
{
  PRINTF14(this,"\tQArrayProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, Bool_t (*proc)(QProcArgs&)<",proc,">, const char *procname<'",procname,"'>, Bool_t selector<",selector,">, Int_t index<",index,">)\n")

  index=AEProcIndexToIndex(index);
  AddProc(name,title,selector,index);
  (*fProcs)[index].SetProc(proc,procname);
  return index;
}

/*
Int_t QArrayProcessor::AddProc(const char *name, const char *title, const char *procname, Bool_t selector, Int_t index)
{
  PRINTF12(this,"\tQArrayProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, const char *procname<'",procname,"'>, Bool_t selector<",selector,">, Int_t index<",index,">)\n")

  index=AEProcIndexToIndex(index);
  AddProc(name,title,selector,index);
  (*fProcs)[index].SetProc(procname);
  return index;
}
*/

/*
Int_t QArrayProcessor::AddProc(const char *name, const char *title, void *proc, const char *procname, Bool_t selector, Int_t index)
{
  PRINTF14(this,"\tQArrayProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, void *proc<",proc,">, const char *procname<'",procname,"'>, Bool_t selector<",selector,">, Int_t index<",index,">)\n")

  index=AEProcIndexToIndex(index);
  AddProc(name,title,selector,index);
  (*fProcs)[index].SetProc(proc,procname);
  return index;
}*/

Int_t QArrayProcessor::AddPSProc(const char *name, const char *title, Bool_t selectedonly, Int_t index)
{
  PRINTF10(this,"\tQArrayProcessor::AddPSProc(const char *name<'",name,"'>, const char *title<'",title,"'>, Bool_t selectedonly<",selectedonly,">, Int_t index<",index,">)\n")

  index=PSProcIndexToIndex(index);
  fProcs->RedimList(fProcs->Count()+1,index);
  fSelProcs->Add(selectedonly,index);
  (*fProcs)[index].SetNameTitle(name,title);
  return index;
}

Int_t QArrayProcessor::AddPSProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname, Bool_t selectedonly, Int_t index)
{
  PRINTF14(this,"\tQArrayProcessor::AddPSProc(const char *name<'",name,"'>, const char *title<'",title,"'>, Bool_t (*proc)(QProcArgs&)<",proc,">, const char *procname<'",procname,"'>, Bool_t selectedonly<",selectedonly,">, Int_t index<",index,">)\n")

  index=PSProcIndexToIndex(index);
  AddPSProc(name,title,selectedonly,index);
  (*fProcs)[index].SetProc(proc,procname);
  return index;
}

/*
Int_t QArrayProcessor::AddPSProc(const char *name, const char *title, const char *procname, Bool_t selectedonly, Int_t index)
{
  PRINTF12(this,"\tQArrayProcessor::AddPSProc(const char *name<'",name,"'>, const char *title<'",title,"'>, const char *procname<'",procname,"'>, Bool_t selectedonly<",selectedonly,">, Int_t index<",index,">)\n")

  index=PSProcIndexToIndex(index);
  AddPSProc(name,title,selectedonly,index);
  (*fProcs)[index].SetProc(procname);
  return index;
}

Int_t QArrayProcessor::AddPSProc(const char *name, const char *title, void *proc, const char *procname, Bool_t selectedonly, Int_t index)
{
  PRINTF14(this,"\tQArrayProcessor::AddPSProc(const char *name<'",name,"'>, const char *title<'",title,"'>, void *proc<",proc,">, const char *procname<'",procname,"'>, Bool_t selectedonly<",selectedonly,">, Int_t index<",index,">)\n")

  index=PSProcIndexToIndex(index);
  AddPSProc(name,title,selectedonly,index);
  (*fProcs)[index].SetProc(proc,procname);
  return index;
}
*/

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
  fAIANames->Clear();
  fOANames->Clear();
  fBuNames->Clear();
  fAPDepends->Clear();
  fObjsPDepends->Clear();
  fIAIndices->Clear();
  fIOIndices->Clear();
  fOAIndices->Clear();
  fOOIndices->Clear();
  fIObjects->Clear();
  fAIObjects->Clear();
  fOObjects->Clear();

  fIAIndices->RedimList(nprocs);
  fIOIndices->RedimList(nprocs);
  fOAIndices->RedimList(nprocs);
  fOOIndices->RedimList(nprocs);
  QDepTree *depprocs=new QDepTree[nprocs]; //QDepTree objects contain a list of processes that should be triggered
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
	iidx=AddUniqueArray(fIANames,qlsbuf);
	oidx=FindArray(fOANames,qlsbuf);

	if(iidx == -2) {
	  fprintf(stderr,"QArrayProcessor: Analyze(): Error with process '%s': Input array '%s\t%s' has a different type from what previously declared\n",proc->GetName(),qlsbuf[0].Data(),qlsbuf[1].Data());
	  throw 1;
	}

	//If the array is not already listed in the list of existing arrays
	if(iidx == -1) {
	  //printf("New input array\n");

	  //If this is also an absolute input, add it to the absolute input array and make room for it in fAPDepends
	  if(oidx==-1) {
	    //Add the current process to the list of processes that depend on the current input array
	    fAIANames->Add(qlsbuf);
	    fAPDepends->RedimList(fAPDepends->Count()+1);
	    (*fAPDepends)[fAPDepends->Count()-1].SetBit(i,kTRUE);
	  }
	  iidx=fIANames->Count()-1;
	  //Else if this is a known absolute array, add the current process in the list of dependencies
	} else if(oidx==-1) (*fAPDepends)[fAIANames->FindFirst(qlsbuf)].SetBit(i,kTRUE);
	(*fIAIndices)[i].Add(iidx);

	if(oidx == -2) {
	  fprintf(stderr,"QArrayProcessor: Analyze(): Error with process '%s': Input array '%s\t%s' has a different type from what previously declared\n",proc->GetName(),qlsbuf[0].Data(),qlsbuf[1].Data());
	  throw 1;
	}

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
	iidx=FindBuffer(fBuNames,qlsbuf[0]);

	if(iidx == -2) {
	  fprintf(stderr,"QArrayProcessor: Analyze(): Error with process '%s': Input buffer '%s' has a different type from what previously declared\n",proc->GetName(),qlsbuf[0].Data());
	  throw 1;
	}

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
      oidx=fOObjects->FindFirst(const_cast<QProcObj*>(proc->IObj(j)));

      if(iidx == -1) {
	iidx=fIObjects->Count()-1;
      
	//If this is also an absolute input, add it to the absolute input array and make room for it in fObjsPDepends
	if(oidx==-1) {
	  fAIObjects->Add(const_cast<QProcObj*>(proc->IObj(j)));
	  fObjsPDepends->RedimList(fObjsPDepends->Count()+1);
          (*fObjsPDepends)[fObjsPDepends->Count()-1].SetBit(i,kTRUE);
	}

      //Else if this is a known absolute input, add the current process in the list of dependencies
      } else if(oidx==-1) (*fObjsPDepends)[fAIObjects->FindFirst(const_cast<QProcObj*>(proc->IObj(j)))].SetBit(i,kTRUE);
      (*fIOIndices)[i][j]=iidx;

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
	iidx=FindArray(fIANames,qlsbuf);

	if(iidx == -2) {
	  fprintf(stderr,"QArrayProcessor: Analyze(): Error with process '%s': Output array '%s\t%s' has a different type from what previously declared\n",proc->GetName(),qlsbuf[0].Data(),qlsbuf[1].Data());
	  throw 1;
	}

	oidx=FindArray(fOANames,qlsbuf);

	if(oidx == -2) {
	  fprintf(stderr,"QArrayProcessor: Analyze(): Error with process '%s': Output array '%s\t%s' has a different type from what previously declared\n",proc->GetName(),qlsbuf[0].Data(),qlsbuf[1].Data());
	  throw 1;
	}

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
	  AddUniqueArray(fOANames,qlsbuf);
	  oidx=fOANames->Count()-1;
	  oalastproc.RedimList(fOANames->Count());
	}
	oalastproc[oidx]=i;
	(*fOAIndices)[i].Add(oidx);

	//Else if the output is to a buffer in memory
      } else {
	//printf("Output to a memory buffer\n");
	//Ensure the output memory buffer is in the list

	oidx=AddUniqueBuffer(fBuNames,qlsbuf[0],kDouble);

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

      //This is required because one does not want a given process to use its own output as an input if it gets triggered by another input object or by a parameter. For arrays also the output would be used as an input for every array event
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

    //Loop over all absolute input arrays
    for(j=0; j<fAPDepends->Count(); j++) {

      //If the current process is triggered by the input branch
      if((*fAPDepends)[j].GetBit(i)) {

	//Loop over dependent processes
	for(k=0; k<dpidx.Count(); k++) {
	  //Add the dependent process to the dependencies list of the current branch
	  (*fAPDepends)[j].SetBit(dpidx[k],kTRUE);
	}
      }
    }

    //Loop over all absolute input objects
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

void QArrayProcessor::Exec() const
{
  //QMask lExecpardiffs;    //! Modified parameters since the last call
  //QMask lExecdepmods;     //! Required processes due to modified input arrays or input objects
  Bool_t lExecrunall;
  Int_t lExeci;
  Int_t lExecj;
  lExecpardiffs.Clear();
  lExecdepmods.Clear();

  //fLastParams gets cleared by the function Analyze, so this is how the first run is identified
  if(fForceExecAll || fLastExec.GetSec() != 0) {

    //Loop over parameters
    for(lExeci=fAParams->Count()-1; lExeci>=0; --lExeci) {

      //If the current parameter value has changed, set the correspongind bit in the parameter mask
      if(*(fAParams->GetArray()[lExeci]) != fLastParams->GetArray()[lExeci]) lExecpardiffs.SetBit(lExeci,1);
    }

    //Loop over all absolute input arrays
    for(lExeci=fAAIArrays->Count()-1; lExeci>=0; --lExeci) {

      //If the current input array has been modified after the last run, add its mask to the mask of required processes.
      if((*fAAIArrays)[lExeci]->NewerThan(fLastExec)) {
	lExecdepmods|=(*fAAPDepends)[lExeci];
	//printf("Absolute input array %i (%p) has been modified ",lExeci,(*fAAIArrays)[lExeci]);
      } else {
	//printf("Absolute input array %i (%p) has not been modified ",lExeci,(*fAAIArrays)[lExeci]);
      }
      //(*fAAPDepends)[lExeci].Print();
    }

    //Loop over all absolute input objects
    for(lExeci=fAAIObjects->Count()-1; lExeci>=0; --lExeci) {

      //If the current absolute input object has been modified after the last run, add its mask to the mask of required processes
      if((*fAAIObjects)[lExeci]->NewerThan(fLastExec)) {
	lExecdepmods|=(*fAObjsPDepends)[lExeci];
      }
    }

    lExecrunall=fForceExecAll;
  } else {
    fNeededIA.RedimList(fIANames->Count());
    fNeededOA.RedimList(fOANames->Count());
    fNeededOO.RedimList(fOObjects->Count());
    lExecrunall=kTRUE;
  }

  //If at least one of the parameters has changed
  if(lExecpardiffs || lExecdepmods || lExecrunall) {

    if(GetVerbosity()&QProcessor::kShowExec) printf("QArrayProcessor('%s')::Exec()\n",GetName());
    //printf("lExecrunall: %i\n",lExecrunall);
    //printf("Parameter mask for the current parameters: ");
    //lExecpardiffs.Print();
    //printf("Object mask for the current parameters: ");
    //lExecdepmods.Print();

    //QList<QProcArray*>  lExeciaarrays;    //! List for needed input all events arrays
    //QList<QProcArray*>  lExecisarrays;    //! List for needed input selected events arrays
    //QList<QProcArray*>  lExecoarrays;     //! List for output arrays needing update
    //QList<QProcObj*>    lExecoobjects;    //! List for output objects needing update
    //QList<Bool_t>       lExecoasproc;     //! Indicate if output array needing update are dependent or not on a selector process
    //QList<TObject*>     lExecprocs;       //! List of needed processes
    Int_t               lExecnaeprocs=0;    //! Number of processes that process all entries in the array
    //QList<Bool_t>       lExecselprocs;    //! Indicate if the needed processes is a selector process or not
    //QList<Bool_t>       lExecseldepprocs; //! Indicate if the needed processes depends on a selector process or not
    //Bool_t              lExecdoselection=kFALSE;
    Long64_t            lExecneaea;       //! Number of entries for arrays that should contain all events
    Long64_t            lExecneaealast=-1;   //! Number of entries for the previous input array that should contain all events
    Long64_t            lExecnesea;       //! Number of entries for arrays that should contain only selected events
    Long64_t            lExecnesealast=-1;   //! Number of entries for the previous input array that should contain only selected events
    Long64_t            lExecli;
    Bool_t              lExeceventselected;

    lExeciaarrays.Clear();
    lExecisarrays.Clear();
    lExecoarrays.Clear();
    lExecoobjects.Clear();
    lExecoasproc.Clear();
    lExecprocs.Clear();
    //lExecnaeprocs=0;
    lExecselprocs.Clear();
    lExecseldepprocs.Clear();
    //lExecdoselection=kFALSE;
    //lExecneaealast=-1;
    //lExecnesealast=-1;

    memset(fNeededIA.GetArray(),0,fNeededIA.Count()*sizeof(Bool_t));
    memset(fNeededOA.GetArray(),0,fNeededOA.Count()*sizeof(Bool_t));
    memset(fNeededOO.GetArray(),0,fNeededOO.Count()*sizeof(Bool_t));

    //Loop over all processes
    for(lExeci=0; lExeci<fProcs->Count(); ++lExeci) {

      //If the current process has never been run or if it is triggered by the parameters mask
      if(((*fProcsAParDepends)[lExeci] && lExecpardiffs) || lExecdepmods.GetBit(lExeci) || lExecrunall) {

	if(GetVerbosity()&QProcessor::kShowExec) printf("\tProcess '%s' will be called\n",(*fProcs)[lExeci].GetName());
	//Add it to the list of needed processes
	lExecprocs.Add(&(*fProcs)[lExeci]);
	lExecselprocs.Add((*fSelProcs)[lExeci]);
	lExecseldepprocs.Add((*fSelDepProcs)[lExeci]);

	//If the current process is processing all events
	if(lExeci < fNAEProcs) {
	  //if((*fSelProcs)[lExeci]) lExecdoselection=kTRUE;
	  //Increment the number of triggered processes that process all events
	  lExecnaeprocs++;
	}

	//Loop over the output arrays of the current process
	for(lExecj=(*fOAIndices)[lExeci].Count()-1; lExecj>=0; --lExecj) {
	  //Add the current output array to the list of needed output arrays
	  fNeededOA[(*fOAIndices)[lExeci][lExecj]]=kTRUE;

	  //printf("Output array %i of process %i (%p) is needed\n",lExecj,lExeci,(*fOArrays)[lExeci]);
	}

	//Loop over output objets of the current process
	for(lExecj=(*fOOIndices)[lExeci].Count()-1; lExecj>=0; --lExecj) {
	  //Add the current object to the list of needed output objects
	  fNeededOO[(*fOOIndices)[lExeci][lExecj]]=kTRUE;
	}

	//Loop over input arrays of the current process
	for(lExecj=(*fIAIndices)[lExeci].Count()-1; lExecj>=0; --lExecj) {
	  //Add the current input array to the list of needed input arrays
	  fNeededIA[(*fIAIndices)[lExeci][lExecj]]=kTRUE;
	}
      }
    }

    //Loop over all output arrays
    for(lExeci=fNeededOA.Count()-1; lExeci>=0; --lExeci) {

      //If the current output array is needed
      if(fNeededOA[lExeci]) {
	//Reset the array
	(*fOArrays)[lExeci]->ResetArray();
	//Add it to the list of needed output arrays
	lExecoarrays.Add((*fOArrays)[lExeci]);
	lExecoasproc.Add((*fOASProc)[lExeci]);
      }
    }

    //Loop over all output objects
    for(lExeci=fNeededOO.Count()-1; lExeci>=0; --lExeci) {

      //If the current output object is needed
      if(fNeededOO[lExeci]) {
	//Add it to the list of needed output objects
	lExecoobjects.Add((*fOObjects)[lExeci]);
	//Initialize the object
	(*fOObjects)[lExeci]->InitProcObj();
      }
    }

    //Loop over all input arrays
    for(lExeci=0; lExeci<fNeededIA.Count(); ++lExeci) {

      //If the current input array is needed and it is not also an output array
      if(fNeededIA[lExeci] && lExecoarrays.FindFirst((*fIArrays)[lExeci]) == -1) {

	//If the current array should contain all events
	if(!(*fIASProc)[lExeci]) {
	  //Add it to the list of needed input all events arrays
	  lExeciaarrays.Add((*fIArrays)[lExeci]);

	  //Get the number of entries for the current array
	  lExecneaea=(*fIArrays)[lExeci]->GetEntries();

	  //If the number of entries for the current input array does not match the number of entries for the previous triggered input array
	  if(lExecneaea != lExecneaealast && lExecneaealast != -1) {
	    fprintf(stderr,"QArrayProcessor::Exec(): Error: The number of entries in array '%s\t%s' (%lli) does not match the number of entries for the previously triggered input array (%lli) \n",(*fIANames)[lExeci][0].Data(),(*fIANames)[lExeci][1].Data(),lExecneaea,lExecneaealast);
	    throw 1;
	  }
	  lExecneaealast=lExecneaea;

	  //Else if the current array should contain only selected events
	} else {
	  //Add it to the list of needed input selected events arrays
	  lExecisarrays.Add((*fIArrays)[lExeci]);

	  //Get the number of entries for the current array
	  lExecnesea=(*fIArrays)[lExeci]->GetEntries();

	  //If the number of entries for the current input array does not match the number of entries for the previous triggered input array
	  if(lExecnesea != lExecnesealast && lExecnesealast != -1) {
	    fprintf(stderr,"QArrayProcessor::Exec(): Error: The number of entries in array '%s\t%s' (%lli) does not match the number of entries for the previously triggered input array (%lli)\n",(*fIANames)[lExeci][0].Data(),(*fIANames)[lExeci][1].Data(),lExecnesea,lExecnesealast);
	    throw 1;
	  }
	  lExecnesealast=lExecnesea;
	}
      }
    }

    //If all absolute input arrays contain selected events
    if(lExecneaealast==-1) {

      //printf("Start exec ArrayProcessor %p\n",this);
      //Loop over the selected events
      for(lExecli=0; lExecli<lExecnesealast; ++lExecli) {
	//printf("Entry %lli/%lli\n",lExecli,lExecnesealast);

	//Load all triggered input arrays
	for(lExecj=lExecisarrays.Count()-1; lExecj>=0; --lExecj) {
	  //printf("\tInput array %i/%i\n",lExecj,lExecisarrays.Count());
	  lExecisarrays.GetArray()[lExecj]->LoadEntry(lExecli);
	}

	//Loop over all triggered processes
	for(lExecj=0; lExecj<lExecprocs.Count(); ++lExecj) {
	  //Exec the process
	  ((QNamedProc*)lExecprocs.GetArray()[lExecj])->Exec();
	}

	//Save all triggered output arrays
	for(lExecj=lExecoarrays.Count()-1; lExecj>=0; --lExecj) {
	  lExecoarrays.GetArray()[lExecj]->Fill();
	}
      }
      //printf("Done exec ArrayProcessor %p\n",this);

      //Else if all input arrays contain all events
    } else if(lExecnesealast==-1) {

      //printf("Start exec ArrayProcessor %p\n",this);
      //Loop over all events
      for(lExecli=0; lExecli<lExecneaealast; ++lExecli) {
	//printf("Entry %lli/%lli\n",lExecli,lExecneaealast);

	//Load all triggered input arrays
	for(lExecj=lExeciaarrays.Count()-1; lExecj>=0; --lExecj) {
	  //printf("\tInput array %i/%i\n",lExecj,lExeciaarrays.Count());
	  lExeciaarrays.GetArray()[lExecj]->LoadEntry(lExecli);
	}
	lExeceventselected=kTRUE;

	//Loop over all triggered non-post-selection processes
	for(lExecj=0; lExecj<lExecnaeprocs; ++lExecj) {
	  //Exec the process. If it is a selector process, use the output to check if the entry is selected or not
	  if(lExecselprocs[lExecj]) lExeceventselected&=((QNamedProc*)lExecprocs.GetArray()[lExecj])->Exec();
	  else ((QNamedProc*)lExecprocs.GetArray()[lExecj])->Exec();
	}

	//Assertion: Post-selection processes that depend on a selected events array are called only for selected events
	//Loop over all triggered post-selection processes
	for(lExecj=lExecnaeprocs; lExecj<lExecprocs.Count(); ++lExecj) {
	  //If the process does not depend on a selector process or if the entry is selected, execute it
	  if(!lExecseldepprocs[lExecj] || lExeceventselected) ((QNamedProc*)lExecprocs.GetArray()[lExecj])->Exec();
	}

	//Assertion: Output arrays are filled when they do not depend directly or indirectly on a selector process or
	//when an event is selected.
	//Save all triggered output arrays
	for(lExecj=lExecoarrays.Count()-1; lExecj>=0; --lExecj) {
	  //printf("\tOutput array %i/%i\n",lExecj,lExecoarrays.Count());
	  if(!lExecoasproc[lExecj] || lExeceventselected) lExecoarrays.GetArray()[lExecj]->Fill();
	}
      }
      //printf("Done exec ArrayProcessor %p\n",this);

      //Else if dealing with selected and all events absolute input arrays (the selection does not need to be done)
    } else {

      //Loop over selected events
      for(lExecli=0; lExecli<lExecnesealast; ++lExecli) {
	//printf("Entry %lli/%lli\n",lExecli,lExecnesealast);

	//Load all triggered input selected events arrays
	for(lExecj=lExecisarrays.Count()-1; lExecj>=0; --lExecj) {
	  //printf("\tInput array %i/%i\n",lExecj,lExecisarrays.Count());
	  lExecisarrays.GetArray()[lExecj]->LoadEntry(lExecli);
	}

	//Loop over all triggered non-post-selection events processes
	for(lExecj=0; lExecj<lExecnaeprocs; ++lExecj) {
	  //Exec the process if it depends on a selected events array
	  if(lExecseldepprocs[lExecj]) ((QNamedProc*)lExecprocs.GetArray()[lExecj])->Exec();
	}

	//Assertion: Post-selection processes that depend on a selected events array are called only for selected events
	//Loop over all triggered post-selection processes
	for(lExecj=lExecnaeprocs; lExecj<lExecprocs.Count(); ++lExecj) {
	  //If the process depends on a selector process
	  if(lExecseldepprocs[lExecj]) ((QNamedProc*)lExecprocs.GetArray()[lExecj])->Exec();
	}

	//Assertion: Output arrays are filled when it depends directly or indirectly on a selector process.
	//Save all triggered output arrays
	for(lExecj=lExecoarrays.Count()-1; lExecj>=0; --lExecj) {
	  //printf("\tOutput array %i/%i\n",lExecj,lExecoarrays.Count());
	  if(lExecoasproc[lExecj]) lExecoarrays.GetArray()[lExecj]->Fill();
	}
      }

      //printf("Start exec ArrayProcessor %p\n",this);
      //Loop over all events
      for(lExecli=0; lExecli<lExecneaealast; ++lExecli) {
	//printf("Entry %lli/%lli\n",lExecli,lExecneaealast);

	//Load all triggered input all events arrays
	for(lExecj=lExeciaarrays.Count()-1; lExecj>=0; --lExecj) {
	  //printf("\tInput array %i/%i\n",lExecj,lExeciaarrays.Count());
	  lExeciaarrays.GetArray()[lExecj]->LoadEntry(lExecli);
	}

	//Loop over all triggered non-post-selection events processes
	for(lExecj=0; lExecj<lExecnaeprocs; ++lExecj) {
	  //Exec the process if it depends on a selected events array
	  if(!lExecseldepprocs[lExecj]) ((QNamedProc*)lExecprocs.GetArray()[lExecj])->Exec();
	}

	//Assertion: Post-selection processes that depend on a selected events array are called only for selected events
	//Loop over all triggered post-selection processes
	for(lExecj=lExecnaeprocs; lExecj<lExecprocs.Count(); ++lExecj) {
	  //If the process does not depend on a selector process, execute it
	  if(!lExecseldepprocs[lExecj]) ((QNamedProc*)lExecprocs.GetArray()[lExecj])->Exec();
	}

	//Assertion: Output arrays are filled when it does not depend directly or indirectly on a selector process.
	//Save all triggered output arrays
	for(lExecj=lExecoarrays.Count()-1; lExecj>=0; --lExecj) {
	  //printf("\tOutput array %i/%i\n",lExecj,lExecoarrays.Count());
	  if(!lExecoasproc[lExecj]) lExecoarrays.GetArray()[lExecj]->Fill();
	}
      }
      //printf("Done exec ArrayProcessor %p\n",this);
    }

    //Loop over needed output objects
    for(lExeci=lExecoobjects.Count()-1; lExeci>=0; --lExeci) {
      //Terminate the object
      lExecoobjects[lExeci]->TerminateProcObj();
      //Update the modification time for the object
      lExecoobjects[lExeci]->UpdateModTime();
    }

    //Loop over needed output arrays
    for(lExeci=lExecoarrays.Count()-1; lExeci>=0; --lExeci) {
      //Terminate the output array
      lExecoarrays[lExeci]->TerminateProcObj();
      //Update the modification time
      lExecoarrays[lExeci]->UpdateModTime();
    }

    //Save the parameters
    for(lExeci=fParams->Count()-1; lExeci>=0; --lExeci) fLastParams->GetArray()[lExeci]=*(fParams->GetArray()[lExeci]);
    fLastExec.Set();
    if(GetVerbosity()&QProcessor::kShowExec2) printf("QArrayProcessor('%s')::Exec() Done\n",GetName());

  } else {

    if(GetVerbosity()&QProcessor::kShowExec2) printf("QArrayProcessor('%s')::Exec() Start and Done\n",GetName());
  }
}

void QArrayProcessor::InitProcess(Bool_t allocateparammem)
{
  TerminateProcess();
  TDirectory *curdir=gDirectory;
  TDirectory *dbuf;
  Int_t i,j,k;
  TString proto;

  Int_t *btypes;

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

    if(!strdiffer(proto,"tree")) {
      //Create the output array and store the pointer
      (*fOArrays).Add(QProcBranchHandler::LoadBranch((TString)(*fOANames)[i][1](j+3,(*fOANames)[i][1].Length()-j-3),(*fOANames)[i][0], kTRUE));

    } else if(!strdiffer(proto,"ttree")) {
      //Create the output array and store the pointer
      (*fOArrays).Add(QProcBranchHandler::LoadBranch((TString)(*fOANames)[i][1](j+3,(*fOANames)[i][1].Length()-j-3),(*fOANames)[i][0], kTRUE, kTRUE, kTRUE));

    } else if(!strdiffer(proto,"qoa")) {
      //Create the output array and store the pointer
      (*fOArrays).Add(QProcQOAHandler::LoadQOA((TString)(*fOANames)[i][1](j+3,(*fOANames)[i][1].Length()-j-3),(*fOANames)[i][0], kTRUE));

    } else {
      fprintf(stderr,"QArrayProcessor::InitProcess(): Error: Array type '%s' is unknown\n",proto.Data());
      throw 1;
    }
    //printf("Output array %i (%p) has been loaded\n",i,fOArrays->GetLast());
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

    if(!strdiffer(proto,"tree") || !strdiffer(proto,"ttree")) {
      //Create the input array and store the pointer
      (*fIArrays).Add(QProcBranchHandler::LoadBranch((TString)(*fIANames)[i][1](j+3,(*fIANames)[i][1].Length()-j-3),(*fIANames)[i][0], kFALSE));

    } else if(!strdiffer(proto,"qoa")) {
      //Create the input array and store the pointer
      (*fIArrays).Add(QProcQOAHandler::LoadQOA((TString)(*fIANames)[i][1](j+3,(*fIANames)[i][1].Length()-j-3),(*fIANames)[i][0], kFALSE));

    } else {
      fprintf(stderr,"QArrayProcessor::InitProcess(): Error: Array type '%s' is unknown\n",proto.Data());
      throw 1;
    }
    //printf("Input array %i (%p) has been loaded\n",i,fIArrays->GetLast());

    //If this is an absolute input array, add it to the list of absolute input arrays
    if(fOArrays->FindFirst(fIArrays->GetLast())==-1) fAIArrays->Add(fIArrays->GetLast());
  }

  //Create output buffers
  fBuffers->RedimList(fBuNames->Count());
  btypes=new Int_t[fBuNames->Count()];

  for(i=fBuNames->Count()-1; i>=0; --i) {
    btypes[i]=QTypes::GetNameTypeID((*fBuNames)[i]);
    (*fBuffers)[i]=AllocType(btypes[i]);
  }

  QStdProcessor::InitProcess(allocateparammem);

  Int_t nprocs=fProcs->Count();
  QNamedProc *proc;
  TString sbuf;
  QList<TString> qlsbuf;
  qlsbuf.RedimList(2);

  //Loop over the processes
  for(i=0; i<nprocs; i++) {
    proc=&((*fProcs)[i]);
    proc->TellAddress();

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
	k=FindArray(fIANames,qlsbuf);
	proc->SetIVarPtr(j,(*fIArrays)[k]->GetBuffer(),(*fIArrays)[k]->GetBTypeID());

	//Else if the input is a memory buffer
      } else {
	//Get the buffer address from the memory buffers list
	k=FindBuffer(fBuNames,qlsbuf[0]);
	proc->SetIVarPtr(j,(*fBuffers)[k],btypes[k]);
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
	k=FindArray(fOANames,qlsbuf);
	proc->SetOVarPtr(j,(*fOArrays)[k]->GetBuffer(),(*fOArrays)[k]->GetBTypeID());

	//Else if the output is a memory buffer
      } else {
	//Get the buffer address from the memory buffers list
	k=FindBuffer(fBuNames,qlsbuf[0]);
	proc->SetOVarPtr(j,(*fBuffers)[k],btypes[k]);
      }
    }
  }

  delete[] btypes;

  if(fAAIArrays!=fAIArrays) {
      delete fAAIArrays;
      fAAIArrays=fAIArrays;
  }

  if(fAAPDepends!=fAPDepends) {
      delete fAAPDepends;
      fAAPDepends=fAPDepends;
  } 

  if(fAAIObjects!=fAIObjects) {
      delete fAAIObjects;
      fAAIObjects=fAIObjects;
  }

  if(fActiveAIO) {
      delete fActiveAIO;
      fActiveAIO=NULL;
  }

  if(fLastActiveAIO) {
      delete fLastActiveAIO;
      fLastActiveAIO=NULL;
  }

  if(fAObjsPDepends!=fObjsPDepends) {
      delete fAObjsPDepends;
      fAObjsPDepends=fObjsPDepends;
  } 

  //Erase last parameters
  fLastParams->Clear();
  fLastParams->RedimList(fParams->Count(),-1,0.);
  fLastExec.SetSec(0);

  curdir->cd();
}

void QArrayProcessor::UpdateProcessFlags()
{
    QMask mask;
    mask.FillMask(fParams->Count());

    //If all parameters are active
    if(fActiveParams && !((*fActiveParams)^mask)) {
	delete fActiveParams;
	fActiveParams=NULL;
    }
    mask.Clear();
    Bool_t didPreviousExec=kFALSE;
    Int_t i,j,k=0;

    //If all parameters were previously active
    if(fAParams==fParams) {

	//If it is no longer the case (assertions: fLastActiveParams==NULL, fProcsAParDepends==fProcsParDepends)
	if(fActiveParams) {
            //First call Exec to update the processors before deactivating some parameters
	    Exec();
	    didPreviousExec=kTRUE;

	    const Int_t& nparams=fParams->Count();

	    for(i=nparams-1; i>=0; --i) if(fActiveParams->GetBit(i)) ++k;

	    fAParams=new QList<Double_t*>;
	    fAParams->RedimList(k);
	    fAParIndexMapping=new QList<Int_t>;
	    fAParIndexMapping->RedimList(nparams,-1,-1);
	    QList<double>* tmpLastParams=new QList<double>;
	    tmpLastParams->RedimList(k);
	    fProcsAParDepends=new QList<QMask>;
	    const Int_t& nprocs=fProcs->Count();
	    fProcsAParDepends->RedimList(nprocs);
	    --k;

	    for(i=nparams-1; i>=0; --i) {

		if(fActiveParams->GetBit(i)) {
		    (*fAParams)[k]=(*fParams)[i];
		    (*fAParIndexMapping)[i]=k;
		    (*tmpLastParams)[k]=(*fLastParams)[i];

		    for(j=nprocs-1; j>=0; --j) (*fProcsAParDepends)[j].SetBit(k,(*fProcsParDepends)[j].GetBit(i));
		    --k;
		}
	    }

	    delete fLastParams;
	    fLastParams=tmpLastParams;
	    fLastActiveParams=new QMask(*fActiveParams);
	}

    //Else if some of the parameters were not previously active
    } else {

	//If some parameters are still inactive
	if(fActiveParams) {
	    k=0;
	    Int_t l=0;
	    Bool_t hasDeactivated=kFALSE;
	    Bool_t hasActivated=kFALSE;
	    const Int_t& nparams=fParams->Count();

	    //Count the number of active parameters and check if some parameters
	    //are being deactivated
	    for(i=nparams-1; i>=0; --i) {
	      if(fActiveParams->GetBit(i)) {
		++k;

		if(fLastActiveParams->GetBit(i)) ++l;
		else if(!hasActivated) hasActivated=kTRUE;

	      } else if(fLastActiveParams->GetBit(i)) {

		if(!hasDeactivated) hasDeactivated=kTRUE;
		++l;
	      }
	    }

	    if(hasDeactivated) {
		Exec();
		didPreviousExec=kTRUE;
	    }

	    fAParams->RedimList(k);
	    fAParIndexMapping->Clear();
	    fAParIndexMapping->RedimList(nparams,-1,-1);
	    QList<double>* tmpLastParams=new QList<double>;
	    tmpLastParams->RedimList(k);
	    fProcsAParDepends->Clear();
	    const Int_t& nprocs=fProcs->Count();
	    fProcsAParDepends->RedimList(nprocs);
	    Int_t j;
	    --k;
	    --l;

	    for(i=nparams-1; i>=0; --i) {

		if(fActiveParams->GetBit(i)) {
		    (*fAParams)[k]=(*fParams)[i];
		    (*fAParIndexMapping)[i]=k;

		    if(fLastActiveParams->GetBit(i)) {
		      (*tmpLastParams)[k]=(*fLastParams)[l];
		      --l;

		    } else (*tmpLastParams)[k]=!*(*fParams)[i];

		    for(j=nprocs-1; j>=0; --j) (*fProcsAParDepends)[j].SetBit(k,(*fProcsParDepends)[j].GetBit(i));
		    --k;

		} else if (fLastActiveParams->GetBit(i)) --l;
	    }

	    delete fLastParams;
	    fLastParams=tmpLastParams;
	    *fLastActiveParams=*fActiveParams;

	    //Update the processes to account for the newly activated parameters
	    if(hasActivated) {
		Exec();
		didPreviousExec=kTRUE;
	    }

	//Else if all parameters are now active
	} else {
	    delete fAParams;
	    fAParams=fParams;
	    delete fAParIndexMapping;
	    fAParIndexMapping=NULL;
	    delete fProcsAParDepends;
	    fProcsAParDepends=fProcsParDepends;

	    QList<double>* tmpLastParams=new QList<double>;
	    const Int_t& nparams=fParams->Count();
	    tmpLastParams->RedimList(nparams);

	    j=0;

	    for(Int_t i=0; i<nparams; ++i) {

		//If the parameter was active
		if(fLastActiveParams->GetBit(i)) {
		    (*tmpLastParams)[i]=(*fLastParams)[j];
		    ++j;

		} else (*tmpLastParams)[i]=!*(*fParams)[i]; //Just setting the last param value to a value
							    //different from the current parameter value
	    }
	    delete fLastParams;
	    fLastParams=tmpLastParams;

	    delete fLastActiveParams;
	    fLastActiveParams=NULL;

	    //Update the processes to account for the newly activated parameters
	    Exec();
	    didPreviousExec=kTRUE;
	}
    }

    mask.FillMask(fAIObjects->Count());

    //If all aios are active
    if(fActiveAIO && !((*fActiveAIO)^mask)) {
	delete fActiveAIO;
	fActiveAIO=NULL;
    }
    mask.Clear();

    //If all aios were previously active
    if(fAAIObjects==fAIObjects) {

	//If it is no longer the case (assertions: fLastActiveAIO==NULL, fAObjsPDepends==fObjsPDepends)
	if(fActiveAIO) {

	    if(!didPreviousExec) Exec();
	    k=0;

	    const Int_t& naios=fAIObjects->Count();

	    for(i=naios-1; i>=0; --i) if(fActiveAIO->GetBit(i)) ++k;

	    fAAIObjects=new QList<QProcObj*>;
	    fAAIObjects->RedimList(k);
	    fAObjsPDepends=new QList<QMask>;
	    const Int_t& nprocs=fProcs->Count();
	    fAObjsPDepends->RedimList(k);
	    --k;

	    for(i=naios-1; i>=0; --i) {

		if(fActiveAIO->GetBit(i)) {
		    (*fAAIObjects)[k]=(*fAIObjects)[i];

		    for(j=nprocs-1; j>=0; --j) (*fAObjsPDepends)[k].SetBit(j,(*fObjsPDepends)[i].GetBit(j));
		    --k;
		}
	    }
	    fLastActiveAIO=new QMask(*fActiveAIO);
	}

    //Else if some of the aios were not previously active
    } else {

	//If some aios are still inactive
	if(fActiveAIO) {
	    k=0;
	    Bool_t hasDeactivated=kFALSE;
	    const Int_t& naios=fAIObjects->Count();

	    //Count the number of active aios and check if some aios
	    //are being deactivated
	    for(i=naios-1; i>=0; --i) {
	      if(fActiveAIO->GetBit(i)) {
		++k;

	      } else if(fLastActiveAIO->GetBit(i)) {

		if(!hasDeactivated) hasDeactivated=kTRUE;
	      }
	    }

	    if(hasDeactivated && !didPreviousExec) Exec();

	    fAAIObjects->RedimList(k);
	    fAObjsPDepends->Clear();
	    const Int_t& nprocs=fProcs->Count();
	    fAObjsPDepends->RedimList(k);
	    --k;

	    for(i=naios-1; i>=0; --i) {

		if(fActiveAIO->GetBit(i)) {
		    (*fAAIObjects)[k]=(*fAIObjects)[i];

		    for(j=nprocs-1; j>=0; --j) (*fAObjsPDepends)[k].SetBit(j,(*fObjsPDepends)[i].GetBit(j));
		    --k;

		}
	    }
	    *fLastActiveAIO=*fActiveAIO;

	//Else if all aios are now active
	} else {
	    delete fAAIObjects;
	    fAAIObjects=fAIObjects;
	    delete fAObjsPDepends;
	    fAObjsPDepends=fObjsPDepends;
	    delete fLastActiveAIO;
	    fLastActiveAIO=NULL;
	}
    }

    mask.FillMask(fAIArrays->Count());

    //If all aias are active
    if(fActiveAIO && !((*fActiveAIO)^mask)) {
	delete fActiveAIO;
	fActiveAIO=NULL;
    }
    mask.Clear();

    //If all aias were previously active
    if(fAAIArrays==fAIArrays) {

	//If it is no longer the case (assertions: fLastActiveAIA==NULL, fAAPDepends==fAPDepends)
	if(fActiveAIA) {

	    if(!didPreviousExec) Exec();
	    k=0;

	    const Int_t& naios=fAIArrays->Count();

	    for(i=naios-1; i>=0; --i) if(fActiveAIA->GetBit(i)) ++k;

	    fAAIArrays=new QList<QProcArray*>;
	    fAAIArrays->RedimList(k);
	    fAAPDepends=new QList<QMask>;
	    const Int_t& nprocs=fProcs->Count();
	    fAAPDepends->RedimList(k);
	    --k;

	    for(i=naios-1; i>=0; --i) {

		if(fActiveAIA->GetBit(i)) {
		    (*fAAIArrays)[k]=(*fAIArrays)[i];

		    for(j=nprocs-1; j>=0; --j) (*fAAPDepends)[k].SetBit(j,(*fAPDepends)[i].GetBit(j));
		    --k;
		}
	    }
	    fLastActiveAIA=new QMask(*fActiveAIA);
	}

    //Else if some of the aias were not previously active
    } else {

	//If some aias are still inactive
	if(fActiveAIA) {
	    k=0;
	    Bool_t hasDeactivated=kFALSE;
	    const Int_t& naios=fAIArrays->Count();

	    //Count the number of active aias and check if some aias
	    //are being deactivated
	    for(i=naios-1; i>=0; --i) {
	      if(fActiveAIA->GetBit(i)) {
		++k;

	      } else if(fLastActiveAIA->GetBit(i)) {

		if(!hasDeactivated) hasDeactivated=kTRUE;
	      }
	    }

	    if(hasDeactivated && !didPreviousExec) Exec();

	    fAAIArrays->RedimList(k);
	    fAAPDepends->Clear();
	    const Int_t& nprocs=fProcs->Count();
	    fAAPDepends->RedimList(k);
	    --k;

	    for(i=naios-1; i>=0; --i) {

		if(fActiveAIA->GetBit(i)) {
		    (*fAAIArrays)[k]=(*fAIArrays)[i];

		    for(j=nprocs-1; j>=0; --j) (*fAAPDepends)[k].SetBit(j,(*fAPDepends)[i].GetBit(j));
		    --k;

		}
	    }
	    *fLastActiveAIA=*fActiveAIA;

	//Else if all aias are now active
	} else {
	    delete fAAIArrays;
	    fAAIArrays=fAIArrays;
	    delete fAAPDepends;
	    fAAPDepends=fAPDepends;
	    delete fLastActiveAIA;
	    fLastActiveAIA=NULL;
	}
    }
}

const QArrayProcessor& QArrayProcessor::operator=(const QArrayProcessor &rhs)
{
    QStdProcessor::operator=(rhs);
    *fSelProcs=*rhs.fSelProcs;
    *fTrigAEProcs=*rhs.fTrigAEProcs;
    fNAEProcs=rhs.fNAEProcs;
    fAnalysisDir=rhs.fAnalysisDir;
    *fIANames=*rhs.fIANames;
    *fAIANames=*rhs.fAIANames;
    *fOANames=*rhs.fOANames;
    *fBuNames=*rhs.fBuNames;
    *fIAIndices=*rhs.fIAIndices;
    *fIOIndices=*rhs.fIOIndices;
    *fOAIndices=*rhs.fOAIndices;
    *fOOIndices=*rhs.fOOIndices;

    if(fAAIArrays!=fAIArrays) {
	delete fAAIArrays;
	fAAIArrays=fAIArrays;
    }
    *fIObjects=*rhs.fIObjects;
    *fAIObjects=*rhs.fAIObjects;

    if(rhs.fAAIObjects==rhs.fAIObjects) {

	if(fAAIObjects!=fAIObjects) {
	    delete fAAIObjects;
	    fAAIObjects=fAIObjects;
	}

    } else {

	if(fAAIObjects==fAIObjects) fAAIObjects=new QList<QProcObj*>(*rhs.fAAIObjects);
	else *fAAIObjects=*rhs.fAAIObjects;
    }

    *fOObjects=*rhs.fOObjects;
    *fSelDepProcs=*rhs.fSelDepProcs;
    *fIASProc=*rhs.fIASProc;
    *fOASProc=*rhs.fOASProc;
    *fAPDepends=*rhs.fAPDepends;

    if(rhs.fAAPDepends==rhs.fAPDepends) {

	if(fAAPDepends!=fAPDepends) {
	    delete fAAPDepends;
	    fAAPDepends=fAPDepends;
	}

    } else {

	if(fAAPDepends==fAPDepends) fAAPDepends=new QList<QMask>(*rhs.fAAPDepends);
	else *fAAPDepends=*rhs.fAAPDepends;
    }
    *fObjsPDepends=*rhs.fObjsPDepends;

    if(rhs.fAObjsPDepends==rhs.fObjsPDepends) {

	if(fAObjsPDepends!=fObjsPDepends) {
	    delete fAObjsPDepends;
	    fAObjsPDepends=fObjsPDepends;
	}

    } else {

	if(fAObjsPDepends==fObjsPDepends) fAObjsPDepends=new QList<QMask>(*rhs.fAObjsPDepends);
	else *fAObjsPDepends=*rhs.fAObjsPDepends;
    }

    if(rhs.fActiveAIO) {

	if(fActiveAIO) *fActiveAIO=*rhs.fActiveAIO;
	else fActiveAIO=new QMask(*rhs.fActiveAIO);

    } else {
	if(fActiveAIO) {
	    delete fActiveAIO;
	    fActiveAIO=NULL;
	}
    }

    if(rhs.fLastActiveAIO) {

	if(fLastActiveAIO) *fLastActiveAIO=*rhs.fLastActiveAIO;
	else fLastActiveAIO=new QMask(*rhs.fLastActiveAIO);

    } else {
	if(fLastActiveAIO) {
	    delete fLastActiveAIO;
	    fLastActiveAIO=NULL;
	}
    }
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

  Int_t i,j,k,l;
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
      if(sbuf.Length()) {
	printf("\t%s",sbuf.Data());

	if((l=fAIANames->FindFirst((*fIANames)[(*fIAIndices)[i][k]]))!=-1) printf(" (%i)",l);
	++k;
      }
      printf("\n");
    }

    printf("\nInput Objects:\n");
    for(j=0; j<(*fIOIndices)[i].Count(); j++) {
      printf("%3i\t",j);
      if(dynamic_cast<TObject*>((*fIObjects)[(*fIOIndices)[i][j]])) printf("%s (%p)",dynamic_cast<TObject*>((*fIObjects)[(*fIOIndices)[i][j]])->GetName(),(*fIObjects)[(*fIOIndices)[i][j]]);
      else printf("%p",(*fIObjects)[(*fIOIndices)[i][j]]);

      if((l=fAIObjects->FindFirst((*fIObjects)[(*fIOIndices)[i][j]]))!=-1) printf(" (%i)\n",l);
      else printf("\n");
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

  printf("\nAbsolute Input Arrays:\n");
  for(i=0; i<fAIANames->Count(); i++) {
    printf("%3i Array '%s\t%s'\t",i,(*fAIANames)[i][0].Data(),(*fAIANames)[i][1].Data());
    (*fAPDepends)[i].Print();
  }

  printf("\nAbsolute Input Objects:\n");
  for(i=0; i<fAIObjects->Count(); i++) {
    printf("%3i\t",i);
    if(dynamic_cast<TObject*>((*fAIObjects)[i])) printf("%s (%p)\t",dynamic_cast<TObject*>((*fAIObjects)[i])->GetName(),(*fAIObjects)[i]);
    else printf("%p\t",(*fAIObjects)[i]);
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

void QArrayProcessor::PrintProcesses(const UInt_t &level, const Bool_t &printdeps) const
{
  Int_t i,j;
  Int_t nprocs=fProcs->Count();
  QNamedProc *proc;
  QMask mask;

  if(printdeps) {

    printf("\n%*sParameters:\n",level*3,"");
    for(j=0; j<GetNParams(); j++) {
      printf("%*s%3i:\t%s\n",level*3,"",j,GetParamName(j));
    }

    printf("\n%*sInput Arrays:\n",level*3,"");
    for(j=0; j<fIANames->Count(); j++) {
      printf("%*s%3i '%s\t%s'\n",level*3,"",j,(*fIANames)[j][0].Data(),(*fIANames)[j][1].Data());
    }

    printf("\n%*sInput Objects:\n",level*3,"");
    for(j=0; j<fIObjects->Count(); j++) {
      printf("%*s%3i ",level*3,"",j);
      if(dynamic_cast<TObject*>((*fIObjects)[j])) printf("%s (%p)\n",dynamic_cast<TObject*>((*fIObjects)[j])->GetName(),(*fIObjects)[j]);
      else printf("%p\n",(*fIObjects)[j]);
    }
  }
  printf("\n");

  for(i=0; i<nprocs; i++) {
    proc=&((*fProcs)[i]);

    if(i<fNAEProcs) {
      printf("%*s%03i All events process '%s'\n",level*3,"",i,proc->GetName());

    } else {
      printf("%*s%03i Selected events process '%s'\n",level*3,"",i,proc->GetName());
    }

    if(printdeps) {
      printf("%*sP: ",level*3+4,"");
      (*fProcsParDepends)[i].Print();

      mask.Clear();
      for(j=0; j<fAIANames->Count(); j++) if((*fAPDepends)[j].GetBit(i)) mask.SetBit(j,kTRUE);
      printf("%*sAIA: ",level*3+4,"");
      mask.Print();

      mask.Clear();
      for(j=0; j<fAIObjects->Count(); j++) if((*fObjsPDepends)[j].GetBit(i)) mask.SetBit(j,kTRUE);
      printf("%*sAIO: ",level*3+4,"");
      mask.Print();
    }
  }
}

void QArrayProcessor::SetAIAActive(const char* name, const char* title, const Bool_t &active)
{
    QList<TString> qlsbuf;

    if(title && strlen(title)) {
	qlsbuf.RedimList(2);
	qlsbuf[0]=name;
	qlsbuf[1]=title;

    } else qlsbuf.Add(name);
    Int_t index;

    if((index=fAIANames->FindFirst(qlsbuf))!=-1) {

	if(!fActiveAIA) {

	    if(!active) {
		fActiveAIA=new QMask;
		fActiveAIA->FillMask(fAIANames->Count());
		fActiveAIA->SetBit(index,kFALSE);
	    }
	} else fActiveAIA->SetBit(index,active);

    } else {
	fprintf(stderr,"QArrayProcessor::SetAIAActive: Error: Absolute input array %s\t%s does not exist\n",name,title?title:"");
	throw 1;
    }
}

void QArrayProcessor::SetAIOActive(QProcObj *const obj, const Bool_t &active)
{
    Int_t index;

    if((index=fAIObjects->FindFirst(obj))!=-1) {

	if(!fActiveAIO) {

	    if(!active) {
		fActiveAIO=new QMask;
		fActiveAIO->FillMask(fAIObjects->Count());
		fActiveAIO->SetBit(index,kFALSE);
	    }
	} else fActiveAIO->SetBit(index,active);

    } else {
	fprintf(stderr,"QArrayProcessor::SetAIOActive: Error: Absolute input object %p does not exist\n",obj);
	throw 1;
    }
}

void QArrayProcessor::TerminateProcess()
{
  Int_t i;

  for(i=fIArrays->Count()-1; i>=0; --i) (*fIArrays)[i]->UnloadArray();
  for(i=fOArrays->Count()-1; i>=0; --i) (*fOArrays)[i]->UnloadArray();
  for(i=fBuffers->Count()-1; i>=0; --i) free((*fBuffers)[i]);

  fIArrays->Clear();
  fOArrays->Clear();
  fBuffers->Clear();
}

void QArrayProcessor::Browse(TBrowser *b)
{   
  if(b) { 
    b->Add(fProcs,"Processes");
  } 
}

Int_t QArrayProcessor::AEProcIndexToIndex(const Int_t &index){
  if(index == -1) return fNAEProcs;

  if(index > fNAEProcs) {
    fprintf(stderr,"QArrayProcessor::AEProcIndexToIndex: Error: Index %i is invalid\n",index);
    throw 1;
  }

  return index;
}

Int_t QArrayProcessor::PSProcIndexToIndex(const Int_t &index){
  if(index == -1) return fProcs->Count();

  if(index < fNAEProcs) {
    fprintf(stderr,"QArrayProcessor::PSProcIndexToIndex: Error: Index %i is invalid\n",index);
    throw 1;
  }

  return index;
}

void QArrayProcessor::BuildObjLists()
{
  Int_t i;

  if(!fAllIObjects) {
    fAllIObjects=new QList<QProcObj*>;
    fAllOObjects=new QList<QProcObj*>;

    fAllIObjects->Add(*fIObjects);
    for(i=fIArrays->Count()-1; i>=0; --i) fAllIObjects->AddUnique((*fIArrays)[i]);

    fAllOObjects->Add(*fOObjects);
    for(i=fOArrays->Count()-1; i>=0; --i) fAllOObjects->AddUnique((*fOArrays)[i]);
  }
}

Int_t QArrayProcessor::AddUniqueArray(QList<QList<TString> > *arraylist, const QList<TString> &arraydesc, const Int_t &deftype)
{
  Int_t i;
  TString lname, name;
  Int_t lid, id;

  for(i=arraylist->Count()-1; i>=0; --i) {

    if((*arraylist)[i][1] == arraydesc[1]) {
      lid=QTypes::GetNameTypeID((*arraylist)[i][0],&lname);
      id=QTypes::GetNameTypeID(arraydesc[0],&name);

      if(name==lname) {
	
	if(lid!=-1 && id!=-1 && lid!=id) return -2;

	if(lid==-1) {

	  if(id!=-1) (*arraylist)[i][0]=name+"/"+QTypes::GetTypeName(id);

	  else if(deftype!=-1) (*arraylist)[i][0]=name+"/"+QTypes::GetTypeName(deftype);
	  return i;
	}
      }
    }
  }
  arraylist->Add(arraydesc);
  id=QTypes::GetNameTypeID(arraydesc[0],&name);
  
  if(id!=-1) arraylist->GetLast()[0]=name+"/"+QTypes::GetTypeName(id);
  
  else if(deftype!=-1) arraylist->GetLast()[0]=name+"/"+QTypes::GetTypeName(deftype);
  return -1;
}

Int_t QArrayProcessor::FindArray(const QList<QList<TString> > *arraylist, const QList<TString> &arraydesc)
{
  Int_t i;
  TString lname, name;
  Int_t lid, id;

  for(i=arraylist->Count()-1; i>=0; --i) {

    if((*arraylist)[i][1] == arraydesc[1]) {
      lid=QTypes::GetNameTypeID((*arraylist)[i][0],&lname);
      id=QTypes::GetNameTypeID(arraydesc[0],&name);

      if(name==lname) {
	
	if(lid!=-1 && id!=-1 && lid!=id) return -2;
	return i;
      }
    }
  }
  return -1;
}

Int_t QArrayProcessor::AddUniqueBuffer(QList<TString> *buflist, const Char_t *bufdesc, const Int_t &deftype)
{
  Int_t i;
  TString lname, name;
  Int_t lid, id;

  for(i=buflist->Count()-1; i>=0; --i) {

    lid=QTypes::GetNameTypeID((*buflist)[i],&lname);
    id=QTypes::GetNameTypeID(bufdesc,&name);

    if(name==lname) {

      if(lid!=-1 && id!=-1 && lid!=id) return -2;

      if(lid==-1) {

	if(id!=-1) (*buflist)[i]=name+"/"+QTypes::GetTypeName(id);

	else if(deftype!=-1) (*buflist)[i]=name+"/"+QTypes::GetTypeName(deftype);
	return i;
      }
    }
  }
  buflist->Add(bufdesc);
  id=QTypes::GetNameTypeID(bufdesc,&name);

  if(id!=-1) buflist->GetLast()=name+"/"+QTypes::GetTypeName(id);
  
  else if(deftype!=-1) buflist->GetLast()=name+"/"+QTypes::GetTypeName(deftype);
  return -1;
}

Int_t QArrayProcessor::FindBuffer(const QList<TString> *buflist, const Char_t *bufdesc)
{
  Int_t i;
  TString lname, name;
  Int_t lid, id;

  for(i=buflist->Count()-1; i>=0; --i) {

    lid=QTypes::GetNameTypeID((*buflist)[i],&lname);
    id=QTypes::GetNameTypeID(bufdesc,&name);

    if(name==lname) {

      if(lid!=-1 && id!=-1 && lid!=id) return -2;
      return i;
    }
  }
  return -1;
}

#include "debugger.h"
