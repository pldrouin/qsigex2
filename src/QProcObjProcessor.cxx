#include "QProcObjProcessor.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QProcObjProcessor)

QProcObjProcessor::~QProcObjProcessor()
{
  PRINTF2(this,"\tQProcObjProcessor::~QProcObjProcessor()\n")
  delete fIOIndices;
  fIOIndices=NULL;
  delete fOOIndices;
  fOOIndices=NULL;
  delete fObjsPDepends;
  fObjsPDepends=NULL;
  delete fIObjects;
  fIObjects=NULL;
  delete fOObjects;
  fOObjects=NULL;
}

Int_t QProcObjProcessor::AddProc(const char *name, const char *title, Int_t index)
{
  PRINTF8(this,"\tQProcObjProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, Int_t index<",index,">)\n")

  index=fProcs->RedimList(fProcs->Count()+1,index);
  (*fProcs)[index].SetNameTitle(name,title);
  return index;
}

Int_t QProcObjProcessor::AddProc(const char *name, const char *title, Bool_t (*proc)(QProcArgs&),const char *procname, Int_t index)
{
  PRINTF12(this,"\tQProcObjProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, Bool_t (*proc)(QProcArgs&)<",proc,">, const char *procname<'",procname,"'>, Int_t index<",index,">)\n")

  index=AddProc(name,title,index);
  (*fProcs)[index].SetProc(proc,procname);
  return index;
}

Int_t QProcObjProcessor::AddProc(const char *name, const char *title, const char *procname, Int_t index)
{
  PRINTF10(this,"\tQProcObjProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, const char *procname<'",procname,"'>, Int_t index<",index,">)\n")

  index=AddProc(name,title,index);
  (*fProcs)[index].SetProc(procname);
  return index;
}

Int_t QProcObjProcessor::AddProc(const char *name, const char *title, void *proc, const char *procname, Int_t index)
{
  PRINTF12(this,"\tQProcObjProcessor::AddProc(const char *name<'",name,"'>, const char *title<'",title,"'>, void *proc<",proc,">, const char *procname<'",procname,"'>, Int_t index<",index,">)\n")

  index=AddProc(name,title,index);
  (*fProcs)[index].SetProc(proc,procname);
  return index;
}

void QProcObjProcessor::Analyze()
{
  QStdProcessor::Analyze();
  Int_t i,j,k;
  QNamedProc *proc;
  QList<TString> params; //Parameters
  TString sbuf;

  Int_t nprocs=fProcs->Count();
  Int_t niobjs, noobjs, nparams;
  Int_t iidx, oidx;

  fObjsPDepends->Clear();
  fIOIndices->Clear();
  fOOIndices->Clear();
  fIObjects->Clear();
  fOObjects->Clear();

  fIOIndices->RedimList(nprocs);
  fOOIndices->RedimList(nprocs);
  QDepTree *depprocs=new QDepTree[nprocs]; //QDepTree objects contain a list of processes that should be triggered
  QList<Int_t> oolastproc; //Index of last process having recorded its output in a given object
  fProcsParDepends->RedimList(nprocs);

  //Section 1: First pass over all processes. Setting as many inputs, outputs and dependencies as possible and doing some checks
  //Loop over the processes
  for(i=0; i<nprocs; i++) {
    proc=&((*fProcs)[i]);
    //printf("Process '%s'\n",proc->GetName());
    if(proc->GetNIVars() != 0) fprintf(stderr,"QProcObjProcessor::Analyze(): Warning: Process '%s' contains input variables. These entries will be ignored\n",proc->GetName());
    niobjs=proc->GetNIObjs();
    if(proc->GetNOVars() != 0) fprintf(stderr,"QProcObjProcessor::Analyze(): Warning: Process '%s' contains output variables. These entries will be ignored\n",proc->GetName());
    noobjs=proc->GetNOObjs();
    nparams=proc->GetNParams();

    (*fProcsParDepends)[i].Clear();

    //Loop over the parameters for the current process
    for(j=0; j<nparams; j++) {
      //Turn on the bit of the dependency mask for the current parameter
      (*fProcsParDepends)[i].SetBit(fParamsNames->FindFirst(proc->GetParam(j).GetName()),1);
    }

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

  //Section 6: Updating input objects dependencies to take into account indirect dependencies
 
  //Loop over all processes
  for(i=0; i<nprocs; i++) {
    proc=&((*fProcs)[i]);
    //printf("%03i Process '%s'\n",i,proc->GetName());
    dpidx=depprocs[i].GetAllDepends();

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

void QProcObjProcessor::DelProc(const char *procname)
{
  Int_t i;
  if((i=FindProcIndex(procname))!=-1) {
    DelProc(i);
  }
}

void QProcObjProcessor::Exec() const
{
  QMask lExecpardiffs;            //Modified parameters since the last call
  QMask lExecdepmods;             //Required processes due to modified input objects
  Bool_t lExecrunall;
  Int_t lExeci;
  Int_t lExecj;
  //lExecpardiffs.Clear();
  //lExecdepmods.Clear();

  //fLastExec gets cleared by the function Analyze, so this is how the first run is identified
  if(fLastExec.GetSec() != 0) {

    //Loop over parameters
    for(lExeci=fParams->Count()-1; lExeci>=0; --lExeci) {

      //If the current parameter value has changed, set the correspongind bit in the parameter mask
      if(*(fParams->GetArray()[lExeci]) != fLastParams->GetArray()[lExeci]) lExecpardiffs.SetBit(lExeci,1);
    }

    //Loop over all input objects
    for(lExeci=fIObjects->Count()-1; lExeci>=0; --lExeci) {

      //If the current input object has been modified after the last run, add its mask to the mask of required processes
      if((*fIObjects)[lExeci]->NewerThan(fLastExec)) lExecdepmods|=(*fObjsPDepends)[lExeci];
    }

    lExecrunall=fForceExecAll;
  } else {
    fNeededOO.RedimList(fOObjects->Count());
    lExecrunall=kTRUE;
  }

  //printf("Mask for the current parameters: ");
  //lExecpardiffs.Print();

  //If at least one of the parameters has changed
  if(lExecpardiffs || lExecdepmods || lExecrunall) {
    if(GetVerbosity()&QProcessor::kShowExec) printf("QProcObjProcessor('%s')::Exec()\n",GetName());

    QList<QProcObj*> lExecoobjects; //List for output objects needing update
    QList<TObject*> lExecprocs;     //List of needed processes
    //lExecoobjects.Clear();
    //lExecprocs.Clear();

    memset(fNeededOO.GetArray(),0,fNeededOO.Count()*sizeof(Bool_t));

    //Loop over all processes
    for(lExeci=0; lExeci<fProcs->Count(); lExeci++) {

      //If the current process has never been run or if it is triggered by the parameters mask
      if(((*fProcsParDepends)[lExeci] && lExecpardiffs) || lExecdepmods.GetBit(lExeci) || lExecrunall) {

	if(GetVerbosity()&QProcessor::kShowExec) printf("\tProcess '%s' will be called\n",(*fProcs)[lExeci].GetName());
	//Add it to the list of needed processes
	lExecprocs.Add(&(*fProcs)[lExeci]);

	//Loop over output objets of the current process
	for(lExecj=(*fOOIndices)[lExeci].Count()-1; lExecj>=0; --lExecj) {
	  //Add the current object to the list of needed output objects
	  fNeededOO[(*fOOIndices)[lExeci][lExecj]]=kTRUE;
	}
      }
    }

    //printf("%p: Init output objects\n",this);
    //Loop over all output objects
    for(lExeci=fNeededOO.Count()-1; lExeci>=0; --lExeci) {
      //printf("%p: %i (%p)\n",this,lExeci,(*fOObjects)[lExeci]);

      //If the current output object is needed
      if(fNeededOO[lExeci]) {
	//Add it to the list of needed output objects
	lExecoobjects.Add((*fOObjects)[lExeci]);
	//Initialize the object
	(*fOObjects)[lExeci]->InitProcObj();
      }
    }
    //printf("%p: Done init output objects\n",this);

    //printf("%p: Exec procs\n",this);
    //Loop over all triggered processes
    for(lExecj=0; lExecj<lExecprocs.Count(); ++lExecj) {
      //printf("%p: %i\n",this,lExecj);
      //Exec the process.
      ((QNamedProc*)lExecprocs.GetArray()[lExecj])->Exec();
    }
    //printf("%p: Done exec procs\n",this);

    //printf("%p: Terminate output objects\n",this);
    //Loop over needed output objects
    for(lExeci=lExecoobjects.Count()-1; lExeci>=0; --lExeci) {
      //printf("%p: %i\n",this,lExeci);
      //Terminate the object
      lExecoobjects[lExeci]->TerminateProcObj();
      //Update the modification time for the object
      lExecoobjects[lExeci]->UpdateModTime();
    }
    //printf("%p: Done terminating output objects\n",this);

    //Save the parameters
    for(lExeci=fParams->Count()-1; lExeci>=0; --lExeci) fLastParams->GetArray()[lExeci]=*(fParams->GetArray()[lExeci]);
    fLastExec.Set();

  } else {

    if(GetVerbosity()&QProcessor::kShowExec2) printf("QProcObjProcessor('%s')::Exec()\n",GetName());
  }
}

void QProcObjProcessor::InitProcess(Bool_t allocateparammem)
{
  TerminateProcess();
  QStdProcessor::InitProcess(allocateparammem);
  Int_t nprocs=fProcs->Count();

  //Loop over the processes
  for(Int_t i=0; i<nprocs; i++) {
    (*fProcs)[i].TellAddress();
  }

  //Erase last parameters
  fLastParams->Clear();
  fLastParams->RedimList(fParams->Count(),-1,0.);
  fLastExec.SetSec(0);
}

const QProcObjProcessor& QProcObjProcessor::operator=(const QProcObjProcessor &rhs)
{
  QStdProcessor::operator=(rhs);
  *fIOIndices=*rhs.fIOIndices;
  *fOOIndices=*rhs.fOOIndices;
  *fIObjects=*rhs.fIObjects;
  *fOObjects=*rhs.fIObjects;
  *fObjsPDepends=*rhs.fObjsPDepends;
  return *this;
}

void QProcObjProcessor::PrintAnalysisResults() const
{
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

    printf("\n%03i Objects process '%s'\n",i,proc->GetName());

    printf("Parameters:\n");
    for(j=0; j<proc->GetNParams(); j++) {
      printf("%3i:\t%s\n",j,proc->GetParam(j).GetName());
    }

    printf("\nInput Objects:\n");
    for(j=0; j<(*fIOIndices)[i].Count(); j++) {
      printf("%3i\t",j);
      if(dynamic_cast<TObject*>((*fIObjects)[(*fIOIndices)[i][j]])) printf("%s (%p)",dynamic_cast<TObject*>((*fIObjects)[(*fIOIndices)[i][j]])->GetName(),(*fIObjects)[(*fIOIndices)[i][j]]);
      else printf("%p",(*fIObjects)[(*fIOIndices)[i][j]]);
      printf(" (%i)\n",(*fIOIndices)[i][j]);
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
  }

  printf("\nAll Input Objects:\n");
  for(i=0; i<fIObjects->Count(); i++) {
    printf("%3i\t",i);
    if(dynamic_cast<TObject*>((*fIObjects)[i])) printf("%s (%p)\t",dynamic_cast<TObject*>((*fIObjects)[i])->GetName(),(*fIObjects)[i]);
    else printf("%p\t",(*fIObjects)[i]);
    (*fObjsPDepends)[i].Print();
  }

  printf("\nAll Output Objects:\n");
  for(i=0; i<fOObjects->Count(); i++) {
    printf("%3i\t",i);
    if(dynamic_cast<TObject*>((*fOObjects)[i])) printf("%s (%p)\n",dynamic_cast<TObject*>((*fOObjects)[i])->GetName(),(*fOObjects)[i]);
    else printf("%p\n",(*fOObjects)[i]);
  }
}

void QProcObjProcessor::PrintProcesses(const UInt_t &level, const Bool_t &printdeps) const
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
    printf("%*s%03i Object process '%s'\n",level*3,"",i,proc->GetName());

    if(printdeps) {
      printf("%*sP: ",level*3+4,"");
      (*fProcsParDepends)[i].Print();

      mask.Clear();
      for(j=0; j<fIObjects->Count(); j++) if((*fObjsPDepends)[j].GetBit(i)) mask.SetBit(j,kTRUE);
      printf("%*sIO: ",level*3+4,"");
      mask.Print();
    }
  }
}

void QProcObjProcessor::TerminateProcess()
{
}

void QProcObjProcessor::Browse(TBrowser *b)
{   
  if(b) { 
    b->Add(fProcs,"Processes");
  } 
}

#include "debugger.h"
