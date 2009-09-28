#include "QProcList.h"

//#define DEBUG

ClassImp(QProcList)

const QProcList *gQProcList;
Bool_t QProcList::fDefPProcessor=kFALSE;

QProcList::~QProcList()
{
  pthread_mutex_destroy(&fChMutex);
  sem_destroy(&fTWSem);
  delete fQPL;
  fQPL=NULL;
}

void QProcList::Analyze()
{
  Int_t i,j;
  Int_t pidx;

  fParamsNames->Clear();
  fParamsChildIndices->Clear();
  fChildParamsMapping->Clear();

  for(i=0; i<fQPL->Count(); i++) {
    try {
    ((QProcessor*)(*fQPL)[i])->Analyze();

    } catch (Int_t e) {
      fprintf(stderr,"QProcList::Analyze(): Error thrown by QProcessor index %i\n",i);
      throw e;
    }

    //Loop over all parameters for the current QProcessor object
    for(j=0; j<((QProcessor*)(*fQPL)[i])->GetNParams(); j++) {
      pidx=fParamsNames->AddUnique(((QProcessor*)(*fQPL)[i])->GetParamName(j));

      if(pidx==-1) {
	pidx=fParamsChildIndices->RedimList(fParamsNames->Count());
	fChildParamsMapping->RedimList(fParamsNames->Count());
      }
      (*fParamsChildIndices)[pidx].Add(i);
      (*fChildParamsMapping)[pidx].Add(j);
    }
  }
}

void QProcList::DeleteChildren(const Int_t &nsublevels)
{
  Int_t i;

  if(nsublevels) {

    for(i=0; i<fQPL->Count(); i++) {

      if(dynamic_cast<QProcList*>(fQPL->GetArray()[i])) {
	dynamic_cast<QProcList*>(fQPL->GetArray()[i])->DeleteChildren(nsublevels-1);
      }
    }
  }

  for(i=0; i<fQPL->Count(); i++) delete fQPL->GetArray()[i]; 
}

void QProcList::Exec() const
{
  if(GetVerbosity()&QProcessor::kShowExec2) printf("QProcList('%s')::Exec()\n",GetName());

  pthread_mutex_lock(&fChMutex);
  fFirstChain=fIFirstChain;
  fLastChain=fILastChain;
  pthread_mutex_unlock(&fChMutex);

  for(Int_t i=fINChains-1; i>=0; --i) sem_post(&fTWSem);

  if(fMTRNotify) {
    if(fMTRNotify->Init) {
      //printf("Waiting for notification %p\n",fMTRNotify);
      sem_wait(&fMTRNotify->RNSem);
      //printf("Received notification %p\n",fMTRNotify);
    }
  }
}

void QProcList::InitProcess(Bool_t allocateparammem)
{
  TerminateProcess();

  for(Int_t i=0; i<fQPL->Count(); ++i)  {

    try {
      ((QProcessor*)(*fQPL)[i])->InitProcess(kFALSE);

    } catch (Int_t e) {
      fprintf(stderr,"QProcList::InitProcess(): Error thrown by QProcessor index %i\n",i);
      throw e;
    }
  }
  QProcessor::InitProcess(allocateparammem);
  InitThreads();
}

void QProcList::InitThreads()
{
  TerminateThreads();

  Int_t i,j,k;
  QList<QList<Int_t> > chains;
  QList<QList<Int_t> > chainsdepons;
  QList<QList<Int_t> > chainsdeps;
  QList<Int_t>         initchains;  //Initial chains
  QList<void*> notifchains; //Chains sending notifications

  if(fPProcessor) {
    QProcessor* proc;
    Int_t npiobjs, npoobjs;
    QList<QProcObj*> iobjs;
    QList<QProcObj*> oobjs;
    Int_t iidx, oidx;

    QDepTree *depprocs=new QDepTree[fQPL->Count()]; //QDepTree objects contain a list of processes that should be triggered
    QList<Int_t> oolastproc; //Index of last process having recorded its output in a given object
    QList<Int_t> iolastproc; //Index of last process having recorded its input in a given object

    for(i=0; i<fQPL->Count(); ++i)  {
      proc=((QProcessor*)(*fQPL)[i]);
      proc->BuildObjLists();
      npiobjs=proc->GetIObjList().Count();
      npoobjs=proc->GetOObjList().Count();

      for(j=npiobjs-1; j>=0; --j) {
	iidx=iobjs.AddUnique(proc->GetIObjList()[j]);

	if(iidx==-1) {
	  iidx=iobjs.Count()-1;
	  ++iolastproc;
	
	//If the object is also read by a previous proc and is not thread safe in reading mode
	} else if(!iobjs[iidx]->IsReadThreadSafe()) {
	  //printf("II Proc %i input object %i (%p) was also an input object for proc %i\n",i,j,iobjs[iidx],iolastproc[iidx]);
	  depprocs[iolastproc[iidx]].AddDepend(i);
	  depprocs[i].AddDepend(iolastproc[iidx]);
	}
	iolastproc[iidx]=i;

	oidx=oobjs.FindFirst(proc->GetIObjList()[j]);

	//If the object has been updated by a previous process
	if(oidx != -1) {
	  //Add the index of the current process to the dependent process list of the last process that updated the current input object and vice-versa
	  //Important to add dependencies in both directions to identify splits correctly.
	  //printf("IO Proc %i input object %i was an output object for proc %i\n",i,j,oolastproc[oidx]);
	  depprocs[oolastproc[oidx]].AddDepend(i);
	  depprocs[i].AddDepend(oolastproc[oidx]);
	}
      }

      for(j=npoobjs-1; j>=0; --j) {
	//Try to find the output object in previous input and output lists
	iidx=iobjs.FindFirst(proc->GetOObjList()[j]);
	oidx=oobjs.FindFirst(proc->GetOObjList()[j]);

	if(oidx==-1 && iidx!=-1 && iolastproc[iidx]!=i) {
	  if(dynamic_cast<TObject*>(proc->GetOObjList()[j])) fprintf(stderr,"QProcList: InitThreads(): Error with process '%s': Object '%s' (%p) cannot be overwritten, since it is an input for previous process '%s'\n",proc->GetName(),dynamic_cast<TObject*>(proc->GetOObjList()[j])->GetName(),proc->GetOObjList()[j],((QProcessor*)(*fQPL)[iolastproc[iidx]])->GetName());
	  else fprintf(stderr,"QProcList: InitThreads(): Error with process '%s': Object %p cannot be overwritten, since it is an input for previous process '%s'\n",proc->GetName(),proc->GetOObjList()[j],((QProcessor*)(*fQPL)[iolastproc[iidx]])->GetName());
	  throw 1;
	}

	//If this is not the first time that the processor outputs to the object and the object was also an input for a previous process
	if(oidx!=-1 && iidx!=-1) {
	  //Add the index of the last process that has saved a value to the current output object to the dependent processes list of the current process and vice-versa.
	  //Important to add dependencies in both directions to identify splits correctly.
	  //printf("OO Proc %i output object %i was also an output object for proc %i\n",i,j,oolastproc[oidx]);
	  depprocs[i].AddDepend(oolastproc[oidx]);
	  depprocs[oolastproc[oidx]].AddDepend(i);
	}

	//Add the object if it has not been generated by a previous process
	if(oidx==-1) {
	  oobjs.Add(proc->GetOObjList()[j]);
	  oidx=oobjs.Count()-1;
	  ++oolastproc;
	}
	oolastproc[oidx]=i;
      }

      if(!proc->fPProcessing) {

	for(j=i-1; j>=0; --j) {
	  depprocs[i].AddDepend(j);
	  depprocs[j].AddDepend(i);
	}

	for(j=fQPL->Count()-1; j>i; --j) {
	  depprocs[j].AddDepend(i);
	  depprocs[i].AddDepend(j);
	}
      }
    }
    for(i=0; i<fQPL->Count(); i++) ((QProcessor*)(*fQPL)[i])->ClearObjLists();

    QDepTree::Simplify();
    QDepTree::GetChains(&chains,&chainsdepons,&chainsdeps);
    delete[] depprocs;

  } else {
    chains++;
    chainsdepons++;
    chainsdeps++;
    chains.GetLast().RedimList(fQPL->Count());
    for(i=fQPL->Count()-1; i>=0; --i) chains.GetLast()[i]=i;
  }

  notifchains.RedimList(chains.Count(),0,NULL);

  fNRNotify=0;
  //Should add code to add other notifications here. These can be identified by setting the corresponding elements in notifchains to non-zero pointers

  if(!fNRNotify) fNRNotify=1;

  if(fNRNotify) {
    fRNotify=new SRNotify[fNRNotify];
    j=0;

    for(i=chains.Count()-1; i>=0; --i) if(notifchains[i] || !chainsdeps[i].Count()) {
      //If no extra notification is requested but the main thread should be notified
      if(notifchains[i]) {
	notifchains[i]=fRNotify+j;
	fRNotify[j].Init=1;
	++j;

      } else {

	if(!fMTRNotify) {
	  fMTRNotify=fRNotify+j;
	  notifchains[i]=fRNotify+j;
	  fRNotify[j].Init=1;
	  ++j;
	} else {
	  notifchains[i]=fMTRNotify;
	  ++(fMTRNotify->Init);
	}
      }
    }

    for(i=fNRNotify-1; i>=0; --i) {
      pthread_mutex_init(&fRNotify[i].RNCMutex,NULL);
      fRNotify[i].RCountDown=fRNotify[i].Init;
      sem_init(&fRNotify[i].RNSem,0,0);
    }
  }

  fNChains=chains.Count();
  fChains=new SChainConfig[fNChains];

  for(i=0; i<fNChains; i++) {
    fChains[i].NProcs=chains[i].Count();
    fChains[i].Procs=new QProcessor*[fChains[i].NProcs];

    //Store QProcessor pointers for the current thread in REVERSE ORDER
    for(j=fChains[i].NProcs-1; j>=0; --j) fChains[i].Procs[j]=((QProcessor*)(*fQPL)[chains[i][fChains[i].NProcs-1-j]]);

    fChains[i].NDChains=chainsdeps[i].Count();

    if(fChains[i].NDChains) {
      fChains[i].DChains=new SChainConfig*[fChains[i].NDChains];

      for(j=fChains[i].NDChains-1; j>=0; --j) fChains[i].DChains[fChains[i].NDChains-1-j]=fChains+chainsdeps[i][j];
    }
    fChains[i].RNotify=(SRNotify*)notifchains[i];
    fChains[i].NRDOChains=fChains[i].NDOChains=chainsdepons[i].Count();

    if(fChains[i].NDOChains>1) {
      pthread_mutex_init(&fChains[i].DOCMutex,NULL);
      fChains[i].Next=fChains[i].NextInit=NULL;

    } else if(fChains[i].NDOChains==1) fChains[i].Next=fChains[i].NextInit=NULL;

    else {
      initchains+=i;
    }
  }

  fIFirstChain=fChains+initchains[0];
  fILastChain=fChains+initchains[initchains.Count()-1];
  fILastChain->Next=fILastChain->NextInit=NULL;
  fINChains=initchains.Count();

  for(i=0; i<initchains.Count()-1; ++i) fChains[initchains[i]].Next=fChains[initchains[i]].NextInit=fChains+initchains[i+1];

  if(GetVerbosity()&QProcessor::kShowExec) {
    printf("QProcList('%s')::InitProcObj(): %i chains have been defined\n",GetName(),chains.Count());

      for(j=0; j<chains.Count(); ++j) {
	printf("\t\tChain %3i (%p):\n",j,fChains+j);

	if(chainsdepons[j].Count()) {
	  printf("\t\t\tWait for chain:");
	  for(k=0; k<chainsdepons[j].Count(); ++k) printf(" %i",chainsdepons[j][k]);
	  printf("\n");

	} else {
	  printf("\t\t\tDo not wait for any chain\n");
	}

	printf("\t\t\tProcessors:\n");
	for(k=0; k<chains[j].Count(); ++k) printf("\t\t\tProcessor '%s' (%i)\n",((QProcessor*)(*fQPL)[chains[j][k]])->GetName(),chains[j][k]);
	printf("\n");

	if(chainsdeps[j].Count()) {
	  printf("\t\t\tSend a signal to chains:");
	  for(k=0; k<chainsdeps[j].Count(); ++k) printf(" %i",chainsdeps[j][k]);
	  printf("\n");

	} else {
	  printf("\t\t\tDo not have to send a signal to any chain\n");
	}
      }
  }

  fStopThreads=kFALSE;
  fFirstChain=NULL;
  fLastChain=NULL;
  fNThreads=(fPProcessor?fRNThreads:1);
  if(fNThreads<0) fNThreads=0;

  if(fNThreads) {
    fThreads=new pthread_t[fNThreads];

    for(i=fNThreads-1; i>=0; --i) {
      pthread_create(&fThreads[i], NULL, QPLThread, this);
    }
  }
}

void QProcList::PrintAnalysisResults() const
{
  Int_t i;

  printf("\n\n********************\nProcessor List '%s'\n********************\n",GetName());

  printf("\nList of parameters for processor list\n");
  for(i=0; i<fParamsNames->Count(); i++) {
    printf("%3i:\t%s\n",i,(*fParamsNames)[i].Data());
  }

  for(i=0; i<fQPL->Count(); i++)  {

    try {
      printf("\n====================\nProcessor '%s'\n====================\n",((QProcessor*)(*fQPL)[i])->GetName());
      ((QProcessor*)(*fQPL)[i])->PrintAnalysisResults();

    } catch (Int_t e) {
      fprintf(stderr,"QProcList::PrintAnalysisResults(): Error thrown by QProcessor index %i\n",i);
      throw e;
    }
  }
}

void QProcList::PrintProcesses(const UInt_t &level, const Bool_t &printdeps) const
{
  Int_t i;

  for(i=0; i<fQPL->Count(); i++) {
    printf("%*s%03i QProcessor '%s'\n",level*3,"",i,((QProcessor*)(*fQPL)[i])->GetName());
    ((QProcessor*)(*fQPL)[i])->PrintProcesses(level+1,printdeps);
  }
}

const QProcList& QProcList::operator=(const QProcList &rhs)
{
  QProcessor::operator=(rhs);
  *fQPL=*rhs.fQPL;
  return *this;
}

void QProcList::SetForceExecAll(const Bool_t &forceexecall)
{
  QProcessor::SetForceExecAll(forceexecall);

  for(Int_t i=0; i<fQPL->Count(); i++) ((QProcessor*)(*fQPL)[i])->SetForceExecAll(forceexecall);
}

void QProcList::SetParamAddress(const Int_t &index, Double_t* const paddr)
{
  QProcessor::SetParamAddress(index,paddr);
  Int_t i;

  //Loop over the children that depend on the current parameter
  for(i=0; i<(*fParamsChildIndices)[index].Count(); i++) {
    //Set the address to the assign buffer for this parameter
    ((QProcessor*)(*fQPL)[(*fParamsChildIndices)[index][i]])->SetParamAddress((*fChildParamsMapping)[index][i],(*fParams)[index]);
  }
}

void QProcList::TerminateProcess()
{
  for(Int_t i=0; i<fQPL->Count(); i++) ((QProcessor*)(*fQPL)[i])->TerminateProcess();
  TerminateThreads();
}

void QProcList::TerminateThreads()
{
  Int_t i;

  if(fThreads) {
    fStopThreads=kTRUE;
    for(i=fINChains-1; i>=0; --i) sem_post(&fTWSem);

    for(i=fNThreads-1; i>=0; --i) pthread_join(fThreads[i],NULL);
    delete[] fThreads;
    fThreads=NULL;
    fNThreads=0;
  }

  if(fChains) {
    for(i=fNChains-1; i>=0; --i) {
      delete[] fChains[i].Procs;
      if(fChains[i].NDChains) delete[] fChains[i].DChains;

      if(fChains[i].NDOChains>1) pthread_mutex_destroy(&fChains[i].DOCMutex);
    }
    delete[] fChains;
    fChains=NULL;
    fNChains=0;
  }

  if(fRNotify) {
    for(i=fNRNotify-1; i>=0; --i) {
      pthread_mutex_destroy(&fRNotify[i].RNCMutex);
      sem_destroy(&fRNotify[i].RNSem);
    }
    delete[] fRNotify;
    fRNotify=NULL;
    fNRNotify=0;
    fMTRNotify=NULL;
  }
  fIFirstChain=NULL;
  fILastChain=NULL;
  fINChains=0;
}

void QProcList::Browse(TBrowser *b)
{
  Int_t i;
  for(i=0; i<fQPL->Count(); i++) b->Add((*fQPL)[i],"TTree Processors");
}

void QProcList::BuildObjLists()
{
  Int_t i,j;

  if(!fAIObjects) {
    fAIObjects=new QList<QProcObj*>;
    fAOObjects=new QList<QProcObj*>;

    for(i=0; i<fQPL->Count(); ++i) {
      ((QProcessor*)(*fQPL)[i])->BuildObjLists();

      for(j=0; j<((QProcessor*)(*fQPL)[i])->GetIObjList().Count(); ++j) fAIObjects->AddUnique(((QProcessor*)(*fQPL)[i])->GetIObjList()[j]);
      for(j=0; j<((QProcessor*)(*fQPL)[i])->GetOObjList().Count(); ++j) fAOObjects->AddUnique(((QProcessor*)(*fQPL)[i])->GetOObjList()[j]);
    }
  }
}

void QProcList::ClearObjLists()
{
  Int_t i;
  if(fAIObjects) {
    delete fAIObjects; fAIObjects=NULL;
    delete fAOObjects; fAOObjects=NULL;
  }

  for(i=0; i<fQPL->Count(); i++) ((QProcessor*)(*fQPL)[i])->ClearObjLists();
}

void* QPLThread(void *args){
  QProcList &plist=*((QProcList*)args);
  QProcList::SChainConfig *chain;
  Int_t i;

  for(;;) {
    sem_wait(&plist.fTWSem);
    if(plist.fStopThreads) return NULL;
    pthread_mutex_lock(&plist.fChMutex);
    //printf("Chain %p is ready for thread\n",plist.fFirstChain);
    chain=plist.fFirstChain;
    if(!chain) {
      sem_getvalue(&plist.fTWSem,&i);
      fprintf(stderr,"Error: No chain found\n");
      fprintf(stderr,"Semaphore value is %i\n",i);
      throw 1;
    }
    //printf("FirstChain %p -> %p\n",plist.fFirstChain,plist.fFirstChain->Next);
    plist.fFirstChain=plist.fFirstChain->Next;
    if(!plist.fFirstChain) plist.fLastChain=NULL;
    pthread_mutex_unlock(&plist.fChMutex);

    for(i=chain->NProcs-1; i>=0; --i) {
      //printf("Start exec proc %i (%p) of chain %p\n",i,chain->Procs[i],chain);
      chain->Procs[i]->Exec();
      //printf("Done exec proc %i (%p) of chain %p\n",i,chain->Procs[i],chain);
    }

    for(i=chain->NDChains-1; i>=0; --i) {
      if(chain->DChains[i]->NDOChains==1) {
	pthread_mutex_lock(&plist.fChMutex);
	//printf("Adding chain %p to queue\n",chain->DChains[i]);

	if(plist.fLastChain) {
	  plist.fLastChain->Next=chain->DChains[i];
	  plist.fLastChain=chain->DChains[i];

	} else {
	  plist.fFirstChain=plist.fLastChain=chain->DChains[i];
	}
	pthread_mutex_unlock(&plist.fChMutex);
	sem_post(&plist.fTWSem);

      } else {
	pthread_mutex_lock(&chain->DChains[i]->DOCMutex);
	//printf("Split-up chain %p dep: %i -> %i\n",chain->DChains[i],chain->DChains[i]->NRDOChains,chain->DChains[i]->NRDOChains-1);
	--(chain->DChains[i]->NRDOChains);

	if(!chain->DChains[i]->NRDOChains) {
	  chain->DChains[i]->NRDOChains=chain->DChains[i]->NDOChains;
	  pthread_mutex_unlock(&chain->DChains[i]->DOCMutex);

	  pthread_mutex_lock(&plist.fChMutex);
	  //printf("Adding split-up chain %p to queue\n",chain->DChains[i]);

	  if(plist.fLastChain) {
	    plist.fLastChain->Next=chain->DChains[i];
	    plist.fLastChain=chain->DChains[i];

	  } else {
	    plist.fFirstChain=plist.fLastChain=chain->DChains[i];
	  }
	  pthread_mutex_unlock(&plist.fChMutex);
	  sem_post(&plist.fTWSem);

	} else pthread_mutex_unlock(&chain->DChains[i]->DOCMutex);
      }
    }

    //Get the chain ready for its next execution
    chain->Next=chain->NextInit;

    if(chain->RNotify) {
      pthread_mutex_lock(&chain->RNotify->RNCMutex);
      //printf("Notification %p: %i -> %i\n",chain->RNotify,chain->RNotify->RCountDown,chain->RNotify->RCountDown-1);
      --(chain->RNotify->RCountDown);

      if(!chain->RNotify->RCountDown) {
	chain->RNotify->RCountDown=chain->RNotify->Init;
        pthread_mutex_unlock(&chain->RNotify->RNCMutex);
	//printf("Sending signal for notification %p\n",chain->RNotify);
	sem_post(&chain->RNotify->RNSem);
      } else pthread_mutex_unlock(&chain->RNotify->RNCMutex);
    }
  }
  return NULL;
}
