#include "QProcList.h"

//#define DEBUG

ClassImp(QProcList)

const QProcList *gQProcList;
Bool_t QProcList::fDefPProcessor=kFALSE;

QProcList::~QProcList()
{
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

  for(lIBuf=fIMutexes->Count()-1; lIBuf>=0; --lIBuf) {
    //printf("Main unlocking initial mutex %p\n",(*fIMutexes)[lIBuf]);
    pthread_mutex_unlock((pthread_mutex_t*)(*fIMutexes)[lIBuf]);
    //printf("Main has unlocked initial mutex %p\n",(*fIMutexes)[lIBuf]);
  }

  for(lIBuf=fThreads->NChains-1; lIBuf>=0; --lIBuf) {
    //printf("Main thread start chain index %i\n",lIBuf);

    for(lIBuf2=fThreads->Chains[lIBuf].NDOMutexes-1; lIBuf2>=0; --lIBuf2) {
      //printf("Main locking DO mutex %p\n",fThreads->Chains[lIBuf].DOMutexes[lIBuf2]);
      pthread_mutex_lock((pthread_mutex_t*)fThreads->Chains[lIBuf].DOMutexes[lIBuf2]);
      //printf("Main has locked DO mutex %p\n",fThreads->Chains[lIBuf].DOMutexes[lIBuf2]);
    }
    for(lIBuf2=fThreads->Chains[lIBuf].NProcs-1; lIBuf2>=0; --lIBuf2) {
      //printf("Main thread start exec proc %i (%p) of chain index %i\n",lIBuf2,fThreads->Chains[lIBuf].Procs[lIBuf2],lIBuf);
      fThreads->Chains[lIBuf].Procs[lIBuf2]->Exec();
      //printf("Main thread done exec proc %i (%p) of chain index %i\n",lIBuf2,fThreads->Chains[lIBuf].Procs[lIBuf2],lIBuf);
    }
    for(lIBuf2=fThreads->Chains[lIBuf].NDMutexes-1; lIBuf2>=0; --lIBuf2) {
      //printf("Main unlocking D mutex %p\n",fThreads->Chains[lIBuf].DMutexes[lIBuf2]);
      pthread_mutex_unlock((pthread_mutex_t*)fThreads->Chains[lIBuf].DMutexes[lIBuf2]);
      //printf("Main has unlocked D mutex %p\n",fThreads->Chains[lIBuf].DMutexes[lIBuf2]);
    }
    //printf("Main thread done chain index %i\n",lIBuf);
  }

  for(lIBuf=fFMutexes->Count()-1; lIBuf>=0; --lIBuf) {
    //printf("Main locking final mutex %p\n",(*fFMutexes)[lIBuf]);
    pthread_mutex_lock((pthread_mutex_t*)(*fFMutexes)[lIBuf]);
    //printf("Main thread has locked final mutex %p\n",(*fFMutexes)[lIBuf]);
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

  Int_t i,j,k,l;
  QList<QList<Int_t> > chains;
  QList<QList<Int_t> > chainsdepons;
  QList<QList<Int_t> > chainsdeps;
  QList<QList<Int_t> > thrchains;
  QList<Int_t> chainthr;
  QList<Int_t> chainidx;

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
	  if(dynamic_cast<TObject*>(proc->GetOObjList()[j])) fprintf(stderr,"QProcList: InitProcess(): Error with process '%s': Object '%s' (%p) cannot be overwritten\n",proc->GetName(),dynamic_cast<TObject*>(proc->GetOObjList()[j])->GetName(),proc->GetOObjList()[j]);
	  else fprintf(stderr,"QProcList: InitProcess(): Error with process '%s': Object %p cannot be overwritten\n",proc->GetName(),proc->GetOObjList()[j]);
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

    depprocs=new QDepTree[chains.Count()];

    for(i=chains.Count()-1; i>=0; --i) {

      for(j=chainsdepons[i].Count()-1; j>=0; --j) {
	depprocs[i].AddDepend(chainsdepons[i][j]);
	depprocs[chainsdepons[i][j]].AddDepend(i);
      }

      for(j=chainsdeps[i].Count()-1; j>=0; --j) {
	depprocs[i].AddDepend(chainsdeps[i][j]);
	depprocs[chainsdeps[i][j]].AddDepend(i);
      }
    }
    chainthr.RedimList(chains.Count());
    chainidx.RedimList(chains.Count());

    while((i=QDepTree::MeasDownPathLengths())!=-1) {
      //printf("Length of chain %i for thread %i: %i\n",depprocs[i].GetIndex(),thrchains.Count(),depprocs[i].GetLPathLength());
      ++thrchains;

      for(;;) {
	chainthr[i]=thrchains.Count()-1;
	thrchains.GetLast()+=i;
	chainidx[i]=thrchains.GetLast().Count()-1;
	depprocs[i].SetState(kFALSE);

	for(j=depprocs[i].GetNUpDepends()-1; j>=0; --j) {
	  //printf("Chain length for chain %i: %i\n",depprocs[i].UpDepend(j).GetIndex(),depprocs[i].UpDepend(j).GetLPathLength());
	  if(depprocs[i].UpDepend(j).GetLPathLength()==depprocs[i].GetLPathLength()-1) {
	    i=depprocs[i].UpDepend(j).GetIndex();
	    break;
	  }
	}

	if(j==-1) break;
      }
    }

    delete[] depprocs;

    //Remove dependencies between chains located in a same thread
    for(i=0; i<thrchains.Count(); ++i) {

      for(j=thrchains[i].Count()-1; j>0; --j) {

	for(k=j-1; k>=0; --k) {

	  if((l=chainsdeps[thrchains[i][j]].BinarySearch(thrchains[i][k]))!=-1) {
	    chainsdeps[thrchains[i][j]].Del(l);
	    chainsdepons[thrchains[i][k]].Del(chainsdepons[thrchains[i][j-1]].BinarySearch(thrchains[i][j]));
	  }
	}
      }
    }

  } else {
    chains++;
    chainsdepons++;
    chainsdeps++;
    chains.GetLast().RedimList(fQPL->Count());
    for(j=fQPL->Count()-1; j>=0; --j) chains.GetLast()[j]=j;

    ++thrchains;
    thrchains[0]+=0;
    chainthr+=0;
    chainidx+=0;
  }

  fNThreads=thrchains.Count();
  fThreads=new SThreadConfig[fNThreads];
  fMutexes=new QList<void*>;
  fIMutexes=new QList<void*>;
  fFMutexes=new QList<void*>;

  for(i=0; i<fNThreads; ++i) {
    fThreads[i].NChains=thrchains[i].Count();
    fThreads[i].Chains=new SChainConfig[fThreads[i].NChains];
  }

  SChainConfig *chain;

  for(i=0; i<chains.Count(); i++) {
    chain=fThreads[chainthr[i]].Chains+chainidx[i];

    chain->NProcs=chains[i].Count();
    chain->Procs=new QProcessor*[chain->NProcs];

    //Store QProcessor pointers for the current thread in REVERSE ORDER
    for(j=chain->NProcs-1; j>=0; --j) chain->Procs[j]=((QProcessor*)(*fQPL)[chains[i][chain->NProcs-1-j]]);

    if(fNThreads>1) {
      if(chainidx[i]!=thrchains[chainthr[i]].Count()-1 || chainsdepons[i].Count()) {
	//Remove 1 entry for chains that are not listed first in their thread (since all chains within a given thread have inter-dependencies by construction)
	chain->NDOMutexes=chainsdepons[i].Count();
	chain->DOMutexes=new pthread_mutex_t*[chain->NDOMutexes];

	for(j=chainsdepons[i].Count()-1; j>=0; --j) {
	  k=chainsdepons[i][j];

	  (*fMutexes)++;
	  fMutexes->GetLast()=new pthread_mutex_t;
	  pthread_mutex_init((pthread_mutex_t*)fMutexes->GetLast(),NULL);
	  chain->DOMutexes[j]=(pthread_mutex_t*)fMutexes->GetLast();
	  //Get the index of the chain for the link currently handled by the current chain
	  fThreads[chainthr[k]].Chains[chainidx[k]].DMutexes[chainsdeps[k].BinarySearch(i)]=(pthread_mutex_t*)fMutexes->GetLast();
	}

	//Else if it is the first chain in the thread and it is not the first (main) thread or if using fast exec mode
      } else if(fPPFastExec || chainthr[i]) {
	chain->NDOMutexes=1;
	chain->DOMutexes=new pthread_mutex_t*[1];
	(*fMutexes)++;
	fMutexes->GetLast()=new pthread_mutex_t;
	pthread_mutex_init((pthread_mutex_t*)fMutexes->GetLast(),NULL);
	chain->DOMutexes[0]=(pthread_mutex_t*)fMutexes->GetLast();
	fIMutexes->Add(fMutexes->GetLast(),0);

      } else {
	chain->NDOMutexes=0;
	chain->DOMutexes=NULL;
      }

      if(chainidx[i]!=0 || chainsdeps[i].Count()) {
	//Remove 1 entry for chains that are not listed last in their thread (since all chains within a given thread have inter-dependencies by construction)
	chain->NDMutexes=chainsdeps[i].Count();
	chain->DMutexes=new pthread_mutex_t*[chain->NDMutexes];

      } else if(fPPFastExec || chainthr[i]) {
	chain->NDMutexes=1;
	chain->DMutexes=new pthread_mutex_t*[1];
	(*fMutexes)++;
	fMutexes->GetLast()=new pthread_mutex_t;
	pthread_mutex_init((pthread_mutex_t*)fMutexes->GetLast(),NULL);
	chain->DMutexes[0]=(pthread_mutex_t*)fMutexes->GetLast();
	fFMutexes->Add(fMutexes->GetLast(),0);

      } else {
	chain->NDMutexes=0;
	chain->DMutexes=NULL;
      }

    } else {
      chain->NDOMutexes=0;
      chain->DOMutexes=NULL;
      chain->NDMutexes=0;
      chain->DMutexes=NULL;
    }
  }

  if(GetVerbosity()&QProcessor::kShowExec) {
    printf("QProcList('%s')::InitProcObj(): %i threads have been defined\n",GetName(),thrchains.Count());

    for(i=0; i<thrchains.Count(); ++i) {
      printf("\tThread %3i (%p):\n",i,&fThreads[i].Thread);

      for(j=thrchains[i].Count()-1; j>=0; --j) {
	printf("\t\tChain %3i:\n",thrchains[i][j]);

	if(chainsdepons[thrchains[i][j]].Count()) {
	  printf("\t\t\tWait for chain:");
	  for(k=0; k<chainsdepons[thrchains[i][j]].Count(); ++k) printf(" %i(%p)",chainsdepons[thrchains[i][j]][k],fThreads[i].Chains[j].DOMutexes[k]);
	  printf("\n");

	} else if(fThreads[i].Chains[j].NDOMutexes==1) {
	  printf("\t\t\tLock initial mutex %p\n",fThreads[i].Chains[j].DOMutexes[0]);
	  
	} else {
	  printf("\t\t\tDo not wait for any chain\n");
	}

	printf("\t\t\tProcessors:\n");
	for(k=0; k<chains[thrchains[i][j]].Count(); ++k) printf("\t\t\tProcessor '%s' (%i)\n",((QProcessor*)(*fQPL)[chains[thrchains[i][j]][k]])->GetName(),chains[thrchains[i][j]][k]);
	printf("\n");

	if(chainsdeps[thrchains[i][j]].Count()) {
	  printf("\t\t\tSend a signal to chains:");
	  for(k=0; k<chainsdeps[thrchains[i][j]].Count(); ++k) printf(" %i(%p)",chainsdeps[thrchains[i][j]][k],fThreads[i].Chains[j].DMutexes[k]);
	  printf("\n");

	} else if (fThreads[i].Chains[j].NDMutexes==1) {
	  printf("\t\t\tUnlock final mutex %p\n",fThreads[i].Chains[j].DMutexes[0]);

	} else {
	  printf("\t\t\tDo not have to send a signal to any chain\n");
	}
      }
    }
  }

  for(i=fMutexes->Count()-1; i>=0; --i) {
    pthread_mutex_lock((pthread_mutex_t*)(*fMutexes)[i]);
  }
  fStopThreads=kFALSE;

  for(i=(!fPProcessor || !fPPFastExec); i<fNThreads; ++i) {
    fThreads[i].Stop=&fStopThreads;
    pthread_create(&fThreads[i].Thread, NULL, QPLThread, &fThreads[i]);
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
    Int_t j;
    fStopThreads=kTRUE;

    for(i=fMutexes->Count()-1; i>=0; --i) pthread_mutex_unlock((pthread_mutex_t*)(*fMutexes)[i]);

    for(i=fNThreads-1; i>=0; --i) {
      if(i) pthread_join(fThreads[i].Thread,NULL);

      for(j=fThreads[i].NChains-1; j>=0; --j) {

	if(i || j) delete[] fThreads[i].Chains[j].DMutexes;
	delete[] fThreads[i].Chains[j].Procs;
	delete[] fThreads[i].Chains[j].DOMutexes;
      }
      delete[] fThreads[i].Chains;
    }
    delete[] fThreads;
    fNThreads=NULL;
  }

  if(fMutexes) {

    for(i=fMutexes->Count()-1; i>=0; --i) {
      pthread_mutex_destroy((pthread_mutex_t*)(*fMutexes)[i]);
      delete (*fMutexes)[i];
    }
    delete fMutexes;
    fMutexes=NULL;
  }

  if(fIMutexes) {
    delete fIMutexes;
    fIMutexes=NULL;
  }

  if(fFMutexes) {
    delete fFMutexes;
    fFMutexes=NULL;
  }
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
  const QProcList::SThreadConfig &config=*((const QProcList::SThreadConfig*)args);
  Int_t i,j;

  while(1) {
    for(i=config.NChains-1; i>=0; --i) {
      //printf("%p,Start chain index %i\n",&config.Thread,i);

      for(j=config.Chains[i].NDOMutexes-1; j>=0; --j) {
	//printf("%p,chain idx %i locking DO mutex %p with index %i\n",&config.Thread,i,config.Chains[i].DOMutexes[j],j);
	pthread_mutex_lock(config.Chains[i].DOMutexes[j]);
	//printf("%p, chain idx %i has locked DO mutex %p with index %i\n",&config.Thread,i,config.Chains[i].DOMutexes[j],j);
      }

      if(!*config.Stop) for(j=config.Chains[i].NProcs-1; j>=0; --j) {
	//printf("%p,Start exec proc %i (%p) of chain index %i\n",&config.Thread,j,config.Chains[i].Procs[j],i);
	config.Chains[i].Procs[j]->Exec();
	//printf("%p,Done exec proc %i (%p) of chain index %i\n",&config.Thread,j,config.Chains[i].Procs[j],i);
      }
      else return NULL;

      for(j=config.Chains[i].NDMutexes-1; j>=0; --j) {
	//printf("%p, chain idx %i unlocking D mutex %p with index %i\n",&config.Thread,i,config.Chains[i].DMutexes[j],j);
	pthread_mutex_unlock(config.Chains[i].DMutexes[j]);
	//printf("%p, chain idx %i has unlocked D mutex %p with index %i\n",&config.Thread,i,config.Chains[i].DMutexes[j],j);
      }
      //printf("%p,Done chain index %i\n",&config.Thread,i);
    }
  }
  return NULL;
}
