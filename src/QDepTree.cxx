#include "QDepTree.h"

ClassImp(QDepTree);

const QDepTree* QDepTree::fInitObj=NULL;
QList<QDepTree*> QDepTree::fQDTObjs;
QList<QDepTree*> QDepTree::fNCalled;

void QDepTree::Simplify()
{
  Int_t i,j,k;
  Int_t n;
  Int_t id1;
  QList<Int_t> idxlist;

  for(i=fQDTObjs.Count()-1; i>=0; --i) {
    fQDTObjs[i]->fSplit=kNoSplit;
    //printf("Node %i (Down %i, Up %i): \n",i,fQDTObjs[i]->GetNDownDepends(),fQDTObjs[i]->GetNUpDepends());

    if((n=fQDTObjs[i]->GetNDownDepends())>1) {
      //printf("Down depends on %i nodes\n",n);

      for(j=n-1; j>=0; --j) {
	//printf("Down dependency %i: %i\n",j,fQDTObjs[i]->DownDepend(j).GetIndex());

	idxlist=fQDTObjs[i]->DownDepend(j).GetAllDownDepends();

	for(k=n-1; k>=0; --k) {
	  if(k==j) continue;
	  //printf("\tDown dependency' %i: %i",k,fQDTObjs[i]->DownDepend(k).GetIndex());
	  id1=fQDTObjs[i]->DownDepend(k).GetIndex();

	  if(idxlist.FindFirst(id1)==-1) {
	    fQDTObjs[i]->fSplit=kSplitDown;
	    //printf(" -> causes splitdown\n");

	  } else {
	    fQDTObjs[i]->fDownDepends.Del(k);
	    //printf(" -> irrelevant\n");

	    if(j>k) --j;
	    --n;
	  }
	}
      }

      if(fQDTObjs[i]->GetNDownDepends()<=1) {
	//printf("Node %i does not down split after all\n",i);
	fQDTObjs[i]->fSplit=0;
      }

    } //else printf("does not down depend on any node\n");

    if((n=fQDTObjs[i]->GetNUpDepends())>1) {
      //printf("Up depends on %i nodes\n",n);

      for(j=n-1; j>=0; --j) {
	//printf("Up dependency %i: %i\n",j,fQDTObjs[i]->UpDepend(j).GetIndex());
	idxlist=fQDTObjs[i]->UpDepend(j).GetAllUpDepends();

	for(k=n-1; k>=0; --k) {
	  if(k==j) continue;
	  //printf("\tUp dependency' %i: %i",k,fQDTObjs[i]->UpDepend(k).GetIndex());
	  id1=fQDTObjs[i]->UpDepend(k).GetIndex();

	  if(idxlist.FindFirst(id1)==-1) {
	    fQDTObjs[i]->fSplit|=kSplitUp;
	    //printf(" -> causes splitup\n");

	  } else {
	    fQDTObjs[i]->fUpDepends.Del(k);
	    //printf(" -> irrelevant\n");

	    if(j>k) --j;
	    --n;
	  }
	}
      }

      if(fQDTObjs[i]->GetNUpDepends()<=1) {
	//printf("Node %i does not up split after all\n",i);
	fQDTObjs[i]->fSplit&=~kSplitUp;
      }
    } //else printf("Does not up depend on any node\n");
  }
}

QList<Int_t> QDepTree::GetAllDepends() const
{
  QList<Int_t> ret;

  if(!fInitObj) {
    fNCalled.Clear();
    fInitObj=this;

  } else {
    ret.Add(fIdx);
  }

  for(Int_t i=0; i<GetNDownDepends(); i++) {
    if(fDownDepends[i] != fInitObj && fDownDepends[i]->fEnabled && fNCalled.FindFirst(fDownDepends[i]) == -1) {
      fNCalled.Add(fDownDepends[i]);
      ret.Add(fDownDepends[i]->GetAllDepends());
    }
  }

  for(Int_t i=0; i<GetNUpDepends(); i++) {
    if(fUpDepends[i] != fInitObj && fUpDepends[i]->fEnabled && fNCalled.FindFirst(fUpDepends[i]) == -1) {
      fNCalled.Add(fUpDepends[i]);
      ret.Add(fUpDepends[i]->GetAllDepends());
    }
  }

  if(fInitObj==this) {
    fNCalled.Clear();
    fInitObj=NULL;
  }

  return ret;
}

QList<Int_t> QDepTree::GetAllDownDepends() const
{
  QList<Int_t> ret;

  if(!fInitObj) {
    fNCalled.Clear();
    fInitObj=this;

  } else {
    ret.Add(fIdx);
  }

  for(Int_t i=0; i<GetNDownDepends(); i++) {
    if(fDownDepends[i] != fInitObj && fDownDepends[i]->fEnabled && fNCalled.FindFirst(fDownDepends[i]) == -1) {
      fNCalled.Add(fDownDepends[i]);
      ret.Add(fDownDepends[i]->GetAllDownDepends());
    }
  }

  if(fInitObj==this) {
    fNCalled.Clear();
    fInitObj=NULL;
  }

  return ret;
}

QList<Int_t> QDepTree::GetAllUpDepends() const
{
  QList<Int_t> ret;

  if(!fInitObj) {
    fNCalled.Clear();
    fInitObj=this;

  } else {
    ret.Add(fIdx);
  }

  for(Int_t i=0; i<GetNUpDepends(); i++) {
    if(fUpDepends[i] != fInitObj && fUpDepends[i]->fEnabled && fNCalled.FindFirst(fUpDepends[i]) == -1) {
      fNCalled.Add(fUpDepends[i]);
      ret.Add(fUpDepends[i]->GetAllUpDepends());
    }
  }

  if(fInitObj==this) {
    fNCalled.Clear();
    fInitObj=NULL;
  }

  return ret;
}

void QDepTree::GetChains(QList<QList< Int_t> > *chains, QList<QList<Int_t> > *chainsdepons, QList<QList<Int_t> > *chainsdeps)
{
  if(fQDTObjs.Count()) {
    Int_t i,j,k;
    QDepTree *obj=NULL, *newobj;
    fNCalled.RedimList(fQDTObjs.Count()-1);

    for(i=0; i<fQDTObjs.Count(); ++i) if(fQDTObjs[i]->fEnabled) {obj=fQDTObjs[i]; break;}

    j=0;
    if(i<fQDTObjs.Count()) for(i=obj->fIdx+1; i<fQDTObjs.Count(); ++i) if(fQDTObjs[i]->fEnabled) fNCalled[j++]=fQDTObjs[i];
    fNCalled.RedimList(j);
    chains->Clear();
    chainsdepons->Clear();
    chainsdeps->Clear();

    if(fNCalled.Count()) {
      ++(*chains);
      //printf("Starting the first chain with process %i\n",obj->GetIndex());
      chains->GetLast()+=obj->GetIndex();

      for(;;) {

	while(obj->GetNDownDepends()==1 && (newobj=obj->fDownDepends[0])->fSplit==kNoSplit) {
	  //printf("Process %i has only one down dependency (%i) which does not split up or down\n",obj->GetIndex(),newobj->GetIndex());
          //printf("Adding dependency %i to chain %i\n",newobj->GetIndex(),chains->Count()-1);
	  obj=newobj;
	  chains->GetLast()+=obj->GetIndex();
	  fNCalled.Del(&obj,1);
	}

	if(fNCalled.Count()) {
	  //printf("%i processes are still not taken care of\n",fNCalled.Count());

	  if(obj->GetNDownDepends()==1 && fNCalled.FindFirst((newobj=obj->fDownDepends[0]))>=0 && newobj->fSplit&kSplitDown) {
	    //printf("Process %i has only 1 down dependency (%i) and this dependency at least splits down\n",obj->GetIndex(),newobj->GetIndex());
	    //printf("The dependency splits down to "); for(i=0; i<newobj->GetNDownDepends(); ++i) printf(" %i",newobj->fDownDepends[i]->GetIndex()); printf("\n");
	    //If kSplitDown AND kSplitUp
	    if(newobj->fSplit!=kSplitDown) {
	      //printf("Dependency %i also splits up\n",newobj->GetIndex());
	      //printf("The dependency splits up to "); for(i=0; i<newobj->GetNUpDepends(); ++i) printf(" %i",newobj->fUpDepends[i]->GetIndex()); printf("\n");
	      ++(*chains);
              //printf("Starting chain %i with dependency %i\n",chains->Count()-1,newobj->GetIndex());
	    //} else {
	    //  printf("Dependency %i does not split up\n",newobj->GetIndex());
            //  printf("Adding dependency %i to chain %i\n",newobj->GetIndex(),chains->Count()-1);
	    }

	    chains->GetLast()+=newobj->GetIndex();
	    fNCalled.Del(&newobj,1);
	  //} else {
	  //  if(obj->GetNDownDepends()!=1) printf("Process %i has %i dependencies\n",obj->GetIndex(),obj->GetNDownDepends());
	  //  if(obj->GetNDownDepends()==1) {
	  //    if((obj->fDownDepends[0])->fSplit&kSplitDown) printf("Process %i's first dependency at least splits down\n",obj->GetIndex());
	  //    else printf("Process %i's first dependency does not split down\n",obj->GetIndex());
	  //  }
	  }
	  obj=fNCalled[0];
	  ++(*chains);
          //printf("Starting chain %i with process %i\n",chains->Count()-1,obj->GetIndex());
	  chains->GetLast()+=obj->GetIndex();
	  fNCalled.Del(0);

	} else break;
      }
    }
    chainsdepons->RedimList(chains->Count());
    chainsdeps->RedimList(chains->Count());

    //Loop over chains
    for(i=0; i<chains->Count(); ++i) {
      //printf("Looking at chain %i\n",i);
      //printf("First process of chain %i up depends on %i other chains\n",i,fQDTObjs[(*chains)[i][0]]->GetNUpDepends());

      //Loop over dependency nodes of the current chain
      for(j=0; j<fQDTObjs[(*chains)[i][0]]->GetNUpDepends(); ++j)

	//Among the previous chains, find the one which last node is the current dependency node of the current chain and add the current chain to the list of dependency chains. Add also the identified chain to the list of chains on which the current chain depends on.
	for(k=0; k<chains->Count() ; ++k) {

	  if(k==i) continue;
	  //printf("\tUp dependency %i is process %i and chain %i's last process is %i\n",j,fQDTObjs[(*chains)[i][0]]->UpDepend(j).GetIndex(),k,(*chains)[k].GetLast());

	  if((*chains)[k].GetLast()==fQDTObjs[(*chains)[i][0]]->UpDepend(j).GetIndex()) {
	    //printf("\tUp dependency %i is chain %i\n",j,k);
	    (*chainsdeps)[k]+=i;
	    (*chainsdepons)[i]+=k;
	    break;
	  }
	}
      (*chainsdepons)[i].Sort();
    }
  }
}

Int_t QDepTree::MeasDownPathLengths(){
  Int_t i,j;
  QDepTree *obj, *obj2;
  Int_t max=0;
  QDepTree *maxobj;
  fNCalled.Clear();

  for(i=fQDTObjs.Count()-1; i>=0; --i) {

    if(fQDTObjs[i]->fEnabled) {

      for(j=fQDTObjs[i]->fUpDepends.Count()-1; j>=0; --j)
	if(fQDTObjs[i]->fUpDepends[j]->fEnabled) break;

      if(j==-1) {
	fNCalled+=fQDTObjs[i];
	fQDTObjs[i]->fLPathLength=0;

      } else fQDTObjs[i]->fLPathLength=-2;

    } else fQDTObjs[i]->fLPathLength=-2;
  }

  if(!fNCalled.Count()) return -1;

  maxobj=fNCalled[0];

  while(fNCalled.Count()) {

    for(i=fNCalled.Count()-1; i>=0; --i) {
      obj=fNCalled[i];
      fNCalled.Del(i);

      for(j=obj->fDownDepends.Count()-1; j>=0; --j) {
	obj2=obj->fDownDepends[j];

	if(obj2->fEnabled) {

	  if(obj->fLPathLength+1>obj2->fLPathLength) {
	    obj2->fLPathLength=obj->fLPathLength+1;

	    if(obj2->fLPathLength>max) {
	      max=obj2->fLPathLength;
	      maxobj=obj2;
	    }
	    fNCalled.AddUnique(obj2);
	  }
	}
      }
    }
  }
  return maxobj->GetIndex();
}
