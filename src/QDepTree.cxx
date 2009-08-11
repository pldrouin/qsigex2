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

    if((n=fQDTObjs[i]->GetNDownDepends())>1) {

      for(j=n-1; j>=0; --j) {

	idxlist=fQDTObjs[i]->DownDepend(j).GetAllUpDepends();

	for(k=n-1; k>=0; --k) {
	  if(k==j) continue;
	  id1=fQDTObjs[i]->DownDepend(k).GetIndex();

	  if(idxlist.FindFirst(id1)==-1) {
	    fQDTObjs[i]->fSplit=kSplitDown;

	  } else {
	    fQDTObjs[i]->fDownDepends.Del(k);

	    if(j>k) --j;
	    --k;
	    --n;
	  }
	}
      }
    }

    if((n=fQDTObjs[i]->GetNUpDepends())>1) {

      for(j=n-1; j>=0; --j) {
	idxlist=fQDTObjs[i]->UpDepend(j).GetAllUpDepends();

	for(k=n-1; k>=0; --k) {
	  if(k==j) continue;
	  id1=fQDTObjs[i]->UpDepend(k).GetIndex();

	  if(idxlist.FindFirst(id1)==-1) {
	    fQDTObjs[i]->fSplit|=kSplitUp;

	  } else {
	    fQDTObjs[i]->fUpDepends.Del(k);

	    if(j>k) --j;
	    --k;
	    --n;
	  }
	}
      }
    }
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
    QDepTree *obj, *newobj;
    fNCalled.RedimList(fQDTObjs.Count()-1);

    for(i=0; i<fQDTObjs.Count(); ++i) if(fQDTObjs[i]->fEnabled) {obj=fQDTObjs[i]; break;}

    j=0;
    if(i<fQDTObjs.Count()) for(i=obj->fIdx+1; i<fQDTObjs.Count(); ++i) if(fQDTObjs[i]->fEnabled) fNCalled[j++]=fQDTObjs[i];
    fNCalled.RedimList(j);
    ++(*chains);
    chains->GetLast()+=obj->GetIndex();

    if(fNCalled.Count()) {

      for(;;) {

	while(obj->GetNDownDepends()==1 && (newobj=obj->fDownDepends[0])->fSplit==kNoSplit) {
	  obj=newobj;
	  chains->GetLast()+=obj->GetIndex();
	  fNCalled.Del(&obj,1);
	}

	if(fNCalled.Count()) {

	  if(obj->GetNDownDepends() && (newobj=obj->fDownDepends[0])->fSplit&kSplitDown) {
	    //If kSplitDown AND kSplitUp
	    if(newobj->fSplit!=kSplitDown) ++(*chains);

	    chains->GetLast()+=newobj->GetIndex();
	    fNCalled.Del(&newobj,1);
	  }
	  obj=fNCalled[0];
	  ++(*chains);
	  chains->GetLast()+=obj->GetIndex();
	  fNCalled.Del(0);

	} else break;
      }
    }
    chainsdepons->RedimList(chains->Count());
    chainsdeps->RedimList(chains->Count());

    //Loop over chains
    for(i=0; i<chains->Count(); ++i) {

      //Loop over dependency nodes of the current chain
      for(j=0; j<fQDTObjs[(*chains)[i][0]]->GetNUpDepends(); ++j)

	//Among the previous chains, find the one which last node is the current dependency node of the current chain and add the current chain to the list of dependency chains. Add also the identified chain to the list of chains on which the current chain depends on.
	for(k=0; k<i ; ++k) if((*chains)[k].GetLast()==fQDTObjs[(*chains)[i][0]]->UpDepend(j).GetIndex()) {
	  (*chainsdeps)[k]+=i;
	  (*chainsdepons)[i]+=k;
	  break;
	}
      (*chainsdepons)[i].Sort();
    }
  }
}

Int_t QDepTree::MeasDownPathLengths(){
  Int_t i,j,k;
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
