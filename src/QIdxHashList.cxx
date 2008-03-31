#include "QIdxHashList.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QIdxHashList)

TObject *QIdxHashList::FindObject(Int_t idx) const
{
  if(idx>=fIdx2Obj.Count() || idx<0) return NULL;
  return fIdx2Obj[idx];
}

void QIdxHashList::AddFirst(TObject *obj, Int_t qidx)
{
  THashList::AddFirst(obj);
  fObj2Idx.Add(qidx,0);
  SetQIdx(obj,qidx);
}

void QIdxHashList::AddFirst(TObject *obj, Option_t *opt, Int_t qidx)
{
  THashList::AddFirst(obj,opt);
  fObj2Idx.Add(qidx,0);
  SetQIdx(obj,qidx);
}

void QIdxHashList::AddLast(TObject *obj, Int_t qidx)
{
  PRINTF6(this,"\tQIdxHashList::AddLast(TObject *obj<",obj,">, Int_t qidx<",qidx,">)\n")
  THashList::AddLast(obj);
  fObj2Idx.Add(qidx);
  SetQIdx(obj,qidx);
}

void QIdxHashList::AddLast(TObject *obj, Option_t *opt, Int_t qidx)
{
  PRINTF8(this,"\tQIdxHashList::AddLast(TObject *obj<",obj,">, Option_t *opt<",opt,">, Int_t qidx<",qidx,">)\n")
  THashList::AddLast(obj,opt);
  fObj2Idx.Add(qidx);
  SetQIdx(obj,qidx);
}

void QIdxHashList::AddAt(TObject *obj, Int_t idx, Int_t qidx)
{
  THashList::AddAt(obj,idx);
  fObj2Idx.Add(qidx,idx);
  SetQIdx(obj,qidx);
}

void QIdxHashList::AddAfter(const TObject *after, TObject *obj, Int_t qidx)
{
  THashList::AddAfter(after,obj);
  Int_t index=IndexOf(obj);

  if(index!=-1){
    fObj2Idx.Add(qidx,index);
    SetQIdx(obj,qidx);
  }
}

void QIdxHashList::AddAfter(TObjLink *after, TObject *obj, Int_t qidx)
{
  THashList::AddAfter(after,obj);
  Int_t index=IndexOf(obj);

  if(index!=-1) {
    fObj2Idx.Add(qidx,index);
    SetQIdx(obj,qidx);
  }
}

void QIdxHashList::AddBefore(const TObject *before, TObject *obj, Int_t qidx)
{
  THashList::AddBefore(before,obj);
  Int_t index=IndexOf(obj);

  if(index!=-1) {
    fObj2Idx.Add(qidx,index);
    SetQIdx(obj,qidx);
  }
}

void QIdxHashList::AddBefore(TObjLink *before, TObject *obj, Int_t qidx)
{
  THashList::AddBefore(before,obj);
  Int_t index=IndexOf(obj);

  if(index!=-1){
    fObj2Idx.Add(qidx,index);
    SetQIdx(obj,qidx);
  }
}

void QIdxHashList::RecursiveRemove(TObject *obj)
{
  PRINTF4(this,"\tQIdxHashList::RecursiveRemove(TObject *obj<",obj,">)\n")
  Int_t index=IndexOf(obj);

  if(index!=-1){
    Int_t qidx=fObj2Idx[index];
    if(qidx!=-1) fIdx2Obj[qidx]=NULL;
    fObj2Idx.Del(index);
  }
  THashList::RecursiveRemove(obj);
}

TObject *QIdxHashList::Remove(TObject *obj)
{
  PRINTF4(this,"\tQIdxHashList::Remove(TObject *obj<",obj,">)\n")
  Int_t index;
  TObjLink *lnk=FindLink(obj, index);

  if(lnk){
    Int_t qidx=fObj2Idx[index];
    if(qidx!=-1) fIdx2Obj[qidx]=NULL;
    fObj2Idx.Del(index);
    return THashList::Remove(lnk);

  } else return NULL;
}

TObject *QIdxHashList::Remove(TObjLink *lnk, Int_t index)
{
  PRINTF6(this,"\tQIdxHashList::Remove(TObjLink *lnk<",lnk,">, Int_t index<",index,">)\n")
  if (!lnk) return 0;

  TObject *obj = lnk->GetObject();

  if(index<0 || index>=GetSize()) index=IndexOf(obj);

  if(index!=-1) {
    Int_t qidx=fObj2Idx[index];
    if(qidx!=-1) fIdx2Obj[qidx]=NULL;
    fObj2Idx.Del(index);
  }

  TList::Remove(lnk);
  return fTable->Remove(obj);
}

TObject *QIdxHashList::Remove(Int_t qidx)
{
  PRINTF4(this,"\tQIdxHashList::Remove(Int_t qidx<",qidx,">)\n")

  if(qidx>=fIdx2Obj.Count() || qidx<0) return NULL;
  TObject *obj=fIdx2Obj[qidx];
  Int_t index;
  fIdx2Obj[qidx]=NULL;
  TObjLink *lnk=FindLink(obj, index);
  if(lnk) fObj2Idx.Del(index);
  return THashList::Remove(lnk);
}

void QIdxHashList::SetObjQIdx(TObject *obj, Int_t qidx)
{
  PRINTF6(this,"\tQIdxHashList::SetObjQIdx(TObject *obj<",obj,">, Int_t qidx<",qidx,">)\n")
  Int_t index=IndexOf(obj);
  if(index==-1) return;
  fObj2Idx[index]=qidx;
  SetQIdx(obj,qidx);
}

void QIdxHashList::SetObjQIdx(Int_t idx, Int_t qidx)
{
  PRINTF6(this,"\tQIdxHashList::SetObjQIdx(Int_t idx<",idx,">, Int_t qidx<",qidx,">)\n")
  if(idx>=fObj2Idx.Count() || idx<0){
    fprintf(stderr,"%p\tQIdxHashList::SetObjQIdx(Int)t idx<%i>, Int_t qidx<%i>): idx value is invalid\n",this,idx,qidx);
    return;
  }
  fObj2Idx[idx]=qidx;
  SetQIdx(At(idx),qidx);
}

void QIdxHashList::SetQIdx(TObject *obj, Int_t qidx){
  PRINTF6(this,"\tQIdxHashList::SetQIdx(TObject *obj<",obj,">, Int_t qidx<",qidx,">)\n")
  if(qidx>-1){
    if(qidx>=fIdx2Obj.Count()) fIdx2Obj.RedimList(qidx+1,-1,NULL);
    fIdx2Obj[qidx]=obj;
  }
}


#include "debugger.h"
