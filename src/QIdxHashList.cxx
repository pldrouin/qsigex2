#include "QIdxHashList.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

ClassImp(QIdxHashList)

TObject *QIdxHashList::FindObject(Int_t idx) const
{
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
  THashList::AddLast(obj);
  fObj2Idx.Add(qidx);
  SetQIdx(obj,qidx);
}

void QIdxHashList::AddLast(TObject *obj, Option_t *opt, Int_t qidx)
{
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
  fObj2Idx.Add(qidx,IndexOf(obj));
  SetQIdx(obj,qidx);
}

void QIdxHashList::AddAfter(TObjLink *after, TObject *obj, Int_t qidx)
{
  THashList::AddAfter(after,obj);
  fObj2Idx.Add(qidx,IndexOf(obj));
  SetQIdx(obj,qidx);
}

void QIdxHashList::AddBefore(const TObject *before, TObject *obj, Int_t qidx)
{
  THashList::AddBefore(before,obj);
  fObj2Idx.Add(qidx,IndexOf(obj));
  SetQIdx(obj,qidx);
}

void QIdxHashList::AddBefore(TObjLink *before, TObject *obj, Int_t qidx)
{
  THashList::AddBefore(before,obj);
  fObj2Idx.Add(qidx,IndexOf(obj));
  SetQIdx(obj,qidx);
}

void QIdxHashList::RecursiveRemove(TObject *obj)
{
  THashList::RecursiveRemove(obj);
}

TObject *QIdxHashList::Remove(TObject *obj)
{
}

TObject *QIdxHashList::Remove(TObjLink *lnk)
{
}

void QIdxHashList::SetQIdx(TObject *obj, Int_t qidx){
  if(qidx!=-1){
    if(qidx>=fIdx2Obj.Count()) fIdx2Obj.RedimList(qidx+1,-1,NULL);
    fIdx2Obj[qidx]=obj;
  }
}


#include "debugger.h"
