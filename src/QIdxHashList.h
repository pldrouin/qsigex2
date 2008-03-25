#ifndef _QIDXHASHLIST_
#define _QIDXHASHLIST_

#include "THashList.h"
#include "QList.h"

//#define DEBUF
//#define DEBUG2


#include "debugger.h"

class QIdxHashList: public THashList
{
  public:
    QIdxHashList(Int_t capacity = TCollection::kInitHashTableCapacity, Int_t rehash=0):THashList(capacity,rehash),fIdx2Obj(),fObj2Idx(){}
    QIdxHashList(TObject* parent, Int_t capacity = TCollection::kInitHashTableCapacity, Int_t rehash = 0):THashList(parent,capacity),fIdx2Obj(),fObj2Idx(){}
    virtual ~QIdxHashList(){Clear();}
    void       Clear(Option_t *option=""){THashList::Clear(option); fIdx2Obj.Clear(); fObj2Idx.Clear();}
    void       Delete(Option_t *option=""){THashList::Delete(option); fIdx2Obj.Clear(); fObj2Idx.Clear();}

    TObject   *FindObject(const char *name) const{return THashList::FindObject(name);}
    TObject   *FindObject(const TObject *obj) const{return THashList::FindObject(obj);}
    TObject   *FindObject(Int_t qidx) const;

    void AddFirst(TObject *obj){AddFirst(obj,-1);}
    void AddFirst(TObject *obj, Int_t qidx);
    void AddFirst(TObject *obj, Option_t *opt){AddFirst(obj,opt,-1);}
    void AddFirst(TObject *obj, Option_t *opt, Int_t qidx);
    void AddLast(TObject *obj){AddLast(obj,-1);}
    void AddLast(TObject *obj, Int_t qidx);
    void AddLast(TObject *obj, Option_t *opt){AddLast(obj,opt,-1);}
    void AddLast(TObject *obj, Option_t *opt, Int_t qidx);
    void AddAt(TObject *obj, Int_t idx){AddAt(obj,idx,-1);}
    void AddAt(TObject *obj, Int_t idx, Int_t qidx);
    void AddAfter(const TObject *after, TObject *obj){AddAfter(after,obj,-1);}
    void AddAfter(const TObject *after, TObject *obj, Int_t qidx);
    void AddAfter(TObjLink *after, TObject *obj){AddAfter(after,obj,-1);}
    void AddAfter(TObjLink *after, TObject *obj, Int_t qidx);
    void AddBefore(const TObject *before, TObject *obj){AddBefore(before,obj,-1);}
    void AddBefore(const TObject *before, TObject *obj, Int_t qidx);
    void AddBefore(TObjLink *before, TObject *obj){AddBefore(before,obj,-1);}
    void AddBefore(TObjLink *before, TObject *obj, Int_t qidx);
    void RecursiveRemove(TObject *obj);
    TObject   *Remove(TObject *obj);
    TObject   *Remove(TObjLink *lnk);

  protected:
    void SetQIdx(TObject *obj, Int_t qidx);
    QList<TObject*> fIdx2Obj;
    QList<Int_t> fObj2Idx;

  private:
    QIdxHashList(const QIdxHashList&):THashList(){}
    const QIdxHashList& operator=(const QIdxHashList&){return *this;}

    ClassDef(QIdxHashList,1) 
};

#include "debugger.h"

#endif
