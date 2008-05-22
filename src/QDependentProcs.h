#ifndef _QDEPENDENTPROCS_
#define _QDEPENDENTPROCS_

#include "QList.h"

class QDependentProcs
{
  public:
    QDependentProcs():fIdx(fQPDObjs.Count()),fDepends(){fQPDObjs.Add(this);}
    virtual ~QDependentProcs(){fQPDObjs.Del(fIdx);}
    QList<Int_t> GetAllDepends() const;
    void AddDepend(Int_t pidx){if(fDepends.FindFirst(pidx) == -1 && pidx != fIdx) fDepends.Add(pidx);}
    void DelDepend(Int_t pidx){Int_t idx=fDepends.FindFirst(pidx); if(idx!=-1) fDepends.Del(idx);}
    Int_t GetNDepends(){return fDepends.Count();}
  private:
    QDependentProcs(const QDependentProcs&){};
    const QDependentProcs& operator=(const QDependentProcs&){return *this;}

    Int_t fIdx;
    QList<Int_t> fDepends;
    static Int_t fInitIdx;
    static QList<void*> fQPDObjs;
    static QList<Int_t> fPCalled;

    ClassDef(QDependentProcs,1) //List direct and indirent dependencies for QNamedProc objects. NEVER USE MORE THAN ONE SET OF INSTANCES!
};

#endif
