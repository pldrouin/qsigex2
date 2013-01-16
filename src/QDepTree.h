// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _QDEPENDENTPROCS_
#define _QDEPENDENTPROCS_

#include "QList.h"

class QDepTree
{
  public:
    QDepTree():fIdx(fQDTObjs.Count()), fEnabled(kTRUE), fSplit(kNoSplit), fUpDepends(), fDownDepends(), fLPathLength(-1) {fQDTObjs.Add(this);}
    virtual ~QDepTree(){fQDTObjs.Del(fIdx);}
    void AddDepend(Int_t pidx){if(pidx==fIdx) return; if(pidx>fIdx) fDownDepends.AddUnique(fQDTObjs[pidx]); else fUpDepends.AddUnique(fQDTObjs[pidx]);}
    static void Simplify();
    void DelDepend(Int_t pidx){fDownDepends.Del(&fQDTObjs[pidx],1); fUpDepends.Del(&fQDTObjs[pidx],1);}
    QDepTree& DownDepend(const Int_t &idx) const{return *((QDepTree*)fDownDepends[idx]);}
    QList<Int_t> GetAllDepends() const;
    QList<Int_t> GetAllDownDepends() const;
    QList<Int_t> GetAllUpDepends() const;
    static void GetChains(QList<QList<Int_t> > *chains, QList<QList<Int_t> > *chainsdepons, QList<QList<Int_t> > *chainsdeps);
    const Int_t& GetIndex(){return fIdx;}
    Int_t GetNDepends() const{return fDownDepends.Count()+fUpDepends.Count();}
    const Int_t& GetNDownDepends() const{return fDownDepends.Count();}
    const Int_t& GetNUpDepends() const{return fUpDepends.Count();}
    const Int_t& GetLPathLength() const{return fLPathLength;}
    static Int_t MeasDownPathLengths();
    void SetState(const Bool_t &enabled=kTRUE){fEnabled=enabled;}

    QDepTree& UpDepend(const Int_t &idx) const{return *((QDepTree*)fUpDepends[idx]);}

    enum eSplitTypes {kNoSplit=0, kSplitUp=1, kSplitDown=2};

  private:
    QDepTree(const QDepTree&){};
    const QDepTree& operator=(const QDepTree&){return *this;}

    Int_t fIdx;
    Bool_t fEnabled;
    Int_t fSplit;
    QList<QDepTree*> fUpDepends;
    QList<QDepTree*> fDownDepends;
    Int_t fLPathLength; //Longest path length
    static const QDepTree* fInitObj;
    static QList<QDepTree*> fQDTObjs;
    static QList<QDepTree*> fNCalled;

    ClassDef(QDepTree,1) //Build a dependency tree. It assumes that a serial iteration over the existing instances (following the order they were created) takes care of the dependencies properly. NEVER USE MORE THAN ONE SET OF INSTANCES!
};

#endif
