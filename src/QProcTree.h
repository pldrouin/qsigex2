#ifndef _QPROCTREE_
#define _QPROCTREE_

#include "TTree.h"
#include "QProcBranch.h"

class QProcTree: public TTree
{
  public:
    QProcTree(): TTree(){}
    QProcTree(const char* name, const char* title, Int_t splitlevel = 99): TTree(name,title,splitlevel){}
    virtual ~QProcTree(){}
    virtual Int_t Branch(TList* list, Int_t bufsize = 32000, Int_t splitlevel = 99){return TTree::Branch(list,bufsize,splitlevel);}
#if !defined(__CINT__)
    virtual TBranch *Branch(const char* name, const char* classname, void* addobj, Int_t bufsize = 32000, Int_t splitlevel = 99){return TTree::Branch(name,classname,addobj,bufsize,splitlevel);}
#endif
    virtual Int_t Branch(const char* folder, Int_t bufsize = 32000, Int_t splitlevel = 99){return TTree::Branch(folder,bufsize,splitlevel);}
    virtual Int_t Branch(TCollection* list, Int_t bufsize = 32000, Int_t splitlevel = 99, const char* name = ""){return TTree::Branch(list,bufsize,splitlevel,name);}
    virtual TBranch* Branch(const char* name, void* address, const char* leaflist, Int_t bufsize = 32000);
  protected:
  private:
    QProcTree(const QProcTree&): TTree(){}
    ClassDef(QProcTree,1)
};

#endif
