// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>

#ifndef _TTREEUTILS_
#define _TTREEUTILS_

#include "debugger.h"

#include "Rtypes.h"
#include "TTree.h"
#include "TObjArray.h"
#include "TBranch.h"

class QTTreeUtils{
  public:
    QTTreeUtils(){}
    virtual ~QTTreeUtils(){}

    template <typename U> static U* AssignBAddress(const Char_t* bname, TTree* tree, U* dummy)
    {
      //This template function creates a new buffer for branch which name is
      //bname if the current branch address is NULL. It returns the branch
      //buffer address. The template type is the branch buffer type.

      U* ret;

      if(!tree->GetBranch(bname)->GetAddress()){
	ret=new U;
	tree->SetBranchAddress(bname,ret);
      }else{
	ret=(U*)tree->GetBranch(bname)->GetAddress();
      }

      return ret;
    }

    static void ClearBranchesAddresses(TTree* tree);

    ClassDef(QTTreeUtils,1) //Contains static member functions that handle TTree branches buffers
};

#include "debugger.h"

#endif
