// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QList                                                                //
//                                                                      //
// Template list class that can contain any data type and any class     //
// instance. However, when the type is a class, the implicit conversion //
// constructor U::U(const U&) and the operator=(const U&) must be       //
// overloaded properly, i.e. member variables must be copied by value   //
// and not by address, since QList class considers that it owns its     //
// memory. operator== must be defined properly too. If these conditions //
// are not respected, a list of instance pointers can be created, but   //
// in this case, member functions like Del, Find and friend functions   //
// like operator==,operator!=,operator<= and operator< have not the     //
// same meaning, and the copy functions (operator=, Clone and           //
// QList(const QList<U>&)) don't create new independant instances.      //
//                                                                      //
// PLEASE REFER TO FILE QList_cxx.h FOR DESCRIPTION OF MEMBER FUNCTIONS //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef _QLIST_
#define _QLIST_

#include <cstdlib>
#include <iostream>
#include <new>
#include <typeinfo>
#include "TObject.h"
#include "TObjectTable.h"
#include "Rtypes.h"
#include "TBrowser.h"
#include "QList_LinkDef_inc.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

using std::cout;

template <typename U> class QList: public TObject
{
 public:
  QList():TObject(),fNElements(0),fUArray(NULL),fChild(NULL),fIsChild(kFALSE){PRINTF4(this,"\tQList<",typeid(U).name(),">::QList()\n") }

  QList(const QList<U>& newqlist);

  QList(const U& newelement):TObject(),fNElements(0),fUArray(NULL),fChild(NULL),fIsChild(kFALSE){PRINTF4(this,"\tQList<",typeid(U).name(),">::QList(const U& newelement)\n") Add(newelement); }

  QList(const U* newelements, Int_t nelements):TObject(),fNElements(0),fUArray(NULL),fChild(NULL),fIsChild(kFALSE){PRINTF6(this,"\tQList<",typeid(U).name(),">::QList(const U* newelements, Int_t nelements<",nelements,">)\n") Add(newelements, nelements); }

  virtual ~QList(){PRINTF4(this,"\tQList<",typeid(U).name(),">::~QList()\n") Clear();}

  void Add(const QList<U>& newqlist, Int_t index=-1);
  void Add(const U& newelement,Int_t index=-1);
  void Add(const U* newelements, Int_t nelements, Int_t index=-1);

  void Set(const QList<U>& newqlist);
  void Set(const U& newelement);
  void Set(const U* newelements, Int_t nelements);

  Int_t Del(const QList<U>& delqlist, Int_t maxmatches=1);
  //  Int_t Del(const U& delu, Int_t maxmatches=1);
  Int_t Del(const U* delus, Int_t nelements=1, Int_t maxmatches=1);
  void Del(Int_t index=-1);

  void Browse(TBrowser *b);
  Bool_t IsFolder() const {return kTRUE;}

  const QList<U>& operator=(const QList<U>& newqlist){PRINTF4(this,"\tconst QList<U>& QList<",typeid(U).name(),">::operator=(const QList<U>& newqlist)\n") Set(newqlist); return *this;}

  const QList<U>& operator=(const U& newelement){PRINTF4(this,"\tconst QList<U>& QList<",typeid(U).name(),">::operator=(const U& newelement)\n") Set(newelement); return *this;}

  const QList<U>& operator+=(const QList<U>& newqlist){PRINTF4(this,"\tconst QList<U>& QList<",typeid(U).name(),">::operator+=(const QList<U>& newqlist)\n") Add(newqlist); return *this;}

  const QList<U>& operator+=(const U& newu){PRINTF4(this,"\tconst QList<U>& QList<",typeid(U).name(),">::operator+=(const U& newu)\n") Add(newu); return *this;}

  const QList<U>& operator-=(const QList<U>& delqlist){PRINTF4(this,"\tconst QList<U>& QList<",typeid(U).name(),">::operator-=(const QList<U>& delqlist)\n") Del(delqlist); return *this;}

  const QList<U>& operator-=(const U& delu){PRINTF4(this,"\tconst QList<U>& QList<",typeid(U).name(),">::operator-=(const U& delu)\n") Del(&delu); return *this;}

  Int_t Count() const{PRINTF4(this,"\tInt_t& QList<",typeid(U).name(),">::Count()\n") return fNElements;}

  U* GetArray(){return fUArray;}
  
  QList<Int_t> Find(const QList<U>& qlist,Int_t maxmatches=0) const;
  QList<Int_t> Find(const U& u,Int_t maxmatches=0) const;
  QList<Int_t> Find(const U* us, Int_t nelements, Int_t maxmatches=0) const;

  template<typename V> friend Bool_t operator==(const QList<V>& lhs,const QList<V>& rhs);
  template<typename V> friend Bool_t operator!=(const QList<V>& lhs,const QList<V>& rhs);
  template<typename V> friend Bool_t operator<=(const QList<V>& lhs,const QList<V>& rhs);
  template<typename V> friend Bool_t operator>=(const QList<V>& lhs,const QList<V>& rhs);
  template<typename V> friend Bool_t operator<(const QList<V>& lhs,const QList<V>& rhs);
  template<typename V> friend Bool_t operator>(const QList<V>& lhs,const QList<V>& rhs);

  U& operator[](Int_t index) const;
  const QList<U>& operator()(Int_t index1,Int_t index2,Int_t step=1) const;
  QList<U>* Clone(const char* newname = "") const{PRINTF4(this,"\tQList<U>* QList<",typeid(U).name(),">::Clone()\n") newname=0; return new QList<U>(*this); }
  U* CloneArray() const;

  Int_t RedimList(Int_t newdim,Int_t index=-1);
  Int_t RedimList(Int_t newdim,Int_t index,const U& fillvalue);

  void Clear(Option_t* option = "")
    {
      PRINTF6(this,"\tvoid QList<",typeid(U).name(),">::Clear(Option_t* option<",option,">)\n")
      option=0;
      if(fChild){
	delete fChild;
	fChild=NULL;
      }

      if(!fIsChild){
	for(Int_t i=0;i<fNElements;i++) fUArray[i].~U();
	free(fUArray);
      }

      fUArray=NULL;
      fNElements=0;
    }
  
 private:
  Int_t fNElements;
  U* fUArray;                //[fNElements]
  mutable QList<U>* fChild;  //! Do not write this member variable into a ROOT file
  Bool_t fIsChild;           //! Do not write this member variable into a ROOT file

  ClassDef(QList,1) //Generic list template class
};

#include "debugger.h"

#include "QList_cxx.h"

#endif






