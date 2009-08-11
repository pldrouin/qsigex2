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
#include <algorithm>
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

  QList(const U* const newelements, Int_t nelements):TObject(),fNElements(0),fUArray(NULL),fChild(NULL),fIsChild(kFALSE){PRINTF6(this,"\tQList<",typeid(U).name(),">::QList(const U* newelements, Int_t nelements<",nelements,">)\n") Add(newelements, nelements); }

  virtual ~QList(){PRINTF4(this,"\tQList<",typeid(U).name(),">::~QList()\n") Clear();}

  void Add(const QList<U>& newqlist, const Int_t &index=-1);
  void Add(const U& newelement, const Int_t &index=-1);
  void Add(const U* const newelements, const Int_t &nelements, const Int_t &index=-1);
  Int_t AddUnique(const QList<U>& newqlist, const Int_t &index=-1);
  Int_t AddUnique(const U& newelement, const Int_t &index=-1);
  Int_t AddUnique(const U* const newelements, const Int_t &nelements, const Int_t &index=-1);

#ifndef __CINT__
  Int_t BinarySearch(const U &u, const Int_t &fromindex=0) const;
#endif

  void Browse(TBrowser *b);

  QList<U>* Clone(const char* newname = "") const{PRINTF4(this,"\tQList<U>* QList<",typeid(U).name(),">::Clone()\n") newname=0; return new QList<U>(*this); }
  U* CloneArray() const;

  const Int_t& Count() const{PRINTF4(this,"\tInt_t& QList<",typeid(U).name(),">::Count()\n") return fNElements;}

  Int_t Del(const QList<U>& delqlist, const Int_t &maxmatches=1);
  //  Int_t Del(const U& delu, Int_t maxmatches=1);
  Int_t Del(const U* const delus, const Int_t &nelements=1, const Int_t &maxmatches=1);
  void Del(const Int_t &index=-1, const Int_t &nelements=1);
  
  QList<Int_t> Find(const QList<U>& qlist, const Int_t &maxmatches=0, const Int_t &fromindex=0) const;
  QList<Int_t> Find(const U& u, const Int_t &maxmatches=0, const Int_t &fromindex=0) const;
  QList<Int_t> Find(const U* const us, const Int_t &nelements, const Int_t &maxmatches=0, const Int_t &fromindex=0) const;
  Int_t FindFirst(const QList<U>& qlist, const Int_t &fromindex=0) const;
  Int_t FindFirst(const U& u, const Int_t &fromindex=0) const;
  Int_t FindFirst(const U* const us, const Int_t &nelements, const Int_t &fromindex=0) const;

#ifdef __CINT__
  U* GetArray() const{return fUArray;}
#else
  U* const & GetArray() const{return fUArray;}
#endif
  U& GetLast() const;

  Bool_t IsFolder() const {return kTRUE;}

  const QList<U>& operator=(const QList<U>& newqlist){PRINTF4(this,"\tconst QList<U>& QList<",typeid(U).name(),">::operator=(const QList<U>& newqlist)\n") Set(newqlist); return *this;}

  const QList<U>& operator=(const U& newelement){PRINTF4(this,"\tconst QList<U>& QList<",typeid(U).name(),">::operator=(const U& newelement)\n") Set(newelement); return *this;}

  const QList<U>& operator+=(const QList<U>& newqlist){PRINTF4(this,"\tconst QList<U>& QList<",typeid(U).name(),">::operator+=(const QList<U>& newqlist)\n") Add(newqlist); return *this;}

  const QList<U>& operator+=(const U& newu){PRINTF4(this,"\tconst QList<U>& QList<",typeid(U).name(),">::operator+=(const U& newu)\n") Add(newu); return *this;}

  const QList<U>& operator++(){RedimList(fNElements+1); return *this;}

  const QList<U> operator++(int){QList<U> ret=*this; RedimList(fNElements+1); return ret;}

  const QList<U>& operator-=(const QList<U>& delqlist){PRINTF4(this,"\tconst QList<U>& QList<",typeid(U).name(),">::operator-=(const QList<U>& delqlist)\n") Del(delqlist); return *this;}

  const QList<U>& operator-=(const U& delu){PRINTF4(this,"\tconst QList<U>& QList<",typeid(U).name(),">::operator-=(const U& delu)\n") Del(&delu); return *this;}

  const QList<U>& operator--(){RedimList(fNElements-1); return *this;}

  const QList<U> operator--(int){QList<U> ret=*this; RedimList(fNElements-1); return ret;}

  U& operator[](const Int_t &index) const;
  const QList<U>& operator()(const Int_t &index1, const Int_t &index2) const;

  Int_t RedimList(const Int_t &newdim, Int_t index=-1);
  Int_t RedimList(const Int_t &newdim, const Int_t &index, const U& fillvalue);

  void Set(const QList<U>& newqlist);
  void Set(const U& newelement);
  void Set(const U* const newelements, const Int_t &nelements);

#ifndef __CINT__
  void Sort(){std::sort(fUArray, fUArray+fNElements);}
  void Sort(Bool_t (*sfunc)(const U&, const U&)){std::sort(fUArray, fUArray+fNElements, sfunc);}
#endif

  template<typename V> friend Bool_t operator==(const QList<V>& lhs,const QList<V>& rhs);
  template<typename V> friend Bool_t operator!=(const QList<V>& lhs,const QList<V>& rhs);
  template<typename V> friend Bool_t operator<=(const QList<V>& lhs,const QList<V>& rhs);
  template<typename V> friend Bool_t operator>=(const QList<V>& lhs,const QList<V>& rhs);
  template<typename V> friend Bool_t operator<(const QList<V>& lhs,const QList<V>& rhs);
  template<typename V> friend Bool_t operator>(const QList<V>& lhs,const QList<V>& rhs);

  void Clear(Option_t* option = "")
    {
      PRINTF6(this,"\tvoid QList<",typeid(U).name(),">::Clear(Option_t* option<",option,">)\n")
      option=0;
      if(fChild){
	delete fChild;
	fChild=NULL;
      }

      if(!fIsChild){
	for(Int_t i=fNElements-1; i>=0; --i) {fUArray[i].~U();}
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






