// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

//#define DEBUG
//#define DEBUG2
#include "debugger.h"

//The following line is needed to generate the dependency file
#include "QList.h"

using std::cout;

////////////////////////////////////////////////////////////////////////
//                                                                    //
// QList                                                              //
//                                                                    //
// Template list class that can contain any data type and any         //
// class instance. However, when the type is a class, the implicit    //
// conversion constructor U::U(const U&) and the operator=(const U&)  //
// must be overloaded properly, i.e. member variables must be copied  //
// by value and not by address, since QList class considers that      //
// it owns its memory. operator== must be defined properly too. If    //
// these conditions are not respected, a list of instance pointers    //
// can be created, but in this case, member functions like Del,       //
// Find and friend functions like operator==,operator!=,operator<=    //
// and operator< have not the same meaning, and the copy functions    //
// (operator=, Clone and QList(const QList<U>&)) don't create new     //
// independant instances.                                             //
//                                                                    //
////////////////////////////////////////////////////////////////////////


template <typename U> QList<U>::QList(const QList<U>& newqlist):TObject(),fNElements(0),fUArray(NULL),fChild(NULL),fIsChild(kFALSE)
{
  PRINTF4(this,"\tQList<",typeid(U).name(),">::QList(const QList<U>& newqlist)\n")

  Set(newqlist);

}

template <typename U> void QList<U>::Add(const QList<U>& newqlist, const Int_t &index)
{
  // This function adds elements to the list, given another QList instance of the same
  // type. Elements are copied using *this[i]=newqlist[i]. Consequently, if U is a data
  // type or a class instance (U must conform to listed requirements), the expanded list
  // (*this) is totally independant of the other QList (newqlist). However, if U is an
  // instance pointer, both QList (*this and newqlist) share the same instances. The
  // second parameter is optional. If it is not used, the new elements are added at the
  // end of the existing list. If a index>=0 is given, the new elements are added to this
  // position, the first new element being located at position=index.

  PRINTF6(this,"\tvoid QList<",typeid(U).name(),">::Add(const QList<U>& newqlist, Int_t index<",index,">)\n")

  Add(newqlist.fUArray,newqlist.fNElements,index);  
}

template <typename U> inline void QList<U>::Add(const U& newelement, const Int_t &index)
{
  // This function adds 1 element to the list, given a U element of the same U type.
  // The element is copied using *this[i]=newelement. Consequently, if U is a data
  // type or a class instance (U must conform to listed requirements), the expanded list
  // (*this) is totally independant of the element newelement. However, if U is an instance
  // pointer, both *this and newelement share the same instance. The second parameter is
  // optional. If it is not used, the new element is added at the end of the existing list.
  // If a index>=0 is given, the new element is added to this position, the new element being
  // located at position=index.

  PRINTF6(this,"\tvoid QList<",typeid(U).name(),">::Add(const U& newelement,Int_t index<",index,">)\n")

  Int_t i=RedimList(fNElements+1,index);
  fUArray[i]=newelement;
}

template <typename U> void QList<U>::Add(const U* newelements, const Int_t &nelements, const Int_t &index)
{
  // This function adds elements to the list, given a U array of the same U
  // type. Elements are copied using *this[i]=newelements[i]. Consequently, if U is a data
  // type or a class instance (U must conform to listed requirements), the expanded list
  // (*this) is totally independant of the U array (newelements). However, if U is an
  // instance pointer, both QList *this and U array newelements share the same instances.
  // The second parameter is optional. If it is not used, the new elements are added at
  // the end of the existing list. If a index>=0 is given, the new elements are added to
  // this position, the first new element being located at position=index.

  PRINTF8(this,"\tvoid QList<",typeid(U).name(),">::Add(const U* newelements, Int_t nelements<",nelements,">, Int_t index<",index,">)\n")

  Int_t cindex=RedimList(fNElements+nelements,index);
  for(Int_t i=nelements-1; i>=0; --i){
    fUArray[i+cindex]=newelements[i];
  }
}

template <typename U> Int_t QList<U>::AddUnique(const QList<U>& newqlist, const Int_t &index)
{
  // This functions has the same behaviour than the Add function with the same arguments,
  // except that elements are added only if they are not found using the corresponding 
  // FindFirst function. The function returns the value returned by FindFirst.

  PRINTF6(this,"\tInt_t QList<",typeid(U).name(),">::AddUnique(const QList<U>& newqlist, Int_t index<",index,">)\n")

  Int_t idx=FindFirst(newqlist);
  if(idx == -1) Add(newqlist,index);  
  return idx;
}

template <typename U> Int_t QList<U>::AddUnique(const U& newelement, const Int_t &index)
{
  // This functions has the same behaviour than the Add function with the same arguments,
  // except that elements are added only if they are not found using the corresponding 
  // FindFirst function. The function returns the value returned by FindFirst.

  PRINTF6(this,"\tInt_t QList<",typeid(U).name(),">::AddUnique(const U& newelement,Int_t index<",index,">)\n")

  Int_t idx=FindFirst(newelement);
  if(idx == -1) Add(newelement,index);
  return idx;
}

template <typename U> Int_t QList<U>::AddUnique(const U* newelements, const Int_t &nelements, const Int_t &index)
{
  // This functions has the same behaviour than the Add function with the same arguments,
  // except that elements are added only if they are not found using the corresponding 
  // FindFirst function. The function returns the value returned by FindFirst.

  PRINTF8(this,"\tInt_t QList<",typeid(U).name(),">::AddUnique(const U* newelements, Int_t nelements<",nelements,">, Int_t index<",index,">)\n")

  Int_t idx=FindFirst(newelements,nelements);
  if(idx == -1) Add(newelements,nelements,index);
  return idx;
}

template <typename U> void QList<U>::Set(const QList<U>& newqlist)
{
  // This function sets the list to have the same values than another QList instance of
  // the same type. Elements are copied using *this[i]=newqlist[i]. Consequently, if U
  // is a data type or a class instance (U must conform to listed requirements), the
  // new list (*this) is totally independant of the other QList (newqlist). However, if
  // U is an instance pointer, both QList (*this and newqlist) share the same instances.

  // N.B. This function could be replaced by another using the appropriate Add function,
  // but it would manage the memory in a less effective way, since the Clear()
  // function would be called

  PRINTF4(this,"\tvoid QList<",typeid(U).name(),">::Set(const QList<U>& newqlist)\n")

  Set(newqlist.fUArray,newqlist.fNElements);  
}

template <typename U> void QList<U>::Set(const U& newelement)
{
  // This function sets the list to hold only 1 element having the same value than newelement.
  // The element is copied using *this[i]=newelement. Consequently, if U is a data
  // type or a class instance (U must conform to listed requirements), the new list of
  // (*this) is totally independant of the element newelement. However, if U is an instance
  // pointer, both *this and newelement share the same instance.

  // N.B. This function could be replaced by another using the appropriate Add function,
  // but it would manage the memory in a less effective way, since the Clear()
  // function would be called

  PRINTF4(this,"\tvoid QList<",typeid(U).name(),">::Set(const U& newelement)\n")

  RedimList(1);
  fUArray[0]=newelement;
}

template <typename U> void QList<U>::Set(const U* newelements, const Int_t &nelements)
{
  // This function sets the list to hold the same values than the ones that are held by a
  // U array of the same U type. Elements are copied using *this[i]=newelements[i].
  // Consequently, if U is a data type or a class instance (U must conform to listed
  // requirements), the expanded list (*this) is totally independant of the U array
  // (newelements). However, if U is an instance pointer, both QList *this and U array
  // newelements share the same instances. The second parameter is optional.

  // N.B. This function could be replaced by another using the appropriate Add function,
  // but it would manage the memory in a less effective way, since the Clear()
  // function would be called

  PRINTF6(this,"\tvoid QList<",typeid(U).name(),">::Set(const U* newelements, Int_t nelements<",nelements,">)\n")

  RedimList(nelements);
  for(Int_t i=nelements-1; i>=0; --i){
    fUArray[i]=newelements[i];
  }
}
  
template <typename U> Int_t QList<U>::Del(const QList<U>& delqlist, const Int_t &maxmatches)
{
  // This function looks in *this for a list part of length delqlist.Count() that has
  // the sames U values than delqlist and deletes it. It repeats this process until
  // no list part be found, or until the number of deleted list parts be equal to
  // maxmatches. If maxmatches<=0, all matching list parts are deleted. After having
  // deleted a part of its list, *this continues to look for a matching list part at
  // the following position, in a way that a match cannot be found with two concatenated
  // fragments. The list is resized properly using RedimList member function.
  // The function returns the number of deleted list parts.

  PRINTF6(this,"\tInt_t QList<",typeid(U).name(),">::Del(const QList<U>& delqlist,Int_t maxmatches<",maxmatches,">)\n")

  return Del(delqlist.fUArray,delqlist.fNElements,maxmatches);
}

/*template <typename U> Int_t QList<U>::Del(const U& delu,Int_t maxmatches)
{
  // This function looks in *this for an element that has the same U value than delu
  // and deletes it. It repeats this process until no matching element is found,
  // or until the number of elements  be equal to maxmatches. If maxmatches<=0, all
  // matching elements are deleted. The list is resized properly using RedimList member
  // function. The function returns the number of deleted elements.

  PRINTF6(this,"\tInt_t QList<",typeid(U).name(),">::Del(const U& delu,Int_t maxmatches<",maxmatches,">)\n")

  return Del(&delu,1,maxmatches);
}*/

template <typename U> Int_t QList<U>::Del(const U* delus, const Int_t &nelements, const Int_t &maxmatches)
{
  // This function looks in *this for a list part of length nelements that has
  // the sames U values than delus and deletes it. It repeats this process until
  // no list part be found, or until the number of deleted list parts be equal to
  // maxmatches. If maxmatches<=0, all matching list parts are deleted. After having
  // deleted a part of its list, *this continues to look for a matching list part at
  // the following position, in a way that a match cannot be found with two concatenated
  // fragments. The list is resized properly using RedimList member function. The function
  // returns the number of deleted list parts .

  PRINTF8(this,"\tInt_t QList<",typeid(U).name(),">::Del(const U* delus, Int_t nelements<",nelements,">,Int_t maxmatches<",maxmatches,">)\n")

  Int_t counter=0;
  Int_t pos=0;
  Int_t matches=0;

  while(pos<=fNElements-nelements && pos+counter<fNElements){
    if(fUArray[pos+counter]==delus[counter]){
      ++counter;
      if(counter==nelements){
	RedimList(fNElements-nelements,pos);
	++matches;
	counter=0;
	if(matches==maxmatches){
	  break;
	}
      }
    } else {
      counter=0;
      ++pos;
    }
  }
  return matches;
}

template <typename U> void QList<U>::Del(const Int_t &index, const Int_t &nelements)
{
  // This function deletes nelements from the array starting at index index. If index
  // is not provided, the last element is deleted. The list is resized properly using
  // RedimList member function.

  PRINTF6(this,"\tvoid QList<",typeid(U).name(),">::Del(Int_t index<",index,">)\n")

  RedimList(fNElements-nelements,index);
}

template <typename U> void QList<U>::Browse(TBrowser *b)
{
  if (b && fUArray) {
    TObject *obj;
    char *strbuf=NULL;

    for(Int_t i=fNElements-1; i>=0; --i) {
      obj=dynamic_cast<TObject*>((TObject*)((void*)(fUArray+i)));
      strbuf=(char*)realloc(strbuf,8+strlen(obj->GetName()));
      sprintf(strbuf,"[%03i]: %s",i,obj->GetName());
      if(obj) b->Add(obj,strbuf);
    }
    free(strbuf);
  }
}

template <typename U> U* QList<U>::CloneArray() const
{
  // This function returns a copy of *this U array when *this instance is assigned
  // to a U*, or casted to U* .

  PRINTF4(this,"\tQList<",typeid(U).name(),">::CloneArray()\n")

  U* newuarray=new U[fNElements];
  for(Int_t i=fNElements-1; i>=0; --i){
    newuarray[i]=fUArray[i];
  }
  return newuarray;
}

template <typename U> QList<Int_t> QList<U>::Find(const QList<U>& qlist, const Int_t &maxmatches) const
{
  // This function browses the QList<U> U list in a similar manner to the Del member function with
  // the same arguments. However, instead of deleting list parts and returning the number of
  // deletions, it returns a QList<Int_t> containing the matching indexes.

  PRINTF6(this,"\tInt_t QList<",typeid(U).name(),">::Find(const QList<U>& qlist,Int_t maxmatches<",maxmatches,">) const\n")

  return Find(qlist.fUArray,qlist.fNElements,maxmatches);
}

template <typename U> QList<Int_t> QList<U>::Find(const U& u, const Int_t &maxmatches) const
{
  // This function browses the QList<U> U list in a similar manner to the Del member function with
  // the same arguments. However, instead of deleting list parts and returning the number of
  // deletions, it returns a QList<Int_t> containing the matching indexes.

  PRINTF6(this,"\tInt_t QList<",typeid(U).name(),">::Find(const U& u,Int_t maxmatches<",maxmatches,">) const\n")

  return Find(&u,1,maxmatches);
}

template <typename U> QList<Int_t> QList<U>::Find(const U* us, const Int_t &nelements, const Int_t &maxmatches) const
{
  // This function browses the QList<U> U list in a similar manner to the Del member function with
  // the same arguments. However, instead of deleting list parts and returning the number of
  // deletions, it returns a QList<Int_t> containing the matching indexes.

  // The implementation of this function is similar to the one of the corresponding Del funcion,
  // but this code has been chosen for an optimisation reason.

  PRINTF8(this,"\tInt_t QList<",typeid(U).name(),">::Find(const U* us, Int_t nelements<",nelements,">,Int_t maxmatches<",maxmatches,">) const\n")

  Int_t counter=0;
  Int_t pos=0;
  Int_t matches=0;
  QList<Int_t> qlistindexes;

  while(pos<=fNElements-nelements && pos+counter<fNElements){
    if(fUArray[pos+counter]==us[counter]){
      ++counter;
      if(counter==nelements){
	qlistindexes+=pos;
	++matches;
	counter=0;
	pos+=nelements;
	if(matches==maxmatches){
	  break;
	}
      }
    } else {
      counter=0;
      ++pos;
    }
  }
  return qlistindexes;
}

template <typename U> Int_t QList<U>::FindFirst(const QList<U>& qlist) const
{
  // This function browses the QList<U> U list in a similar manner to the Del member function with
  // the same arguments. However, instead of deleting list parts and returning the number of
  // deletions, it returns an Int_t containing the first matching index. -1 is returned if no match
  // is found.

  PRINTF4(this,"\tInt_t QList<",typeid(U).name(),">::Find(const QList<U>& qlist) const\n")

  return FindFirst(qlist.fUArray,qlist.fNElements);
}

template <typename U> Int_t QList<U>::FindFirst(const U& u) const
{
  // This function browses the QList<U> U list in a similar manner to the Del member function with
  // the same arguments. However, instead of deleting list parts and returning the number of
  // deletions, it returns an Int_t containing the firsdt matching index. -1 is returned if no match
  // is found.

  PRINTF4(this,"\tInt_t QList<",typeid(U).name(),">::Find(const U& u) const\n")

  return FindFirst(&u,1);
}

template <typename U> Int_t QList<U>::FindFirst(const U* us, const Int_t &nelements) const
{
  // This function browses the QList<U> U list in a similar manner to the Del member function with
  // the same arguments. However, instead of deleting list parts and returning the number of
  // deletions, it returns an Int_t containing the first matching index. -1 is returned if no match
  // is found.

  // The implementation of this function is similar to the one of the corresponding Del funcion,
  // but this code has been chosen for an optimisation reason.

  PRINTF6(this,"\tInt_t QList<",typeid(U).name(),">::Find(const U* us, Int_t nelements<",nelements,">) const\n")

  Int_t counter=0;
  Int_t pos=0;
  while(pos<=fNElements-nelements && pos+counter<fNElements){
    if(fUArray[pos+counter]==us[counter]){
      ++counter;
      if(counter==nelements){
	return pos;
      }
    } else {
      counter=0;
      ++pos;
    }
  }
  return -1;
}

template <typename U> inline U& QList<U>::GetLast() const
{
  //Returns the last element of the list

  try{

    if(fNElements) return fUArray[fNElements-1];
    cout << "QList<" << typeid(U).name() << ">: The list is empty\n"; throw 4000;

  }catch(Int_t e){
    cout << "Exception handled by QList::GetLast()\n";
    throw e;
  }
}

template<typename V> Bool_t operator==(const QList<V>& lhs,const QList<V>& rhs)
{
  // This friend function compare list values of 2 QList<V> instances, and returns
  // true when the list lenghts and values are the same. If V is a data type or a
  // class instance, it uses V::operator== to compare the QList<V> instances
  // (V must conform to listed requirements). However, if V is a pointer, it compares
  // the addresses.

  PRINTF5("\tBool_t operator==(const QList<",typeid(V).name(),">& lhs,const QList<",typeid(V).name(),">& rhs)\n")

  if(lhs.fNElements!=rhs.fNElements) return false;
  for(Int_t i=lhs.fNElements-1; i>=0; --i) if(!(lhs.fUArray[i]==rhs.fUArray[i])) return false;
  return true;
}

template<typename V> Bool_t operator!=(const QList<V>& lhs,const QList<V>& rhs)
{
  // This friend function is equivalent to !operator==(const QList<V>&,const QList<V>&).

  PRINTF5("\tBool_t operator!=(const QList<",typeid(V).name(),">& lhs,const QList<",typeid(V).name(),">& rhs)\n")

  return !operator==(lhs,rhs);
}

template<typename V> Bool_t operator<=(const QList<V>& lhs,const QList<V>& rhs)
{
  // This friend function returns true when lhs is contained in rhs (in an
  // unfragmented manner).

  PRINTF5("\tBool_t operator<=(const QList<",typeid(V).name(),">& lhs,const QList<",typeid(V).name(),">& rhs)\n")

  return (rhs.Find(lhs,1).Count()>0);
}

template<typename V> Bool_t operator>=(const QList<V>& lhs,const QList<V>& rhs)
{
  // This friend function returns true when rhs is contained in lhs (in an
  // unfragmented manner).

  PRINTF5("\tBool_t operator>=(const QList<",typeid(V).name(),">& lhs,const QList<",typeid(V).name(),">& rhs)\n")

  return (lhs.Find(rhs,1).Count()>0);
}

template<typename V> Bool_t operator<(const QList<V>& lhs,const QList<V>& rhs)
{
  // This friend function returns true when lhs respects these 2 conditions:
  // 1) It is contained in rhs (in an unfragmented manner)
  // 2) lhs.Count() is smaller than rhs.Count().

  PRINTF5("\tBool_t operator<(const QList<",typeid(V).name(),">& lhs,const QList<",typeid(V).name(),">& rhs)\n")

  return (rhs.Find(lhs,1).Count()>0 && rhs.Count()>lhs.Count());
}

template<typename V> Bool_t operator>(const QList<V>& lhs,const QList<V>& rhs)
{
  // This friend function returns true when rhs respects these 2 conditions:
  // 1) It is contained in lhs (in an unfragmented manner)
  // 2) rhs.Count() is smaller than lhs.Count().

  PRINTF5("\tBool_t operator>(const QList<",typeid(V).name(),">& lhs,const QList<",typeid(V).name(),">& rhs)\n")

  return (lhs.Find(rhs,1).Count()>0 && lhs.Count()>rhs.Count());
}

template <typename U> inline U& QList<U>::operator[](const Int_t &index) const
{
  // This function returns the U reference corresponding to index.

  PRINTF6(this,"\tU& QList<",typeid(U).name(),">::operator[](Int_t index<",index,">)\n")

  PRINTF3("fNElements:",fNElements,"\n")

#ifndef QSFAST
  try{
    if(index<0 || index>=fNElements) {cout << "QList<" << typeid(U).name() << ">: A bad index has been passed (" << index << ")\n"; throw 4000;}
    
    return fUArray[index];
  }catch(Int_t e){
    cout << "Exception handled by QList::operator[]\n";
    throw e;
  }
#else
  return fUArray[index];
#endif
}

template <typename U> const QList<U>& QList<U>::operator()(const Int_t &index1, const Int_t &index2) const
{
  PRINTF8(this,"\tconst QList<U>& QList<",typeid(U).name(),">::operator()(Int_t index1<",index1,">,Int_t index2<",index2,">)\n")

  try{

    if(index1<0 || index1>=fNElements || index2<0 || index2>=fNElements) {cout << "QList<" << typeid(U).name() << ">: A bad index has been passed\n"; throw 4000;}
    
    if(!fChild){
      fChild=new QList<U>;
      fChild->fIsChild=kTRUE;
    }
    
    fChild->fNElements=abs(index2-index1+1);
    fChild->fUArray=fUArray+index1;
    
    return *fChild;
  }catch(Int_t e){
    cout << "Exception handled by QList::operator()\n";
    throw e;
  }
}

template <typename U> Int_t QList<U>::RedimList(const Int_t &newdim, Int_t index)
{
  // This function redimensions the QList<U> U array to dimension newdim, adding or
  // removing elements at position equal to index. If RedimList is used to increase
  // the array size, the first new element is located at position equal to index and
  // existing elements with greater index have their index shifted up. If RedimList
  // is used to decrease the array size, the first deleted element is located at
  // position equal to index and non-deleted elements with greater index have their
  // index shited down. When index is not provided, both increases and decreases of
  // the array size are performed at the end of the existing array. The function
  // return the index of the first element having been added or deleted.

  PRINTF8(this,"\tInt_t QList<",typeid(U).name(),">::RedimList(Int_t newdim<",newdim,">,Int_t index<",index,">)\n")

  try{
    if(fChild){
      delete fChild;
      fChild=NULL;
    }
    
    if(newdim<fNElements){
      Int_t dimdif=newdim-fNElements;
      if(index==-1) index=newdim;
      if(index<0 || index>newdim) {cout << "QList<" << typeid(U).name() << ">: A bad index has been passed (" << index << ")\n"; throw 4000;}
      Int_t i;
      for(i=index-dimdif; i<fNElements; ++i){
	fUArray[i+dimdif]=fUArray[i];
      }
      for(i=newdim; i<fNElements; ++i){
	fUArray[i].~U();
      }
      fNElements=newdim;
      fUArray=(U*)realloc(fUArray,fNElements*sizeof(U));
      return index;
      
    } else if(newdim>fNElements){
      Int_t dimdif=newdim-fNElements;
      U* newarray;
      if(index==-1) index=fNElements;
      if(index<0 || index>fNElements) {cout << "QList<" << typeid(U).name() << ">: A bad index has been passed (" << index << ")\n"; throw 4000;}
      newarray=(U*)realloc(fUArray,newdim*sizeof(U));
      Int_t i;
      
      //If the address of the array has changed and the elements were stored in the object table
      //TObjectTable::PtrIsValid is called to detect if the array elements of the old table were stored in the object table.
      //The cast to TObject* should not matter since the memory pointed by the address is not accessed by PtrIsValid.
      if(newarray!=fUArray && gObjectTable && gObjectTable->PtrIsValid((TObject*)fUArray)){

	for(i=fNElements-1; i>=0; --i){
	  //Update the address of the elements in the object table
	  gObjectTable->Remove((TObject*)(fUArray+i));
	  gObjectTable->Add((TObject*)(newarray+i));
	}
      }
      fUArray=newarray;
      for(i=fNElements;i<newdim;++i){
	new(fUArray+i) U;
      }
      fNElements=newdim;
      for(i=fNElements-1;i>=index+dimdif;--i){
	fUArray[i]=fUArray[i-dimdif];
      }
      return index;
      
    } else {
      if(index==-1) index=fNElements;
      if(index<0 || index>fNElements) {cout << "QList<" << typeid(U).name() << ">: A bad index has been passed (" << index << ")\n"; throw 4000;}
      return index;
    }
  }catch(Int_t e){
    cout << "Exception handled by QList::RedimList\n";
    throw e;
  }
}

template <typename U> Int_t QList<U>::RedimList(const Int_t &newdim, const Int_t &index, const U& fillvalue)
{
  Int_t nebefore=fNElements;
  Int_t pos=RedimList(newdim,index);

  if(fNElements>nebefore){
    Int_t i;

    for(i=pos;i<pos+fNElements-nebefore;++i) fUArray[i]=fillvalue;

  }
  return pos;
}

#include "QList_Dict_cxx.h"
#include "debugger.h"
