#include "arrayutils.h"

#include "debugger.h"

template <typename U> void DelPArray(U** array, Int_t size, Int_t type)
{
  for(Int_t i=0;i<size;i++){
    delete array[i];
  }
  switch(type){
  case 0:
    delete[] array;
    break;
  case 1:
    free(array);
    break;
  }
}

#include "debugger.h"
