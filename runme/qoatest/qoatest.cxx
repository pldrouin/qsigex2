#include <cstdio>
#include "QProcQOAHandler.h"

int main()
{
  //QOversizeArray::SetMemConstraints(1500*1024*1024,1200*1024*1024,1300*1024*1024,-1);
  QOversizeArray::SetMemConstraints(1500*1024*1024,1200*1024*1024,1300*1024*1024,1400*1024*1024);
  QOversizeArray::SetNLoaders(2);

  const Int_t ninstances=10;
  const Int_t nentries=20971520;
  QProcArray *array[ninstances];
  Double_t dbuf[ninstances];
  Char_t filename[32];
  Int_t i,j;

  for(j=0; j<ninstances; ++j) {
    sprintf(filename,"test%05i.qoa",j);
    array[j]=QProcQOAHandler::LoadQOA(filename,"test/Double_t",kTRUE);
    //array[j]=QProcQOAHandler::LoadQOA(filename,"test/Double_t",kFALSE);
    array[j]->SetBuffer(dbuf+j);
  }

  for(i=0; i<nentries; ++i) {

    for(j=ninstances-1; j>=0; --j) {
      dbuf[j]=i/(j+3.);
      array[j]->Fill();
    }
  }
  printf("done writing\n");

  for(i=0; i<nentries; ++i) {

    for(j=ninstances-1; j>=0; --j) {
      array[j]->LoadEntry(i);

      if(dbuf[j]!=i/(j+3.)) {
	printf("Error: %i, %i: %f != %f\n",j,i,dbuf[j],i/(j+3.));
	return 1;
      }
    }
  }
  printf("done loading\n");

  printf("Unloading arrays\n");
  for(j=ninstances-1; j>=0; --j) array[j]->UnloadArray();
  return 0;
}
