// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>
// Copyright Carleton University

#include "QHN.h"

template <typename U> QHN<U>::QHN(const QHN &qthn): QDis(qthn), fNDims(qthn.fNDims), fAxes(NULL), fEntries(qthn.fEntries), fBinContent(NULL), fNBins(qthn.fNBins)
{
  Int_t i;
  fAxes=new QAxis*[fNDims];

  for(i=0; i<fNDims; i++) fAxes[i]=qthn.fAxes[i]?(QAxis*)qthn.fAxes[i]->Clone():NULL;
  fBinContent=(U*)malloc(fNBins*sizeof(U));
  memset(fBinContent,0,fNBins*sizeof(U));
  Long64_t li;
  Long64_t nfbins=qthn.GetNFbins();

  for(li=0; li<nfbins; li++) {
    fBinContent[qthn.GetFBinCoord(li)]=qthn.GetFBinContent(li);
  }
}

template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &ndims): QDis(name,title), fNDims(ndims), fAxes(new QAxis*[ndims]), fBinContent(NULL), fNBins(0)
{
  if(fNDims<=0) {
    fprintf(stderr,"QHN::QHN: Error: Number of dimensions is invalid\n");
    throw 1;
  }
  memset(fAxes,0,fNDims*sizeof(QAxis*));
}

#ifndef __CINT__
template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t &zlow, const Float_t &zhigh, const Bool_t &init): QDis(name,title)
{
  if(nbinsy==0) {
    fNDims=1;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xlow,xhigh);

  } else if(nbinsz==0) {
    fNDims=2;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xlow,xhigh);
    fAxes[1]=new QAxis(nbinsy,ylow,yhigh);

  } else {
    fNDims=3;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xlow,xhigh);
    fAxes[1]=new QAxis(nbinsy,ylow,yhigh);
    fAxes[2]=new QAxis(nbinsz,zlow,zhigh);
  }
  if(init) Init();
}

template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins, const Bool_t &init): QDis(name,title)
{
  fNDims=3;
  fAxes=new QAxis*[fNDims];
  fAxes[0]=new QAxis(nbinsx,xlow,xhigh);
  fAxes[1]=new QAxis(nbinsy,ylow,yhigh);
  fAxes[2]=new QAxis(nbinsz,zbins);
  if(init) Init();
}

template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t &zlow, const Float_t &zhigh, const Bool_t &init): QDis(name,title)
{
  if(nbinsz==0) {
    fNDims=2;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xlow,xhigh);
    fAxes[1]=new QAxis(nbinsy,ybins);

  } else {
    fNDims=3;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xlow,xhigh);
    fAxes[1]=new QAxis(nbinsy,ybins);
    fAxes[2]=new QAxis(nbinsz,zlow,zhigh);
  }
  if(init) Init();
}

template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t &zlow, const Float_t &zhigh, const Bool_t &init): QDis(name,title)
{
  if(nbinsy==0) {
    fNDims=1;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xbins);

  } else if(nbinsz==0) {
    fNDims=2;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xbins);
    fAxes[1]=new QAxis(nbinsy,ylow,yhigh);

  } else {
    fNDims=3;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xbins);
    fAxes[1]=new QAxis(nbinsy,ylow,yhigh);
    fAxes[2]=new QAxis(nbinsz,zlow,zhigh);
  }
  if(init) Init();
}

template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins, const Bool_t &init): QDis(name,title)
{
  fNDims=3;
  fAxes=new QAxis*[fNDims];
  fAxes[0]=new QAxis(nbinsx,xlow,xhigh);
  fAxes[1]=new QAxis(nbinsy,ybins);
  fAxes[2]=new QAxis(nbinsz,zbins);
  if(init) Init();
}

template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins, const Bool_t &init): QDis(name,title)
{
  fNDims=3;
  fAxes=new QAxis*[fNDims];
  fAxes[0]=new QAxis(nbinsx,xbins);
  fAxes[1]=new QAxis(nbinsy,ylow,yhigh);
  fAxes[2]=new QAxis(nbinsz,zbins);
  if(init) Init();
}

template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t &zlow, const Float_t &zhigh, const Bool_t &init): QDis(name,title)
{
  if(nbinsz==0) {
    fNDims=2;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xbins);
    fAxes[1]=new QAxis(nbinsy,ybins);

  } else {
    fNDims=3;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xbins);
    fAxes[1]=new QAxis(nbinsy,ybins);
    fAxes[2]=new QAxis(nbinsz,zlow,zhigh);
  }
  if(init) Init();
}

template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins, const Bool_t &init): QDis(name,title)
{
  fNDims=3;
  fAxes=new QAxis*[fNDims];
  fAxes[0]=new QAxis(nbinsx,xbins);
  fAxes[1]=new QAxis(nbinsy,ybins);
  fAxes[2]=new QAxis(nbinsz,zbins);
  if(init) Init();
}
#endif

template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t &zlow, const Double_t &zhigh, const Bool_t &init): QDis(name,title)
{
  if(nbinsy==0) {
    fNDims=1;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xlow,xhigh);

  } else if(nbinsz==0) {
    fNDims=2;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xlow,xhigh);
    fAxes[1]=new QAxis(nbinsy,ylow,yhigh);

  } else {
    fNDims=3;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xlow,xhigh);
    fAxes[1]=new QAxis(nbinsy,ylow,yhigh);
    fAxes[2]=new QAxis(nbinsz,zlow,zhigh);
  }
  if(init) Init();
}

#ifndef __CINT__
template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins, const Bool_t &init): QDis(name,title)
{
  fNDims=3;
  fAxes=new QAxis*[fNDims];
  fAxes[0]=new QAxis(nbinsx,xlow,xhigh);
  fAxes[1]=new QAxis(nbinsy,ylow,yhigh);
  fAxes[2]=new QAxis(nbinsz,zbins);
  if(init) Init();
}

template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t &zlow, const Double_t &zhigh, const Bool_t &init): QDis(name,title)
{
  if(nbinsz==0) {
    fNDims=2;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xlow,xhigh);
    fAxes[1]=new QAxis(nbinsy,ybins);

  } else {
    fNDims=3;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xlow,xhigh);
    fAxes[1]=new QAxis(nbinsy,ybins);
    fAxes[2]=new QAxis(nbinsz,zlow,zhigh);
  }
  if(init) Init();
}
#endif

template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t &zlow, const Double_t &zhigh, const Bool_t &init): QDis(name,title)
{
  if(nbinsy==0) {
    fNDims=1;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xbins);

  } else if(nbinsz==0) {
    fNDims=2;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xbins);
    fAxes[1]=new QAxis(nbinsy,ylow,yhigh);

  } else {
    fNDims=3;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xbins);
    fAxes[1]=new QAxis(nbinsy,ylow,yhigh);
    fAxes[2]=new QAxis(nbinsz,zlow,zhigh);
  }
  if(init) Init();
}

#ifndef __CINT__
template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins, const Bool_t &init): QDis(name,title)
{
  fNDims=3;
  fAxes=new QAxis*[fNDims];
  fAxes[0]=new QAxis(nbinsx,xlow,xhigh);
  fAxes[1]=new QAxis(nbinsy,ybins);
  fAxes[2]=new QAxis(nbinsz,zbins);
  if(init) Init();
}

template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins, const Bool_t &init): QDis(name,title)
{
  fNDims=3;
  fAxes=new QAxis*[fNDims];
  fAxes[0]=new QAxis(nbinsx,xbins);
  fAxes[1]=new QAxis(nbinsy,ylow,yhigh);
  fAxes[2]=new QAxis(nbinsz,zbins);
  if(init) Init();
}
#endif

template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t &zlow, const Double_t &zhigh, const Bool_t &init): QDis(name,title)
{
  if(nbinsz==0) {
    fNDims=2;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xbins);
    fAxes[1]=new QAxis(nbinsy,ybins);

  } else {
    fNDims=3;
    fAxes=new QAxis*[fNDims];
    fAxes[0]=new QAxis(nbinsx,xbins);
    fAxes[1]=new QAxis(nbinsy,ybins);
    fAxes[2]=new QAxis(nbinsz,zlow,zhigh);
  }
  if(init) Init();
}

template <typename U> QHN<U>::QHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins, const Bool_t &init): QDis(name,title)
{
  fNDims=3;
  fAxes=new QAxis*[fNDims];
  fAxes[0]=new QAxis(nbinsx,xbins);
  fAxes[1]=new QAxis(nbinsy,ybins);
  fAxes[2]=new QAxis(nbinsz,zbins);
  if(init) Init();
}

template <typename U> void QHN<U>::Clear(Option_t* option)
{
  QDis::Clear(option);

for(Int_t i=fNDims-1; i>=0; --i) {
    if(fAxes[i]) delete fAxes[i];
  }
  delete[] fAxes;

  if(fNBins) {
    free(fBinContent);
  }
  fNDims=0;
  fAxes=NULL;
  fEntries=0;
  fBinContent=NULL;
  fNBins=0;
}

template <typename U> void QHN<U>::ComputeNBins()
{
  fNBins=1;
  for(Int_t i=fNDims-1; i>=0; --i) if(fAxes[i]) fNBins*=fAxes[i]->GetNBins()+2;

  if(fBinContent) free(fBinContent);
  fBinContent=(U*)malloc(fNBins*sizeof(U));
}

template <typename U> void QHN<U>::Init()
{
  fNBins=1;
  for(Int_t i=fNDims-1; i>=0; --i) fNBins*=fAxes[i]->GetNBins()+2;
  fBinContent=(U*)malloc(fNBins*sizeof(U));
  memset(fBinContent,0,fNBins*sizeof(U));
  fEntries=0;
}

template <typename U> Long64_t QHN<U>::FindBin(const Double_t &x0) const
{
  return fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Double_t &x0, const Double_t &x1) const
{
  return (Long64_t)fAxes[1]->FindBin(x1)*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Double_t &x0, const Double_t &x1, const Double_t &x2) const
{
  return ((Long64_t)fAxes[2]->FindBin(x2)*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3) const
{
  return (((Long64_t)fAxes[3]->FindBin(x3)*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4) const
{
  return ((((Long64_t)fAxes[4]->FindBin(x4)*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5) const
{
  return (((((Long64_t)fAxes[5]->FindBin(x5)*(fAxes[4]->GetNBins()+2)+fAxes[4]->FindBin(x4))*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6) const
{
  return ((((((Long64_t)fAxes[6]->FindBin(x6)*(fAxes[5]->GetNBins()+2)+fAxes[5]->FindBin(x5))*(fAxes[4]->GetNBins()+2)+fAxes[4]->FindBin(x4))*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7) const
{
  return (((((((Long64_t)fAxes[7]->FindBin(x7)*(fAxes[6]->GetNBins()+2)+fAxes[6]->FindBin(x6))*(fAxes[5]->GetNBins()+2)+fAxes[5]->FindBin(x5))*(fAxes[4]->GetNBins()+2)+fAxes[4]->FindBin(x4))*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7, const Double_t &x8) const
{
  return ((((((((Long64_t)fAxes[8]->FindBin(x8)*(fAxes[7]->GetNBins()+2)+fAxes[7]->FindBin(x7))*(fAxes[6]->GetNBins()+2)+fAxes[6]->FindBin(x6))*(fAxes[5]->GetNBins()+2)+fAxes[5]->FindBin(x5))*(fAxes[4]->GetNBins()+2)+fAxes[4]->FindBin(x4))*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Double_t &x0, const Double_t &x1, const Double_t &x2, const Double_t &x3, const Double_t &x4, const Double_t &x5, const Double_t &x6, const Double_t &x7, const Double_t &x8, const Double_t &x9) const
{
  return (((((((((Long64_t)fAxes[9]->FindBin(x9)*(fAxes[8]->GetNBins()+2)+fAxes[8]->FindBin(x8))*(fAxes[7]->GetNBins()+2)+fAxes[7]->FindBin(x7))*(fAxes[6]->GetNBins()+2)+fAxes[6]->FindBin(x6))*(fAxes[5]->GetNBins()+2)+fAxes[5]->FindBin(x5))*(fAxes[4]->GetNBins()+2)+fAxes[4]->FindBin(x4))*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(Double_t const* const &x) const
{
  Long64_t bin=fAxes[fNDims-1]->FindBin(x[fNDims-1]);

  for(Int_t i=fNDims-2; i>=0; --i) {
    bin=bin*(fAxes[i]->GetNBins()+2)+fAxes[i]->FindBin(x[i]);
  }
  return bin;
}

template <typename U> Long64_t QHN<U>::FindBin(const Float_t &x0) const
{
  return fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Float_t &x0, const Float_t &x1) const
{
  return (Long64_t)fAxes[1]->FindBin(x1)*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Float_t &x0, const Float_t &x1, const Float_t &x2) const
{
  return ((Long64_t)fAxes[2]->FindBin(x2)*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3) const
{
  return (((Long64_t)fAxes[3]->FindBin(x3)*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4) const
{
  return ((((Long64_t)fAxes[4]->FindBin(x4)*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5) const
{
  return (((((Long64_t)fAxes[5]->FindBin(x5)*(fAxes[4]->GetNBins()+2)+fAxes[4]->FindBin(x4))*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6) const
{
  return ((((((Long64_t)fAxes[6]->FindBin(x6)*(fAxes[5]->GetNBins()+2)+fAxes[5]->FindBin(x5))*(fAxes[4]->GetNBins()+2)+fAxes[4]->FindBin(x4))*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7) const
{
  return (((((((Long64_t)fAxes[7]->FindBin(x7)*(fAxes[6]->GetNBins()+2)+fAxes[6]->FindBin(x6))*(fAxes[5]->GetNBins()+2)+fAxes[5]->FindBin(x5))*(fAxes[4]->GetNBins()+2)+fAxes[4]->FindBin(x4))*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &x8) const
{
  return ((((((((Long64_t)fAxes[8]->FindBin(x8)*(fAxes[7]->GetNBins()+2)+fAxes[7]->FindBin(x7))*(fAxes[6]->GetNBins()+2)+fAxes[6]->FindBin(x6))*(fAxes[5]->GetNBins()+2)+fAxes[5]->FindBin(x5))*(fAxes[4]->GetNBins()+2)+fAxes[4]->FindBin(x4))*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(const Float_t &x0, const Float_t &x1, const Float_t &x2, const Float_t &x3, const Float_t &x4, const Float_t &x5, const Float_t &x6, const Float_t &x7, const Float_t &x8, const Float_t &x9) const
{
  return (((((((((Long64_t)fAxes[9]->FindBin(x9)*(fAxes[8]->GetNBins()+2)+fAxes[8]->FindBin(x8))*(fAxes[7]->GetNBins()+2)+fAxes[7]->FindBin(x7))*(fAxes[6]->GetNBins()+2)+fAxes[6]->FindBin(x6))*(fAxes[5]->GetNBins()+2)+fAxes[5]->FindBin(x5))*(fAxes[4]->GetNBins()+2)+fAxes[4]->FindBin(x4))*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QHN<U>::FindBin(Float_t const* const &x) const
{
  Long64_t bin=fAxes[fNDims-1]->FindBin(x[fNDims-1]);

  for(Int_t i=fNDims-2; i>=0; --i) {
    bin=bin*(fAxes[i]->GetNBins()+2)+fAxes[i]->FindBin(x[i]);
  }
  return bin;
}

template <typename U> TH1* QHN<U>::GenTH(const char *name) const
{
  if(fNDims>3) {
    fprintf(stderr,"QHN::GenTH: Error: Cannot generate a ROOT histogram because QHN dimension is too high\n");
    throw 1;
  }

  TH1* th;
  Long64_t li;
  Int_t coords[3];
  Long64_t nfbins=GetNFbins();

  switch(fNDims) {
    case 1:
      if(!fAxes[0]->GetBins()) th=new TH1D(name,name,fAxes[0]->GetNBins(),fAxes[0]->GetMin(),fAxes[0]->GetMax());
      else th=new TH1D(name,name,fAxes[0]->GetNBins(),fAxes[0]->GetBins());
      th->GetXaxis()->SetTitle(fAxes[0]->GetTitle());

      for(li=0; li<nfbins; li++) {
	GetFBinCoords(li,coords);
	th->SetBinContent(coords[0],fBinContent[li]);
      }
      break;

    case 2:
      th=new TH2D(name,name,fAxes[0]->GetNBins(),fAxes[0]->GetMin(),fAxes[0]->GetMax(),fAxes[1]->GetNBins(),fAxes[1]->GetMin(),fAxes[1]->GetMax());
      if(fAxes[0]->GetBins()) th->GetXaxis()->Set(fAxes[0]->GetNBins(),fAxes[0]->GetBins());
      if(fAxes[1]->GetBins()) th->GetYaxis()->Set(fAxes[1]->GetNBins(),fAxes[1]->GetBins());
      th->GetXaxis()->SetTitle(fAxes[0]->GetTitle());
      th->GetYaxis()->SetTitle(fAxes[1]->GetTitle());

      for(li=0; li<nfbins; li++) {
	GetFBinCoords(li,coords);
	th->SetBinContent(coords[0],coords[1],fBinContent[li]);
      }
      break;

    case 3:
      th=new TH3D(name,name,fAxes[0]->GetNBins(),fAxes[0]->GetMin(),fAxes[0]->GetMax(),fAxes[1]->GetNBins(),fAxes[1]->GetMin(),fAxes[1]->GetMax(),fAxes[2]->GetNBins(),fAxes[2]->GetMin(),fAxes[2]->GetMax());
      if(fAxes[0]->GetBins()) th->GetXaxis()->Set(fAxes[0]->GetNBins(),fAxes[0]->GetBins());
      if(fAxes[1]->GetBins()) th->GetYaxis()->Set(fAxes[1]->GetNBins(),fAxes[1]->GetBins());
      if(fAxes[2]->GetBins()) th->GetZaxis()->Set(fAxes[2]->GetNBins(),fAxes[2]->GetBins());
      th->GetXaxis()->SetTitle(fAxes[0]->GetTitle());
      th->GetYaxis()->SetTitle(fAxes[1]->GetTitle());
      th->GetZaxis()->SetTitle(fAxes[2]->GetTitle());

      for(li=0; li<nfbins; li++) {
	GetFBinCoords(li,coords);
	th->SetBinContent(coords[0],coords[1],coords[2],fBinContent[li]);
      }
      break;

    default:
      return NULL;
  }

  th->SetEntries(fEntries);
  return th;
}

template <typename U> Long64_t QHN<U>::GetBin(const Int_t *coords) const
{
  Long64_t bin=coords[fNDims-1];

  for(Int_t i=fNDims-2; i>=0; --i) {
    bin=bin*(fAxes[i]->GetNBins()+2)+coords[i];
  }
  return bin;
}

template <typename U> Long64_t QHN<U>::GetBin(const Int_t &coord0, const Int_t &coord1) const
{
    return (Long64_t)coord1*(fAxes[0]->GetNBins()+2)+coord0;
}

template <typename U> Long64_t QHN<U>::GetBin(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2) const
{
    return ((Long64_t)coord2*(fAxes[1]->GetNBins()+2)+coord1)*(fAxes[0]->GetNBins()+2)+coord0;
}

template <typename U> Long64_t QHN<U>::GetBin(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3) const
{
    return (((Long64_t)coord3*(fAxes[2]->GetNBins()+2)+coord2)*(fAxes[1]->GetNBins()+2)+coord1)*(fAxes[0]->GetNBins()+2)+coord0;
}

template <typename U> Long64_t QHN<U>::GetBin(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4) const
{
    return ((((Long64_t)coord4*(fAxes[3]->GetNBins()+2)+coord3)*(fAxes[2]->GetNBins()+2)+coord2)*(fAxes[1]->GetNBins()+2)+coord1)*(fAxes[0]->GetNBins()+2)+coord0;
}

template <typename U> Long64_t QHN<U>::GetBin(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5) const
{
    return (((((Long64_t)coord5*(fAxes[4]->GetNBins()+2)+coord4)*(fAxes[3]->GetNBins()+2)+coord3)*(fAxes[2]->GetNBins()+2)+coord2)*(fAxes[1]->GetNBins()+2)+coord1)*(fAxes[0]->GetNBins()+2)+coord0;
}

template <typename U> Long64_t QHN<U>::GetBin(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5, const Int_t &coord6) const
{
    return ((((((Long64_t)coord6*(fAxes[5]->GetNBins()+2)+coord5)*(fAxes[4]->GetNBins()+2)+coord4)*(fAxes[3]->GetNBins()+2)+coord3)*(fAxes[2]->GetNBins()+2)+coord2)*(fAxes[1]->GetNBins()+2)+coord1)*(fAxes[0]->GetNBins()+2)+coord0;
}

template <typename U> Long64_t QHN<U>::GetBin(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5, const Int_t &coord6, const Int_t &coord7) const
{
    return (((((((Long64_t)coord7*(fAxes[6]->GetNBins()+2)+coord6)*(fAxes[5]->GetNBins()+2)+coord5)*(fAxes[4]->GetNBins()+2)+coord4)*(fAxes[3]->GetNBins()+2)+coord3)*(fAxes[2]->GetNBins()+2)+coord2)*(fAxes[1]->GetNBins()+2)+coord1)*(fAxes[0]->GetNBins()+2)+coord0;
}

template <typename U> Long64_t QHN<U>::GetBin(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5, const Int_t &coord6, const Int_t &coord7, const Int_t &coord8) const
{
    return ((((((((Long64_t)coord8*(fAxes[7]->GetNBins()+2)+coord7)*(fAxes[6]->GetNBins()+2)+coord6)*(fAxes[5]->GetNBins()+2)+coord5)*(fAxes[4]->GetNBins()+2)+coord4)*(fAxes[3]->GetNBins()+2)+coord3)*(fAxes[2]->GetNBins()+2)+coord2)*(fAxes[1]->GetNBins()+2)+coord1)*(fAxes[0]->GetNBins()+2)+coord0;
}

template <typename U> Long64_t QHN<U>::GetBin(const Int_t &coord0, const Int_t &coord1, const Int_t &coord2, const Int_t &coord3, const Int_t &coord4, const Int_t &coord5, const Int_t &coord6, const Int_t &coord7, const Int_t &coord8, const Int_t &coord9) const
{
    return (((((((((Long64_t)coord9*(fAxes[8]->GetNBins()+2)+coord8)*(fAxes[7]->GetNBins()+2)+coord7)*(fAxes[6]->GetNBins()+2)+coord6)*(fAxes[5]->GetNBins()+2)+coord5)*(fAxes[4]->GetNBins()+2)+coord4)*(fAxes[3]->GetNBins()+2)+coord3)*(fAxes[2]->GetNBins()+2)+coord2)*(fAxes[1]->GetNBins()+2)+coord1)*(fAxes[0]->GetNBins()+2)+coord0;
}

template <typename U> void QHN<U>::GetBinCoords(Long64_t bin, Int_t *coords) const
{
  coords[0]=bin%(fAxes[0]->GetNBins()+2);

  for(Int_t i=1; i<fNDims; i++) {
    bin=(bin-coords[i-1])/(fAxes[i-1]->GetNBins()+2);
    coords[i]=bin%(fAxes[i]->GetNBins()+2);
  }
}

template <typename U> void QHN<U>::GetCorrelationMatrix(TMatrixDSym* covmat, Bool_t width, Bool_t varianceondiag) const
{
  Int_t i,j;

  GetCovarianceMatrix(covmat,width);

  for(i=0; i<fNDims; i++) {

    for(j=i+1; j<fNDims; j++) {
      (*covmat)[i][j]=(*covmat)[j][i]=(*covmat)[i][j]/TMath::Sqrt((*covmat)[i][i]*(*covmat)[j][j]);
    }
  }

  if(!varianceondiag) {
    for(i=0; i<fNDims; i++) (*covmat)[i][i]=1;
  }
}

template <typename U> void QHN<U>::GetCovarianceMatrix(TMatrixDSym* covmat, Bool_t width) const
{
  Int_t i,j;
  Long64_t li;
  QAxis **vbsaxis=new QAxis*[fNDims];      //array of axis using variable bin width
  Int_t *vbaindex=new Int_t[fNDims];
  Int_t nvbaxes=0;
  Int_t *coords=new Int_t[fNDims];
  Double_t *means=new Double_t[fNDims];
  Double_t *covs=new Double_t[fNDims*(fNDims+1)/2];
  Double_t integral=0;
  memset(means,0,fNDims*sizeof(Double_t));
  memset(covs,0,fNDims*(fNDims+1)/2*sizeof(Double_t));
  Long64_t nfbins=GetNFbins();

  if(width) {
    memset(vbsaxis,0,fNDims*sizeof(QAxis*)); //Initialize axis pointers to NULL

    for(i=0; i<fNDims; i++) {
      //Add QAxis pointers to vbsaxis for axis having variable bin width

      if(fAxes[i]->GetNBins()>1 && fAxes[i]->GetBins()) {
	vbsaxis[nvbaxes]=fAxes[i];
	vbaindex[nvbaxes]=i;
	nvbaxes++;
      }
    }
  }

  if(width && nvbaxes) {
    Double_t binvol;

    for(li=0; li<nfbins; li++) {
      GetFBinCoords(li,coords);
      binvol=vbsaxis[0]->GetBinWidth(coords[vbaindex[0]]);

      for(i=1; i<nvbaxes; i++) binvol*=vbsaxis[i]->GetBinWidth(coords[vbaindex[i]]);
      binvol*=fBinContent[li];
      integral+=binvol;

      for(i=0; i<fNDims; i++) {
	means[i]+=fAxes[i]->GetBinCenter(coords[i])*binvol;

	for(j=i; j<fNDims; j++) {
	  covs[(2*fNDims-1-i)*i/2+j]+=fAxes[i]->GetBinCenter(coords[i])*fAxes[j]->GetBinCenter(coords[j])*binvol;
	}
      }
    }

  } else {

    for(li=0; li<nfbins; li++) {
      GetFBinCoords(li,coords);
      integral+=fBinContent[li];

      for(i=0; i<fNDims; i++) {
	means[i]+=fAxes[i]->GetBinCenter(coords[i])*fBinContent[li];

	for(j=i; j<fNDims; j++) {
	  covs[(2*fNDims-1-i)*i/2+j]+=fAxes[i]->GetBinCenter(coords[i])*fAxes[j]->GetBinCenter(coords[j])*fBinContent[li];
	}
      }
    }
  }

  covmat->ResizeTo(fNDims,fNDims);
  memset(covmat->GetMatrixArray(),0,fNDims*fNDims*sizeof(Double_t));

  if(integral) {

    for(i=0; i<fNDims; i++) {
      means[i]/=integral;
    }

    for(i=0; i<fNDims; i++) {

      for(j=i; j<fNDims; j++) {
	(*covmat)[i][j]=(*covmat)[j][i]=covs[(2*fNDims-1-i)*i/2+j]/integral-means[i]*means[j];
      }
    }
  }

  delete[] means;
  delete[] covs;
  delete[] coords;
  delete[] vbsaxis;
  delete[] vbaindex;
}

template <typename U> void QHN<U>::GetMeans(Double_t means[], Bool_t width) const
{
  Int_t i;
  Long64_t li;
  QAxis **vbsaxis=new QAxis*[fNDims];      //array of axis using variable bin width
  Int_t *vbaindex=new Int_t[fNDims];
  Int_t nvbaxes=0;
  Int_t *coords=new Int_t[fNDims];
  Double_t integral=0;
  memset(means,0,fNDims*sizeof(Double_t));
  Long64_t nfbins=GetNFbins();

  if(width) {
    memset(vbsaxis,0,fNDims*sizeof(QAxis*)); //Initialize axis pointers to NULL

    for(i=0; i<fNDims; i++) {
      //Add QAxis pointers to vbsaxis for axis having variable bin width

      if(fAxes[i]->GetNBins()>1 && fAxes[i]->GetBins()) {
	vbsaxis[nvbaxes]=fAxes[i];
	vbaindex[nvbaxes]=i;
	nvbaxes++;
      }
    }
  }

  if(width && nvbaxes) {
    Double_t binvol;

    for(li=0; li<nfbins; li++) {
      GetFBinCoords(li,coords);
      binvol=vbsaxis[0]->GetBinWidth(coords[vbaindex[0]]);

      for(i=1; i<nvbaxes; i++) binvol*=vbsaxis[i]->GetBinWidth(coords[vbaindex[i]]);
      binvol*=fBinContent[li];
      integral+=binvol;

      for(i=0; i<fNDims; i++) {
	means[i]+=fAxes[i]->GetBinCenter(coords[i])*binvol;
      }
    }

  } else {

    for(li=0; li<nfbins; li++) {
      GetFBinCoords(li,coords);
      integral+=fBinContent[li];

      for(i=0; i<fNDims; i++) {
	means[i]+=fAxes[i]->GetBinCenter(coords[i])*fBinContent[li];
      }
    }
  }

  if(integral) {

    for(i=0; i<fNDims; i++) {
      means[i]/=integral;
    }
  }

  delete[] coords;
  delete[] vbsaxis;
  delete[] vbaindex;
}

template <typename U> Double_t QHN<U>::Integral(Int_t const* const* binranges, const Bool_t *widths) const
{
  Int_t mins[fNDims], maxs[fNDims];
  Int_t i, j[fNDims];
  Long64_t li;
  Bool_t cbw=kTRUE; //Constant bin width in all directions
  Bool_t bwi=kFALSE;//Integration using bin width for at least some direction
  Long64_t nfbins=GetNFbins();

  for(i=fNDims-1; i>=0; --i){

    if(binranges && binranges[i]){
      mins[i]=(*(binranges[i])>1)?*(binranges[i]):1;
      maxs[i]=(*(binranges[i]+1)<fAxes[i]->GetNBins())?*(binranges[i]+1):fAxes[i]->GetNBins(); 
    } else {
      mins[i]=1;
      maxs[i]=fAxes[i]->GetNBins();
    }
    if(fAxes[i]->GetBins()) cbw=kFALSE;
    if(!widths || widths[i]) bwi=kTRUE;
    //cout << mins[i] << "\t" << maxs[i] << "\n";
  }

  Double_t integral=0;
  Double_t binvol;

  //If bin width is constant in all directions
  if(cbw) {

    //If integrating using bin width for all axes
    if(!widths) {
      binvol=fAxes[fNDims-1]->GetBinWidth(1);
      for(i=fNDims-2; i>=0; --i) binvol*=fAxes[i]->GetBinWidth(1);

      //Else if using bin width for only some axes
    } else {
      binvol=1;
      for(i=fNDims-1; i>=0; --i) if(widths[i]) binvol*=fAxes[i]->GetBinWidth(1);
    }

    for(li=nfbins-1; li>=0; --li) {
      if(IsFBinIncluded(li,mins,maxs)) integral+=fBinContent[li];
    }
    integral*=binvol;

  //Else if bin width is not constant for some direction
  } else {

    //If integrating using bin width for all axes
    if(!widths) {

      for(li=nfbins-1; li>=0; --li) {
	GetFBinCoords(li,j);

	for(i=fNDims-1; i>=0; --i) if(j[i]<mins[i] || j[i]>maxs[i]) break; 

	if(i==-1) {
	  binvol=fAxes[fNDims-1]->GetBinWidth(j[fNDims-1]);
	  for(i=fNDims-2; i>=0; --i) binvol*=fAxes[i]->GetBinWidth(j[i]);
	  integral+=fBinContent[li]*binvol;
	}
      }

      //Else if integrating without using bin width at all
    } else if(!bwi) {

      for(li=nfbins-1; li>=0; --li) {
	if(IsFBinIncluded(li,mins,maxs)) integral+=fBinContent[li];
      }

      //Else if integrating using bin width for some of the directions only
    } else {

      for(li=nfbins-1; li>=0; --li) {
	GetFBinCoords(li,j);

	for(i=fNDims-1; i>=0; --i) if(j[i]<mins[i] || j[i]>maxs[i]) break; 

	if(i==-1) {
	  binvol=(widths[0]?fAxes[0]->GetBinWidth(j[0]):1);
	  for(i=1; i<fNDims; i++) binvol*=(widths[i]?fAxes[i]->GetBinWidth(j[i]):1);
	  integral+=fBinContent[li]*binvol;
	}
      }
    }
  }
  return integral;
}

template <typename U> Bool_t QHN<U>::IsFBinIncluded(const Long64_t &fbin, const Int_t *mins, const Int_t *maxs) const
{
  Long64_t bin=fbin;
  Int_t coord=bin%(fAxes[0]->GetNBins()+2);
  if(coord<mins[0] || coord>maxs[0]) return kFALSE;

  for(Int_t i=1; i<fNDims; i++) {
    bin=(bin-coord)/(fAxes[i-1]->GetNBins()+2);
    coord=bin%(fAxes[i]->GetNBins()+2);
    if(coord<mins[i] || coord>maxs[i]) return kFALSE;
  }
  return kTRUE;
}

template <typename U> QHN<Double_t>* QHN<U>::MarginalPDF(const char *name, const Int_t *axes, const Int_t &naxes) const
{
  Int_t nfix=(Int_t)fNFixedCoords>fNDims?fNDims:(Int_t)fNFixedCoords; //Number of fixed coordinates for a conditional PDF

  if(naxes<=0 || !axes || naxes>=fNDims-nfix) return NULL;

  Int_t i;

  for(i=0; i<naxes; i++) {
    if(axes[i]<0 || axes[i]>=fNDims-nfix) {
      fprintf(stderr,"QHN<U>::MarginalPDF: Error: Invalid axis index: %i\n",axes[i]);
      throw 1;
    }
  }

  Int_t naaxes=naxes+nfix;
  Int_t *aaxes=new Int_t[naaxes];
  memcpy(aaxes,axes,naxes*sizeof(Int_t));

  for(i=0; i<nfix; i++) {
    aaxes[naxes+i]=i+fNDims-nfix;
  }

  QHN<Double_t> *th=NewD(name,name,naaxes);
  th->SetNormFlags(GetNormFlags()&~(QDis::kEventsFilled|QDis::kVarBinSizeEventsFilled));
  th->SetNFixedCoords(GetNFixedCoords());

  Bool_t *widths=new Bool_t[fNDims];
  Int_t **binranges=new Int_t*[fNDims];
  Int_t *biniter=new Int_t[naaxes];    //Integers used as indices for iteration

  //Initialize binranges pointers to NULL
  memset(binranges,0,fNDims*sizeof(Int_t*));

  for(i=0; i<fNDims; i++) widths[i]=kTRUE;

  for(i=0; i<naaxes; i++) {
    th->SetAxis(i,fAxes[aaxes[i]]);
    binranges[aaxes[i]]=new Int_t[2];
    widths[aaxes[i]]=kFALSE;
    biniter[i]=1;
  }

  //Loop over bin indices of projection axes
  do{

    for(i=0; i<naaxes; i++) binranges[aaxes[i]][0]=binranges[aaxes[i]][1]=biniter[i];
    //Scale the bin value
    th->SetBinContent(biniter,Integral(binranges,widths));
    biniter[0]++;

    i=0;
    while(biniter[i]>fAxes[aaxes[i]]->GetNBins()){
      biniter[i]=1;
      i++;

      if(i>=naaxes) break;
      biniter[i]++;
    }
  } while(i<naaxes);
  th->fEntries=fEntries;

  delete[] widths;

  for(i=0; i<naaxes; i++) delete[] binranges[aaxes[i]];
  delete[] binranges;
  delete[] biniter;
  delete[] aaxes;

  return th;
}

template <typename U> void QHN<U>::Normalize(Double_t* integral)
{
  //This function normalizes the PDF according to the normalization
  //flags sets by SetNormFlags. This
  //string is a standard ROOT selection expression that contains x
  //and/or y and/or z variables.

  if(!(fNormFlags&kNoNorm)) {
    Double_t cutintbuf;         //Buffer for integral value(s)
    Int_t nfix=fNFixedCoords;   //Number of fixed coordinates for a conditional PDF
    Bool_t *widths=NULL;

    //If not using bin width for normalization for all directions
    if(fNormFlags&kNoBinWidthNorm) {
      widths=new Bool_t[fNDims];
      memset(widths,0,fNDims*sizeof(Bool_t));
    }

    Int_t *nbins=new Int_t[fNDims];     //Number of bins for each dimension
    Int_t *biniter=new Int_t[fNDims];   //Integers used as indices for iteration
    Int_t i;
    Double_t scale=fEntries; //scaling factor used for normalization of histograms filled with number of events

    //Get the number of bins for each dimension
    for(i=fNDims-1; i>=0; --i) nbins[i]=fAxes[i]->GetNBins();

    //Normalization of histograms with variable size for which the bin content corresponds to a number of events
    if(fNormFlags&(kEventsFilled|kVarBinSizeEventsFilled)){
      QAxis **vbsaxis=new QAxis*[fNDims];      //array of axis using variable bin width
      Double_t binvol;                      //buffer for variable bin width/area/volume
      memset(vbsaxis,0,fNDims*sizeof(QAxis*)); //Initialize axis pointers to NULL
      Bool_t hasvbaxes=kFALSE;

      for(i=fNDims-1; i>=0; --i) {
	//Add QAxis pointers to vbsaxis for axis having variable bin width

	if(fAxes[i]->GetNBins()>1 && fAxes[i]->GetBins()) {
	  vbsaxis[i]=fAxes[i];
	  hasvbaxes=kTRUE;
	} else if(!widths || widths[i]) scale*=fAxes[i]->GetBinWidth(1);
      }

      if(hasvbaxes && !(fNormFlags&kNoBinWidthNorm)) {
	Long64_t li;

	for(li=GetNFbins()-1; li>=0; --li) {
	  GetFBinCoords(li,biniter);
	  binvol=(vbsaxis[0]?vbsaxis[0]->GetBinWidth(biniter[0]):1);
	  for(i=1; i<fNDims; ++i) if(vbsaxis[i]) binvol*=vbsaxis[i]->GetBinWidth(biniter[i]);
	  ScaleFBinContent(li,1/binvol);
	}
      }
      delete[] vbsaxis;
    }

    //If the histogram has to be normalized as a conditional PDF
    if(nfix){
      Int_t fcoord, coord;                //Index of the first fixed coordinate
      Int_t **binranges=new Int_t*[fNDims]; //Bin indices
      Double_t scutintbuf;               //Integral value and error buffers

      //If the number of fix dimensions is greater than the total number of
      //dimensions, set the number of fix dimensions to the total number of
      //dimensions
      if(nfix>fNDims) nfix=fNDims;

      cutintbuf=0;

      //Initialized the binranges pointers to NULL
      memset(binranges,0,fNDims*sizeof(Int_t*));


      if(!widths) {
	widths=new Bool_t[fNDims];
	//Loop over non-fixed dimensions
	for(fcoord=fNDims-nfix-1; fcoord>=0; --fcoord) widths[fcoord]=kTRUE;
      }

      //Loop over the fixed dimensions
      for(fcoord=fNDims-nfix; fcoord<fNDims; ++fcoord){
	//Allocate memory for the current fixed dimension
	binranges[fcoord]=new Int_t[2];
	//Initialize the binrange for the current fixed dimension to 1
	*(binranges[fcoord]+1)=*(binranges[fcoord])=1;
	widths[fcoord]=kFALSE;
      }

      //Loop over the bin indices of fixed dimensions
      do{
	//Compute the integral of the conditional PDF for a given fixed bin
	scutintbuf=Integral(binranges,widths);
	//Add the integral value to the total
	cutintbuf+=scutintbuf;

	//If the integral value is not 0
	if(scutintbuf){
	  //Loop over the indices of non-fixed dimensions
	  for(coord=fNDims-nfix-1; coord>=0; --coord) biniter[coord]=1;

	  //Loop over the indices of fixed dimensions
	  for(coord=fNDims-nfix; coord<fNDims; ++coord){
	    //Set the biniter elements that are associated to fixed variables
	    //to the current bin
	    biniter[coord]=*(binranges[coord]);
	  }

	  //Loop over bin indices of non-fixed dimensions
	  do{
	    //Scale the bin value
	    ScaleBinContent(biniter, 1./scutintbuf);
	    biniter[0]++;

	    coord=0;
	    while(biniter[coord]>nbins[coord]){
	      biniter[coord]=1;
	      coord++;

	      if(coord>=fNDims-nfix) break;
	      biniter[coord]++;
	    }
	  } while(coord<fNDims-nfix);
	}
	//Set fcoord to the index of the first fixed dimension
	fcoord=fNDims-nfix;
	*(binranges[fcoord]+1)=++*(binranges[fcoord]);

	while(*(binranges[fcoord])>nbins[fcoord]){
	  *(binranges[fcoord]+1)=*(binranges[fcoord])=1;
	  fcoord++;

	  if(fcoord>=fNDims) break;
	  *(binranges[fcoord]+1)=++*(binranges[fcoord]);
	}
      } while(fcoord<fNDims);

      //Loop over the fixed dimensions
      for(fcoord=fNDims-nfix; fcoord<fNDims; ++fcoord){
	//Delete the array
	delete[] binranges[fcoord];
      }
      delete[] binranges;

      //If histogram has not to be normalized as a conditional PDF
    } else {

      if(fNormFlags&kEventsFilled) {
	cutintbuf=scale;

      } else {
	//Compute the integral of the histogram
	cutintbuf=Integral(NULL,widths);
      }

      //If the integral value is not 0, normalize the PDF
      if (cutintbuf) Scale(1/cutintbuf);
    }

    if(integral) *integral=cutintbuf;
    if(widths) delete[] widths;
    delete[] nbins;
    delete[] biniter;
  }
}

template <typename U> const QHN<U>& QHN<U>::operator=(const QHN<U> &qthn)
{
  Clear();
  QDis::operator=(qthn);
  fNDims=qthn.fNDims;
  fEntries=qthn.fEntries;
  fNBins=qthn.fNBins;
  fAxes=new QAxis*[fNDims];
  for(Int_t i=fNDims-1; i>=0; --i) fAxes[i]=qthn.fAxes[i]?(QAxis*)qthn.fAxes[i]->Clone():NULL;
  fBinContent=(U*)malloc(fNBins*sizeof(U));

  memset(fBinContent,0,fNBins*sizeof(U));
  Long64_t li;
  Long64_t nfbins=qthn.GetNFbins();

  for(li=nfbins-1; li>=0; --li) {
    fBinContent[qthn.GetFBinCoord(li)]=qthn.GetFBinContent(li);
  }
  return *this;
}

template <typename U> void QHN<U>::Reset()
{
  memset(fBinContent,0,fNBins*sizeof(U));
  fEntries=0;
}

template <typename U> QHN<U>* QHN<U>::Projection(const char *name, const Int_t *axes, const Int_t &naxes) const
{
  const Int_t nsdims=fNDims-naxes;

  if(naxes<=0 || !axes || naxes>=fNDims) return NULL;

  Int_t i,j,l,m;

  for(i=0; i<naxes; i++) {
    if(axes[i]<0 || axes[i]>=fNDims) {
      fprintf(stderr,"QHN<U>::Projection: Error: Invalid axis index: %i\n",axes[i]);
      throw 1;
    }
  }

  QHN<U> *th=New(name,name,naxes);

  for(i=0; i<naxes; i++) {
    th->SetAxis(i,fAxes[axes[i]]);
  }

  Int_t *indices=new Int_t[nsdims]; //Axes indices for axes that are not projected
  Int_t *biniter=new Int_t[fNDims]; //Integers used as indices for iteration over bins of original histogram
  Int_t *pbiniter=new Int_t[naxes]; //Integers used as indices for iteration over bins of projected histogram
  U dbuf;
  l=0;

  for(i=0; i<fNDims; i++) {

    m=0;
    for(j=0; j<naxes; j++) if(axes[j]==i) m++;
    if(!m) {
      indices[l]=i;
      l++;
    }
  }

  for(i=0; i<naxes; i++) biniter[axes[i]]=1;

  //Loop over bin indices of projection axes
  do{
    dbuf=0;

    for(i=0; i<nsdims; i++) biniter[indices[i]]=1;

    do{
      dbuf+=GetBinContent(GetBin(biniter));
      biniter[indices[0]]++;
      
      i=0;
      while(biniter[indices[i]]>fAxes[indices[i]]->GetNBins()){
	biniter[indices[i]]=1;
	i++;

	if(i>=nsdims) break;
	biniter[indices[i]]++;
      }

    } while(i<nsdims);

    for(i=0; i<naxes; i++) pbiniter[i]=biniter[axes[i]];
    th->SetBinContent(pbiniter,dbuf);

    biniter[axes[0]]++;

    i=0;
    while(biniter[axes[i]]>fAxes[axes[i]]->GetNBins()){
      biniter[axes[i]]=1;
      i++;

      if(i>=naxes) break;
      biniter[axes[i]]++;
    }

  } while(i<naxes);

  delete[] indices;
  delete[] biniter;
  delete[] pbiniter;

  th->fEntries=fEntries;
  return th;
}

template <typename U> void QHN<U>::ScaleBinContent(const Long64_t &bin, const Double_t &scale)
{
  Long64_t fbin=GetFBin(bin);
  if(fbin!=-1) fBinContent[fbin]*=(U)scale;
}

template <typename U> void QHN<U>::ScaleBinContent(const Int_t *coords, const Double_t &scale)
{
  Long64_t fbin=GetFBin(coords);
  if(fbin!=-1) fBinContent[fbin]*=(U)scale;
}

template <typename U> void QHN<U>::SetAxis(const Int_t &axis, const Int_t &nbins, const Double_t &min, const Double_t &max)
{
  if(axis<0 || axis>=fNDims) {
    fprintf(stderr,"QHN::SetAxis: Error: axis number is invalid\n");
    throw 1;
  }
  if(fAxes[axis]) delete fAxes[axis];
  fAxes[axis]=new QAxis(nbins,min,max);
  ComputeNBins();
  Reset();
}

template <typename U> void QHN<U>::SetAxis(const Int_t &axis, const Int_t &nbins, const Double_t *bins)
{
  if(axis<0 || axis>=fNDims) {
    fprintf(stderr,"QHN::SetAxis: Error: axis number is invalid\n");
    throw 1;
  }

  if(fAxes[axis]) delete fAxes[axis];
  fAxes[axis]=new QAxis(nbins,bins);
  ComputeNBins();
  Reset();
}

template <typename U> void QHN<U>::SetAxis(const Int_t &axis, const Int_t &nbins, const Float_t *bins)
{
  if(axis<0 || axis>=fNDims) {
    fprintf(stderr,"QHN::SetAxis: Error: axis number is invalid\n");
    throw 1;
  }

  if(fAxes[axis]) delete fAxes[axis];
  fAxes[axis]=new QAxis(nbins,bins);
  ComputeNBins();
  Reset();
}

template <typename U> void QHN<U>::SetAxis(const Int_t &axis, const QAxis *anaxis)
{
  if(axis<0 || axis>=fNDims) {
    fprintf(stderr,"QHN::SetAxis: Error: axis number is invalid\n");
    throw 1;
  }
  if(fAxes[axis]) delete fAxes[axis];
  fAxes[axis]=(QAxis*)anaxis->Clone();
  ComputeNBins();
  Reset();
}

#include "QHN_Dict_cxx.h"
