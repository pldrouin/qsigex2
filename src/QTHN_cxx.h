// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>
// Copyright Carleton University

#include "QTHN.h"

template <typename U> QTHN<U>::QTHN(const QTHN &qthn): TNamed(qthn), fNDims(qthn.fNDims), fAxes(NULL), fEntries(qthn.fEntries), fBinContent(NULL), fNBins(qthn.fNBins)
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

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &ndims): TNamed(name,title), fNDims(ndims), fAxes(new QAxis*[ndims]), fBinContent(NULL), fNBins(0)
{
  if(fNDims<=0) {
    fprintf(stderr,"QTHN::QTHN: Error: Number of dimensions is invalid\n");
    throw 1;
  }
  memset(fAxes,0,fNDims*sizeof(QAxis*));
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t &zlow, const Float_t &zhigh): TNamed(name,title), fNDims(0), fAxes(NULL), fBinContent(NULL), fNBins(0)
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
  ComputeNBins();
  Reset();
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): TNamed(name,title), fNDims(0), fAxes(NULL), fBinContent(NULL), fNBins(0)
{
  fNDims=3;
  fAxes=new QAxis*[fNDims];
  fAxes[0]=new QAxis(nbinsx,xlow,xhigh);
  fAxes[1]=new QAxis(nbinsy,ylow,yhigh);
  fAxes[2]=new QAxis(nbinsz,zbins);
  ComputeNBins();
  Reset();
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t &zlow, const Float_t &zhigh): TNamed(name,title), fNDims(0), fAxes(NULL), fBinContent(NULL), fNBins(0)
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
  ComputeNBins();
  Reset();
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t &zlow, const Float_t &zhigh): TNamed(name,title), fNDims(0), fAxes(NULL), fBinContent(NULL), fNBins(0)
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
  ComputeNBins();
  Reset();
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t &xlow, const Float_t &xhigh, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): TNamed(name,title), fNDims(0), fAxes(NULL), fBinContent(NULL), fNBins(0)
{
  fNDims=3;
  fAxes=new QAxis*[fNDims];
  fAxes[0]=new QAxis(nbinsx,xlow,xhigh);
  fAxes[1]=new QAxis(nbinsy,ybins);
  fAxes[2]=new QAxis(nbinsz,zbins);
  ComputeNBins();
  Reset();
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t &ylow, const Float_t &yhigh, const Int_t &nbinsz, const Float_t *zbins): TNamed(name,title), fNDims(0), fAxes(NULL), fBinContent(NULL), fNBins(0)
{
  fNDims=3;
  fAxes=new QAxis*[fNDims];
  fAxes[0]=new QAxis(nbinsx,xbins);
  fAxes[1]=new QAxis(nbinsy,ylow,yhigh);
  fAxes[2]=new QAxis(nbinsz,zbins);
  ComputeNBins();
  Reset();
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t &zlow, const Float_t &zhigh): TNamed(name,title), fNDims(0), fAxes(NULL), fBinContent(NULL), fNBins(0)
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
  ComputeNBins();
  Reset();
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Float_t *xbins, const Int_t &nbinsy, const Float_t *ybins, const Int_t &nbinsz, const Float_t *zbins): TNamed(name,title), fNDims(0), fAxes(NULL), fBinContent(NULL), fNBins(0)
{
  fNDims=3;
  fAxes=new QAxis*[fNDims];
  fAxes[0]=new QAxis(nbinsx,xbins);
  fAxes[1]=new QAxis(nbinsy,ybins);
  fAxes[2]=new QAxis(nbinsz,zbins);
  ComputeNBins();
  Reset();
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t &zlow, const Double_t &zhigh): TNamed(name,title), fNDims(0), fAxes(NULL), fBinContent(NULL), fNBins(0)
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
  ComputeNBins();
  Reset();
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): TNamed(name,title), fNDims(0), fAxes(NULL), fBinContent(NULL), fNBins(0)
{
  fNDims=3;
  fAxes=new QAxis*[fNDims];
  fAxes[0]=new QAxis(nbinsx,xlow,xhigh);
  fAxes[1]=new QAxis(nbinsy,ylow,yhigh);
  fAxes[2]=new QAxis(nbinsz,zbins);
  ComputeNBins();
  Reset();
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t &zlow, const Double_t &zhigh): TNamed(name,title), fNDims(0), fAxes(NULL), fBinContent(NULL), fNBins(0)
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
  ComputeNBins();
  Reset();
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t &zlow, const Double_t &zhigh): TNamed(name,title), fNDims(0), fAxes(NULL), fBinContent(NULL), fNBins(0)
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
  ComputeNBins();
  Reset();
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t &xlow, const Double_t &xhigh, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): TNamed(name,title), fNDims(0), fAxes(NULL), fBinContent(NULL), fNBins(0)
{
  fNDims=3;
  fAxes=new QAxis*[fNDims];
  fAxes[0]=new QAxis(nbinsx,xlow,xhigh);
  fAxes[1]=new QAxis(nbinsy,ybins);
  fAxes[2]=new QAxis(nbinsz,zbins);
  ComputeNBins();
  Reset();
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t &ylow, const Double_t &yhigh, const Int_t &nbinsz, const Double_t *zbins): TNamed(name,title), fNDims(0), fAxes(NULL), fBinContent(NULL), fNBins(0)
{
  fNDims=3;
  fAxes=new QAxis*[fNDims];
  fAxes[0]=new QAxis(nbinsx,xbins);
  fAxes[1]=new QAxis(nbinsy,ylow,yhigh);
  fAxes[2]=new QAxis(nbinsz,zbins);
  ComputeNBins();
  Reset();
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t &zlow, const Double_t &zhigh): TNamed(name,title), fNDims(0), fAxes(NULL), fBinContent(NULL), fNBins(0)
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
  ComputeNBins();
  Reset();
}

template <typename U> QTHN<U>::QTHN(const Char_t *name, const Char_t *title, const Int_t &nbinsx, const Double_t *xbins, const Int_t &nbinsy, const Double_t *ybins, const Int_t &nbinsz, const Double_t *zbins): TNamed(name,title), fNDims(0), fAxes(NULL), fBinContent(NULL), fNBins(0)
{
  fNDims=3;
  fAxes=new QAxis*[fNDims];
  fAxes[0]=new QAxis(nbinsx,xbins);
  fAxes[1]=new QAxis(nbinsy,ybins);
  fAxes[2]=new QAxis(nbinsz,zbins);
  ComputeNBins();
  Reset();
}

template <typename U> void QTHN<U>::Clear(Option_t* option)
{
  TNamed::Clear(option);

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

template <typename U> void QTHN<U>::ComputeNBins()
{
  fNBins=1;
  for(Int_t i=fNDims-1; i>=0; --i) if(fAxes[i]) fNBins*=fAxes[i]->GetNBins()+2;

  if(fBinContent) free(fBinContent);
  fBinContent=(U*)malloc(fNBins*sizeof(U));
}

template <typename U> template <typename V> Long64_t QTHN<U>::FindBin(const V &x0) const
{
  return fAxes[0]->FindBin(x0);
}

template <typename U> template <typename V> Long64_t QTHN<U>::FindBin(const V &x0, const V &x1) const
{
  return (Long64_t)fAxes[1]->FindBin(x1)*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> template <typename V> Long64_t QTHN<U>::FindBin(const V &x0, const V &x1, const V &x2) const
{
  return ((Long64_t)fAxes[2]->FindBin(x2)*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> template <typename V> Long64_t QTHN<U>::FindBin(const V &x0, const V &x1, const V &x2, const V &x3) const
{
  return (((Long64_t)fAxes[3]->FindBin(x3)*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> template <typename V> Long64_t QTHN<U>::FindBin(const V &x0, const V &x1, const V &x2, const V &x3, const V &x4) const
{
  return ((((Long64_t)fAxes[4]->FindBin(x4)*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> template <typename V> Long64_t QTHN<U>::FindBin(const V &x0, const V &x1, const V &x2, const V &x3, const V &x4, const V &x5) const
{
  return (((((Long64_t)fAxes[5]->FindBin(x5)*(fAxes[4]->GetNBins()+2)+fAxes[4]->FindBin(x4))*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> template <typename V> Long64_t QTHN<U>::FindBin(const V &x0, const V &x1, const V &x2, const V &x3, const V &x4, const V &x5, const V &x6) const
{
  return ((((((Long64_t)fAxes[6]->FindBin(x6)*(fAxes[5]->GetNBins()+2)+fAxes[5]->FindBin(x5))*(fAxes[4]->GetNBins()+2)+fAxes[4]->FindBin(x4))*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> template <typename V> Long64_t QTHN<U>::FindBin(const V &x0, const V &x1, const V &x2, const V &x3, const V &x4, const V &x5, const V &x6, const V &x7) const
{
  return (((((((Long64_t)fAxes[7]->FindBin(x7)*(fAxes[6]->GetNBins()+2)+fAxes[6]->FindBin(x6))*(fAxes[5]->GetNBins()+2)+fAxes[5]->FindBin(x5))*(fAxes[4]->GetNBins()+2)+fAxes[4]->FindBin(x4))*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> template <typename V> Long64_t QTHN<U>::FindBin(const V &x0, const V &x1, const V &x2, const V &x3, const V &x4, const V &x5, const V &x6, const V &x7, const V &x8) const
{
  return ((((((((Long64_t)fAxes[8]->FindBin(x8)*(fAxes[7]->GetNBins()+2)+fAxes[7]->FindBin(x7))*(fAxes[6]->GetNBins()+2)+fAxes[6]->FindBin(x6))*(fAxes[5]->GetNBins()+2)+fAxes[5]->FindBin(x5))*(fAxes[4]->GetNBins()+2)+fAxes[4]->FindBin(x4))*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> template <typename V> Long64_t QTHN<U>::FindBin(const V &x0, const V &x1, const V &x2, const V &x3, const V &x4, const V &x5, const V &x6, const V &x7, const V &x8, const V &x9) const
{
  return (((((((((Long64_t)fAxes[9]->FindBin(x9)*(fAxes[8]->GetNBins()+2)+fAxes[8]->FindBin(x8))*(fAxes[7]->GetNBins()+2)+fAxes[7]->FindBin(x7))*(fAxes[6]->GetNBins()+2)+fAxes[6]->FindBin(x6))*(fAxes[5]->GetNBins()+2)+fAxes[5]->FindBin(x5))*(fAxes[4]->GetNBins()+2)+fAxes[4]->FindBin(x4))*(fAxes[3]->GetNBins()+2)+fAxes[3]->FindBin(x3))*(fAxes[2]->GetNBins()+2)+fAxes[2]->FindBin(x2))*(fAxes[1]->GetNBins()+2)+fAxes[1]->FindBin(x1))*(fAxes[0]->GetNBins()+2)+fAxes[0]->FindBin(x0);
}

template <typename U> Long64_t QTHN<U>::FindBin(Double_t const* const &x) const
{
  Long64_t bin=fAxes[fNDims-1]->FindBin(x[fNDims-1]);

  for(Int_t i=fNDims-2; i>=0; --i) {
    bin=bin*(fAxes[i]->GetNBins()+2)+fAxes[i]->FindBin(x[i]);
  }
  return bin;
}

template <typename U> TH1* QTHN<U>::GenTH(const char *name) const
{
  if(fNDims>3) {
    fprintf(stderr,"QTHN::GenTH: Error: Cannot generate a ROOT histogram because QTHN dimension is too high\n");
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

      for(li=0; li<nfbins; li++) {
	GetFBinCoords(li,coords);
	th->SetBinContent(coords[0],fBinContent[li]);
      }
      break;

    case 2:
      th=new TH2D(name,name,fAxes[0]->GetNBins(),fAxes[0]->GetMin(),fAxes[0]->GetMax(),fAxes[1]->GetNBins(),fAxes[1]->GetMin(),fAxes[1]->GetMax());
      if(fAxes[0]->GetBins()) th->GetXaxis()->Set(fAxes[0]->GetNBins(),fAxes[0]->GetBins());
      if(fAxes[1]->GetBins()) th->GetYaxis()->Set(fAxes[1]->GetNBins(),fAxes[1]->GetBins());

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

template <typename U> Long64_t QTHN<U>::GetBin(const Int_t *coords) const
{
  Long64_t bin=coords[fNDims-1];

  for(Int_t i=fNDims-2; i>=0; --i) {
    bin=bin*(fAxes[i]->GetNBins()+2)+coords[i];
  }
  return bin;
}

template <typename U> void QTHN<U>::GetBinCoords(Long64_t bin, Int_t *coords) const
{
  coords[0]=bin%(fAxes[0]->GetNBins()+2);

  for(Int_t i=1; i<fNDims; i++) {
    bin=(bin-coords[i-1])/(fAxes[i-1]->GetNBins()+2);
    coords[i]=bin%(fAxes[i]->GetNBins()+2);
  }
}

template <typename U> void QTHN<U>::GetCorrelationMatrix(TMatrixDSym* covmat, Bool_t width, Bool_t varianceondiag) const
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

template <typename U> void QTHN<U>::GetCovarianceMatrix(TMatrixDSym* covmat, Bool_t width) const
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

template <typename U> void QTHN<U>::GetMeans(Double_t means[], Bool_t width) const
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

template <typename U> Double_t QTHN<U>::Integral(Int_t const* const* binranges, const Bool_t *widths) const
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

template <typename U> Bool_t QTHN<U>::IsFBinIncluded(const Long64_t &fbin, const Int_t *mins, const Int_t *maxs) const
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

template <typename U> const QTHN<U>& QTHN<U>::operator=(const QTHN<U> &qthn)
{
  Clear();
  TNamed::operator=(qthn);
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

template <typename U> void QTHN<U>::Reset()
{
  memset(fBinContent,0,fNBins*sizeof(Double_t));
  fEntries=0;
}

template <typename U> QTHN<U>* QTHN<U>::Projection(const char *name, const Int_t *axes, const Int_t &naxes, QTHN<U> *th) const
{
  const Int_t nsdims=fNDims-naxes;

  if(naxes<=0 || !axes || naxes>=fNDims) return NULL;

  Int_t i,j,l,m;

  for(i=0; i<naxes; i++) {
    if(axes[i]<0 || axes[i]>=fNDims) {
      fprintf(stderr,"QTHN<U>::Projection: Error: Invalid axis index: %i\n",axes[i]);
      throw 1;
    }
  }

  if(th) {
    th->SetNDims(naxes);
    th->SetNameTitle(name,name);

  } else th=new QTHN<U>(name,name,naxes);

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

template <typename U> void QTHN<U>::ScaleBinContent(const Long64_t &bin, const Double_t &scale)
{
  Long64_t fbin=GetFBin(bin);
  if(fbin!=-1) fBinContent[fbin]*=(U)scale;
}

template <typename U> void QTHN<U>::ScaleBinContent(const Int_t *coords, const Double_t &scale)
{
  Long64_t fbin=GetFBin(coords);
  if(fbin!=-1) fBinContent[fbin]*=(U)scale;
}

template <typename U> void QTHN<U>::SetAxis(const Int_t &axis, const Int_t &nbins, const Double_t &min, const Double_t &max)
{
  if(axis<0 || axis>=fNDims) {
    fprintf(stderr,"QTHN::SetAxis: Error: axis number is invalid\n");
    throw 1;
  }
  if(fAxes[axis]) delete fAxes[axis];
  fAxes[axis]=new QAxis(nbins,min,max);
  ComputeNBins();
  Reset();
}

template <typename U> void QTHN<U>::SetAxis(const Int_t &axis, const Int_t &nbins, const Double_t *bins)
{
  if(axis<0 || axis>=fNDims) {
    fprintf(stderr,"QTHN::SetAxis: Error: axis number is invalid\n");
    throw 1;
  }

  if(fAxes[axis]) delete fAxes[axis];
  fAxes[axis]=new QAxis(nbins,bins);
  ComputeNBins();
  Reset();
}

template <typename U> void QTHN<U>::SetAxis(const Int_t &axis, const Int_t &nbins, const Float_t *bins)
{
  if(axis<0 || axis>=fNDims) {
    fprintf(stderr,"QTHN::SetAxis: Error: axis number is invalid\n");
    throw 1;
  }

  if(fAxes[axis]) delete fAxes[axis];
  fAxes[axis]=new QAxis(nbins,bins);
  ComputeNBins();
  Reset();
}

template <typename U> void QTHN<U>::SetAxis(const Int_t &axis, const Int_t &nbins, const Int_t *bins)
{
  if(axis<0 || axis>=fNDims) {
    fprintf(stderr,"QTHN::SetAxis: Error: axis number is invalid\n");
    throw 1;
  }

  if(fAxes[axis]) delete fAxes[axis];
  fAxes[axis]=new QAxis(nbins,bins);
  ComputeNBins();
  Reset();
}

template <typename U> void QTHN<U>::SetAxis(const Int_t &axis, const QAxis *anaxis)
{
  if(axis<0 || axis>=fNDims) {
    fprintf(stderr,"QTHN::SetAxis: Error: axis number is invalid\n");
    throw 1;
  }
  if(fAxes[axis]) delete fAxes[axis];
  fAxes[axis]=(QAxis*)anaxis->Clone();
  ComputeNBins();
  Reset();
}

#include "QTHN_Dict_cxx.h"
