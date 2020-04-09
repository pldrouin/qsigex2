// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#include "QPHN.h"

ClassImp(QPHN)

void QPHN::CopyStruct(const QHN_D &qthn)
{
  QHN_D::CopyStruct(qthn);
  const QPHN *qphn=dynamic_cast<const QPHN*>(&qthn);
  fBinEntries=(Double_t*)malloc(fNBins*sizeof(Double_t));

  if(qphn) memcpy(fBinEntries,qphn->fBinEntries,fNBins*sizeof(Double_t));
  else for(Long64_t li=fNBins-1; li>=0; --li) fBinEntries[li]=1;
}

const QPHN& QPHN::operator=(const QPHN &qthn)
{
  QHN_D::operator=(qthn);
  fBinEntries=(Double_t*)malloc(fNBins*sizeof(Double_t));
  memcpy(fBinEntries,qthn.fBinEntries,fNBins*sizeof(Double_t));
  return *this;
}

QHN_D* QPHN::Projection(const char *name, const Int_t *axes, const Int_t &naxes) const
{
  const Int_t nsdims=fNDims-naxes;

  if(naxes<=0 || !axes || naxes>=fNDims) return NULL;

  Int_t i,j,l,m;

  for(i=0; i<naxes; ++i) {
    if(axes[i]<0 || axes[i]>=fNDims) {
      fprintf(stderr,"QPHN::Projection: Error: Invalid axis index: %i\n",axes[i]);
      throw 1;
    }
  }

  QPHN *th=new QPHN(name,name,naxes);

  for(i=0; i<naxes; ++i) {
    th->SetAxis(i,fAxes[axes[i]]);
  }

  Int_t *indices=new Int_t[nsdims]; //Axes indices for axes that are not projected
  Int_t *biniter=new Int_t[fNDims]; //Integers used as indices for iteration over bins of original histogram
  Int_t *pbiniter=new Int_t[naxes]; //Integers used as indices for iteration over bins of projected histogram
  Double_t bcbuf, bebuf;
  Long64_t bin;
  l=0;

  for(i=0; i<fNDims; ++i) {

    m=0;
    for(j=0; j<naxes; ++j) if(axes[j]==i) m++;
    if(!m) {
      indices[l]=i;
      ++l;
    }
  }

  for(i=0; i<naxes; ++i) biniter[axes[i]]=1;

  //Loop over bin indices of projection axes
  for(;;) {
    bcbuf=bebuf=0;

    for(i=0; i<nsdims; ++i) biniter[indices[i]]=1;

    for(;;) {
      bin=GetBin(biniter);
      bcbuf+=fBinContent[bin];
      bebuf+=fBinEntries[bin];

      if(biniter[indices[0]]<fAxes[indices[0]]->GetNBins()) ++(biniter[indices[0]]);
      else {

	for(i=0;;) {
	  biniter[indices[i]]=1;
	  ++i;

	  if(i>=nsdims) goto doneloop1;

	  if(biniter[indices[i]]<fAxes[indices[i]]->GetNBins()) {
	    ++(biniter[indices[i]]);
	    break;
	  }
	}
      }
    }
doneloop1:

    for(i=0; i<naxes; ++i) pbiniter[i]=biniter[axes[i]];
    bin=th->GetBin(pbiniter);
    th->fBinContent[bin]=bcbuf;
    th->fBinEntries[bin]=bebuf;

    if(biniter[axes[0]]<fAxes[axes[0]]->GetNBins()) ++(biniter[axes[0]]);
    else {

      for(i=0;;) {
	biniter[axes[i]]=1;
	++i;

	if(i>=naxes) goto doneloop0;

	if(biniter[axes[i]]<fAxes[axes[i]]->GetNBins()) {
	  ++(biniter[axes[i]]);
	  break;
	}
      }
    }
  }
doneloop0:

  delete[] indices;
  delete[] biniter;
  delete[] pbiniter;

  th->fEntries=fEntries;
  return th;
}

void QPHN::Streamer(TBuffer &R__b)
{ 
  // Stream an object of class QPHN.

  UInt_t R__s, R__c;

  if (R__b.IsReading()) {
    Version_t R__v = R__b.ReadVersion(&R__s, &R__c); if (R__v) { }
    QHN_D::Streamer(R__b);
    R__b.ReadFastArray(fBinEntries,fNBins);

    R__b.CheckByteCount(R__s, R__c, QPHN::IsA());

  } else {
    R__c = R__b.WriteVersion(QPHN::IsA(), kTRUE);
    QHN_D::Streamer(R__b);

    R__b.WriteFastArray(fBinEntries,fNBins);

    R__b.SetByteCount(R__c, kTRUE);
  }
}
