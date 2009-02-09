// Author: Pierre-Luc Drouin <http://www.physics.carleton.ca/~pldrouin>
// Copyright Carleton University

#ifndef _QTHNDL_
#define _QTHNDL_

#include <cstdio>
#include <cstdlib>
#include "QTHN.h"

template <typename U> class QTHNDL: public QTHN<U>
{
  public:
    QTHNDL(): QTHN<U>(), fFBins(NULL){};
    QTHNDL(const QTHNDL &qthn);
    QTHNDL(const QTHN<U> &qthn): QTHN<U>(qthn), fFBins(NULL){ComputeNBins();}
    QTHNDL(const Char_t *name, const Char_t *title, Int_t ndims): QTHN<U>(name,title,ndims), fFBins(NULL){}
    virtual ~QTHNDL(){Clear();}
    void AddBinContent(const Long64_t &bin, const U &w=1);
    void Clear(Option_t* option="");
    TObject* Clone(const char* newname = NULL) const{QTHNDL<U>* ret=new QTHNDL(*this); if(newname) ret->SetName(newname); return ret;}
    Long64_t GetFBin(const Int_t *coords) const{return fFBins[QTHN<U>::GetBin(coords)];}
    Long64_t GetFBin(const Long64_t &bin) const;
    const U& GetBinContent(const Long64_t &bin) const;
    const QTHNDL<U>& operator=(const QTHNDL<U> &qthn);
    const QTHNDL<U>& operator=(const QTHN<U> &qthn){Clear(); QTHN<U>::operator=(qthn); ComputeNBins(); return *this;}
    QTHN<U>* Projection(const char *name="_pd", const Int_t *axes=NULL, Int_t naxes=0, QTHN<U> *th=NULL) const;
    void Reset();
    void SetBinContent(const Long64_t &bin, const U &content);
    void SetFBinContent(const Long64_t &fbin, const U &content);
  protected:
    void ComputeNBins();
    Long64_t *fFBins; //!

    ClassDef(QTHNDL,1) //Multidimensional histogram template class with double list for bin/filled bin indices
};

#include "QTHNDL_cxx.h"

#endif