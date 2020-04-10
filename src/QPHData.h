#ifndef _QPHDATA_
#define _QPHDATA_

#include <cmath>

#include "Rtypes.h"

class QPHN;

class QPHData
{
  public:
    QPHData(const Double_t& content=0, const Double_t& entries=0): fContent(content), fEntries(entries){}
    //inline void Add(const Double_t& content){fContent+=content; ++fEntries;}
    //inline void Add(const Double_t& content, const Double_t& w){fContent+=w*content; fEntries+=w;}
    inline const Double_t& Content() const{return fContent;}
    inline const Double_t& Entries() const{return fEntries;}
    inline operator Double_t() const{return (fEntries?fContent/fEntries:0);}
    inline const QPHData& operator=(const QPHData& rhs){fContent=rhs.fContent; fEntries=rhs.fEntries; return *this;}
    inline const QPHData& operator+=(const QPHData& rhs){fContent+=rhs.fContent; fEntries+=rhs.fEntries; return *this;}
    inline const QPHData& operator*=(const Double_t& scale){fContent*=scale; return *this;}
    inline const QPHData& operator/=(const Double_t& scale){fContent/=scale; return *this;}
    inline void Set(const Double_t& content, const Double_t& entries){fContent=content; fEntries=entries;}
  protected:
    Double_t fContent;
    Double_t fEntries;
  private:
    friend class QPHN;
};

class QPHEData: public QPHData
{
  public:
    QPHEData(const Double_t& content=0, const Double_t& content2=0, const Double_t& entries=0): QPHData(content, entries), fContent2(content2){}
    inline const Double_t& Content2() const{return fContent2;}
    inline Double_t SampStd() const{return sqrt(SampVar());}
    inline Double_t SampVar() const{if(fEntries<=1) return 0; const Double_t mean=fContent/fEntries; return (fContent2/fEntries-mean*mean)*fEntries/(fEntries-1);}
    inline Double_t Std() const{return sqrt(Var());}
    inline Double_t Var() const{if(!fEntries) return 0; const Double_t mean=fContent/fEntries; return fContent2/fEntries-mean*mean;}
    inline const QPHEData& operator=(const QPHEData& rhs){QPHData::operator=(rhs); fContent2=rhs.fContent2; return *this;}
    inline const QPHEData& operator+=(const QPHEData& rhs){QPHData::operator+=(rhs); fContent2+=rhs.fContent2; return *this;}
    inline const QPHEData& operator*=(const Double_t& scale){QPHData::operator*=(scale); fContent2*=scale*scale; return *this;}
    inline const QPHEData& operator/=(const Double_t& scale){QPHData::operator/=(scale); fContent2/=scale*scale; return *this;}
    inline void Set(const Double_t& content, const Double_t& content2, const Double_t& entries){QPHData::Set(content, entries); fContent2=content2;}
  protected:
    Double_t fContent2;
  private:
    friend class QPHN;
};

#endif
