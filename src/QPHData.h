#ifndef _QPHDATA_
#define _QPHDATA_

#include <cmath>

#include "Rtypes.h"

class QPHData
{
  public:
    QPHData(const Double_t& content=0, const Double_t& entries=0): fContent(content), fEntries(entries){}
    inline const Double_t& Content() const{return fContent;}
    inline Double_t& Content(){return fContent;}
    inline const Double_t& Entries() const{return fEntries;}
    inline Double_t& Entries(){return fEntries;}
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

    ClassDef(QPHData,1) //Container for Double_t profile data
};

class QPHEData
{
  public:
    QPHEData(const Double_t& content=0, const Double_t& entries=0): fContent(content), fEntries(entries), fContent2(content*content){}
    QPHEData(const Double_t& content, const Double_t& entries, const Double_t& content2): fContent(content), fEntries(entries), fContent2(content2){}
    inline const Double_t& Content() const{return fContent;}
    inline Double_t& Content(){return fContent;}
    inline const Double_t& Content2() const{return fContent2;}
    inline Double_t& Content2(){return fContent2;}
    inline const Double_t& Entries() const{return fEntries;}
    inline Double_t& Entries(){return fEntries;}
    inline operator Double_t() const{return (fEntries?fContent/fEntries:0);}
    inline Double_t Error() const{return SampStd();}
    inline const QPHEData& operator=(const QPHEData& rhs){fContent=rhs.fContent; fEntries=rhs.fEntries; fContent2=rhs.fContent2; return *this;}
    inline const QPHEData& operator+=(const QPHEData& rhs){fContent+=rhs.fContent; fEntries+=rhs.fEntries; fContent2+=rhs.fContent2; return *this;}
    inline const QPHEData& operator*=(const Double_t& scale){fContent*=scale; fContent2*=scale*scale; return *this;}
    inline const QPHEData& operator/=(const Double_t& scale){fContent/=scale; fContent2/=scale*scale; return *this;}
    inline void Set(const Double_t& content, const Double_t& entries, const Double_t& content2){fContent=content; fEntries=entries; fContent2=content2;}
    inline Double_t SampStd() const{return sqrt(SampVar());}
    inline Double_t SampVar() const{if(fEntries<=1) return 0; const Double_t mean=fContent/fEntries; return (fContent2/fEntries-mean*mean)*fEntries/(fEntries-1);}
    inline Double_t Std() const{return sqrt(Var());}
    inline Double_t Var() const{if(!fEntries) return 0; const Double_t mean=fContent/fEntries; return fContent2/fEntries-mean*mean;}
  protected:
    Double_t fContent;
    Double_t fEntries;
    Double_t fContent2;
  private:

    ClassDef(QPHEData,1) //Container for Double_t profile data with variance
};

#endif
