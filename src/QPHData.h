#ifndef _QPHDATA_
#define _QPHDATA_

#include <cmath>

#include "Rtypes.h"

class QPHData
{
  public:
    inline QPHData(const Int_t& zero=0): fContent(0), fEntries(0){} //Dummy zero initializer
    inline QPHData(const Double_t& content, const Double_t& entries): fContent(content), fEntries(entries){}
    inline ~QPHData(){}
    inline static QPHData Generator(const Double_t& content){return QPHData(content);}
    inline const Double_t& Content() const{return fContent;}
    inline Double_t& Content(){return fContent;}
    inline const Double_t& Entries() const{return fEntries;}
    inline Double_t& Entries(){return fEntries;}
    inline operator Double_t() const{return (fEntries?fContent/fEntries:0);}
    inline const QPHData& operator=(const QPHData& rhs){fContent=rhs.fContent; fEntries=rhs.fEntries; return *this;}
    inline const QPHData& operator+=(const QPHData& rhs){fContent+=rhs.fContent; fEntries+=rhs.fEntries; return *this;}
    inline const QPHData& operator*=(const Double_t& scale){fContent*=scale; return *this;}
    inline const QPHData& operator/=(const Double_t& scale){fContent/=scale; return *this;}
    inline const QPHData& operator++(){++fContent; ++fEntries; return *this;}
    inline QPHData operator*(const Double_t& scale) const{return QPHData(fContent*scale,fEntries);}
    inline QPHData operator/(const Double_t& scale) const{return QPHData(fContent/content,fEntries);}
    inline void Reset(){fContent=fEntries=0;}
    inline void Set(const Double_t& content, const Double_t& entries){fContent=content; fEntries=entries;}
    inline void SetToOne(){fContent=1; fEntries=1;}
  protected:
    Double_t fContent;
    Double_t fEntries;
  private:
    inline QPHData(const Double_t& content): fContent(content), fEntries(1){}
    inline const QPHData& operator=(const Int_t&){return *this;}
};

class QPHEData: public QHBinWithError
{
  public:
    inline QPHEData(const Int_t& zero=0): fContent(0), fEntries(0), fContent2(0){} //Dummy zero initializer
    inline QPHEData(const Double_t& content, const Double_t& entries, const Double_t& content2): fContent(content), fEntries(entries), fContent2(content2){}
    inline ~QPHEData(){}
    inline static QPHEData Generator(const Double_t& content){return QPHEData(content);}
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
    inline const QPHEData& operator++(){++fContent; ++fEntries; ++fContent2; return *this;}
    inline QPHEData operator*(const Double_t& scale) const{return QPHEData(fContent*scale,fContent2*scale*scale,fEntries);}
    inline QPHEData operator/(const Double_t& scale) const{return QPHEData(fContent/scale,fContent2/(scale*scale),fEntries);}
    inline void Reset(){fContent=fContent2=fEntries=0;}
    inline void Set(const Double_t& content, const Double_t& entries, const Double_t& content2){fContent=content; fEntries=entries; fContent2=content2;}
    inline void SetToOne(){fContent=1; fEntries=1; fContent2=1;}
    inline Double_t SampStd() const{return sqrt(SampVar());}
    inline Double_t SampVar() const{if(fEntries<=1) return 0; const Double_t mean=fContent/fEntries; return (fContent2/fEntries-mean*mean)*fEntries/(fEntries-1);}
    inline Double_t Std() const{return sqrt(Var());}
    inline Double_t Var() const{if(!fEntries) return 0; const Double_t mean=fContent/fEntries; return fContent2/fEntries-mean*mean;}
  protected:
    Double_t fContent;
    Double_t fEntries;
    Double_t fContent2;
  private:
    inline QPHEData(const Double_t& content): fContent(content), fEntries(1), fContent2(content*content){}
    inline const QPHEData& operator=(const Int_t&){return *this;}
};

#endif
