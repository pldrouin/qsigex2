#ifndef _QPHDATA_
#define _QPHDATA_

#include "Rtypes.h"

class QPHN;

class QPHData
{
  public:
    QPHData(const Double_t& content=0, const Double_t& entries=0): fContent(content), fEntries(entries){}
    inline void Add(const Double_t& content){fContent+=content; ++fEntries;}
    inline void Add(const Double_t& content, const Double_t& entries){fContent+=content; fEntries+=entries;}
    inline const Double_t& Content() const{return fContent;}
    inline const Double_t& Entries() const{return fEntries;}
    inline operator Double_t() const{return (fEntries?fContent/fEntries:0);}
    inline const QPHData& operator=(const QPHData& rhs){fContent=rhs.fContent; fEntries=rhs.fEntries; return *this;}
    inline const QPHData& operator++(){++fContent; ++fEntries; return *this;}
    inline const QPHData& operator+=(const QPHData& rhs){fContent+=rhs.fContent; fEntries+=rhs.fEntries; return *this;}
    inline const QPHData& operator+=(const Double_t& w){fContent+=w; fEntries+=w; return *this;}
    inline const QPHData& operator-=(const QPHData& rhs){fContent-=rhs.fContent; fEntries-=rhs.fEntries; return *this;}
    inline const QPHData& operator-=(const Double_t& w){fContent-=w; fEntries-=w; return *this;}
    inline const QPHData& operator*=(const Double_t& scale){fContent*=scale; return *this;}
    inline const QPHData& operator/=(const Double_t& scale){fContent/=scale; return *this;}
    inline void Set(const Double_t& content, const Double_t& entries){fContent=content; fEntries=entries;}
  protected:
    Double_t fContent;
    Double_t fEntries;
  private:
    friend class QPHN;
};

#endif
