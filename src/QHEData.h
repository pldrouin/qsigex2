#ifndef _QHEDATA_
#define _QHEDATA_

#include <cmath>

#include "Rtypes.h"

//QH container for bin data with errors that are user-defined
template <typename U> class QHEData: public QHBinWithError
{
  public:
    inline QHEData(const Int_t& zero=0): fContent(0), fError(0){} //Dummy zero initializer
    inline QHEData(const U& content, const U& error): fContent(content), fError(error){}
    inline ~QHEData(){}
    inline const U& Content() const{return fContent;}
    inline U& Content(){return fContent;}
    inline operator U() const{return fContent;}
    inline const U& Entries() const{return fContent;}
    inline const U& Error() const{return fError;}
    inline const QHEData& operator=(const QHEData& rhs){fContent=rhs.fContent; fError=rhs.fError; return *this;}
    inline const QHEData& operator+=(const QHEData& rhs){fContent+=rhs.fContent; fError=sqrt(fError*fError+rhs.fError*rhs.fError); return *this;}
    inline const QHEData& operator*=(const Double_t& scale){fContent*=scale; fError*=scale; return *this;}
    inline const QHEData& operator/=(const Double_t& scale){fContent/=scale; fError/=scale; return *this;}
    inline const QHEData& operator++(){++fContent; return *this;}
    inline void Reset(){fContent=fError=0;}
    inline void Set(const U& content, const U& error){fContent=content; fError=error;}
    inline void SetPoissonError(){fError=sqrt(fContent);}
    inline void SetToOne(){fContent=1; fError=0;}
  protected:
    U fContent;
    U fError;
  private:
    inline QHEData(const U& content): fContent(content), fError(0){}
    inline const QHEData& operator=(const Int_t&){return *this;}
};

#endif
