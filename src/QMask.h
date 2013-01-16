// Author: Pierre-Luc Drouin <pldrouin@pldrouin.net>
// All information contained herein is, and remains the property of Pierre-Luc Drouin,
// may be covered by patents and is protected by copyright law. Direct or indirect usage
// of information from this material, including, but not limited to, inspection and distribution,
// is strictly forbidden unless prior permission is obtained from the author.

#ifndef _QMASK_
#define _QMASK_

#include "TMath.h"
#include "QList.h"

#include "debugger.h"

class QMask: public QList<UChar_t>
{
  public:
    QMask(): QList<UChar_t>(){}
    QMask(const QMask &rhs): QList<UChar_t>(rhs){}
    virtual ~QMask(){}
    QMask GetComplement(const UInt_t &nbits) const;
    void Crop();
    void FillMask(const UInt_t &nbits);
    inline Bool_t GetBit(const UInt_t &n) const{Int_t nbytes=n/8; if(nbytes >= Count()) return kFALSE; return (GetArray()[nbytes]>>(n%8))&1;}
    void Print(const Option_t* opt=NULL) const;
    void SetBit(const UInt_t &n, const Bool_t &value);
    const QMask& operator=(const QMask &rhs){QList<UChar_t>::operator=(rhs); return *this;}
    const QMask& operator=(const QList<UChar_t> &rhs){QList<UChar_t>::operator=(rhs); return *this;}
    const QMask& operator&=(const QMask &rhs);
    const QMask& operator|=(const QMask &rhs);
    const QMask& operator^=(const QMask &rhs);
    QMask operator>>(const UInt_t &n) const;
    QMask operator<<(const UInt_t &n) const;
    operator Bool_t() const;

    friend QMask operator&(const QMask &lhs, const QMask &rhs);
    friend QMask operator|(const QMask &lhs, const QMask &rhs);
    friend QMask operator^(const QMask &lhs, const QMask &rhs);
    friend Bool_t operator&&(const QMask &lhs, const QMask &rhs);
    friend Bool_t operator||(const QMask &lhs, const QMask &rhs);
  protected:
  ClassDef(QMask,0) //Generic mask class
};

#include "debugger.h"

#endif
