#ifndef _QMASK_
#define _QMASK_

#include "TMath.h"
#include "QList.h"

#include "debugger.h"

class QMask: public QList<char>
{
  public:
    QMask(): QList<char>(){}
    QMask(const QMask &rhs): QList<char>(rhs){}
    virtual ~QMask(){}
    QMask GetComplement(UInt_t nbits) const;
    void Crop();
    void FillMask(UInt_t nbits);
    Bool_t GetBit(UInt_t n) const;
    void Print(const Option_t* opt=NULL) const;
    void SetBit(UInt_t n, Bool_t value);
    const QMask& operator=(const QMask &rhs){QList<char>::operator=(rhs); return *this;}
    const QMask& operator=(const QList<char> &rhs){QList<char>::operator=(rhs); return *this;}
    const QMask& operator&=(const QMask &rhs);
    const QMask& operator|=(const QMask &rhs);
    const QMask& operator^=(const QMask &rhs);
    QMask operator>>(UInt_t n) const;
    QMask operator<<(UInt_t n) const;
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
