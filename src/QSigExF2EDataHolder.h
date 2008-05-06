// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Author: Juergen Wendland <mailto:juergen@phas.ubc.ca>

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExF2EDataHolder                                                  //
//                                                                      //
// This class holds static member variables and can be used to pass     //
// information btw QSigExFlux2Events and the minimization fctn. Only    //
// the first instance can modify the internal variables, so there's no  //
// risk of making unwanted changes on these ones with other instances.  //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

#ifndef _QSIGEXF2EDATAHOLDER_
#define _QSIGEXF2EDATAHOLDER_

#include <iostream>
#include <cstdlib>
#include "Rtypes.h"
#include "QDis.h"

#define DEBUG
#define DEBUG2

#include "debugger.h"

using std::cout;
using std::endl;

class TList;
class TString;
template <typename U> class QList;

class QSigExF2EDataHolder 
{
 public:
  QSigExF2EDataHolder(){fNInstances++;}

  QSigExF2EDataHolder(TList* aF2EList){
    fNInstances++;
    // read the TF objects from the tdirectory, use the input names to assign axes names.
    AddMapping(aF2EList);
  }

  Int_t AddMapping(TList* aF2EList);
  Int_t AddFluxIndexing(const Char_t* cardfilename);
  
  Int_t GetNMappings(){return fNEntries;};
  Int_t GetNGroups(){return fNFluxes;}
  Int_t GetAxesIndex(Int_t aMapping, Int_t aAxes){return fAxesIndex[aMapping][aAxes];};
  Int_t GetNAxes(Int_t aMapping){return fNAxes[aMapping];};
  Int_t GetTreeIndex(Int_t aMapping){return fTreeIndex[aMapping];};
  QDis* GetMapping(Int_t aMapping){return fFlux2Events[aMapping];};

  virtual ~QSigExF2EDataHolder(){fNInstances--;}

 private:
  QSigExF2EDataHolder(const QSigExF2EDataHolder &);
  void CheckRights();
  static QDis **fFlux2Events; //!
  static TString** fAxesNames; //!
  static TString* fFluxes; //!

  static Int_t** fAxesIndex; //!

  static Int_t fNTrees; //!
  static Int_t fNEntries; //!
  static Int_t *fNAxes; //!
  static Int_t fNFluxes;
  static Int_t *fTreeIndex; //!
  static Int_t fNInstances; //!

  ClassDef(QSigExF2EDataHolder,1) //Securely passes information from QSigExF2E to a minimization function 
    };

#include "debugger.h"

#endif
