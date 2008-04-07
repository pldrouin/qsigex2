// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QSigExCuts.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"


////////////////////////////////////////////////////////////////////////
//                                                                    //
// QSigExCuts                                                         //
//                                                                    //
// This class loads a set of cuts into the TDirectory structure. The  //
// cuts can be defined as ROOT selection expressions. To simplify     //
// these expressions, equivalences (alias expresions) can be created. //
//                                                                    //
// The output of this class is a set of QNamed objects located in     //
// subfolders "Cuts Expressions" or "Equivalences" in "Cuts"          //
// TDirectory.                                                        //
//                                                                    //
////////////////////////////////////////////////////////////////////////

ClassImp(QSigExCuts)

void QSigExCuts::FormatDir()
{
  //This protected member function gives to the fMyDir TDirectory the
  //structure that is needed to store the information produced by this
  //class. It creates the TDirectory "Event Info" into fMyDir.

  PRINTF2(this,"\tvoid QSigExCuts::FormatDir()\n");
  try{
    //Same thing for Cuts
    if(!(fCutsDir=(TDirectory*)fMyDir->Get("Cuts"))) {
      fCutsDir=fMyDir->mkdir("Cuts","Cuts applied on raw data");
    }

    //If Cuts/Equivalences is neither in memory nor on Disk, create it in
    //memory
    if(!FindObjKey("Equivalences",fCutsDir))
      fCutsDir->mkdir("Equivalences","Equivalences");

    //Same thing for Cuts/Cuts Expressions
    if(!FindObjKey("Cuts Expressions",fCutsDir))
      fCutsDir->mkdir("Cuts Expressions","Cuts Expressions");

  }catch(int e){
    cout << "Exception handled by QSigExCuts::FormatDir\n";
    throw e;
  }
}

void QSigExCuts::CleanDir()
{
  //This function reinitializes the part of the fMyDir TDirectory structure that
  //belongs to QSigExCuts. It removes the TDirectory "Cuts" and calls
  //FormatDir().

  PRINTF2(this,"\tvoid QSigExCuts::CleanDir()\n");
  try{
    DelObjsKeys("Cuts",fMyDir);
    fCutsDir=NULL;
  }catch(int e){
    cout << "Exception handled by QSigExCuts::CleanDir\n";
    throw e;
  }
}

void QSigExCuts::ClearCardBuf()
{
  //This member function clears the internal variables used to store the
  //configuration information of this class in card file format 

  fCutsCard.Clear();
  fEqCard.Clear();
}

void QSigExCuts::LoadCardFile(const Char_t* cardfilename)
{
  //This function reads the card file with filename cardfilename and saves the
  //information related to the cuts and equivalences in QSigexCuts internal
  //member variables.
  //
  //Equivalences: Syntax of card file entries:
  //equivalence	[name]	[expression]
  //where [name] is the name assigned to the equivalence and where
  //[expression] is its expression
  //
  //Cuts: Syntax of card file entries:
  //cut	[name]	[cexpr]
  //where [name] is the name of the cut and where [cexpr] is the
  //conditional expression representing the cut
  //
  //WARNING: DON'T USE SPACES/TABS IN YOUR EQUIVALENCES/CUTS EXPRESSIONS
  //
  //Example: If you want to set an upper limit of 10 on a radius defined by the
  //3 coordinates x,y,z, you can define the following entries in the card file:
  //equivalence	R	sqrt(x*x+y*y+z*z)
  //cut		cutR	R<10

  PRINTF4(this,"\tvoid QSigExCuts::LoadCardFile(const Char_t* cardfilename<",
		  cardfilename,">)\n");
  try{
    if(cardfilename){
      fReader.SetFilename(cardfilename);
      fReader.SetKeyword("equivalence");
      fEqCard = fReader.GetMany(); 
      fReader.SetKeyword("cut");
      fCutsCard = fReader.GetMany();
    }
  }catch(int e){
    cout << "Exception handled by QSigExCuts::LoadCardFile\n";
    throw e;
  }
}

Int_t QSigExCuts::GetEquivalences()
{
  //This protected function loads the equivalences and stores the information
  //into the TDirectory "Cuts/Equivalences" of fMyDir TDirectory structure. The
  //function returns the number of equivalences that have been loaded.

  PRINTF2(this,"\tInt_t QSigExCuts::GetEquivalences()\n")

  try{
    //Field indexes in the "equivalence" area of the card file:
    const Int_t varnameindex = 1;
    const Int_t formulaindex = 2;

    //cd() to the Equivalences directory
    fCutsDir->cd("Equivalences");
    TObject* buf=NULL;

    //Loop over the number of equivalences
    for(Int_t i = 0; i<fEqCard.Count();i++){
      //Create a new object for the list
      buf=new QNamedVar<TString>(fEqCard[i][varnameindex],fEqCard[i][formulaindex]);
      //Add the object to the list
      gDirectory->Add(buf);
    }
    return fEqCard.Count();

  }catch(Int_t e){
    cout <<"Exception handled by QSigExCuts::GetEquivalences\n";
    throw e;
  }
}

Int_t QSigExCuts::GetCuts()
{
  //This protected function loads the cuts and stores the information in the
  //TDirectory "Cuts/Cuts Expressions" of fMyDir TDirectory structure.

  PRINTF2(this,"\tInt_t QSigExCuts::GetCuts()\n")

  try{

    //set up the field indexing for this part of the card file
    const Int_t cutnameindex = 1;
    const Int_t exprindex=2;
    const Int_t expectedcolumns = 3;    //number of fields in this area

    Int_t i;

    fCutsDir->cd("Cuts Expressions");
    TObject* buf2=NULL;

    //Loop over the cut commands
    for(i=0;i<fCutsCard.Count();i++){

      CheckCardNFields(fCutsCard[i].Count(),expectedcolumns,expectedcolumns);

      buf2=new QNamedVar<TString>(fCutsCard[i][cutnameindex].Data(),fCutsCard[i][exprindex].Data());
      gDirectory->Add(buf2);
    }
    return fCutsCard.Count();
  }catch(Int_t e){
    cout << "Exception handled by QSigExCuts::GetCuts\n";
    throw e;
  }

}

Int_t QSigExCuts::Get()
{
  //This function stores the equivalences and cuts of internal member variables
  //in the TDirectory structure, by calling protected functions
  //GetEquivalences() and GetCuts()

  PRINTF2(this,"\tInt_t QSigExCuts::Get()\n");

  try{
    FormatDir();
    GetEquivalences();
    return GetCuts(); 
  }catch(Int_t e){
    cout << "Exception handled by QSigExCuts::Get\n";
    throw e;
  }
}

void QSigExCuts::AddCut(const Char_t* name, const Char_t* expression)
{
  //This function adds a cut in the internal member variables, given its name
  //and its expression

  QList<TString> entry;
  entry+="cut";
  entry+=name;
  entry+=expression;
  fCutsCard+=entry;
  entry.Clear();
}

void QSigExCuts::AddEquivalence(const Char_t* name, const Char_t* expression)
{
  //This function adds an equivalence in the internal member variables, given its name
  //and its expression

  QList<TString> entry;
  entry+="equivalence";
  entry+=name;
  entry+=expression;
  fEqCard+=entry;
  entry.Clear();
}
    
void QSigExCuts::DelCut(const Char_t* name)
{
  //This function removes a cut from the TDirectory structure and from the card file entries in the
  //internal member variables, given its name

  TDirectory *cdir;
  TList listbuf;

  Int_t i;
  const Int_t cutnameindex = 1;

  for(i=0;i<fCutsCard.Count();i++){

    if(!fCutsCard[i][cutnameindex].CompareTo(name)){
      fCutsCard.Del(i);
      i--;
    }
  }

  if(!(fCutsDir=dynamic_cast<TDirectory*>(fMyDir->Get("Cuts")))) return;

  //If there is no "Cuts Expressions" directory, return
  if(!(cdir=dynamic_cast<TDirectory*>(fCutsDir->Get("Cuts Expressions")))) return;

  //Delete all the objects corresponding to name in the cuts directory
  DelObjsKeys(name,cdir);
  //Load the list of remaining objects in the "Cuts Expressions" TDirectory
  GetObjs(&listbuf,cdir);
  //If the list is empty, delete the "Cuts Expressions" directory
  if(!listbuf.GetSize()) DelObjsKeys("Cuts Expressions",fCutsDir);
  //Clear the list without deleting objects
  listbuf.Clear("nodelete");
}

void QSigExCuts::DelEquivalence(const Char_t* name)
{
  //This function removes an equivalence from the TDirectory structure and from the card file entries in the
  //internal member variables, given its name

  TDirectory *edir;
  TList listbuf;

  Int_t i;
  const Int_t varnameindex = 1;

  for(i=0;i<fEqCard.Count();i++){

    if(!fEqCard[i][varnameindex].CompareTo(name)){
      fEqCard.Del(i);
      i--;
    }
  }

  if(!fCutsDir) return;

  //If there is no "Equivalences" directory, return
  if(!(edir=dynamic_cast<TDirectory*>(fCutsDir->Get("Equivalences")))) return;

  //Delete all the objects corresponding to name in the cuts directory
  DelObjsKeys(name,edir);
  //Load the list of remaining objects in the "Cuts Expressions" TDirectory
  GetObjs(&listbuf,edir);
  //If the list is empty, delete the "Cuts Expressions" directory
  if(!listbuf.GetSize()) DelObjsKeys("Equivalences",fCutsDir);
  //Clear the list without deleting objects
  listbuf.Clear("nodelete");
}

void QSigExCuts::ShowCuts()
{
  //This function prints the list of cuts that are stored
  //in the TDirectory "Event Info".
  cout <<"\nCurrent Set of Cuts for Data and PDFs:\n";
  cout <<"------------------------------------------\n";

  fCutsDir->cd("Cuts Expressions");
  TList cuts;
  GetObjs(&cuts,gDirectory);

  for(Int_t j=0;j<cuts.GetSize();j++){
    cout << dynamic_cast<QNamedVar<TString>*>(cuts.At(j))->GetName() << ": " 
      << dynamic_cast<QNamedVar<TString>*>(cuts.At(j))->GetValue() << "\n";
  }
  cuts.Clear();
}

#include "debugger.h"





