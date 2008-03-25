// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QSigExCleanData.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExCleanData                                                      //
//                                                                      //
// This class applies the cuts defined in the TDirectory structure and  //
// applies them on raw data events to produce a TTree containing clean  //
// data. The new TTree is placed in "Event Info" TDirectory             //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(QSigExCleanData)

void QSigExCleanData::FormatDir()
{
  //This protected member function gives to the fMyDir TDirectory the structure
  //that is needed to store the information produced by this class. It creates
  //the TDirectory "Event Info" 

  PRINTF2(this,"\tvoid QSigExCleanData::FormatDir()\n")
  try{
    //If Event Info is neither in memory nor on Disk, create it in memory
    if(!(fEInfoDir=(TDirectory*)fMyDir->Get("Event Info"))){
      fEInfoDir=fMyDir->mkdir("Event Info","Event Information");
    }

  }catch(int e){
    cout << "Exception handled by QSigExCleanData::FormatDir\n";
    throw e;
  }
}

void QSigExCleanData::CleanDir()
{
  //This public member function reinitializes the part of the fMyDir TDirectory
  //structure that belongs to QSigExCleanData. It removes the TDirectory
  //"Event Info"

  PRINTF2(this,"\tvoid QSigExCleanData::CleanDir()\n")
  try{
    DelObjsKeys("Event Info",fMyDir);
    fEInfoDir=NULL;
  }catch(int e){
    cout << "Exception handled by QSigExCleanData::CleanDir\n";
    throw e;
  }
}

void QSigExCleanData::ClearCardBuf()
{
  //This member function clears the internal variables used to store the
  //configuration information of this class in card file format 

  fDataFName="";
  fDataOName="";
}

void QSigExCleanData::LoadCardFile(const Char_t* cardfilename)
{
  //This public member function reads the card file with filename cardfilename
  //and saves the information related to the filename and object name of the raw
  //data TTree into internal member variables of this class. 
  //
  //Syntax of card file entry:
  //DATA_FILE	[filename]	[objectname]
  //where [filename] is the filename of the ROOT file and [objectname] is is the
  //raw data TTree object name relative to the root of the ROOT file. 
  //
  //Example: To read a TTree named "mytree"  in subfoler "myfolder" of ROOT file
  //"myfile.root", use the following line in the card file: 
  //DATA_FILE	myfile.root	myfolder/mytree

  PRINTF4(this,"\tvoid QSigExCleanData::LoadCardFile(const Char_t* cardfilename<",cardfilename,">)\n")
  try{
    if(cardfilename){
      fReader.SetFilename(cardfilename);
      QList<TString> cardbuf;     //Buffer filled by fReader output

      //Load in the Data File name:
      fReader.SetKeyword("DATA_FILE");
      cardbuf=fReader.Get();

      CheckCardNFields(cardbuf.Count(),3,3);

      fDataFName=cardbuf[1];
      fDataOName=cardbuf[2];

    }
  }catch(int e){
    cout << "Exception handled by QSigExCleanData::LoadCardFile\n";
    throw e;
  }
}

void QSigExCleanData::SetDataFilename(const Char_t* fname)
{
  //This function sets the filename of the raw data file to fname

  PRINTF4(this,"\tvoid QSigExCleanData::SetDataFilename(const Char_t* fname<",fname,">)\n")

  fDataFName=fname;
}

void QSigExCleanData::SetDataObjectName(const Char_t* oname)
{
  //This function sets the object name, relative to the file root, of the TTree
  //object containing the raw data. See QSigExCleanData::LoadCardFile() for an
  //example of object name.


  PRINTF4(this,"\tvoid QSigExCleanData::SetDataObjectName(const Char_t* oname<",oname,">)\n")

  fDataOName=oname;
}

Int_t QSigExCleanData::Get()
{
  //This function uses the cuts and the equivalences stored in fMyDir directory
  //structure to apply cuts on raw data events and to produce a TTree in "Event
  //Info" that contains only the clean events.

  PRINTF2(this,"\tInt_t QSigExCleanData::Get()\n")

  try{
    FormatDir();
    //Do some checks
    CheckCuts();
    CheckIfDataFilename();
    CheckIfDataObjectName();
    
    Int_t i,j;
    TDirectory* cutsdir=dynamic_cast<TDirectory*>(fMyDir->Get("Cuts"));

    //Loads the equivalence TDirectory in memory
    cutsdir->cd("Equivalences");
    TDirectory* eqdir=dynamic_cast<TDirectory*>(cutsdir->Get("Equivalences"));
    TList equiv; //TList for equivalences
    //Load the objects of Equivalences TDirectory into memory
    GetObjs(&equiv,eqdir);
    QList<TString> eqnames; //List of equivalences names
    QList<TString> eqformulas; //List of equivalences formulas
    TString buf; //TString buffer

    //Loop over the equivalences
    for(j=0;j<equiv.GetSize();j++){
      
      //Add the equivalence name
      eqnames+=(TString)dynamic_cast<TNamed*>(equiv.At(j))->GetName();
      
      //Add the equivalence expression
      eqformulas+=(TString)dynamic_cast<TNamed*>(equiv.At(j))->GetTitle();
    }
    //Clear the list of equivalences and delete its objects from memory
    equiv.Clear();


    TCut ccuts;             //TCut object that will contain combined cuts
    //Change directory to "Cuts Expressions"
    cutsdir->cd("Cuts Expressions");
    TList cuts; //List of simple cuts
    //Get the list of cuts in current TDirectory ("Cuts Expressions")
    GetObjs(&cuts,gDirectory);

    //Loop over the cuts
    for(i=0;i<cuts.GetSize();i++){
      //Read the cut expression
      buf=dynamic_cast<TNamed*>(cuts.At(i))->GetTitle();
      
      //Loop over the equivalences
      for(j=0;j<eqnames.Count();j++){
	//Substitute equivalence names by their expression
	QFormulaUtils::ReplaceVar(&buf,eqnames[j],eqformulas[j]);
      }
      //Add the cut to the combined cuts
      ccuts+=buf.Data();
    }
    //Clear the list of cuts and delete its objects from memory
    cuts.Clear();

    //Open the data file
    TFile datafile(fDataFName.Data(),"READ");

    TTree* readtree; //Raw data tree
    //cd into "Envent Info" directory
    fEInfoDir->cd();                        
    TTree* cdtree; //Clean Data Tree
    Int_t nrawentries; //Number of raw entries that have been read
    Char_t** bbufs=NULL; //Branch data buffers
    TBranch *branch; //TBranch buffer
    TLeaf *lastleaf; //TLeaf buffer
    Int_t nbranches; //Number of branches in data TTree
    Int_t nleaves;  //Number of leaves in a specific branch

    //Try to load the raw data TTree
    readtree=dynamic_cast<TTree*>(datafile.Get(fDataOName));
    //If nothing has been loaded, throw an exception
    if(!readtree){
      cout << "Error: TTree '" << fDataOName << "' in file '" << fDataFName
	   << "' has not been found\n";
      throw 1;
    }
    
    //Get the number of branches in the raw data TTree
    nbranches=readtree->GetListOfBranches()->GetEntries();
    //If the number of branches is greater than 0
    if(nbranches>0){
      //Define an array of pointers
      bbufs=new Char_t*[nbranches];
      //Initialize its addresses to 0x0
      memset(bbufs,0,nbranches*sizeof(Char_t**));
      i=0;

      //Iterator on raw data TTree branches
      TIter next(readtree->GetListOfBranches());

      //Loop over the branches of raw data TTree
      while((branch=dynamic_cast<TBranch*>(next()))){
	//Read the number of leaves in the branch
	nleaves=branch->GetListOfLeaves()->GetEntries();
	//If the number of leaves is greater than 0
	if(nleaves>0){
	  //Get a pointer to the last leaf
	  lastleaf=dynamic_cast<TLeaf*>(branch->GetListOfLeaves()->At(nleaves-1));
	  //Create a buffer that is sufficiently large to contain 1 event
	  bbufs[i]=new Char_t[lastleaf->GetOffset()+lastleaf->GetLenType()];
	  //Set the branch address to the new buffer address
	  branch->SetAddress(bbufs[i]);
	  //Increment the branch counter
	  i++;
	}
      }
    }
    
    //Add the number of entries of readtree to nrawentries
    nrawentries=(Int_t)readtree->GetEntries();
    
    //Copy the entries of readtree that pass the cuts into cdtree
    cdtree=readtree->CopyTree(ccuts);
    
    //Clear the branch buffers properly
    if(bbufs){
      for(i=0;i<nbranches;i++) delete[] bbufs[i];
      delete[] bbufs;
    }
    
    cout << (int)(cdtree->GetEntries()) << "/" << nrawentries <<
      " entries (" << 100.*cdtree->GetEntries()/nrawentries <<
      " %) have passed the cuts\n";
    
    //Close the raw data file
    datafile.Close();
    
    //return the number of clean data entries
    return (Int_t)cdtree->GetEntries();
      
  }catch(Int_t e){
    cout << "Exception handled by QSigExCleanData::Get\n";
    throw e;
  }
}

void QSigExCleanData::CheckIfDataFilename() const
{
  //This protected member function check if a raw data filename has been
  //assigned to the instance and throw an exception if it's not the case.

  if(!fDataFName.Length()){ 
    cout <<"No data filename specified!\n";
    throw 1;
  }
}

void QSigExCleanData::CheckIfDataObjectName() const
{
  //This protected member function check if a raw data object name has been
  //assigned to the instance and throw an exception if it's not the case.

  if(!fDataOName.Length()){ 
    cout <<"No data object name specified!\n";
    throw 1;
  }
}

void QSigExCleanData::CheckCuts() const
{
  //Check if the TDirectories "Cuts Expressions" and "Equivalences" exist

  TDirectory* cutsdir=dynamic_cast<TDirectory*>(fMyDir->Get("Cuts"));

  if(!cutsdir || !(cutsdir->FindObject("Cuts Expressions") ||
		   cutsdir->FindKey("Cuts Expressions")) ||
     !(cutsdir->FindObject("Equivalences") ||
       cutsdir->FindKey("Equivalences"))) {
    cout <<"Problem with cuts:  cuts not input correctly in card file\n"
         <<"or ReadCuts() was not called before it was needed\n";
    throw 1;
  }
}

#include "debugger.h"





