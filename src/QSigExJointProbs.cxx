// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QSigExJointProbs.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QSigExJointProbs                                                     //
//                                                                      //
// This class computes joint probability densities of                   //
// NON-CORRELATED VARIABLES. For each flux group, it reads the marginal //
// probability densities from the corresponding TTree in                //
// "Probs/PDFsProbs", multiplies them together and put the result in a  //
// branch of the output TTree "Probs/JointPDFsProbs/JointPDFsProbs".    //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(QSigExJointProbs)

void QSigExJointProbs::FormatDir()
{
  //This protected member function gives to the fMyDir TDirectory the structure
  //that is needed to store the information produced by this class. It creates
  //the TDirectory "PDFs/JointPDFsProbs". 

  PRINTF2(this,"\tvoid QSigExJointProbs::FormatDir()\n")
  try{
    //If Probs is neither in memory nor on Disk, create it in memory
    if(!(fProbsDir=(TDirectory*)fMyDir->Get("Probs"))){
      fProbsDir=fMyDir->mkdir("Probs","Probabilities");
    }

    //If JointPDFsProbs is neither in memory nor on Disk, create it in memory
    if(!(fJointProbsDir=(TDirectory*)fProbsDir->Get("JointPDFsProbs"))){
      fJointProbsDir=fProbsDir->mkdir("JointPDFsProbs","Uncorrelated joint probabilitiy densities from PDFs");
    }
  }catch(int e){
    cout << "Exception handled by QSigExJointProbs::FormatDir\n";
    throw e;
  }
}

void QSigExJointProbs::CleanDir()
{
  //This public member function reinitialize the part of the fMyDir directory
  //structure that belongs to QSigExJointProbs. It removes the TDirectory
  //"PDFs/JointPDFsProbs".

  PRINTF2(this,"\tvoid QSigExJointProbs::CleanDir()\n")
  try{
    if((fProbsDir=(TDirectory*)fMyDir->Get("Probs"))) DelObjsKeys("JointPDFsProbs",fProbsDir);
    fJointProbsDir=fProbsDir=NULL;
  }catch(int e){
    cout << "Exception handled by QSigExJointProbs::CleanDir\n";
    throw e;
  }
}

void QSigExJointProbs::LoadCardFile(const Char_t* cardfilename)
{
  //This function does actually nothing in this class

  PRINTF4(this,"\tvoid QSigExJointProbs::LoadCardFile(const Char_t* cardfilename<",cardfilename,">)\n")

  cardfilename=NULL;

}

int QSigExJointProbs::Get()
{
//This function computes the joint probability densities of NON-CORRELATED
//VARIABLES. For each flux group, it reads the marginal probability densities
//from the corresponding TTree in "Probs/PDFsProbs", multiplies them together
//and put the result in a branch of the output TTree
//"Probs/JointPDFsProbs/JointPDFsProbs". The output TTree branch names are the
//flux group name.
//
//The function returns the number of computed joint probability densities
//
//WARNING: THE INPUT TTREES MUST HAVE BRANCHES OF TYPE DOUBLE_T
//         THE OUTPUT TTREE HAS BRANCHES OF TYPE DOUBLE_T 
  
  PRINTF2(this,"\tint QSigExJointProbs::Get()\n")

  try{
    //Call FormatDir()
    FormatDir();
    //Check if there's a "PDFs/PDFsProbs" TDirectory
    CheckPDFsProbs();
    
    //Get a pointer to the "PDFs/PDFsProbs" TDirectory
    TDirectory* pdfsprobsdir=dynamic_cast<TDirectory*>(fProbsDir->Get("PDFsProbs"));
    
    //Change directory to "Probs/JointPDFsProbs"
    fJointProbsDir->cd();
    
    TList ppdlist; //List of objects in "PDFs/PDFsProbs" TDirectory 
    TList pplist;  //List of PDFs probs TTree objects 
    
    //Fill ppdlist with the list of objects in "PDFs/PDFsProbs"
    GetObjs(&ppdlist,pdfsprobsdir);

    TIter iter(&ppdlist); //Iterator on ppdlist elements 
    
    TObject* pptreeobj; //Pointer to an object in ppdlist
    TTree* pptree=NULL; //Pointer to a PDFs probs TTree
    TObjArray* blist;   //List of branches in a PDFs probs TTree 
    
    Int_t i,j; //Iterator
    
    Int_t lnentries=-1; //Number of entries in the last PDFs probs TTree
    Int_t npptrees; //Number of PDFs probs TTrees
    
    Int_t e; //Iterator
    
    //Create the output joint probs TTree
    TTree* jptree=new TTree("JointPDFsProbs","Joint probabilities from the PDFs");
    
    //Create an array of PDFs probs TTrees branch buffers pointers
    Double_t** ppoaddrs=new Double_t*[ppdlist.GetSize()];
    //Create an array to contain the number of branches in each PDFs probs TTree
    Int_t* nppb=new Int_t[ppdlist.GetSize()];
    
    //Initialize the PDFs probs index
    j=-1;

    //Loop while there are objects left in the ppdlist
    while((pptreeobj=iter())){

      //If the current object is derived from (or is) a TTree 
      if((pptree=dynamic_cast<TTree*>(pptreeobj))){
	//Increment the PDFs probs index
	j++;
	//Add the current TTree to the list of PDFs probs TTrees
	pplist.AddLast(pptree);
        //Get the list of branches of the current TTree
	blist=pptree->GetListOfBranches();
	//Get the number of branches in this list
	nppb[j]=blist->GetEntries();
	//Create an array of branch buffers for this TTree
	ppoaddrs[j]=new Double_t[nppb[j]];

        //Loop over the branches
	for(i=0;i<nppb[j];i++){
	  //Set the branch address to the appropriate buffer address
	  dynamic_cast<TBranch*>(blist->At(i))->SetAddress((ppoaddrs[j])+i);
	}

	//If the number of entries in this TTree is different from the number of
	//entries in the last TTree, throw an exception 
	if(lnentries!=-1 && pptree->GetEntries()!=lnentries){
	  cout << "Error with PDFs probabilities Ttrees: The number of entries is not the same\n";
	  throw 1;
	}
	//Update the last number of entries
	lnentries=(int)(pptree->GetEntries());
	//Add a new branch to the output TTree and set its address to the one of
	//the first branch of the current PDFs probs TTree
	jptree->Branch(pptree->GetName(),ppoaddrs[j],((TString)(pptree->GetName())+"/D"));
      }
    }
    
    //Get the number of PDFs probs TTrees
    npptrees=pplist.GetSize();

    cout << "Now compute joint probs\n";
    QProgress progress(lnentries,1000);

    //Loop over the entries in the PDFs probs TTrees
    for(e=0;e<lnentries;e++){
      //if(e<10) cout << "Entry " << e << endl;
      progress(e);

      //Loop over the PDFs probs TTrees
      for(i=0;i<npptrees;i++){
	//if(e<10) cout << "\tFlux group: " << i << endl;
	//Load the entry for the current PDFs probs TTree
	dynamic_cast<TTree*>(pplist.At(i))->GetEntry(e);
	//if(e<10) cout << "\t\tInputs: " << (ppoaddrs[i])[0] << "\t";

	//Loop over the current PDFs probs TTree branches, beginning at second branch
	for(j=1;j<nppb[i];j++){
	  //if(e<10) cout << (ppoaddrs[i])[j] << "\t";
	  //Multiply the marginal probability densities together
	  (ppoaddrs[i])[0]*=(ppoaddrs[i])[j];
	}
	//if(e<10) cout << "\n\t\tJointProb: " << (ppoaddrs[i])[0] << endl;
      }
      //Add the entry to the output TTree
      jptree->Fill();
    }
    progress(e,kTRUE);
    cout << "\tdone\n\n";
    
    //Clear the output TTree branches addresses 
    QTTreeUtils::ClearBranchesAddresses(jptree);

    //Loop over the input TTrees
    for(i=0;i<npptrees;i++){
      //Clear the current input TTree branches addresses
      QTTreeUtils::ClearBranchesAddresses(dynamic_cast<TTree*>(pplist.At(i)));
      //Delete the buffers for he current input TTree
      delete[] ppoaddrs[i];
    }
    //Delete the array of input TTrees buffer arrays
    delete[] ppoaddrs;
    //Delete the array of input TTrees number of branches
    delete[] nppb;
    
    //Clear the input TTrees list without deleting its objects
    pplist.Clear("nodelete");
    //Clear the "Probs/PDFsProbs" objects list and delete its objects
    ppdlist.Clear();
    
    //Return the number of joint probability densities written
    return lnentries;
  }catch(Int_t e){
    cout << "Exception handled by QSigExJointProbs::Get\n";
    throw e;
  }
}

void QSigExJointProbs::CheckPDFsProbs() const
{
  //This protected function checks if a "Probs/PDFsPRobs" TDirectory exists

  PRINTF2(this,"\tvoid QSigExJointProbs::CheckPDFsProbs() const\n")

  if(!FindObjKey("PDFsProbs",fProbsDir)){
    cout << "Error: There's no \"Probs/PDFsProbs\" TDirectory in fMyDir\n";
    throw 1;
  }
}

#include "debugger.h"





