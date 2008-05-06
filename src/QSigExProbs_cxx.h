// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QSigExProbs.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

//The following line is needed to generate the dependency file
#include "QSigExProbs.h"

////////////////////////////////////////////////////////////////////////
//                                                                    //
// QSigExProbs                                                        //
//                                                                    //
// This class uses the marginal PDFs stored in "PDFs" TDirectory to   //
// compute the marginal probability densities of the events stored in //
// "Event Info" TDirectory. Each PDF can be an instance of any class  //
// derived from QDis class. The output of this function is a set      //
// of TTree objects (one per flux group) located in "Probs/PDFsProbs" //
// TDirectory.                                                        //
//                                                                    //
////////////////////////////////////////////////////////////////////////

template <typename U> void QSigExProbs<U>::FormatDir()
{
  //This protected member function gives to the fMyDir TDirectory the structure
  //that is needed to store the information produced by this class. It creates
  //directories "Probs" and "Probs/PDFsProbs". 

  PRINTF2(this,"\tvoid QSigExProbs<U>::FormatDir()\n")
  try{
    //If Probs is neither in memory nor on Disk, create it in memory
    if(!(fProbsDir=(TDirectory*)fMyDir->Get("Probs"))){
      fProbsDir=fMyDir->mkdir("Probs","Probabilities");
    }

    //If PDFsProbs is neither in memory nor on Disk, create it in memory
    if(!(fPDFsProbsDir=(TDirectory*)fProbsDir->Get("PDFsProbs"))){
      fPDFsProbsDir=fProbsDir->mkdir("PDFsProbs","Probability densities from PDFs");
    }

  }catch(int e){
    cout << "Exception handled by QSigExProbs<U>::FormatDir\n";
    throw e;
  }
}

template <typename U> void QSigExProbs<U>::CleanDir()
{
  //This public member function reinitializes the part of the fMyDir directory
  //structure that belongs to QSigExProbs. It removes TDirectory PDFsProbs.

  PRINTF2(this,"\tvoid QSigExProbs<U>::CleanDir()\n")
  try{
    if((fProbsDir=(TDirectory*)fMyDir->Get("Probs"))) DelObjsKeys("PDFsProbs",fProbsDir);
    fPDFsProbsDir=fProbsDir=NULL;
  }catch(int e){
    cout << "Exception handled by QSigExProbs<U>::CleanDir\n";
    throw e;
  }
}

template <typename U> void QSigExProbs<U>::LoadCardFile(const Char_t* cardfilename)
{
  //This function does actually nothing in this class
  
  PRINTF4(this,"\tvoid QSigExProbs<U>::LoadCardFile(const Char_t* cardfilename<",cardfilename,">)\n")

  cardfilename=NULL;
}

template <typename U> Int_t QSigExProbs<U>::Get()
{
  //This function loads the marginal PDFs (QDis derived objects) stored in the
  //PDFs subfolders of "PDFs" TDirectory and the clean TTree located in "Event
  //Info" TDirectory. It loops over the events in the TTree and computes for
  //each one the marginal probability densities (one probability density per
  //PDF). The link between the TTree branch names and the PDFs coordinates is
  //made via the input names/expressions contained in the "Inputs" directories
  //of PDFs directories. For each flux group, this function produces a TTree
  //object (named according to the flux group name) that contains the marginal
  //probability densities. These objects are located in "Probs/PDFSProbs"
  //TDirectory.
  //
  //WARNING: THE INPUT TTREE USED BRANCHES MUST BE OF CLASS TEMPLATE TYPE
  //         THE OUTPUT TTREES HAVE BRANCHES OF TYPE DOUBLE_T
  //
  //WARNING: WHEN A PDF INPUT IS AN EXPRESSION (NOT A SIMPLE TTREE BRANCH
  //         NAME), THIS EXPRESSION MUST NOT CONTAIN MORE THAN 4 DIFFERENT
  //         VARIABLES, DUE TO A LIMITATION OF TFORMULA CLASS
  //
  //The function returns the number of computed probability densities 

  //Maximum number of variables in a TFormula
  const Int_t MAXNVARTFORMULA=4;
  //Maximum dimensions of a PDF
  const Int_t MAXNDIMPDF=3;

  try{
    //Call FormatDir()
    FormatDir();
    //Check if there's a "PDFs" TDirectory
    CheckPDFs();
    //Check if there's a "Event Info" TDirectory
    CheckCleanData();
    
    Int_t i,j,k,l,m; //iterators
    
    TList eilist; //list of objects in "Event Info"
    //Fill eilist with the list of objects in "Event Info" TDirectory
    GetObjs(&eilist,dynamic_cast<TDirectory*>(fMyDir->Get("Event Info")));
    
    TTree* cdtree=NULL; //Pointer to a clean data TTree

    //Loop over the objects in "Event Info" TDirectory
    for(i=0;i<eilist.GetSize();i++){
      //If the current object is a TTree, get a pointer to this object and exit
      //the loop
      if((cdtree=dynamic_cast<TTree*>(eilist.At(i)))) break;
    }

    //If no TTree has been found in "Event Info" TDirectory, throw an exception
    if(!cdtree){
      cout << "Error: There's no PDF in the TDirectory\n";
      throw 1;
    }
    
    //Get a pointer to the "PDFs" TDirectory
    TDirectory* pdfsdir=dynamic_cast<TDirectory*>(fMyDir->Get("PDFs"));
    
    //Change directory to "Probs/PDFsProbs" TDirectory
    fPDFsProbsDir->cd();
    
    TList fgroups; //List of flux group directories in "PDFs" TDirectory
    //Fill fgroups with the list of flux group directories in pdfsdir
    GetDirs(&fgroups,pdfsdir);
    TList sgroups; //List of syst. group directories in a flux group TDirectory
    TDirectory *pfgroup; //Pointer to a flux group TDirectory
    TDirectory *psgroup; //Pointer to a syst. group TDirectory
    TDirectory *idir; //Pointer to an "Inputs" TDirectory
    TTree** fgroup=NULL; //Pointer to a flux group probability densities TTree
                         //pointers array
    Int_t nfgroups; //Number of flux groups
    TList pdfsm; //List of PDF directories in a syst. group TDirectory
    TDirectory *pdfdir; //Pointer to a PDF TDirectory in a syst. group 
    QDis *pdf; //Pointer to a PDF (QDis derived object)
    TString strbuf,strbuf2; //2 TString buffers
    
    TList pdfs; //List of PDF pointers (QDis pointers)
    Int_t npdfs; //Number of PDFs
    TList anlist; //List of objects in an "Inputs" TDirectory
    QNamedVar<TString>* qnvbuf; //QNamedVar buffer for inputs;
    QList<QList<TString> > axesnames; //List of coordinate names for all PDFs
    Double_t **poaddrs=NULL;  //PDFs outputs addresses
    U **piaddrs;    //PDFs inputs addresses
    QList<TString> cistr; //Combined inputs strings (when an input is a formula)
    Int_t ncinputs; //Number of combined inputs
    U **ciaddrs;    //Combined inputs inputs addresses
    U **coaddrs;    //Combined intpus outputs addresses
    U dummy=0;      //Dummy address for a PDF with less than 3 dimensions
                          //and for TFormula with less than 4 variables
    TFormula **cformulas; //Array of combined formulas
    Int_t nevents; //Number of events in input and output TTree objects
    Int_t e; //iterator
    TObjArray* blist; //List of branches in the input TTree
    //list of variable names in a TFormula
    const Char_t* labels[MAXNVARTFORMULA+1]={"x","y","z","t","!"};
    U* fbuf; //U buffer

    Bool_t candelete; //Was a particular PDF already loaded in memory?

    //Get the number of flux group directories
    nfgroups=fgroups.GetSize();

    //Create an array to contain probability densities TTree pointers
    fgroup=new TTree*[nfgroups];

    //Loop over the flux group directories in "PDFs"
    for(i=0;i<nfgroups;i++){
      //Get a pointer to the current flux group TDirectory
      pfgroup=dynamic_cast<TDirectory*>(fgroups.At(i));
      //Create a new TTree of probability densities for this fux group
      fgroup[i]=new TTree(pfgroup->GetName(),pfgroup->GetName());

      //Fill sgroups with the list of syst. group directories in pfgroup
      GetDirs(&sgroups,pfgroup);

      //Loop over the syst. group directories in the current flux group
      for(j=0;j<sgroups.GetSize();j++){
        //Get a pointer to the current syst. group TDirectory
	psgroup=dynamic_cast<TDirectory*>(sgroups.At(j));

	//Fill pdfsn wuth the list of PDFs directories in psgroup
	GetDirs(&pdfsm,psgroup);

	//Loop over the PDFs directories
	for(k=0;k<pdfsm.GetSize();k++){
	  //Get a pointer to the current PDF TDirectory
	  pdfdir=dynamic_cast<TDirectory*>(pdfsm.At(k));
	  //Is the PDF already loaded in memory?
	  candelete=pdfdir->FindKey(pdfdir->GetName()) &&
	    !pdfdir->FindObject(pdfdir->GetName());

	  //If there's a PDF in the PDF TDirectory
	  if((pdf=dynamic_cast<QDis*>(pdfdir->Get(pdfdir->GetName())))){
	    //If the PDF was already loaded, allow the list pdfs to delete it
	    if(candelete) pdf->SetBit(kCanDelete);
	    //Add the PDF to the list of pdfs
	    pdfs.AddLast(pdf);
            //Resize the array of PDFs outputs to the number of PDFs
	    poaddrs=(Double_t**)realloc(poaddrs,pdfs.GetSize()*sizeof(Double_t*));
	    //Assign a new Double_t to the new cell 
	    poaddrs[pdfs.GetSize()-1]=new Double_t;
	    //Put the name of the PDF into strbuf
	    strbuf=pdf->GetName();
	    //For the current output TTree, add a branch named according to the
	    //PDF name and for which the buffer address is the PDF output
	    fgroup[i]->Branch(strbuf,poaddrs[pdfs.GetSize()-1],(strbuf+"/D"));
	  //Else if there's no PDF in the PDF TDirectory, throw an exception
	  }else{
	    cout << "Error: TDirectory" << pdfdir->GetName() << 
	      "doesn't contain a QDis object of the same name\n";
	    throw 1;
	  }

	  //If there's an "Inputs" TDirectory in the current PDF TDirectory
	  if((idir=dynamic_cast<TDirectory*>(pdfdir->Get("Inputs")))){
	    //Fill anlist with the list of objects in the "Inputs" TDirectory
	    GetObjs(&anlist,idir);

	    //Add a PDF to the list of PDF inputs
	    axesnames.RedimList(axesnames.Count()+1);
	    //Initialize the number of valid inputs to 0
	    m=0;

	    //Loop over the objects in the "Inputs" TDirectory
	    for(l=0;l<anlist.GetSize();l++){

	      //If a valid input is found
	      if((qnvbuf=dynamic_cast<QNamedVar<TString>*>(anlist.FindObject(((TString)"Input ")+
		      (Long_t)l)))){
		//Increment the number of valid inputs
		m++;
		//Add the input title to the list of inputs of the current PDF
		axesnames.GetLast()+=qnvbuf->GetValue();
	      }
	    }

	    //If the number of inputs is not equal to the number of dimensions
	    //of the current PDF, throw an exception
	    if(m!=((QDis*)(pdfs.At(pdfs.GetSize()-1)))->GetDimension()){
	      cout << "Error: The number of inputs in TDirectory " <<
	       	pdfdir->GetName() << " doesn't match the dimension of the QDis object\n";
	      throw 1;
	    }
	    //Clear anlist and delete its objects 
	    anlist.Clear();

	  //Else if there's no "Inputs" TDirectory in the current PDF
	  //TDirectory, throw an exception
	  }else{
	    cout << "Error: TDirectory" << pdfdir->GetName() << 
	      "doesn't contain an Inputs directory\n";
	    throw 1;
	  }
	}
	//Clear the list of PDFs directories without deleting its objects 
	pdfsm.Clear("nodelete");
      }
      //Clear the list of syst. group directories without deleting its objects
      sgroups.Clear("nodelete");


    }

    //Clear the clean data TTree branch addresses 
    QTTreeUtils::ClearBranchesAddresses(cdtree);
    //Get the list of branch pointers of cdtree
    blist=cdtree->GetListOfBranches();
    
    //Create an array of U pointers to hold PDFs inputs addresses
    piaddrs=new U*[pdfs.GetSize()*MAXNDIMPDF];
    //Initialize the combined inputs inputs array pointer to NULL
    ciaddrs=NULL;
    //Initialize the combined inputs outputs array pointer to NULL
    coaddrs=NULL;
    //Clear the list of combined inputs formulas strings
    cistr.Clear();
    //Initialize the "combined inputs TFormula" pointers array pointer to NULL
    cformulas=NULL;

    //Loop over the PDFs
    for(j=0;j<pdfs.GetSize();j++){
      //Get a pointer to the current PDF
      pdf=(QDis*)(pdfs.At(j));
      //      cout << "PDF " << pdf->GetName() << endl;

      //Loop over the dimensions of the current PDF
      for(k=0;k<pdf->GetDimension();k++){

	//If the clean data TTree has a branch having the same name than the PDF
	//current input (simple input)
	if(cdtree->GetBranch(axesnames[j][k])){
	  //Create a new U buffer for the clean data TTree if needed, set
	  //the appropriate branch address to the buffer address and bind this
	  //address to the current PDF input
	  piaddrs[j*MAXNDIMPDF+k]=QTTreeUtils::AssignBAddress(axesnames[j][k].Data(),cdtree,&dummy);
	  //	  cout << "Simple: " << axesnames[j][k]<< "\t" << piaddrs[j*MAXNDIMPDF+k] << endl;

	//Else if the input has a different name that is not a branch name
	}else{

	  //If the input name is found among the existing combined inputs
	  //formulas strings
	  if(cistr.Find(axesnames[j][k]).Count()){
	    //Bind the existing address of the appropriate combined input output to the current PDF input 
	    piaddrs[j*MAXNDIMPDF+k]=coaddrs[cistr.Find(axesnames[j][k])[0]];
	    //	    cout << "Combined (already exist): " << axesnames[j][k] << "\t" << piaddrs[j*MAXNDIMPDF+k] << endl;

	  //Else if it's a new combined input
	  }else{
	    //Copy the input name into strbuf2
	    strbuf2=axesnames[j][k];
	    //Initialize the TFormula variable name index
	    m=0;

	    //Loop over the clean TTree branches
	    for(l=0;l<blist->GetEntries();l++){
	      //Copy strbuf2 string into strbuf
	      strbuf=strbuf2;
	      //Replace, in the input expression, all occurence of the current
	      //branch name by the current TFormula variable name
	      //If the string has changed (if the input expression contains the
	      //branch name)
	      if(QFormulaUtils::ReplaceVar(&strbuf2,blist->At(l)->GetName(),labels[m])){

		//If the number of variables exceed the number of variables
		//allowed by a TFormula, throw an exception 
		if(m>=MAXNVARTFORMULA){
		  cout << "Error in QSigExProbs<U>::Get: Too much variables in expression for a TFormula\n";
		  throw 1;
		}
		//Increment the variable index
		m++;
		//Resize the array of combined inputs intputs if needed
		ciaddrs=(U**)realloc(ciaddrs,MAXNVARTFORMULA*(cistr.Count()+1)*sizeof(U*));
		//If there's no buffer assigned to the clean data TTree branch
		//corresponding to the current variable, create one. Assign the
		//buffer address to the current variable of the new combined
		//input.
		ciaddrs[MAXNVARTFORMULA*cistr.Count()+m-1]=QTTreeUtils::AssignBAddress(blist->At(l)->GetName(),cdtree,&dummy);
	      }
	    }

	    //Loop over the remaining possible variables in a TFormula 
	    for(l=m;m && l<MAXNVARTFORMULA;l++){
	      //Assign a dummy buffer address for this non-existing variable to
	      //the current combined input input
	      ciaddrs[MAXNVARTFORMULA*cistr.Count()+l]=&dummy;
	    }

	    //Resize the array of combined inputs outputs addresses
	    coaddrs=(U**)realloc(coaddrs,(cistr.Count()+1)*sizeof(U*));
	    //Resize the array of combined inputs TFormulas addresses
	    cformulas=(TFormula**)realloc(cformulas,(cistr.Count()+1)*sizeof(TFormula*));
	    //Assign a new U for the new combined input output
	    coaddrs[cistr.Count()]=new U;
	    //The current PDF input address is set to the combined inputs output
	    //address 
	    piaddrs[j*MAXNDIMPDF+k]=coaddrs[cistr.Count()];
	    //A new TFormula object is created using the modified combined input
	    //expression
	    cformulas[cistr.Count()]=new TFormula("cformula",strbuf2.Data());
	    //The original combined input expression is added to the list of
	    //known combined inputs expressions
	    cistr+=axesnames[j][k];
	    //	      cout << "Combined (new): " << axesnames[j][k] << "\t" << piaddrs[j*MAXNDIMPDF+k] << endl;
	  }
	}
      }

      //Loop over the remaining possible dimensions of a PDF
      for(k=pdf->GetDimension();k<MAXNDIMPDF;k++){
	//Assign a dummy buffer address for this non-existing dimension to the
	//current PDF input addresses
	piaddrs[j*MAXNDIMPDF+k]=&dummy;
      }
    }
    
    
    cout << "\nNow compute probs\n";

    //Copy the number of combined inputs into ncinputs
    ncinputs=cistr.Count();
    //Get the number of events in the input TTree
    nevents=(int)cdtree->GetEntries();
    //Copy the number of PDFs into npdfs
    npdfs=pdfs.GetSize();

    QProgress progress(nevents,1000);

    //Loop over the events in the clean TTree
    for(e=0;e<nevents;e++){

      progress(e);
      //if(e<10) cout << "Entry " << e << endl;
      //Read the current entry
      cdtree->GetEntry(e);

      //Loop over the combined inputs
      for(j=0;j<ncinputs;j++){
	//cout << "\tInput " << j << endl;
	//Compute the current combined input output using the appropriate
	//TFormula object and combined input inputs addresses
	*(coaddrs[j])=cformulas[j]->Eval(*(ciaddrs[MAXNVARTFORMULA*j]),
	    *(ciaddrs[MAXNVARTFORMULA*j+1]),
	    *(ciaddrs[MAXNVARTFORMULA*j+2]),*(ciaddrs[MAXNVARTFORMULA*j+3]));
      }

      //Loop over the PDFs
      for(j=0;j<npdfs;j++){
	//Get a pointer to the current PDF
	pdf=(QDis*)(pdfs.At(j));
	//if(e<10) cout << "\tPDF " << pdf->GetName() << endl;
	//if(e<10) cout << "\t\tInput: " << *(piaddrs[j*MAXNDIMPDF]) << "\t" << *(piaddrs[j*MAXNDIMPDF+1]) << "\t" << *(piaddrs[j*MAXNDIMPDF+2]) << endl;
	//Compute the current PDF output using QDis::ProbDensity and the
	//current PDF inputs addresses
	*(poaddrs[j])=pdf->ProbDensity(*(piaddrs[j*MAXNDIMPDF]),
	    *(piaddrs[j*MAXNDIMPDF+1]),*(piaddrs[j*MAXNDIMPDF+2]));
	//if(e<10) cout << "\t\tProb: " << *(poaddrs[j]) << endl;
      }

      //Loop over the flux groups
      for(j=0;j<nfgroups;j++){
	//	cout << "\tFlux group " << fgroup[j]->GetName() << endl;
	//Fill the current flux group TTree
	fgroup[j]->Fill();
      }
    }
    progress(e,kTRUE);

    cout << "\tdone\n\n";
    
    //Get the list of branches in the clean TTree
    blist=cdtree->GetListOfBranches();
    
    //Loop over the clean TTree branches
    for(j=0;j<blist->GetEntries();j++){
      //Get the address of the current branch
      fbuf=(U*)(dynamic_cast<TBranch*>(blist->At(j))->GetAddress());
      //If this address is not NULL, delete the buffer
      if(fbuf) delete fbuf;
      //Set the branch address to NULL
      dynamic_cast<TBranch*>(blist->At(j))->SetAddress(NULL);
    }

    //Loop over the flux groups
    for(j=0;j<nfgroups;j++){
      //Clear the branch addresses for the current flux group TTree
      QTTreeUtils::ClearBranchesAddresses(fgroup[j]);
    }

    //Delete the PDFs outputs buffers and the PDFs outputs buffers pointers
    //array
    DelPArray(poaddrs,npdfs,1);
    //Delete the PDFs intputs buffers pointers array
    delete[] piaddrs;

    //Delete the combined inputs TFormulas and the combined inputs TFormulas
    //pointers array
    DelPArray(cformulas,ncinputs,1);

    //Delete the combined inputs outputs buffers and the combined inputs outputs
    //buffers pointers array
    DelPArray(coaddrs,ncinputs,1);

    //Delete the combined inputs inputs buffers pointers array
    free(ciaddrs);

    //Clear the list of PDFs and delete its objects
    pdfs.Clear();
    //Dekete the flux group TTree pointers array
    delete[] fgroup;

    //Delete the list of flux group directories without deleting its objects
    fgroups.Clear("nodelete");
    //Delete the list of objects in the "Event Info" TDirectory and delete its
    //objects 
    eilist.Clear();

    //Returun the number of read/written events
    return nevents;
  }catch(Int_t e){
    cout << "Exception handled by QSigExProbs<U>::Get\n";
    throw e;
  }
}

template <typename U> void QSigExProbs<U>::CheckPDFs() const
{
  //This protected function check if "PDFs" TDirectory exists
  if(!FindObjKey("PDFs",fMyDir)){
    cout << "Error: There's no PDF in fMyDir\n";
    throw 1;
  }
}

template <typename U> void QSigExProbs<U>::CheckCleanData() const
{
  //This protected function check if "Event Info" TDirectory exists
  if(!FindObjKey("Event Info",fMyDir)){
    cout << "Error: There's no clean data in fMyDir\n";
    throw 1;
  }
}

#include "debugger.h"
