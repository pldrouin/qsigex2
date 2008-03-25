// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QSigExGaussCor.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

////////////////////////////////////////////////////////////////////////
//                                                                    //
// QSigExGaussCor                                                     //
//                                                                    //
// This class, used with class QSigExGCJointProbs, allows to          //
// compute joint probability densities of flux groups considering the //
// correlations between their variables. To achieve this, it creates  //
// for each variable x a mapping function y(x) that has a gaussian    //
// distribution, such that the distribution of y(x) variables for     //
// each flux group is multi-gaussian. The covariance matrix of y(x)   //
// variables being computed for each flux group, it is possible to    //
// compute the joint probabilities using the multi-gaussian equation. //
//                                                                    //
// The class QSigExGaussCor loops over the flux groups in "PDFs"      //
// TDirectory and finds the ones that have a clean TTree named        //
// "Event Info". For each flux group, it finds QSigExDisTH PDFs and   //
// produces a y(x) mapping function with gaussian distribution (TF1   //
// object). Then, it uses the set of y(x) functions and the clean     //
// TTree belonging to the flux group to compute the covariance matrix //
// (TMatrixDSym object).  Matrix indexes are identified using TNamed  //
// objects in the PDFs directories.                                   //
//                                                                    //
////////////////////////////////////////////////////////////////////////

ClassImp(QSigExGaussCor)

void QSigExGaussCor::FormatDir()
{
  //This protected member function gives to the fMyDir TDirectory the structure
  //that is needed to store the information produced by this class. It doesn't
  //create any objects actually. 

}

void QSigExGaussCor::CleanDir()
{
  //This public member function reinitializes the part of the fMyDir directory
  //structure that belongs to QSigExGaussCor. It removes the y(x) mapping
  //functions and matrix indexes from PDFs directories and it removes covariance
  //matrices in flux group directories.

  PRINTF2(this,"\tvoid QSigExGaussCor::CleanDir()\n")
  try{
    //Check if the "PDFs" TDirectory exists
    CheckPDFs();
    TDirectory *pdfsdir,*fdir,*sdir; //"PDFs", flux group and systematic group
				     //TDirectory pointers
    //Get a pointer to "PDFs" TDirectory
    pdfsdir=dynamic_cast<TDirectory*>(fMyDir->Get("PDFs"));
    TList flist,slist,plist; //flux group, syst. group and PDFs TDirectory lists
    Int_t i,j,k; //iterators
    //Fill flist with the list of flux group directories in "PDFs" 
    GetDirs(&flist,pdfsdir);

    //Loops over the flux group directories in "PDFs"
    for(i=0;i<flist.GetSize();i++){
      //Get a pointer to the current flux group TDirectory
      fdir=dynamic_cast<TDirectory*>(flist.At(i));
      //Delete covariance matrix object/keys in the flux group TDirectory
      DelObjsKeys("TMatrixDSym",fdir);
      //Fill slist with the list of systematic groups directories in fdir
      GetDirs(&slist,fdir);

      //Loops over the syst. group directories in fdir
      for(j=0;j<slist.GetSize();j++){
	//Get a pointer to the current syst. group TDirectory
	sdir=dynamic_cast<TDirectory*>(slist.At(j));
	//Fill plist with the list of PDF directories in sdir
	GetDirs(&plist,sdir);

	//Loops over the PDF directories in sdir
	for(k=0;k<plist.GetSize();k++){
	  //Delete y(x) function object/keys in the PDF TDirectory
	  DelObjsKeys("GaussMapping",dynamic_cast<TDirectory*>(plist.At(k)));
	  //Dekete matrix index object/keys in the PDf TDirectory
	  DelObjsKeys("TMatrixIndex",dynamic_cast<TDirectory*>(plist.At(k)));
	}
	//Clear the PDF directories list without deleting the objects
	plist.Clear("nodelete");
      }
      //Clear the syst. group directories list without deleting the objects
      slist.Clear("nodelete");
    }
    //Clear the flux group directories list without deleting the objects
    flist.Clear("nodelete");
    
  }catch(int e){
    cout << "Exception handled by QSigExGaussCor::CleanDir\n";
    throw e;
  }
}

void QSigExGaussCor::LoadCardFile(const Char_t* cardfilename)
{
  //This is an empty function in this class actually
  PRINTF4(this,"\tvoid QSigExGaussCor::LoadCardFile(const Char_t* cardfilename<",
      cardfilename,">)\n")
  cardfilename=NULL;
}

Int_t QSigExGaussCor::Get()
{
  //This function uses the "Event Info" TTree objects in flux group directories
  //and QSigExDisTH* PDFs in pdf directories to create TMatrixDSym covariance matrices
  //in flux group directories after to have produced "GaussMapping" TF1 objects
  //(y(x) mapping functions) and "TMatrixIndex" TNamed objects (matrix indexes)
  //in PDF directories

  PRINTF2(this,"\tInt_t QSigExGaussCor::Get()\n")

  //Call FormatDir()
  FormatDir();

  QSigExDisTH *qthbuf; //PDF buffer
  TH1 *thbuf; //TH1 buffer
  TF1 *tfbuf; //TF1 buffer
  TNamed* nbuf; //TNamed buffer
  TTree* eitree; //Pointer to "Event Info" TTree in flux group directories
  QList<TString> vars; //List of correlated variable names
  //"PDFs", flux group, syst. group, pdf and "Inputs" TDirectory pointers
  TDirectory *pdfsdir,*fdir,*sdir,*pdir,*idir;
  Bool_t tloaded; //Was the "Event Info" TTree already loaded in memory?
  QSigExStaticList* staticlist; //Used to pass information to y(x) functions
  
  //flux group, syst. group, pdf and "Inputs" directories list
  TList flist,slist,plist,ilist;
  TList mflist; //y(x) functions list
  Int_t i,j,k,l,m; //iterators
  
  TString strbuf; //TString buffer
  Double_t dbuf1,dbuf2; //Double_t buffers
  Float_t *farray; //"Event Info" TTree branches addresses
  Double_t *m1array; //Array of first moments*n
  Double_t *m2array; //Array of sum(xi*xj)
  Double_t *bufarray; //Buffer for y(x) function values
  Int_t ibuf1,ibuf2; //2 Int_t buffers
  Int_t nentries,nignored; //Number of entries in "Event Info" TTree
  TMatrixDSym* matrix; //pointer to a covariance matrix
  Bool_t ignore; //flag that indicate if an event must be ignored

  //Get a pointer to the "PDFs" TDirectory
  pdfsdir=dynamic_cast<TDirectory*>(fMyDir->Get("PDFs"));
  //Fill flist with the list of flux groups directories in the "PDFs" TDirectory
  GetDirs(&flist,pdfsdir);

  //Loop over the flux group directories in "PDFs"
  for(i=0;i<flist.GetSize();i++){
    //Clear the list of correlated variable names
    vars.Clear();
    //Get a pointer to the current flux group TDirectory
    fdir=dynamic_cast<TDirectory*>(flist.At(i));
      
    //Is the "Event Info" TTree in fdir already loaded in memory?
    tloaded=fdir->FindKey("Event Info") && !fdir->FindObject("Event Info");
    //If there's an "Event Info" TTree in the flux group TDirectory

    if((eitree=dynamic_cast<TTree*>(fdir->Get("Event Info")))){
      //Fill slist with the list of systematic groups directories in fdir
      GetDirs(&slist,fdir);
      
      //Loop over the systematic groups in fdir
      for(j=0;j<slist.GetSize();j++){
	//Get a pointer to the current syst. group TDirectory 
	sdir=dynamic_cast<TDirectory*>(slist.At(j));
	//Fill plist with the list of PDFs directories in sdir
	GetDirs(&plist,sdir);
	
	//Loop over the PDFs directories in sdir
	for(k=0;k<plist.GetSize();k++){
	  //Get a pointer to the current PDF TDirectory in sdir
	  pdir=dynamic_cast<TDirectory*>(plist.At(k));
	  //Fill strbuf with the name of y(x) function
	  strbuf="GaussMapping_";
	  strbuf+=pdir->GetName();

	  //If there's an "Inputs" TDirectory in pdir and if there's a QSigExDisTH
	  //object with the same name than pdir in pdir (save pointers)
	  if((idir=dynamic_cast<TDirectory*>(pdir->Get("Inputs"))) &&
	      (qthbuf=dynamic_cast<QSigExDisTH*>(pdir->Get(pdir->GetName()))) &&
	      qthbuf->GetDimension()==1){
	    //Fill ilist with the list of objects in ilist TDirectory
	    GetObjs(&ilist,idir);

	    //If the list is not empty and if the first object in the list is a
	    //TNamed object
	    if(ilist.GetSize() && (nbuf=dynamic_cast<TNamed*>(ilist.At(0)))){
	      //Resize the list of correlated variable names
	      vars.RedimList(vars.Count()+1);
	      //Add the title of the TNamed object to the list of correlated variables
	      vars[vars.Count()-1]=nbuf->GetTitle();

	    //Else if there's no TNamed object in "Inputs" TDirectory, throw an exception  
	    }else{
	      cout << "Error in QSigExGaussCor::Get: There's no Inputs directory for pdf " << qthbuf->GetName() << "\n";
	      throw 1;
	    }
	    //Clear the list of inputs and delete its owned objects from memory
	    ilist.Clear();
	    //Create an instamce of QSigExStaticList
	    staticlist=new QSigExStaticList;
	    //Get the TH1F object from the current PDF
	    thbuf=dynamic_cast<TH1F*>(qthbuf->GetObject());
	    //Add the TH1F object to the static list
	    staticlist->Add(thbuf);
	    cout << "Building TF1... \n";
	    //Get the low edge of the first PDF bin
	    dbuf1=thbuf->GetBinLowEdge(1);
	    //Get the high edge if the last PDF bin
	    dbuf2=thbuf->GetBinLowEdge(thbuf->GetNbinsX())+thbuf->GetBinWidth(thbuf->GetNbinsX());
	    //	      dbuf1=eitree->GetMinimum(vars[vars.Count()-1]);
	    //	      dbuf2=eitree->GetMaximum(vars[vars.Count()-1]);
	    //Create a new TF1 object (y(x) function) with same range than the
	    //PDF. A pointer to QSigExGaussMapping function is passed.
	    tfbuf=new TF1(strbuf,QSigExGaussMapping,dbuf1,dbuf2,0);
	    //Set the number of points to 10 times the number of bins in the PDF
	    tfbuf->SetNpx(10*thbuf->GetNbinsX());
	    //Store the array of (x,y) points in the internal member variables
	    //of the TF1 object
	    tfbuf->Save(dbuf1,dbuf2,0,0,0,0);
	    //Deallocate the function pointer
	    tfbuf->SetFunction(NULL);
	    cout << "Min/Max: " << dbuf1 << "/" << dbuf2 << "\n";

	    //Add the new y(x) function to the list of functions
	    mflist.AddLast(tfbuf);
	    cout << "done\n";
	    //Add the TF1 object to the PDF TDirectory
	    pdir->Add(tfbuf);
	    //Clear the static list without deleting its objects
	    staticlist->Clear("nodelete");
	    //Delete the static list instance
	    delete staticlist;
	    strbuf="";
	    //Assign the new correlated variable index to strbuf (string)
	    strbuf+=vars.Count()-1;
	    //Create a new TNamed object with name "TMatrixIndex" and with the
	    //correlated variable index string as title
	    nbuf=new TNamed("TMatrixIndex",strbuf.Data());
	    //Add it to the PDF TDirectory
	    pdir->Add(nbuf);
	  }
	}
	  //Clear the list of PDF directories without deleting its objects
	  plist.Clear("nodelete");
      }
        //Clear the list of syst. directories without deleting its objects
	slist.Clear("nodelete");

	//Assign the number of correlated variables to ibuf1
	ibuf1=vars.Count();
	//Create an array of ibuf1 cells to hold "Event Info" TTree branch
	//addresses
	farray=new Float_t[ibuf1];
	//Create an array of ibuf1 cells to hold a set of y(x) values 
	bufarray=new Double_t[ibuf1];
	//Create an array of ibuf1 cells to hold the first moments x n
	m1array=new Double_t[ibuf1];
	//Initialize this array to 0
	memset(m1array,0,ibuf1*sizeof(Double_t));

	//Number of cells in the packed notation of a triangular matrix ibuf1 x inbuf1 
	ibuf2=ibuf1*(ibuf1+1)/2;
	//Create an array to hold sum(xi*xj) values
	m2array=new Double_t[ibuf2];
	//Initialize the array to 0
	memset(m2array,0,ibuf2*sizeof(Double_t));
	//Clear the branches of th "Event Info" TTree
	QTTreeUtils::ClearBranchesAddresses(eitree);

	//Loop over the number of correlated variables
	for(j=0;j<ibuf1;j++){
	  //Give to the appropriate branch of "Event Info" TTree a buffer address
	  eitree->SetBranchAddress(vars[j],farray+j);
	}
	
	//Set nentries to the number of events in "Event Info" TTree
	nentries=(Int_t)(eitree->GetEntries());
	//Initialize the number of ignored events
	nignored=0;

	cout << "Now compute covariance matrix for flux group " << fdir->GetName() << endl; 
	QProgress progress(nentries,1000);
	//Loop over the entries in "Event Info" TTree
	for(j=0;j<nentries;j++){
          progress(j);
       
	  //Load the entry in the branch buffers
	  eitree->GetEntry(j);
	  //Set ignore to false
	  ignore=kFALSE;

	  //Loop over the correlated variables (first index)
	  for(k=0;k<ibuf1;k++){
	    //Evaluate y(x) for this variable
	    bufarray[k]=((TF1*)mflist.At(k))->Eval(farray[k]);
	    //Get the domain range of this y(x) function
	    ((TF1*)mflist.At(k))->GetRange(dbuf1,dbuf2);

	    //If the x value for the current event is outside this range, set
	    //ignore to true and stop looping on variables
	    if(farray[k]<dbuf1 || farray[k]>dbuf2){
	      //	      cout << "pdf " << k << "\t" << farray[k] << "\t" << dbuf1 << "\t" << dbuf2 << endl;
	      ignore=kTRUE;
	      break;
	    }
	  }

	  //If ignore is true, increment nignored and go to next entry
	  if(ignore){
	    nignored++;
	    continue;
	  }

	  //Set the packed notation index to 0
	  m=0;

	  //Loop over the correlated variables (first index) for this event
	  for(k=0;k<ibuf1;k++){
	    //Add the y(x) value to the moment x n value of this variable
	    m1array[k]+=bufarray[k];

	    //Loop over the correlated variables (second index), from the
	    //variable k to the end
	    for(l=k;l<ibuf1;l++){
	      //Add xk*xl value to the sum(xk*xl) array 
	      m2array[m]+=bufarray[k]*bufarray[l];
	      //Increment the packed notaiton index
	      m++;
	    }
	  }
	}
	progress(j,kTRUE);
	cout << "\tdone\n";

	cout << nignored << " events outside of range for flux group " << fdir->GetName() << "\n";

	//Clear the "Event Info" TTree branch addresses
	QTTreeUtils::ClearBranchesAddresses(eitree);

	//If "Event Info" TTree has been loaded by this function, delete it
	if(tloaded) eitree->Delete();

	//Delete the branch buffers
	delete[] farray;
	//Delete the y(x) values buffer
	delete[] bufarray;
	
	cout << "The covariance matrix of flux group " << fdir->GetName() << " has been computed for " << nentries-nignored << "/" << nentries << " events\n";

	//Create an new square TMatrixDSym matrix
	matrix=new TMatrixDSym(ibuf1);

	//Initiazlie the packed notation index
	m=0;

	//Loop over the correlated variables (first index)
	for(k=0;k<ibuf1;k++){

	  //Loop over the correlated variables (second index), from the
	  //variable k to the end
	  for(l=k;l<ibuf1;l++){
	    //Assign the covariance value to symmetric cells in the covariance matrix
	    (*matrix)(k,l)=(*matrix)(l,k)=(m2array[m]-m1array[k]*m1array[l]/(nentries-nignored))/(nentries-nignored);
	    //Increment the packed notation index
	    m++;
	  }
	}

	//Print the covariance matrix (covariance in y domain)
	matrix->Print();
	//Add the matrix to the flux group TDirectory
	fdir->Add(matrix);

	//Delete m1array and m2arra arrays
	delete[] m1array;
	delete[] m2array;
	//Clear the list of y(x) functions without deleting its objects
	mflist.Clear("nodelete");
    }
  }
  //Clear the list of flux group directories without deleting its objects
  flist.Clear("nodelete");
  //Return 0;
  return 0;
}

void QSigExGaussCor::CheckPDFs() const
{
  //This protected function check if "PDFs" TDirectory exists
  if(!FindObjKey("PDFs",fMyDir)){
    cout << "Error: There's no PDF in fMyDir\n";
    throw 1;
  }
}

#include "debugger.h"





