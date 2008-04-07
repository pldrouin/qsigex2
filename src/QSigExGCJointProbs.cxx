// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QSigExGCJointProbs.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

////////////////////////////////////////////////////////////////////////
//                                                                    //
// QSigExGCJointProbs                                                 //
//                                                                    //
// This class, used with class QSigExGaussCor, allows to compute      //
// joint probability densities of flux groups considering the         //
// correlations between their variables. To achieve this, it can use, //
// for each variable x, a mapping function y(x) that has a gaussian   //
// distribution, such that the distribution of y(x) variables for     //
// each flux group is multi-gaussian. The covariance matrix of y(x)   //
// variables being computed for each flux group, it is possible to    //
// compute the joint probabilities using the multi-gaussian equation. //
//                                                                    //
// The class QSigExGCJointProbs computes the joint probability        //
// densities using already existing covariance matrices and y(x)      //
// mapping functions, as described in QSigExGCJointProbs::Get(). The  //
// output information is stored in a TTree located in                 //
// "Probs/JointPDFsProbs".                                            //
//                                                                    //
////////////////////////////////////////////////////////////////////////

ClassImp(QSigExGCJointProbs)

void QSigExGCJointProbs::FormatDir()
{
  //This protected member function gives to the fMyDir TDirectory the structure
  //that is needed to store the information produced by this class. It creates
  //TDirectory "PDFs/JointPDFsProbs". 

  PRINTF2(this,"\tvoid QSigExGCJointProbs::FormatDir()\n")
  try{
    //If Probs is neither in memory nor on Disk, create it in memory
    if(!(fProbsDir=(TDirectory*)fMyDir->Get("Probs"))){
      fProbsDir=fMyDir->mkdir("Probs","Probabilities");
    }

    //If JointPDFsProbs is neither in memory nor on Disk, create it in memory
    if(!(fGCJointProbsDir=(TDirectory*)fProbsDir->Get("JointPDFsProbs"))){
      fGCJointProbsDir=fProbsDir->mkdir("JointPDFsProbs","Joint probability densities from PDFs using gaussian correlations");
    }
  }catch(int e){
    cout << "Exception handled by QSigExGCJointProbs::FormatDir\n";
    throw e;
  }
}

void QSigExGCJointProbs::CleanDir()
{
  //This function reinitializes the part of the fMyDir directory structure that
  //belongs to QSigExGCJointProbs. It removes TDirectory "Probs/JointPDFsProbs".

  PRINTF2(this,"\tvoid QSigExGCJointProbs::CleanDir()\n")
  try{
    if((fProbsDir=(TDirectory*)fMyDir->Get("Probs"))) DelObjsKeys("JointPDFsProbs",fProbsDir);
    fGCJointProbsDir=fProbsDir=NULL;
  }catch(int e){
    cout << "Exception handled by QSigExGCJointProbs::CleanDir\n";
    throw e;
  }
}

void QSigExGCJointProbs::LoadCardFile(const Char_t* cardfilename)
{
  //This function does actually nothing in this class. 

  PRINTF4(this,"\tvoid QSigExGCJointProbs::LoadCardFile(const Char_t* cardfilename<",cardfilename,">)\n")

  cardfilename=NULL;

}

Int_t QSigExGCJointProbs::Get()
{
  //This function computes the joint probability densities of correlated or
  //non-correlated variables. For each event in each flux group, it reads the
  //marginal probability density from the corresponding TTree in
  //"Probs/PDFsProbs" and multiply them together, to get the joint probability
  //density as if the variables of this flux group were not correlated. Then, it
  //looks in the flux group directories in "PDFs" TDirectory to find covariance
  //matrices in y variables space (space in which the set of variables have a
  //multi-gaussian distribution). If this matrix exists, it determines which
  //marginal PDFs in this flux group are represented in this matrix (using a
  //"TMatrixIndex" QNamedVar<Int_t> object in the PDFs directories). For each of these
  //PDFs, it loads the y(x) mapping function, TF1 object named "GaussMapping"
  //located in the marginal PDF directory. The function then loops over the
  //events in the TTree located in "Event Info" main TDirectory (not the one in
  //the flux groups directories). For each event in each flux group, it uses the
  //set of y(x) values and the covariance matrix to compute a joint probability
  //density correlation correction factor. The correlated joint probability
  //densities are finally computed by multiplying the uncorrelated joint
  //probability densities by their correction factor. The output of this
  //function is a "JointPDFsProbs" TTree located in "Probs/JointPDFsPRobs"
  //TDirectory.
  //
  //The previously explained algorithm allows to compute joint probability
  //densities of flux groups for which a covariance matrix has been produced or
  //not. In addition, all the marginal PDFs in a flux group don't need to be
  //included in the covariance matrix, in a way that the marginal PDFs that are
  //not correlated to the other PDFs can be excluded of the correlation
  //correction process.
  //
  //The function returns the number of computed joint probability densities
  //
  //Summary:
  //  Inputs: -TTree objects in "Probs/PDFsProbs"
  //          -When they exist, TMatrixDSym objects (covariance matrices in y
  //           space) in the flux group directories located in "PDFs" TDirectory
  //          -When they exist, "GaussMapping" TF1 objects (y(x) mapping 
  //           functions) and "TMatrixIndex" QNamedVar<Int_t> objects located in the PDFs
  //           directories
  //          -clean TTree located in "Event Info" main TDirectory
  //
  //  Ouput:  -"JointPDFsProbs" joint probability densities TTree object located
  //           in "Probs/JointPDFsProbs" 
  //
  //WARNING: THE INPUT TTREES MUST HAVE BRANCHES OF TYPE DOUBLE_T
  //         THE OUTPUT TTREE HAS BRANCHES OF TYPE DOUBLE_T 


  PRINTF2(this,"\tInt_t QSigExGCJointProbs::Get()\n")

  try{
    //NB: "marginal probability densities TTree <=> "PDFs probs TTree" 

    Int_t h,i,j,k; //iterators
    
    //Format the TDirectory structure and do some checks
    FormatDir();
    CheckPDFs();
    CheckPDFsProbs();
    CheckCleanData();
    
    //Get a pointer on the "Probs/PDFsProbs" and PDFs TDirectory objects
    TDirectory* ppdir=dynamic_cast<TDirectory*>(fProbsDir->Get("PDFsProbs"));
    TDirectory* pdfsdir=dynamic_cast<TDirectory*>(fMyDir->Get("PDFs"));
    
    TList ppdlist; //List of objects in the "Probs/PDFsProbs" TDirectory
    TList pplist;  //List of marginal probability densities TTree 
    
    //Fill ppdlist with the list of objects in "Probs/PDFsProbs" TDirectory
    GetObjs(&ppdlist,ppdir);

    TTree* tbuf; //TTree pointer buffer

    TList eilist; //List of objects in "Event Info" TDirectory
    GetObjs(&eilist,dynamic_cast<TDirectory*>(fMyDir->Get("Event Info")));
    
    TTree* cdtree=NULL; //TTree pointer to the clean data TTree

    //Loop over the objects in "Event Info" TDirectory
    for(i=0;i<eilist.GetSize();i++){
      //If the current object is a TTree, get a pointer to it and exit the loop
      if((cdtree=dynamic_cast<TTree*>(eilist.At(i)))) break;
    }

    //If there's no TTree object in "Event Info" TDirectory, throw an exception
    if(!cdtree){
      cout << "Error: There's no PDF in the TDirectory\n";
      throw 1;
    }
    
    //Loop over the objects in "Probs/PDFsProbs" TDirectory
    for(i=0;i<ppdlist.GetSize();i++){

      //If the current object is a TTree, get a pointer to it
      if((tbuf=dynamic_cast<TTree*>(ppdlist.At(i)))){

	//If the number of entries in the current marginal probability densities
	//TTree is different from the number of entries in the clean data TTree,
	//throw an exception.
	if(tbuf->GetEntries()!=cdtree->GetEntries()){
	  cout << "Error: The number of entries in the probs TTree " << tbuf->GetName() << " doesn't match the number of entries in the clean data TTree\n";
	  throw 1;
	}
	//Clear the current marginal probability densities TTree branch
	//addresses
	QTTreeUtils::ClearBranchesAddresses(tbuf);
	//Add the current TTree to the list of marginal probability densities
	//TTrees
	pplist.Add(tbuf);
      }
    }

    //Get the number of TTrees in this list
    Int_t nfg=pplist.GetSize();

    //Change directory to "Probs/JointPDFsProbs" TDirectory
    fGCJointProbsDir->cd();
    //Create a new TTree to put the joint probs with gaussian correlation
    TTree* gcjptree=new TTree("JointPDFsProbs","Joint probabilities from PDFs using gaussian correlations");
    
    TTree* ppt; //A PDF probs TTree pointer

    //Branch addresses for the PDFs probs TTree objects
    Double_t** ppbaddrs=new Double_t*[nfg]; 
    //Number of branches for the PDFs probs TTree objects
    Int_t* nppb=new Int_t[nfg];  
    //Flux group indexes of gaussian correlated flux groups
    Int_t* gcgroups=new Int_t[nfg]; 
    //inv(U)-I matrix (U:Gaussian correlation covariance matrice)
    TMatrixDSym **gcmat=new TMatrixDSym*[nfg]; 
    //Gaussian correlation covariance matrices determinants^(-1/2)
    Double_t *gcmatdet=new Double_t[nfg]; 
    Float_t ***x=new Float_t**[nfg];   //x variables values
    memset(x,0,nfg*sizeof(Float_t**)); //Initialize the x array pointers to NULL
    TF1 ***yfunc=new TF1**[nfg];       //y(x) mapping functions
    memset(yfunc,0,nfg*sizeof(TF1**)); //Initialize the yfunc array pointers to NULL
    Int_t* ngcp=new Int_t[nfg];        //Number of gaussian correlated PDFs
    Int_t maxngcp=0;                   //Maximum number of gaussian correlated PDFs
    memset(ngcp,0,nfg*sizeof(Int_t));  //Initialize the number of g.c. PDFs to 0
    
    //Flux group, systematics group, PDF and inputs TDirectory pointers
    TDirectory *gdir, *sdir, *pdir, *idir; 
    TString strbuf; //String buffer
    Int_t ngcgroups=0; //Number of flux groups with gaussian correlations
    
    //Set cdtree branch addresses to NULL
    QTTreeUtils::ClearBranchesAddresses(cdtree);
    TObjArray* cdblist=cdtree->GetListOfBranches();//List of branches in cdtree
    
    //Lists of objects in "PDFs", system. groups, a particular PDF and an
    //"Inputs" TDirectory
    TList pdfslist, glist, slist, anlist;
    
    //Some multi-purpose buffers
    Int_t ibuf;
    QNamedVar<Int_t>* namedbuf;
    QNamedVar<TString>* namedbuf2;
    TF1* tfbuf;
    TObject *objbuf;
    TMatrixDSym* matbuf;
    TBranch* bbuf;
    
    //Dummy argument for TTreeUtils::AssignBAddress
    Float_t *dummy=NULL;
    
    //Get the list of objects in PDFs TDirectory
    GetDirs(&pdfslist,pdfsdir);

    //Loop over the PDFs probs TTrees 
    for(h=0;h<nfg;h++){
      //Get a pointer to the current marginal probability densities TTree
      ppt=dynamic_cast<TTree*>(pplist.At(h));

      //Store the current TTree number of branches
      nppb[h]=ppt->GetNbranches();
      //Create a array of buffers for the branches of this TTree
      ppbaddrs[h]=new Double_t[nppb[h]];

      //Loop over the current TTRee branches
      for(k=0;k<nppb[h];k++){
	//Get a pointer to the current branch
	bbuf=dynamic_cast<TBranch*>(ppt->GetListOfBranches()->At(k));
	//Set the address of this branch to the appropriate buffer
	bbuf->SetAddress(ppbaddrs[h]+k);
      }

      //Create a new g.c. joint probability densities TTree branch named
      //according to the flux group name (the name of the current PDFs probs
      //TTree)
      gcjptree->Branch(ppt->GetName(),ppbaddrs[h],((TString)ppt->GetName())+"/D");

      //If the "PDFs" TDirectory contains a flux group TDirectory with the same
      //name
      if((gdir=dynamic_cast<TDirectory*>(pdfslist.FindObject(ppt->GetName())))){
	//cout << "Flux group " << gdir->GetName() << endl;

	//Try to load the matrix from a TKey
	matbuf=dynamic_cast<TMatrixDSym*>(gdir->Get("TMatrixDSym"));
	//If there was no TMatrixDSym TKey
	if(!matbuf){
	  //Loop over the objects in gdir
	  for(i=0;i<gdir->GetList()->GetSize();i++){
	    //Get a pointer to the current object
	    objbuf=gdir->GetList()->At(i);
	    //If the current object name is "TMatrixDSym" and that it's derived from
	    //TMatrixDSym, assign the pointer to matbuf
	    if(!strcmp(objbuf->GetName(),"TMatrixDSym") &&
		(matbuf=dynamic_cast<TMatrixDSym*>(objbuf))) break;
	  }
	}

	//If there's a covariance TMatrixDSym in the flux group TDirectory
	if(matbuf){

	  cout << "Flux group " << gdir->GetName() << " has a correlation matrix\n";

	  //If this matrix is not square and symmetric, throw an exception
	  if(matbuf->GetNrows()!=matbuf->GetNcols() || !matbuf->IsSymmetric()){
	    cout << "Error: The TMatrixDSym loaded is not square or is not symmetric\n";
	    throw 1;
	  }

	  //Store the flux group index of the current g.c. flux group
	  gcgroups[ngcgroups]=h;

	  //Fill glist with the list of syst. group directories in the current
	  //flux group TDirectory
	  GetDirs(&glist,gdir);

	  //Loop over these objects
	  for(i=0;i<glist.GetSize();i++){

	    //Get a pointer to the current syst. group TDirectory
	    sdir=dynamic_cast<TDirectory*>(glist.At(i));
	    //cout << "\tSystematic group " << sdir->GetName() << endl;

	    //Fill slist with the list of PDFs directories in the current syst.
	    //group TDirecotry
	    GetDirs(&slist,sdir);

	    //Loop over these objects
	    for(j=0;j<slist.GetSize();j++){

	      //Get a pointer to the current PDF TDirectory
	      pdir=dynamic_cast<TDirectory*>(slist.At(j));
	      //cout << "\t\tPDF " << pdir->GetName() << endl;

	      //Fill strbuf with the name of y(x)
	      strbuf="GaussMapping_";
	      strbuf+=pdir->GetName();

	      //If there's a covariance matrix coordinate index and a y(x) mapping function for this PDF
	      if((namedbuf=dynamic_cast<QNamedVar<Int_t>*>(pdir->Get("TMatrixIndex"))) && (tfbuf=dynamic_cast<TF1*>(pdir->Get(strbuf)))){

		cout << "\tCorrelations considered for PDF " << pdir->GetName() << endl;

		//Read the coordinate index
		ibuf=*namedbuf;

		//Delete the QNamedVar object (the covariance matrix index object);
		delete namedbuf;

		//If the current index is greater than the others read before
		if(ngcp[ngcgroups]<ibuf+1){
		  //Update the dimensions for x and yfunc arrays
		  ngcp[ngcgroups]=ibuf+1;

		  //If the number of g.c. PDFs in the current g.c. flux group is
		  //greater than the maximum known number of g.c. PDFs in a g.c.
		  //flux group, update this maximum
		  if(ngcp[ngcgroups]>maxngcp) maxngcp=ngcp[ngcgroups];
		  //Resize the x array for the current g.c. flux group
		  x[ngcgroups]=(Float_t**)realloc(x[ngcgroups],ngcp[ngcgroups]*sizeof(Float_t*));
		  //Resize the y(x) functions array for the current g.c. flux group
		  yfunc[ngcgroups]=(TF1**)realloc(yfunc[ngcgroups],ngcp[ngcgroups]*sizeof(TF1*));
		}

		//If there's an "Inputs" TDirectory for the current g.c. PDF
		if((idir=dynamic_cast<TDirectory*>(pdir->Get("Inputs")))){
		  //Fill anlist with the list of objects in the "Inputs"
		  //TDirectory
		  GetObjs(&anlist,idir);

		  //If there's no object or if there's no Input 0, throw an error
		  if(anlist.GetSize()==0 || !(namedbuf2=dynamic_cast<QNamedVar<TString>*>(anlist.FindObject("Input 0")))){
		    cout << "Error: There is no input for pdf " << pdir->GetName() << endl;
		    throw 1;
		  }

		  //If the clean data TTree contains a TBranch corresponding to
		  //the Input 0
		  if(cdblist->FindObject(namedbuf2->GetValue())){
		    //Assign an address to the branch of the clean data TTree if
		    //not already done and store the address in x
		    (x[ngcgroups])[ibuf]=QTTreeUtils::AssignBAddress(namedbuf2->GetValue(),cdtree,dummy);

		  //Else
		  }else{
		    cout << "Error: There's no branch '" << namedbuf2->GetValue() << "' in the clean data TTree\n";
		  }

		  //Store the address of the current g.c. PDF y(x) function into
		  //yfunc array
		  (yfunc[ngcgroups])[ibuf]=tfbuf;

		  //cout << "\t\t\tThe PDF has addresses " << (x[ngcgroups])[ibuf] << " and " << (yfunc[ngcgroups])[ibuf] << " for x and yfunc for Input 0 equal to " << namedbuf->GetTitle() << endl;

		  //Delete the loaded objects in Inputs
		  anlist.Clear();
		  namedbuf=NULL;

		  //Else if there's no "Inputs" TDirectory for this PDF
		}else{
		  cout << "Error: TDirectory" << pdir->GetName() << "doesn't contain an \"Inputs\" directory\n";
		  throw 1;
		}
	      }else{
		cout << "\tCorrelations CAN'T be considered for PDF " << pdir->GetName() << endl;
	      }
	    } 
	    //Clear the list of TDirectory objects in the systematic TDirectory
	    //(don't delete them)
	    slist.Clear("nodelete");
	  }

	  //Clear the list of TDirectory objects in the flux group TDirectory
	  //(don't delete them)
	  glist.Clear("nodelete");

	  //Throw an exception if the number of correlated PDFs doesn't match
	  //the dimensions of the covariance TMatrixDSym
	  if(ngcp[ngcgroups]!=matbuf->GetNrows()){
	    cout << "Error: The number of PDFs with correlation in flux group " << gdir->GetName() << "doesn't match the dimensions of the covariance TMatrixDSym\n";
	    throw 1;
	  }

	  //Compute the determinant of the covariance TMatrixDSym and store:
	  //det^(1/2) in gcmatdet
	  gcmatdet[ngcgroups]=1/TMath::Sqrt(matbuf->Determinant());

	  //Pack inv(U)-I (U: the covariance TMatrixDSym) in a c-style array
	  //Inverse of the matrix
	  TDecompChol mathandler(*matbuf);
	  *matbuf=mathandler.Invert();
//	  matbuf->InvertPosDef();
	  //remove 1 to the diagonal
	  TMatrixDDiag(*matbuf)+=-1;
	  //Store the matrix pointer
	  gcmat[ngcgroups]=matbuf;

	  //Increment the number of flux groups with gaussian correlation
	  ngcgroups++;
	}else{
	  cout << "Flux group " << gdir->GetName() << " DOESN'T have a correlation matrix\n";
	}

	//Throw an error if the joint probs group is not found among the PDfs groups
      }else{
	cout << "Error: Joint probabilities group '" << ppt->GetName() << "' is not found among PDFs groups\n";
      }
    } //Continue looping on flux groups

    
    TMatrixD y(maxngcp,1);
    //Number of entries in clean data tree;
    Int_t nentries=(Int_t)cdtree->GetEntries(); 

    cout << "Now compute joint probs using gaussian correlations\n";
    QProgress progress(nentries,1000);

    //Loop over the entries in the clean data TTree
    for(i=0;i<nentries;i++){
      //if(i<10) cout << "Entry " << i << endl;
      progress(i);
      //Fill the current entry clean data TTree buffers
      cdtree->GetEntry(i);

      //Loop over all the flux groups
      for(j=0;j<nfg;j++){
	//if(i<10) cout << "\tFlux group: " << pplist.At(j)->GetName() << endl;
	//if(i<10) cout << "\t\tInputs: " << (ppbaddrs[j])[0] << "\t";
	//Fill the current entry current marginal probability densities TTree
	//buffers 
	dynamic_cast<TTree*>(pplist.At(j))->GetEntry(i);

	//Loop over the number of branches in this TTree (number of marginal
	//probability densities)
	for(k=1;k<nppb[j];k++){
	  //if(i<10) cout << (ppbaddrs[j])[k] << "\t";
	  //Multiply these probability densities together
	  (ppbaddrs[j])[0]*=(ppbaddrs[j])[k];
	}
	//if(i<10) cout << "\n\t\tJointProb: " << (ppbaddrs[j])[0] << endl;
      }

      //Loop over the flux groups with gaussian correlations
      for(j=0;j<ngcgroups;j++){
	//if(i<10) cout << "\tFlux group: " << pplist.At(gcgroups[j])->GetName() << endl;

	//Loop over the g.c. marginal PDFs
	for(k=0;k<ngcp[j];k++){
	  //Evaluate the y(x) function for the current marginal PDF
	  y(k,0)=((yfunc[j])[k])->Eval(*((x[j])[k]));
	}
	//Compute y'(inv(U)-I)y
	TMatrixD buf(y, TMatrixD::kTransposeMult, *gcmat[j]);
	TMatrixD fact(buf, TMatrixD::kMult, y);
	//Multiply the non-correlated joint probability density of the
	//appropropriate flux group by the gaussian correlation correction
	//factor
	ppbaddrs[gcgroups[j]][0]*=gcmatdet[j]*TMath::Exp(-0.5*fact(0,0));
	//if(i<10) cout << "\t\tJointProb correlated: " << (ppbaddrs[j])[0] << endl;
      }
      //Fill the g.c. joint probability densities TTree with the current event
      //g.c. joint probability density
      gcjptree->Fill();
    }
    progress(i,kTRUE);
    cout << "\tdone\n";
    
    //Loop over all the flux groups
    for(i=0;i<nfg;i++){
      //Clear the current PDFs probs TTree branches addresses
      QTTreeUtils::ClearBranchesAddresses(dynamic_cast<TTree*>(pplist.At(i)));
      //Delete the current PDFs probs TTree branch buffers array 
      delete[] ppbaddrs[i];
    }
    //Delete the array of PDFs probs TTree branch buffers arrays
    delete[] ppbaddrs;
    //Delete the array of number of PDFs probs TTree branches
    delete[] nppb;
    //Delete the g.c. flux group indexes array
    delete[] gcgroups;

    //Clear the g.c. joint probabability densities TTree branches addresses
    QTTreeUtils::ClearBranchesAddresses(gcjptree);

    //Clear the list of TDirectory objects in the PDFs (don't delete them)
    pdfslist.Clear("nodelete");
    
    //Loop over the branches in the clean data TTree 
    for(i=0;i<cdtree->GetNbranches();i++){
      //Get a pointer to the current branch
      bbuf=dynamic_cast<TBranch*>(cdtree->GetListOfBranches()->At(i));
      //Delete the associated buffer
      delete (Float_t*)bbuf->GetAddress();
      //Set the branch address to NULL
      bbuf->SetAddress(NULL);
    }
    
    //Loop over the g.c. flux groups
    for(i=0;i<ngcgroups;i++){
      //Delete the x buffers pointers array for the current g.c. flux group
      free(x[i]);
    }
    //Delete the array of buffers pointers arrays pointers
    delete[] x;

    //Loop over the g.c. flux groups
    for(i=0;i<ngcgroups;i++){
      //Loop over the number of correlated marginal PDFs in this flux group
      for(j=0;j<ngcp[i];j++){
	//Delete the y(x) function of this marginal PDF
	delete (yfunc[i])[j];
      }
      //Delete the y(x) function pointers array
      free(yfunc[i]);
      //Delete the packed covariance matrix for this flux group
      delete gcmat[i];
    }

    //Delete the array of packed covariance matrices pointers 
    delete[] gcmat;
    //Delete the array of y(x) function pointers arrays pointers
    delete[] yfunc;
    
    //Delete the g.c. covariance matrices determinants array
    delete[] gcmatdet;

    //Dekete the array of number of g.c. marginal PDFs
    delete[] ngcp;

    //Delete the list of objects in the "Event Info" TDirectory and delete its
    //owned objects
    eilist.Clear();

    //Delete the list of marginal probability densities TTrees without deleting
    //its objects  
    pplist.Clear("nodelete");
    //Delete the list of objects in "Probs/PDFsProbs" TDirectory and delete its
    //owned objects
    ppdlist.Clear();

    //Return the number of joint probability densities with gaussian correlation
    return nentries;
  }catch(Int_t e){
    cout << "Exception handled by QSigExGCJointProbs::Get\n";
    throw e;
  }
}

void QSigExGCJointProbs::CheckPDFs() const
{
  //This protected function check if "PDFs" TDirectory exists
  if(!FindObjKey("PDFs",fMyDir)){
    cout << "Error: There's no PDF in fMyDir\n";
    throw 1;
  }
}

void QSigExGCJointProbs::CheckPDFsProbs() const
{
  //This protected function checks if a "Probs/PDFsProbs" TDirectory exists
  PRINTF2(this,"\tvoid QSigExGCJointProbs::CheckPDFsProbs() const\n")

  if(!FindObjKey("PDFsProbs",fProbsDir)){
    cout << "Error: There's no \"Probs/PDFsProbs\" TDirectory in fMyDir\n";
    throw 1;
  }
}

void QSigExGCJointProbs::CheckCleanData() const
{
  //This protected function check if "Event Info" TDirectory exists
  if(!FindObjKey("Event Info",fMyDir)){
    cout << "Error: There's no clean data in fMyDir\n";
    throw 1;
  }
}

#include "debugger.h"





