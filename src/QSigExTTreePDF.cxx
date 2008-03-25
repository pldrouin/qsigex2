// Author: Pierre-Luc Drouin <http://www.pldrouin.net>
// Copyright Carleton University

#include "QSigExTTreePDF.h"

//#define DEBUG
//#define DEBUG2

#include "debugger.h"

////////////////////////////////////////////////////////////////////////
//                                                                    //
// QSigExTTreePDF                                                     //
//                                                                    //
// This class is a QSigExPDFs derived class that creates 1d           //
// QSigExDisTH marginal PDFs from raw TTree objects. For each flux    //
// group, it also creates a clean TTree which branches are those used //
// to create the marginal PDFs.                                       //
//                                                                    //
////////////////////////////////////////////////////////////////////////


ClassImp(QSigExTTreePDF)

QSigExDis* QSigExTTreePDF::GetFunction(const QList<TString>& pdfentry, TDirectory* fluxdir, const TCut& fgcuts, QList<TString>* inputs, Bool_t *pdfneedscuts)
{
  //This protected function uses the pdf card file formatted entry pdfentry, a
  //pointer to the TDirectory of the PDF flux group and the combined cuts fgcuts
  //to create a function used in QSigExPDFs::Get() to produce a marginal PDF
  //object. In order to create this function, it loads a raw TTree object from a
  //ROOT file for the PDF flux group, creates a new TTree using the combined
  //cuts and puts it in fluxdir. Finally, it creates a new 1d QSigExDisTH object from
  //this new TTree. GetFunction() returns a QSigExDis pointer to this new object.
  //inputs is filled with the coordinate name of the QSigExDisTH function.
  //pdfneedscuts is set to kFALSE since the function applies cuts on the
  //raw TTree. The clean TTree can be used by other QSigExDirHandler classes to
  //take into account the correlations between PDFs variables.
  //
  //Syntax of a card file entry: 
  //pdf  [...]  [objname]  [filename]  [c1] [nbins] 
  //where [...] are the fields defined in QSigExPDFs::LoadCardFile, [objname]
  //the name of the object in the ROOT file [filename], where [c1] is the
  //coordinate name and where [nbins] is the number of bins for the new QSigExDisTH
  //function. The [typeid] field defined in QSigExPDFs must be equal to "TTree".
  //If not, the pdf entry is ignored and GetFunction returns NULL.

  //WARNING: WHEN PDFS ARE GENERATED FROM A TTREE, ALL THE PDF LINES OF A
  //SPECIFIC FLUX GROUP IN THE CARD FILE THAT ARE OF THE TYPE TTREE MUST USE THE
  //SAME TTREE

  try{
    //Set up the indices for the fields in this part of the card file:
    const Int_t minfields = 7;         //minimum number of fields
    const Int_t maxfields = 8;         //maximum number of fields
    const Int_t pdftypeindex = 2;      //ROOT class name of the read function
    
    const Int_t pdfgroupindex = 3;     //Flux group name
    const Int_t systnameindex = 4;     //Systematics group name
    
    const Int_t objnameindex = 5 ;     //object name of the function
    const Int_t filenameindex = 6 ;    //ROOT filename
    const Int_t firstcoordindex = 7;   //First coordinate
    
    
    QSigExDis* pdfbuf=NULL;
    
    if(pdfentry[pdftypeindex]=="TTree"){    
      CheckCardNFields(pdfentry.Count()-1,minfields,maxfields);
      
      TH1F* thbuf;         //TH1F buffer
      TTree *tbuf=NULL,*ctbuf=NULL;  //TTree buffers
      TFile *treefile;     //TFile pointer for PDFs generated from TTrees
      Int_t intbuf;        //Int_t buffer
      
      TString strbuf;
      
      //If there's no Event Info TDirectory in fluxdir
      if(!(fluxdir->FindObject("Event Info"))){
	//Open the filename specified in the card file
	treefile=new TFile(pdfentry[filenameindex],"READ");
	//Load the TTree
	tbuf=dynamic_cast<TTree*>(treefile->Get(pdfentry[objnameindex]));
	//Change directory to the fluxdir
	fluxdir->cd();
	//Get a clean TTree using all cuts
	ctbuf=tbuf->CopyTree(fgcuts);
	//Set the name of the clean TTree to "Event Info" 
	ctbuf->SetName("Event Info");
	cout << ctbuf->GetEntries() << "/" << tbuf->GetEntries() <<
	  " entries (" << 100.*ctbuf->GetEntries()/(tbuf->GetEntries()) <<
	  " %) have passed the cuts\n";
	
	//Call ClearBranchesAddresses to set clean
	//TTree branches addresses to NULL
	QTTreeUtils::ClearBranchesAddresses(ctbuf);
	
	//Close the read TTree
	treefile->Close();
	//Remove it from gROOT
	gROOT->Delete(pdfentry[filenameindex]);
	
	//Else if there's an "Event Info" object in fluxdir but it's
	//not dervied from TTree, throw an exception
      } else if(!(ctbuf=dynamic_cast<TTree*>(fluxdir->Get("Event Info")))){
	cout << "Error in QSigExPDFs::Get: Clean TTree not present\n";
	throw 1;
      }
      
      //Change directory to fluxdir
      fluxdir->cd();
      
      //Generate the name for the new PDF
      strbuf=(pdfentry[pdfgroupindex]+"_")+
	(pdfentry[systnameindex]+"_")+
	pdfentry[firstcoordindex];
      
      //Read the number of bins for the new PDF
      sscanf(pdfentry[firstcoordindex+1],"%i",&intbuf);
      
      //Create a new TH1F
      thbuf=new TH1F(strbuf,strbuf,intbuf,
		     ctbuf->GetMinimum(pdfentry[firstcoordindex]),
		     ctbuf->GetMaximum(pdfentry[firstcoordindex]));
      
      //Create a new PDF (QSigExDisTH) using the TH1F instance
      //(it's copied)
      pdfbuf=new QSigExDisTH("TH1F",*thbuf);
      //Delete the TH1F instance 
      thbuf->Delete();
     
      //Get a pointer to the new TH1F object
      thbuf=dynamic_cast<TH1F*>(dynamic_cast<QSigExDisTH*>(pdfbuf)->GetObject());

      //Append the TH1F instance of the PDF to fluxdir
      thbuf->SetDirectory(fluxdir);
      //Fill it using TTree::Draw
      ctbuf->Draw((pdfentry[firstcoordindex]+">>")+thbuf->GetName(),"1","goff");
      
      //Remove the TH1F instamce of the PDF from fluxdir
      thbuf->SetDirectory(NULL);
      //Set the name of the PDF (QSigExDisTH)
      pdfbuf->SetName(thbuf->GetName());
      
    }
    
    if(pdfbuf){
      (*inputs)=pdfentry[firstcoordindex];
    }
    (*pdfneedscuts)=kFALSE;

    return pdfbuf;
  }catch(Int_t e){
    cout << "Exception handled by QSigExTTreePDF::GetFunction\n";
    throw e;
  }
}

const Char_t* QSigExTTreePDF::GetPDFName(Int_t i)
{
  const Int_t pdfgroupindex = 3;     //Flux group name
  const Int_t systnameindex = 4;     //Systematics group name

  const Int_t firstcoordindex = 7;   //First coordinate

  try{
    return ((fPDFCard[i][pdfgroupindex]+"_")+(fPDFCard[i][systnameindex]+"_")+fPDFCard[i][firstcoordindex]).Data();

  }catch(Int_t e){
    cout << "Exception handled by QSigExStdPDFs::GetPDFName\n";
    throw e;
  }
}

#include "debugger.h"





