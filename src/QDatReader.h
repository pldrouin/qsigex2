// Author: Kathryn Miknaitis <mailto:gator@u.washington.edu>
// J. Wendland <mailto:juergen@phas.ubc.ca>
// P.-L. Drouin <mailto: pldrouin@physics.carleton.ca>

#ifndef _QDATREADER_
#define _QDATREADER_

#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cstring>
#include "Rtypes.h"
#include "TString.h"
#include "QList.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QDatReader                                                           //
//                                                                      //
// This class retrieves information from an ASCII file. Keywords are    //
// used to identify the different entries. The information is returned  //
// using QList<TString> objects.                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

using namespace std;

class QDatReader {
 public:
  QDatReader():fnsize(0),filename(NULL),file(),kwsize(0),keyword(NULL),currentline(0),firstkwline(-1){}
  QDatReader(const QDatReader& rhs){
    SetFilename(rhs.filename);
    SetKeyword(rhs.keyword);
  }

  QDatReader(const Char_t* newfilename,const Char_t* newkeyword=NULL,Int_t newkwfield=0):fnsize(0),filename(NULL),file(),kwsize(0),keyword(newkeyword),currentline(0),firstkwline(-1){newkwfield=0; SetFilename(newfilename);}
  virtual ~QDatReader(){}

  const Char_t* GetFilename() const{ return filename;}

  void SetFilename(const Char_t* newfilename)
    {
      filename=newfilename; file.open(filename,ios::in | ios::ate);
      if(file.fail()){
	cout << "<QDatReader::SetFilename> ERROR " << filename << " not found.\n";
	throw 100;
      }
      fnsize=strlen(filename);
      currentline = 0;
      firstkwline = -1;
      file.seekg(0);
    }

  const Char_t* GetKeyword() const{ return keyword;}

  void SetKeyword(const Char_t* newkeyword=NULL,Int_t newkwfield=0)
    {
      newkwfield=0;
      keyword=newkeyword;
      if(keyword) kwsize=strlen(keyword);
      else kwsize=0;
      file.clear();             //clear the eof or fail bits
      file.seekg(0);            //point to the beginning of the file
      currentline = 0;          //reset the counters
      firstkwline = -1;
    }

  QList<TString> Get(Int_t offset=0);
  QList<QList<TString> > GetMany(Int_t offset=0,Int_t maxncommands=0);

  void Clear(){
    filename=NULL;
    file.close();
    keyword=NULL;
    
  }

 private:
  Int_t fnsize;
  const Char_t* filename; //[fnsize]
  ifstream file;
  Int_t kwsize;
  const Char_t* keyword;  //[kwsize]
  Int_t currentline;
  Int_t firstkwline;

  ClassDef(QDatReader,1)  //Retrieves information from an ASCII file
};

#endif
