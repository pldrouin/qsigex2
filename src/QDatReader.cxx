// Author: Kathryn Miknaitis <mailto:gator@u.washington.edu>
// J. Wendland <mailto:juergen@phas.ubc.ca>
// P.-L. Drouin <mailto: pldrouin@physics.carleton.ca>

#include "QDatReader.h"

//////////////////////////////////////////////////////////////////////////
//                                                                      //
// QDatReader                                                           //
//                                                                      //
// This class retrieves information from an ASCII file. Keywords are    //
// used to identify the different entries. The information is returned  //
// using QList<TString> objects.                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////

ClassImp(QDatReader)

QList<TString> QDatReader::Get(Int_t offset)
{
  //Looks for the next matching line. If offset is specified, it starts
  //to look at the last matching line + offset + 1.
  //
  //The function returns the next matching line as a QList<TString>.

  //check that we have an open file and valid input:
    if(!filename){
      cout << "QDatReader::Get: Error: No associated filename\n";
      throw 2501;
    } else if(offset<0){
      cout << "QDatReader::Get: Error: Invalid linenumber\n";
      throw 2502;
    }


    //set a maximum number of characters to read in from a line (should be 
    //larger than any lines actually expected)
    Int_t maxlinelength = 500;                  


    Char_t* linebuf = new Char_t[maxlinelength]; //buffer for lines read in
    Char_t* token;                               //buffer for words in a line
    QList<TString> ret;                          //the return array
    Bool_t linefound = false;                    //to indicate valid line found
    Bool_t keywordfound = false;                 //to indicate keyword found
    Int_t linecounter = currentline;             //counts lines read in so far
    Int_t wantedline = firstkwline!=-1 ? offset + firstkwline + 1: offset + 1; //line we want

    

    //If there is no keyword specified, we just want to grab the first valid
    //line in the file that occurs after "offset":
    if (!keyword){

      //loop through the file:
      while (linefound == false && file.fail() == false){

	//get each line.  
        file.getline(linebuf, maxlinelength, '\n');

        //"tokenize" the line, using spaces and tabs as separators:
        token = (Char_t*)strtok(linebuf," \t");

        //if the line is a valid line to read then we increment the counter:
        if (token && token[0] !='#') {
	  ++linecounter;
	  
	//only continue with the line if it's at least wantedline
       	  if ( linecounter >= wantedline) {

	    //save this position to the external variable currentline
       	    currentline = linecounter;  

	    //if it's the first match to keyword, save the position
            if (firstkwline==-1) firstkwline = linecounter-1;
	   
	    linefound = true;

  	    //load up the return list with the words from this line
	    while (token != NULL){         //goes through each token in line
	      ret += token;                //appends word to ret list
              token = strtok(NULL," \t");  //loads new word into "token"
	    }
	  }
	}
      }
      

    } else {

      //if a keyword has been specified, loop through to find it
      while (keywordfound == false && file.fail() == false){
	//get each line.  
	file.getline(linebuf, maxlinelength, '\n');

	//"tokenize" the line using spaces and tabs as separators
	token = (Char_t*)strtok(linebuf," \t");

	//if it's a valid line, increment the counter and continue
        if (token && token[0]!='#') {

	  ++linecounter;

	  //if the first token matches the keyword and we're at least
	  //at the line "wantedline", then this line will be read into ret:
	  if (linecounter >= wantedline && strcmp(token, keyword)==0) {

	    //save this position to the external variable currentline
      	    currentline = linecounter;

	    //if it's the first match to keyword, save the position
	    if (firstkwline==-1) firstkwline = linecounter-1;

	    keywordfound = true;

   	    //load up the return list with the words from this line
	    while (token != NULL){         //go through each token in the line
	      ret += token;
	      token = strtok(NULL," \t");  //load each successively into token
	    }
	  }
        }
      } 

    }

  delete linebuf; 
  return ret;

}


QList<QList<TString> > QDatReader::GetMany(Int_t offset,Int_t maxncommands)
{
  //Looks for the next matching lines, up to a maximum of maxncommands lines
  //if specified. If offset is specified, it starts to look at the last
  //matching line + offset + 1.
  //
  //The function returns the next matching lines as a QList<QList<TString>>.

  QList<QList<TString> > ret;             //return array (2-d)

  Int_t i=1;
  
  while(i!=maxncommands){
    i++;

    ret+=Get(i-2+offset);                //Get an array for each line

    if(!ret[i-2].Count()){               //if the line is empty, erase
      ret.Del();                         //it and end the loop
      i=maxncommands;
    }

  }

  
  return ret;
}
