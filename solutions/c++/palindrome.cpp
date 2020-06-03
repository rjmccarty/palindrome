#include <sys/stat.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <algorithm>

using namespace std;
const char *OPTSTRING = "c:r";
const char *USAGE = "USAGE: %s [-c maxrecords] [-r] [inputfile]\n";
const char *SCANFORMAT = "%25s%25s";
const int MAXPARMS = 1;
const int NAMEWIDTH = 25;

class Parms {
  private:
    int maxRecords;
    bool report;
    string inputFile;

  public:
    Parms();
    void setMaxRecords(int);
    int getMaxRecords(void);
    void setReport(bool);
    bool getReport(void);
    void setInputFile(string);
    string getInputFile(void);
};

Parms::Parms(void) {
   this->maxRecords = 0;
   this->report = false;
   this->inputFile = "STDIN";

   return;
}

void Parms::setMaxRecords(int max) {
  this->maxRecords = max;
  return;
}

int Parms::getMaxRecords(void) {
  return (this->maxRecords);
}

void Parms::setReport(bool val) {
  this->report = val;
  return;
}

bool Parms::getReport(void) {
  return (this->report);
}

void Parms::setInputFile(string filepath) {
  this->inputFile = filepath;
  return;
}

string Parms::getInputFile() {
  return (this->inputFile);
}

Parms *processOptions(int argc, char** argv) {
  fstream newfile;
  int maxRecords = 0;
  int option;
  char **arguments = NULL;
  Parms *parmObject = new Parms;

  extern int optind;
  extern char *optarg;
  
  while ((option = getopt(argc, argv, OPTSTRING)) >= 0)
    switch(option) {
      case 'c':
	parmObject->setMaxRecords(atoi(optarg));
	break;

      case 'r':
	parmObject->setReport(true);
	break;
	
      case '?':
	cerr << "ERROR: Unknown option." << endl;
	fprintf(stderr, USAGE, argv[0]);

      default:
	cout << endl;
	exit(1);
    }

  if (argc - optind > MAXPARMS) {
    cerr << "ERROR: Too many positional parameters." << endl;
    fprintf(stderr, USAGE, argv[0]);
    return (NULL);
  }

  arguments = &argv[optind];

  if (argc > optind)
    parmObject->setInputFile(arguments[0]);

  return (parmObject);
}

string& trimRight(string& str, const string& trimchars = "\t\n\v\f\r ")
{
    str.erase(str.find_last_not_of(trimchars) + 1);
    return (str);
}

void str2Upper(string& str) {
  transform(str.begin(), str.end(), str.begin(), (int(*)(int)) toupper);
  return;
}

bool isPalindrome(string const& str)
{
  string strupcase = str;
  str2Upper(strupcase);
    
  return equal(strupcase.begin(),
	       strupcase.begin() + strupcase.length() / 2,
	       strupcase.rbegin());
}

bool fileExists(const string& filename) {
  struct stat buffer;
  return (stat(filename.c_str(), &buffer) == 0); 
}

void processLine(string line) {
  char fNameArray[NAMEWIDTH + 1];
  char lNameArray[NAMEWIDTH + 1];
  string firstName;
  string lastName;

  // This manipulation between std::string types and char[]/char * types
  // reminds me of why C++ is not exactly my favorite language. I see no
  // reason to use std::string over normal C types. But for this example
  // program I will try to stick with the C++'isms as much as I can.
  
  // sscanf will remove trailing whitespace from the fields,
  // so no need to trim.
  sscanf(line.c_str(), SCANFORMAT, fNameArray, lNameArray);

  firstName = fNameArray;
  lastName = lNameArray;
  
  printf("%3s %-25s %3s %-25s\n",
	 (isPalindrome(firstName) ? "YES" : " NO"),
	 firstName.c_str(),
	 (isPalindrome(lastName) ? "YES" : " NO"),
	 lastName.c_str()
	 );
  
  return;
}

void processFile(istream& inputStream, Parms *commandParms) {
  string line;
  int maxRecords = commandParms->getMaxRecords();
  int recordsProcessed = 0;
  
  try {
    while ((maxRecords <= 0 || recordsProcessed < maxRecords) &&
	   getline(inputStream, line)) {
      //      cout << line << endl;
      processLine(line);
      ++recordsProcessed;
    }
  } catch (istream::failure& e) {
    cerr << "File read error ("
	 << commandParms->getInputFile() << ")" << endl
	 << e.code().message() << endl;
    exit(1);
  }

  if (commandParms->getReport() == true)
    cout << "Records processed: " << recordsProcessed << endl;
  
  return;
}

int main(int argc, char** argv) {
  ifstream inputFS;
  int maxRecords = 0;
  int option;
  string inputFile;
  Parms *commandParms;

  try {
    if ((commandParms = processOptions(argc, argv)) == NULL)
      throw "Unable to process command line.";

    inputFile = commandParms->getInputFile();
    maxRecords = commandParms->getMaxRecords();
  } catch (const char *message) {
    cerr << message << endl;
    exit(1);
  }

  //  cout << "maxRecords = " << commandParms->getMaxRecords() << endl;
  //  cout << "inputFile = " << commandParms->getInputFile() << endl;

  inputFS.exceptions(ifstream::failbit | ifstream::badbit);

  if (inputFile.compare("STDIN") == 0) {
    processFile(cin, commandParms);
  } else {
    try {
      // Have to check this explicitly because catching
      // the error in open() will not produce a useful
      // error message.
      if (!fileExists(inputFile))
      	throw "";

      inputFS.open(inputFile, ios::in);
    } catch (system_error& e) {
      cerr << "Unable to open file ("
	   << inputFile << ")" << endl
	   << e.code().message() << endl;
      exit(1);
    } catch (const char *message) {
      cerr << "File not found. (" << inputFile << ")" << endl;
      exit(1);
    }

    // Need to reset the exception mask or otherwise we will not
    // detect EOF in readFile()
    inputFS.exceptions(ifstream::goodbit);
    processFile(inputFS, commandParms);
    inputFS.close();
  }
  
  exit(0);
}
