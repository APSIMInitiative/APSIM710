#include <../General/pch.h>
//---------------------------------------------------------------------------

#include <fstream>
#include <sstream>

#include <vector>
#include <string>
#include <General/TreeNodeIterator.h>
#include <General/xml.h>
#include <General/macro.h>
#include <General/platform.h>
using namespace std;

// ------------------------------------------------------------------
// provide an entry point for .net and other languages.
// ------------------------------------------------------------------
extern "C" void EXPORT STDCALL doMacroTransform(const char* xmlFileName,
                 const char* macroFileName,
                 const char* outputDirectory)
   {
   XMLDocument doc(xmlFileName);
   Macro macro;
   ifstream in(macroFileName);
   ostringstream contents;
   contents << in.rdbuf();

   vector<string> filesGenerated;
   macro.go(doc.documentElement(), contents.str(), filesGenerated, outputDirectory);
   }



 
