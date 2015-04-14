#include <../General/pch.h>
//---------------------------------------------------------------------------
#include <stdlib.h>
#include <string.h>
#include <fstream>
#include "macro.h"
#include <General/TreeNodeIterator.h>
#include <General/xml.h>
#include <General/string_functions.h>
//#include <General/io_functions.h>
#include <General/stl_functions.h>
#include <General/StringTokenizer.h>

#include <stdexcept>

using namespace std;
// ------------------------------------------------------------------
// Do all macro replacement in specified text for the given macro node.
// ------------------------------------------------------------------
string replaceMacros(const string& originalContents,
                     const string& parentName,
                     const XMLNode& node)
   {
   string contents = originalContents;
   string stringToReplace = parentName + ".name";
   replaceAll(contents, stringToReplace, node.getAttribute("name"));

   std::vector<std::string> attributes;
   node.getAttributes(attributes);
   for (unsigned i = 0; i != attributes.size(); i++)
      {
      stringToReplace = parentName + "." + attributes[i];
      replaceAll(contents, stringToReplace, node.getAttribute(attributes[i]));
      }


   unsigned counter = 0;
   for (XMLNode::iterator i = node.begin(); i != node.end(); i++)
      {
      counter++;
      stringToReplace = parentName + ".counter";
      replaceAll(contents, stringToReplace, itoa(counter));

      stringToReplace = parentName + "." + i->getName();
      replaceAll(contents, stringToReplace, i->getValue());
      }
   return contents;
   }
// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
Macro::Macro()
   {
   }
// ------------------------------------------------------------------
// Destructor
// ------------------------------------------------------------------
Macro::~Macro()
   {
   }
// ------------------------------------------------------------------
// generate the files.
// ------------------------------------------------------------------
void Macro::go(const XMLNode& values,
               const string& macroContents,
               std::vector<std::string>& filesGenerated,
               const std::string& outputDirectory)
   {
   macroValues = &values;
   filesGenerated.erase(filesGenerated.begin(), filesGenerated.end());
   string contents = macroContents;
   contents = parseForEach(contents, "", *macroValues);
   writeStringToFiles(contents, filesGenerated, outputDirectory);
   }
// ------------------------------------------------------------------
// generate the files.
// ------------------------------------------------------------------
void Macro::go(const XMLNode& values,
               const std::string& macroContents,
               std::ostream& out)
   {
   macroValues = &values;
   string contents = macroContents;
   contents = parseForEach(contents, "", *macroValues);
   contents = replaceMacros(contents, "", *macroValues);
   parseIf(contents);

   out << contents;
   }
// ------------------------------------------------------------------
// Adjust the start of the specified tag. This routine will remove
// unwanted spaces on the front of the tag.
// ------------------------------------------------------------------
size_t getStartOfTag(const string& st, size_t posTag)
   {
   if (posTag > 0)
      {
      posTag = st.find_last_not_of(' ', posTag-1);
      if (posTag == string::npos)
         return 0;
      else if (st[posTag] == '\n')
         return posTag + 1;
      else
         return posTag+1;
      }
   else
      return 0;
   }
// ------------------------------------------------------------------
// Adjust the end of the specified tag. This routine will remove
// unwanted spaces and a single CR on the end of the tag.
// ------------------------------------------------------------------
size_t getEndOfTag(const string& st, size_t posTag, const string& tagName)
   {
   size_t posEndTag = posTag + tagName.length();
   posEndTag = st.find_first_not_of(' ', posEndTag);
   if (posEndTag == string::npos)
      return st.length()-1;
   else if (st[posEndTag] == '\n')
      return posEndTag;
   else
      return posTag + tagName.length()-1;
   }
// ------------------------------------------------------------------
// Parse and remove all for_each macros from specified string.
// ------------------------------------------------------------------
string Macro::parseForEach(const string& originalContents,
                           const string& parentName,
                           const XMLNode& valuesNode) const
   {
   string contents = originalContents;
   size_t posForEach = contents.find("#for_each");
   while (posForEach != string::npos)
      {
      StringTokenizer tokenizer(contents, posForEach, " \n");
      string foreach = tokenizer.nextToken();
      string macroName = tokenizer.nextToken();
      if (macroName == "")
         throw runtime_error("Can't find macro name after #for_each");
      size_t posStartForEachBody = tokenizer.currentPos();
      if (contents[posStartForEachBody] == '\n')
         posStartForEachBody++;

      // Now search through to find the end of the for loop
      // This current loop may contain nested loops - therefore
      // need to count #for_each and #endfor statements.
      size_t posEndFor;
      size_t currentPos = posStartForEachBody;
      unsigned count = 1;
      do
         {
         size_t posForEach = contents.find("#for_each", currentPos);
         posEndFor = contents.find("#endfor", currentPos);
         if (posForEach != string::npos && posForEach < posEndFor)
            {
            count++;
            currentPos = posForEach + 1;
            }
         else
            {
            count--;
            currentPos = posEndFor;
            }
         currentPos++;
         }
      while (count > 0);

      // get the bit of text before the #for_each
      string preBody = contents.substr(0, getStartOfTag(contents, posForEach));

      // get the contents of the #for_each body
      string forEachBody = contents.substr(posStartForEachBody,
                                           getStartOfTag(contents, posEndFor)-posStartForEachBody);

      // get the bit of text after the #endfor
      string postBody = contents.substr(getEndOfTag(contents, posEndFor, "#endfor")+1);

      // recurse back and parse the forEachBody for other for_each macros.
      string body;

      size_t posChild = macroName.rfind(".");
      if (posChild != string::npos)
         {
         string childToMatch = macroName.substr(posChild+1);
         for (XMLNode::iterator i = valuesNode.begin(); i != valuesNode.end(); i++)
            {
            if (Str_i_Eq(i->getName(), childToMatch))
               body += replaceMacros(parseForEach(forEachBody, childToMatch, *i), childToMatch, *i);
            }
         }
      else
         {
         for (XMLNode::iterator i = macroValues->begin(); i != macroValues->end(); i++)
            {
            if (Str_i_Eq(i->getName(), macroName))
               body += replaceMacros(parseForEach(forEachBody, macroName, *i), macroName, *i);
            }
         }
      forEachBody = body;

      // Resolve any #if defines.
      parseIf(forEachBody);

      contents = preBody + forEachBody + postBody;

      // locate next for_each
      posForEach = contents.find("#for_each");
      }
   return contents;
   }
// ------------------------------------------------------------------
// Write everything between #file/#endfile pairs to the
// file name listed after the #file macro.
// ------------------------------------------------------------------
void Macro::writeStringToFiles(string contents,
                               std::vector<std::string>& fileNamesCreated,
                               const string& outputDirectory) const
   {
   size_t posFile = contents.find("#file");
   while (posFile != string::npos)
      {
      posFile += strlen("#file");
      size_t posEol = contents.find("\n", posFile);
      string filename=contents.substr(posFile, posEol-posFile);
      stripLeadingTrailing(filename, " ");
      if (outputDirectory != "")
         filename = outputDirectory + "\\" + filename;
      //replaceAll(filename, "%apsuite", getApsimDirectory());

      size_t posStartFileBody = posEol + 1;
      size_t posEndFileBody = contents.find("#endfile", posStartFileBody);
      if (posEndFileBody == string::npos)
         throw runtime_error("Cannot find a matching #endfile tag");

      string fileContents = contents.substr(posStartFileBody, posEndFileBody-posStartFileBody);
      replaceGlobalCounter(fileContents);

      // Dump the file text into the given file name
      ofstream out;
      out.open (filename.c_str());
      out << fileContents;
      out.close();
      fileNamesCreated.push_back(ExpandFileName(filename.c_str()).c_str());

      posFile = contents.find("#file", posEndFileBody);
      }
   }

// ------------------------------------------------------------------
// Replace all global counter macros in the specified string.
// ------------------------------------------------------------------
void Macro::replaceGlobalCounter(string& contents) const
   {
   int globalCounter = 1;

   static const char* GLOBAL_COUNTER_INC = "#global.counter inc";
   const char* posGlobalCounter = stristr(contents.c_str(), "global.counter");
   while (posGlobalCounter != NULL)
      {
      ptrdiff_t posCounter = posGlobalCounter - contents.c_str();
      if (contents[posCounter-1] == '#'
          && Str_i_Eq(contents.substr(posCounter-1, strlen(GLOBAL_COUNTER_INC)),
                      GLOBAL_COUNTER_INC))
         {
         size_t posStartLine = posCounter;
         posStartLine = contents.rfind('\n', posStartLine);
         if (posStartLine == string::npos)
            posStartLine = 0;
         else
            posStartLine++;

         size_t posEndLine = contents.find('\n', posStartLine);
         if (posEndLine == string::npos)
            posEndLine = contents.length();

         contents.erase(posStartLine, posEndLine - posStartLine + 1);
         posGlobalCounter = stristr(contents.c_str(), "global.counter");

         globalCounter++;
         }
      else
         {
         contents.replace(posCounter, strlen("global.counter"), itoa(globalCounter));
         }
      posGlobalCounter = stristr(contents.c_str(), "global.counter");
      }
   }
// ------------------------------------------------------------------
// Resolve any #if defines.
// ------------------------------------------------------------------
void Macro::parseIf(string& st) const
   {
   size_t posElseIf = 0;
   size_t posElse = 0;
   size_t posOpenBracket;
   size_t posCloseBracket;
   size_t posCondition = st.find("#if");
   while (posCondition != string::npos)
      {
      size_t posEndIf = st.find("#endif", posCondition+1);
      size_t posEndBlock = min(min(st.find("#elseif", posCondition+1),
                                     st.find("#else", posCondition+1)), posEndIf);
      if (posEndBlock == string::npos)
         throw runtime_error("Missing endif for if: " + st.substr(posCondition));
      bool ok;
      if (posCondition == posElse && posCondition != posElseIf)
         {
         posOpenBracket = string::npos;
         posCloseBracket = string::npos;
         ok = true;
         }
      else
         {
         posOpenBracket = st.find('(', posCondition);
         posCloseBracket = st.find(')', posOpenBracket);
         ok = evaluateIf(st.substr(posOpenBracket+1, posCloseBracket-posOpenBracket-1));
         }
      if (ok)
         {
         posEndBlock = getStartOfTag(st, posEndBlock);
         posEndIf = getEndOfTag(st, posEndIf, "#endif");

         // remove everything from the end of block to after the endif.
         st.erase(posEndBlock, posEndIf-posEndBlock+1);

         // remove the condition line.
         size_t posEndCondition;
         if (posCloseBracket != string::npos)
            posEndCondition = getEndOfTag(st, posCloseBracket, ")");
         else
            posEndCondition = getEndOfTag(st, posCondition, "#else");
         posCondition = getStartOfTag(st, posCondition);
         st.erase(posCondition, posEndCondition-posCondition+1);
         }
      else
         {
         // remove everything from start of condition down to end of block.
         posCondition = getStartOfTag(st, posCondition);
         if (posEndBlock == posEndIf)
            posEndBlock = getEndOfTag(st, posEndBlock, "#endif");
         else
            posEndBlock = getStartOfTag(st, posEndBlock);
         st.erase(posCondition, posEndBlock-posCondition);
         }

      size_t posIf = st.find("#if");
      posElse = st.find("#else");
      posElseIf = st.find("#elseif");
      posCondition = min(min(posIf, posElse), posElseIf);
      }
   }
// ------------------------------------------------------------------
// Evaluated the specified #if statement.
// ------------------------------------------------------------------
bool Macro::evaluateIf(const string& conditionLine) const
   {
   vector<string> words;
   SplitStringHonouringQuotes(conditionLine, " ", words);
   if (words.size() != 3)
      throw runtime_error("Badly formatted #if statement: " + conditionLine);
   string lhs = words[0];
   string op = words[1];
   string rhs = words[2];
   stripLeadingTrailing(lhs, " ");
   stripLeadingTrailing(rhs, " ");
   if (op == "=")
      return Str_i_Eq(lhs, rhs);
   else if (op == "<>")
      return !Str_i_Eq(lhs, rhs);
   else
      {
      double lhsValue = atof(lhs.c_str());
      double rhsValue = atof(rhs.c_str());
      if (op == "<")
         return (lhsValue < rhsValue);
      else if (op == "<=")
         return (lhsValue <= rhsValue);
      else if (op == ">")
         return (lhsValue > rhsValue);
      else if (op == ">=")
         return (lhsValue >= rhsValue);
      else
         throw runtime_error("Unknown #if operator: " + op);
      }
   }

void Macro::go(const XMLNode& macroValues,
               const std::string& macroContents,
               std::vector<std::string>& filesGenerated)
   {
   std::string outputDirectory("");
   go(macroValues, macroContents, filesGenerated, outputDirectory);    
   };

