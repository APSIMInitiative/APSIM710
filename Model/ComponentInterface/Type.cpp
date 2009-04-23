#pragma hdrstop

#include <stdexcept>
#include "Type.h"
using namespace protocol;

// ------------------------------------------------------------------
// Return the value of the specified attribute
// ------------------------------------------------------------------
FString Type::getAttribute(const char* attributeName) const
   {
   char stringToLocate[100];
   strcpy(stringToLocate, attributeName);
   strcat(stringToLocate, "=\"");
   unsigned posAttr = type.find(stringToLocate);
   if (posAttr == FString::npos)
      return "";
   else
      {
      posAttr += strlen(stringToLocate);
      FString attrPlusRemainder = type.substr(posAttr);
      unsigned posEndQuote = attrPlusRemainder.find("\"");
      return attrPlusRemainder.substr(0, posEndQuote);
      }
   }
// ------------------------------------------------------------------
// Set the value of the specified attribute in this type.
// ------------------------------------------------------------------
void Type::setAttribute(const char* attributeName, const char* value)
   {
   char stringToLocate[100];
   strcpy(stringToLocate, attributeName);
   strcat(stringToLocate, "=\"");
   unsigned posInsertion = type.find(stringToLocate);
   if (posInsertion == FString::npos)
      {
      posInsertion = type.find("/>");
      if (posInsertion == FString::npos)
         {
         char buffer[200];
         strcpy(buffer, "Invalid type string : ");
         strncat(buffer, type.f_str(), type.length());
         throw std::runtime_error(buffer);
         }
      else
         {
         char st[100];
         strcpy(st, " ");
         strcat(st, attributeName);
         strcat(st, "=\"");
         strcat(st, value);
         strcat(st, "\"");
         type.insert(posInsertion, st);
         }
      }
   else
      {
      posInsertion += strlen(stringToLocate);
      unsigned posEndQuote = type.find("\"", posInsertion);
      if (posEndQuote != FString::npos)
         {
         type.erase(posInsertion, posEndQuote-posInsertion);
         type.insert(posInsertion, value);
         }
      }
   }
// ------------------------------------------------------------------
//  Short description:
//     determine the data type from the string passed in.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void Type::determineType(void) const
   {
   FString kind = getAttribute("kind");
   if (kind == "integer1")
      code = DTint1;
   else if (kind == "integer2")
      code = DTint2;
   else if (kind == "integer4")
      code = DTint4;
   else if (kind == "integer8")
      code = DTint8;
   else if (kind == "single")
      code = DTsingle;
   else if (kind == "double")
      code = DTdouble;
   else if (kind == "boolean")
      code = DTboolean;
   else if (kind == "char")
      code = DTchar;
   else if (kind == "string")
      code = DTstring;
   else
      code = DTunknown;
   }
// ------------------------------------------------------------------
// convert a type code to a type string.
// ------------------------------------------------------------------
FString Type::codeToString(DataTypeCode code)
   {
   if (code == DTint1)
      return "integer1";
   else if (code == DTint2)
      return "integer2";
   else if (code == DTint4)
      return "integer4";
   else if (code == DTint8)
      return "integer8";
   else if (code == DTsingle)
      return "single";
   else if (code == DTdouble)
      return "double";
   else if (code == DTboolean)
      return "boolean";
   else if (code == DTchar)
      return "char";
   else if (code == DTstring)
      return "string";
   else
      return "unknown";
   }
// ------------------------------------------------------------------
// if isArray = true then makes this type an array type.
// ------------------------------------------------------------------
void Type::setArray(bool isArray)
   {
   if (isArray)
      setAttribute("array", "T");
   else
      setAttribute("array", "F");

   }

