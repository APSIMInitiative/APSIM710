#include "FortranString.h"
#include <ComponentInterface2/MessageData.h>
#include <ComponentInterface2/BuiltIns.h>

using namespace std;
void unpack(MessageData& messageData, FortranString& value)
   {
   int numChars;
   unpack(messageData, numChars);
   value = FortranString(messageData.ptr(), numChars);
   messageData.movePtrBy(numChars);
   };
void pack(MessageData& messageData, const FortranString& value)
   {
   int numChars = (int)value.length();
   pack(messageData, numChars);
   messageData.copyFrom(value.f_str(), numChars);
   }
unsigned memorySize(const FortranString& value)
   {
   return value.length() + sizeof(int);
   }
std::string DDML(const FortranString& )
   {
   return "<type kind=\"string\" />";
   }

void unpack(MessageData& messageData, FortranStrings& strings)
   {
   vector<string> sts;
   ::unpack(messageData, sts);
   strings = sts;
   }
void pack(MessageData& messageData, const FortranStrings& strings)
   {
   ::pack(messageData, (int) strings.getNumElements());
   for (unsigned i = 0; i < strings.getNumElements(); i++)
      ::pack(messageData, strings.getString(i));
   }
unsigned memorySize(const FortranStrings& strings)
   {
   unsigned size = 4;
   for (unsigned i = 0; i < strings.getNumElements(); i++)
      size += ::memorySize(strings.getString(i));
   return size;
   }
std::string DDML(const FortranStrings& )
   {
   return "<type kind=\"string\" array=\"T\" />";
   }

void Convert(bool source, FortranString& dest)
   {
   string st;
   ::Convert(source, st);
   dest = st;
   }
void Convert(int source, FortranString& dest)
   {
   string st;
   ::Convert(source, st);
   dest = st;
   }
void Convert(const std::string& source, FortranString& dest)
   {
   dest = source;
   }
void Convert(const std::vector<std::string>& source, FortranStrings& dest)
   {
   for (unsigned i = 0; i != source.size(); i++)
     dest.addString(source[i]);
   }
void Convert(float source, FortranString& dest)
   {
   string st;
   ::Convert(source, st);
   dest = st;
   }
void Convert(double source, FortranString& dest)
   {
   string st;
   ::Convert(source, st);
   dest = st;
   }


void Convert(bool source, FortranStrings& dest)
   {
   string st;
   ::Convert(source, st);
   dest.addString(st);
   }
void Convert(int source, FortranStrings& dest)
   {
   string st;
   ::Convert(source, st);
   dest.addString(st);
   }
void Convert(const std::string& source, FortranStrings& dest)
   {
   dest.addString(source);
   }
void Convert(float source, FortranStrings& dest)
   {
   string st;
   ::Convert(source, st);
   dest.addString(st);
   }
void Convert(double source, FortranStrings& dest)
   {
   string st;
   ::Convert(source, st);
   dest.addString(st);
   }

