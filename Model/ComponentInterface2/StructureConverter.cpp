#include "StructureConverter.h"
#include <ComponentInterface2/MessageData.h>
#include <ComponentInterface2/CMPData.h>

using namespace std;
#define max_array_size 100
// ------------------------------------------------------------------
// Find a variable on the messageData assuming an ApsimVariant. Returns
// true if found, false otherwise.
// ------------------------------------------------------------------
template <class T>
bool unpackFromApsimVariant(MessageData& messageData, const string& variableName, T& data)
   {
   char* savedPtr = messageData.ptr();

   string varName;
   int numBytes;
   unpack(messageData, varName);
   unpack(messageData, numBytes);
   bool found;
   while (!(found = (Str_i_Eq(varName, variableName))))
      {
      messageData.movePtrBy(numBytes);
      if (!messageData.isValid()) break;
      unpack(messageData, varName);
      unpack(messageData, numBytes);
      }
   if (found)
      {
      int code;
      bool isArray;
      unpack(messageData, code);
      unpack(messageData, isArray);
      string ddml;
      switch (code)
         {
         case 0 : ddml = "<type kind=\"integer1\" />"; break;
         case 1 : ddml = "<type kind=\"integer2\" />"; break;
         case 2 : ddml = "<type kind=\"integer4\" />"; break;
         case 3 : ddml = "<type kind=\"integer8\" />"; break;
         case 4 : ddml = "<type kind=\"single\" />"; break;
         case 5 : ddml = "<type kind=\"double\" />"; break;
         case 6 : ddml = "<type kind=\"boolean\" />"; break;
         case 7 : ddml = "<type kind=\"char\" />"; break;
         case 8 : ddml = "<type kind=\"string\" />"; break;
         case 9 : ddml = "<type kind=\"wchar\" />"; break;
         case 10 : ddml = "<type kind=\"wstring\" />"; break;
         case 11 : throw runtime_error("Unknown type in ApsimVariant");
         }
      if (isArray)
         replaceAll(ddml, "/>", "array=\"T\" />");

      CMPBuiltIn<T&> converter(data);
      converter.unpack(messageData, ddml);
      }
   messageData.seek(savedPtr);
   return found;
   }

bool IsVarFieldName(const std::string& name)
{
	return ((name != "") &&	(name.substr(0,5) != "param" || name.substr(6,1) != "_"));
}

bool EXPORT unpackField(MessageData& messageData, const std::string& ddml, bool& data, const std::string& name)
   {
   data = false;
   if (ddml.find("name=\"param1_numbytes\"") != string::npos && IsVarFieldName(name))
      return unpackFromApsimVariant(messageData, name, data);
   else
      unpack(messageData, data);
   return true;
   }
bool EXPORT unpackField(MessageData& messageData, const std::string& ddml, int& data, const std::string& name)
   {
   data = 0;
   if (ddml.find("name=\"param1_numbytes\"") != string::npos && IsVarFieldName(name))
      return unpackFromApsimVariant(messageData, name, data);
   else
      unpack(messageData, data);
   return true;
   }
bool EXPORT unpackField(MessageData& messageData, const std::string& ddml, float& data, const std::string& name)
   {
   data = 0.0;
   if (ddml.find("name=\"param1_numbytes\"") != string::npos && IsVarFieldName(name))
      return unpackFromApsimVariant(messageData, name, data);
   else
      unpack(messageData, data);
   return true;
   }
bool EXPORT unpackField(MessageData& messageData, const std::string& ddml, double& data, const std::string& name)
   {
   data = 0.0;
   if (ddml.find("name=\"param1_numbytes\"") != string::npos && IsVarFieldName(name))
      return unpackFromApsimVariant(messageData, name, data);
   else
      unpack(messageData, data);
   return true;
   }
bool EXPORT unpackField(MessageData& messageData, const std::string& ddml, std::string& data, const std::string& name)
   {
   data = "";
   if (ddml.find("name=\"param1_numbytes\"") != string::npos && IsVarFieldName(name))
     return unpackFromApsimVariant(messageData, name, data);
   else
      unpack(messageData, data);
   return true;
   }
bool EXPORT unpackField(MessageData& messageData, const std::string& ddml, char* data, const std::string& name)
   {
   if (ddml.find("name=\"param1_numbytes\"") != string::npos && IsVarFieldName(name))
      {
      string wrapper;
      bool found = unpackFromApsimVariant(messageData, name, wrapper);
      wrapper.copy(data, max_array_size);
      memset(&data[wrapper.length()], ' ', max_array_size - wrapper.length());
      return found;
      }
   return true;
   }

