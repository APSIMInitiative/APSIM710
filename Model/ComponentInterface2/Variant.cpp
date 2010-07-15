
#include <stdexcept>

#include "Interfaces.h"
#include "BuiltIns.h"
#include "ArraySpecifier.h"
#include "DataTypes.h"
#include "MessageData.h"

#include "Variant.h"

using namespace std;

// UGLY HACK for workarounds. rewrite this asap
Variant::Variant() { bufStart = NULL; bufLen = 0;} 

Variant::~Variant() { if (bufStart) {free(bufStart); bufLen = 0;}};

   void pack(MessageData& messageData, const Variant& data)
       {
       messageData.copyFrom(data.bufStart, data.bufLen);
       }
   void unpack(MessageData& messageData, Variant& data)
       {
       if (data.bufStart) {free( data.bufStart);}
       data.bufStart = (char*) malloc (messageData.bytesUnRead());
       data.bufLen = messageData.bytesUnRead();
       memcpy(data.bufStart, messageData.ptr(), messageData.bytesUnRead()); 
       }
   unsigned memorySize(Variant& data) 
       {
       return data.bufLen;
       }
   std::string DDML(const Variant& data)
       {
	   ApsimVariantType dummy;	
	   return DDML(dummy);
       }


enum DataTypeCode  {DTint1 = 0,
                    DTint2 = 1,
                    DTint4 = 2,
                    DTint8 = 3,
                    DTsingle = 4,
                    DTdouble = 5,
                    DTboolean = 6,
                    DTchar = 7,
                    DTstring = 8,
                    DTwchar = 9,
                    DTwstring = 10,
                    DTunknown = 11};

bool Variant::get(const std::string& name, std::string &value)
{
   MessageData m(bufStart, bufLen);
   while (m.isValid()) 
      {
      std::string fieldName;
      int numBytes; 
      ::unpack(m, fieldName); 
      ::unpack(m, numBytes); 
      if (name == fieldName) 
         {
         int typeCode;
         bool isArray;
         ::unpack(m, typeCode); 
         if (typeCode != 8) throw std::runtime_error("Variant Type (string) conversion NYI");
         unpack(m, isArray); 
		 if (isArray) 
		 {
      		 int arraySize;
			 ::unpack(m, arraySize);
             if (arraySize > 1) throw std::runtime_error("Multiple values provided when only one was expected");
		 }
         ::unpack(m, value); 
         return true;
         }
      m.movePtrBy(numBytes);
      }
   return false;
}

bool Variant::get(const std::string& name, float &value)
{
   MessageData m(bufStart, bufLen);
   while (m.isValid()) 
      {
      std::string fieldName;
      int numBytes; 
      ::unpack(m, fieldName); 
      ::unpack(m, numBytes); 
      if (name == fieldName) 
         {
         int typeCode;
         bool isArray;
         ::unpack(m, typeCode); 
         ::unpack(m, isArray); 
		 if (isArray) 
		 {
      		 int arraySize;
			 ::unpack(m, arraySize);
             if (arraySize > 1) throw std::runtime_error("Multiple values provided when only one was expected");
		 }
         if (typeCode == 8)
            {
            std::string scratch;
            ::unpack(m, scratch); 
            Convert(scratch, value);
            return true;
            }
         if (typeCode != 4) throw std::runtime_error("Variant Type (float) conversion NYI");
         ::unpack(m, value); 
         return true;
         }
      m.movePtrBy(numBytes);
      }
   return false;
}

void Variant::pack(const std::string& name, const std::string &value)
{
	std::vector<std::string>values;
	values.push_back(value);
	pack(name, values);
//   printf("packVariant(string): %d = %d\n", newSize, m.totalBytes());
}

void Variant::pack(const std::string& name, std::vector<float> &value)
{
	std::vector<std::string>values;
  for (int i = 0; i < (int)value.size(); ++i ) 
	{
		values.push_back (ftoa(value[i],12));
	}
	pack(name, values);
//   printf("packVariant (float[]): %d = %d\n", newSize, m.totalBytes());
}

void Variant::pack(const std::string& name, std::vector<std::string> &value)
{
   int numBytes = 4;
   for (unsigned int i = 0; i != value.size(); i++)
       numBytes += 4 + value[i].size();
   int newSize = 4 + name.size() +     /* name */
                 4 +                   /* numBytes */
                 4 +                   /* typeCode */
                 1 +                   /* isArray */
                 numBytes;

   bufStart = (char *) realloc(bufStart, bufLen + newSize);
   MessageData m(bufStart, bufLen + newSize);
   m.seek(bufStart+bufLen);
   bufLen += newSize;
   
   ::pack(m, name);
   ::pack(m, (int)(5 + numBytes)); 
   ::pack(m, (int)DTstring); 
   ::pack(m, (bool)true); 
   ::pack(m, value); 
//   printf("packVariant (string[]): %d = %d\n", newSize, m.totalBytes());
}
