//---------------------------------------------------------------------------
#ifndef MessageDataH
#define MessageDataH

#include <General/platform.h>
#include <string.h>
#include <string>
#ifdef __WIN32__
   #include <memory.h>
#endif

#include "message.h"

class FString;
class FStrings;

namespace protocol {

typedef unsigned char byte;
typedef wchar_t WCHAR;
inline unsigned int memorySize(const bool& value)
   {
   return 1;
   }
inline unsigned int memorySize(const byte& value)
   {
   return 1;
   }
inline unsigned int memorySize(const short& value)
   {
   return 2;
   }
inline unsigned int memorySize(const int& value)
   {
   return 4;
   }
inline unsigned int memorySize(const unsigned int& value)
   {
   return 4;
   }
inline unsigned int memorySize(const long int& value)
   {
   return 8;
   }
inline unsigned int memorySize(const float& value)
   {
   return 4;
   }
inline unsigned int memorySize(const double& value)
   {
   return 8;
   }
inline unsigned int memorySize(const char& value)
   {
   return 1;
   }
inline unsigned int memorySize(const WCHAR& value)
   {
   return 2;
   }

class EXPORT MessageData
   {
   private:
      const char* startBuffer;
      char* currentPtr;
      size_t bufferSize;
   public:
      MessageData(void) : startBuffer(NULL), currentPtr(NULL), bufferSize(0) { }
      MessageData(char* dataPtr, size_t numBytes)
         : startBuffer((char*)dataPtr),
           currentPtr(dataPtr),
           bufferSize(numBytes)
         {
         }
      MessageData(const Message* msg)
         : startBuffer((char*)msg->dataPtr),
           currentPtr(msg->dataPtr),
           bufferSize(msg->nDataBytes)
         {
         }

      void reset(void) {currentPtr = (char*)startBuffer;}
      bool isValid(void) const
         {
         return (startBuffer != NULL && currentPtr != NULL
                 && bytesRead() < totalBytes());}
      char* ptr(void) const {return currentPtr;}
      const char* start(void) const {return startBuffer;}
      void seek(char* ptr) {currentPtr = ptr;}
      size_t totalBytes(void)  const {return bufferSize;}
      size_t bytesRead(void)   const {return currentPtr-startBuffer;}
      size_t bytesUnRead(void) const
         {return totalBytes()-bytesRead();}

      void movePtrBy(ptrdiff_t numBytes)
         {
         currentPtr += numBytes;
         }
      void copyFrom(const char* from, ptrdiff_t numBytes)
         {
         memcpy(currentPtr, from, numBytes);
         currentPtr += numBytes;
         }

      // boolean
      inline MessageData& operator>> (bool& value)
         {
         value = *((byte*)currentPtr) != 0;
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const bool& value)
         {
         *((byte*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
      // byte
      inline MessageData& operator>> (byte& value)
         {
         value = *((byte*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const byte& value)
         {
         *((byte*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
      // short
      inline MessageData& operator>> (short& value)
         {
         value = *((short*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const short& value)
         {
         *((short*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
      // int
      inline MessageData& operator>> (int& value)
         {
         value = *((int*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const int& value)
         {
         *((int*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
      // int
      inline MessageData& operator>> (unsigned int& value)
         {
         value = *((unsigned int*)currentPtr);
         currentPtr += memorySize((const int)value);
         return *this;
         }
      inline MessageData& operator<< (const unsigned int& value)
         {
         *((unsigned int*)currentPtr) = value;
         currentPtr += memorySize((const int)value);
         return *this;
         }
      // long int
      inline MessageData& operator>> (long int& value)
         {
         value = *((long int*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const long int& value)
         {
         *((long int*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
      // float
      inline MessageData& operator>> (float& value)
         {
         value = *((float*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const float& value)
         {
         *((float*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
      // double
      inline MessageData& operator>> (double& value)
         {
         value = *((double*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const double& value)
         {
         *((double*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
      // char
      inline MessageData& operator>> (char& value)
         {
         value = *((char*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const char& value)
         {
         *((char*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
      // wchar
      inline MessageData& operator>> (WCHAR& value)
         {
         value = *((WCHAR*)currentPtr);
         currentPtr += memorySize(value);
         return *this;
         }
      inline MessageData& operator<< (const WCHAR& value)
         {
         *((WCHAR*)currentPtr) = value;
         currentPtr += memorySize(value);
         return *this;
         }
   };

   MessageData EXPORT & operator>>(MessageData& messageData, FString& value);
   MessageData EXPORT & operator<<(MessageData& messageData, const FString& value);
   unsigned int EXPORT  memorySize(const FString& value);

   MessageData EXPORT & operator>> (MessageData& messageData, FStrings& strings);
   MessageData EXPORT & operator<< (MessageData& messageData, const FStrings& strings);
   unsigned int EXPORT memorySize(const FStrings& strings);

   std::string EXPORT DDML(const FString& );
   std::string EXPORT DDML(const FStrings& );

   } // end namespace protocol

//  std::string EXPORT DDML(const protocol::vector<FString>& ) ;
  //std::string EXPORT DDML(const ApsimVariant& );

#endif
