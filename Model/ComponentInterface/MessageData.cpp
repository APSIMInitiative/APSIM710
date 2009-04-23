#include <stdlib.h>
#include <string.h>
#include <stdexcept>

#include <ApsimShared/FString.h>
#include <General/platform.h>
#include "MessageData.h"
#include "message.h"
#include "ProtocolVector.h"

#include "MessageDataExt.h"
using namespace std;
namespace protocol {

// FSTRING specialisations
MessageData& EXPORT operator>>(MessageData& messageData, FString& value)
   {
   int numChars;
   messageData >> numChars;
   value.aliasTo(messageData.ptr(), numChars);
   messageData.movePtrBy(numChars);
   return messageData;
   };
MessageData& EXPORT operator<<(MessageData& messageData, const FString& value)
   {
   messageData << (int)value.length();
   messageData.copyFrom(value.f_str(), value.length());
   return messageData;
   }
unsigned int EXPORT memorySize(const FString& value)
   {
   return value.length() + sizeof(int);
   }

// FSTRINGS specialisations
MessageData& EXPORT operator>> (MessageData& messageData, FStrings& strings)
   {
   unsigned numElements;
   messageData >> numElements;
   for (unsigned int i = 0; i < numElements; i++)
      {
      FString rhs;
      messageData >> rhs;
      strings.addString(rhs);
      }
   return messageData;
   }
MessageData& EXPORT operator<< (MessageData& messageData, const FStrings& strings)
   {
   messageData << strings.getNumElements();
   for (unsigned int i = 0; i < strings.getNumElements(); i++)
      messageData << strings.getString(i);

   return messageData;
   }
unsigned int EXPORT memorySize(const FStrings& strings)
   {
   unsigned size = 4;
   for (unsigned int i = 0; i < strings.getNumElements(); i++)
      size += memorySize(strings.getString(i));
   return size;
   }

static const char* stringType = "<type kind=\"string\"/>";
static const char* stringArrayType = "<type kind=\"string\" array=\"T\"/>";

  std::string DDML(const FString& )
     { return(stringType); }
  std::string DDML(const FStrings& )
     { return(stringArrayType); }

} // end namespace protocol

/*  std::string EXPORT DDML(const protocol::vector<FString>& )
     { return(stringArrayType); }
*/