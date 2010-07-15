#ifndef MessageDataExtH
#define MessageDataExtH

#include <iostream>
#include "MessageData.h"
#include <string>
#include <vector>

namespace protocol {

// STRING specialisations
inline MessageData& operator>>(MessageData& messageData, std::string& value)
   {
   int numChars;
   messageData >> numChars;
   value = std::string(messageData.ptr(), numChars);
   messageData.movePtrBy(numChars);
   return messageData;
   };
inline MessageData& operator<<(MessageData& messageData, const std::string& value)
   {
   messageData << (int)value.length();
   messageData.copyFrom(value.c_str(), value.length());
   return messageData;
   }
inline unsigned int memorySize(const std::string& value)
   {
   return value.length() + sizeof(int);
   }

// VECTOR type
template <class T>
inline MessageData& operator<<(MessageData& messageData, const std::vector<T>& values)
   {
   messageData << (int)values.size();
   for (unsigned int i = 0; i < values.size(); i++)
      messageData << values[i];
   return messageData;
   }
template <class T>
inline MessageData& operator>>(MessageData& messageData, std::vector<T>& values)
   {
   values.erase(values.begin(), values.end());
   unsigned int numValues;
   messageData >> numValues;
   for (unsigned int i = 0; i < numValues; i++)
      {
      T value;
      messageData >> value;
      values.push_back(value);
      }
   return messageData;
   };
template <class T>
inline unsigned int memorySize(const std::vector<T>& values)
   {
   unsigned size = 4;
   for (unsigned i = 0; i != values.size(); i++)
      size += memorySize(values[i]);
   return size;
   }

   } // end namespace protocol
#endif
