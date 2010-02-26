//---------------------------------------------------------------------------
#ifndef ApsimVariantH
#define ApsimVariantH
#include "MessageData.h"
#include "Type.h"
#include "Variant.h"

#include <stdexcept>

// turn of the warnings about "Functions containing for are not expanded inline.
#pragma warn -inl

namespace protocol {

//---------------------------------------------------------------------------
// This class encapsulate an apsim variant that is capable of being
// sent to another module.  Variables of different types can be stored
// in the variant and retrieved in any order.
//
//---------------------------------------------------------------------------

class ApsimVariant
   {
   public:
      ApsimVariant(Component* p)
         : messageData(buffer, sizeof(buffer)), parent(p)
         { }
      ApsimVariant(Component* p, Variant& variant)
         : messageData(* variant.getMessageData()), parent(p)
         {
         }

      void reset(void)
         {
         messageData.reset();
         }

      template <class T>
      void store(const FString& variableName, DataTypeCode typeCode, bool isArray, const T& value)
         {
         messageData << variableName << protocol::memorySize(value)+5 << typeCode << isArray << value;
         if (!messageData.isValid())
            {
//            char m[100];
//            sprintf(m, "%p", &messageData);
//            ::MessageBox(NULL, m, "", MB_OK);
//            DebugException();
            char msg[100];
            strcpy(msg, "Too many items stored in variant/postbox.  Variable name: ");
            strncat(msg, variableName.f_str(), variableName.length());
     	    throw std::runtime_error(msg);
            }

         }

      template <class T>
      bool get(const FString& variableName, DataTypeCode typeCode, bool array, T& value)
         {
         if (findVariable(variableName))
            {
            int code;
            bool isArray;
            messageData >> code >> isArray;
            TypeConverter* converter;
            getTypeConverter(variableName,
                                 (DataTypeCode)code, typeCode,
                                 isArray, array,
                                 converter);
            if (converter == NULL)
               messageData >> value;
            else
               {
               converter->getValue(messageData, value);
               delete converter;
               }
            return true;
            }
         return false;
         }

      void aliasTo(const MessageData& fromMessageData)
         {
         messageData = fromMessageData;
         messageData.reset();
         }
      void writeTo(MessageData& toMessageData) const
         {
         toMessageData.copyFrom(messageData.start(), messageData.bytesRead());
         }
      unsigned memorySize(void) const
         {
         return messageData.ptr() - buffer;
         }

   private:
      Component* parent;
      char buffer[10000];
      Type type;
      MessageData messageData;

      bool findVariable(const FString& variableName)
         {
         reset();

         if (!messageData.isValid())
            return false;
         FString varName;
         unsigned numBytes;
         messageData >> varName >> numBytes;
         bool found;
         while (!(found = (varName == variableName)))
            {
            messageData.movePtrBy(numBytes);
            if (!messageData.isValid()) break;
            messageData >> varName >> numBytes;
            }
         return found;
         }

   };
inline MessageData& operator>> (MessageData& messageData, ApsimVariant& variant)
   {
   variant.aliasTo(messageData);
   return messageData;
   }
inline MessageData& operator<< (MessageData& messageData, const ApsimVariant& variant)
   {
   variant.writeTo(messageData);
   return messageData;
   }
inline unsigned int memorySize(const ApsimVariant& variant)
   {
   return variant.memorySize();
   }

std::string EXPORT DDML(const ApsimVariant& value);

} // namespace protocol

// restore the warnings about "Functions containing for are not expanded inline.
#pragma warn .inl

#endif
