//---------------------------------------------------------------------------
#ifndef ApsimVariantH
#define ApsimVariantH
#include "MessageData.h"
#include "Type.h"
#include "Variant.h"

#include <stdexcept>

namespace protocol {

//---------------------------------------------------------------------------
// This class encapsulate an apsim variant that is capable of being
// sent to another module.  Variables of different types can be stored
// in the variant and retrieved in any order.
//
//---------------------------------------------------------------------------

class EXPORT ApsimVariant
   {
   public:
      ApsimVariant()
         : messageData(buffer, sizeof(buffer))
         { }
      ApsimVariant(Variant& variant)
         : messageData(* variant.getMessageData())
         {
         }

      void reset(void)
         {
		 memset(buffer, 0, sizeof(buffer));
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
      bool getInternal(const FString& variableName, DataTypeCode typeCode, bool array, T& value)
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

      bool get(const FString& variableName, bool& value);
      bool get(const FString& variableName, int& value);
      bool get(const FString& variableName, float& value);
      bool get(const FString& variableName, double& value);
      bool get(const FString& variableName, FString& value);
      bool get(const FString& variableName, std::string& value);
      bool get(const FString& variableName, std::vector<int>& value);
      bool get(const FString& variableName, std::vector<float>& value);
      bool get(const FString& variableName, std::vector<double>& value);
      bool get(const FString& variableName, std::vector<std::string>& value);
      bool get(const FString& variableName, FStrings& value);


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

#endif
