#ifndef VariantH
#define VariantH
#include "MessageData.h"
#include "TypeConverter.h"
#include "Type.h"
#include "ArraySpecifier.h"

// turn of the warnings about "Functions containing for are not expanded inline.
#pragma warn -inl

namespace protocol {
// ------------------------------------------------------------------
//  Short description:
//     Variant class for handling different data types.

//  Notes:
//     When a Variant is created from another variant, it
//     takes a copy of the message data bytes so that
//     it is completely indendant of other variants or messages

//  Changes:
//    DPH 20/6/2001

// ------------------------------------------------------------------
class Variant
   {
   public:
      Variant(void)
         : newDataPtr(NULL) { }

      // this constructor is needed for storing a copy of a variant in a
      // variants class.
      Variant(const Variant& from)
         {
         copyFrom(from);
         }

      ~Variant(void) {delete [] newDataPtr;}
      Variant& operator= (const Variant& rhs)
         {
         delete [] newDataPtr;
         copyFrom(rhs);
         return *this;
         }

      bool isValid(void) {return messageData.isValid();}
      const Type& getType(void) const {return type;}
      unsigned getFromId(void) {return fromId;}
      void setFromId(unsigned id) {fromId = id;}

      template <class T>
      bool unpack(T& obj)
         {
         messageData >> obj;
         return true;
         }

      template <class T>
      bool unpack(TypeConverter *typeConverter, ArraySpecifier *arraySpecifier, T& obj)
         {
         if (arraySpecifier != NULL)
            {
            if (!type.isArray())
               return false;
            else
               arraySpecifier->convert(messageData, type.getCode());
            }
         if (typeConverter != NULL)
            typeConverter->getValue(messageData, obj);

         else
            messageData >> obj;
         return true;
         }

      template <class T>
      bool unpackArray(T obj[], unsigned& numValues)
         {
         messageData >> numValues;
         for (unsigned i = 0; i != numValues; i++)
            messageData >> obj[i];
         return true;
         }

      template <class T>
      bool unpackArray(TypeConverter *typeConverter, ArraySpecifier *arraySpecifier, T obj[], unsigned& numValues)
         {
         if (arraySpecifier != NULL)
            {
            if (!type.isArray())
               return false;
            else
               arraySpecifier->convert(messageData, type.getCode());
            }
         messageData >> numValues;
         for (unsigned i = 0; i != numValues; i++)
            messageData >> obj[i];
         return true;
         }
      void aliasTo(Variant& variant)
         {
         type = Type(variant.type);
         fromId = variant.fromId;
         messageData = MessageData(variant.messageData.ptr(), variant.messageData.bytesUnRead());
         }
      void aliasTo(MessageData& fromMessageData)
         {
         // extract the type and simply alias to the remaining unread bytes.
         fromMessageData >> type;
         messageData = MessageData(fromMessageData.ptr(), fromMessageData.bytesUnRead());
         }
      void writeTo(MessageData& toMessageData) const
         {
         // write type and then the data.
         toMessageData << type;
         toMessageData.copyFrom(messageData.start(), messageData.totalBytes());
         }
      unsigned size(void) const {return memorySize(type) + messageData.totalBytes();}
      const MessageData& getMessageData(void) const {return messageData;}

   private:
      char* newDataPtr;
      Type type;
      MessageData messageData;
      unsigned fromId;
      
      void copyFrom(const Variant& from)
         {
         newDataPtr = new char[from.messageData.totalBytes()+1];
         messageData = MessageData(newDataPtr, from.messageData.totalBytes());
         messageData.copyFrom(from.messageData.start(), from.messageData.totalBytes());
         messageData.reset();
         type = from.type;
         fromId = from.fromId;
         }

   };

inline MessageData& operator>> (MessageData& messageData, Variant& value)
   {
   value.aliasTo(messageData);
   return messageData;
   }

inline MessageData& operator<< (MessageData& messageData, const Variant& value)
   {
   value.writeTo(messageData);
   return messageData;
   }
inline unsigned int memorySize(Variant& value)
   {
   return value.size();
   }

} // namespace protocol

// restore the warnings about "Functions containing for are not expanded inline.
#pragma warn .inl

#endif
