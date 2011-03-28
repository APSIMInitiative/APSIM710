#ifndef TypeH
#define TypeH
#include "MessageData.h"
#include <ApsimShared/FString.h>
#include <General/platform.h>
#include <vector>
namespace protocol {

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

#define DTint1String     "<type kind=\"integer1\"/>"
#define DTint2String     "<type kind=\"integer2\"/>"
#define DTint4String     "<type kind=\"integer4\"/>"
#define DTint8String     "<type kind=\"integer8\"/>"
#define DTsingleString   "<type kind=\"single\"/>"
#define DTdoubleString   "<type kind=\"double\"/>"
#define DTbooleanString  "<type kind=\"boolean\"/>"
#define DTcharString     "<type kind=\"char\"/>"
#define DTstringString   "<type kind=\"string\"/>"
#define DTwcharString    "<type kind=\"wchar\"/>"
#define DTwstringString  "<type kind=\"wstring\"/>"

// ------------------------------------------------------------------
// Encapsulates a DDML type string.  Does NOT allocate any
// memory.  Relies on the string passed in.  If a const string
// is passed in then the type is read-only and cannot be
// modified.
// ------------------------------------------------------------------
class EXPORT Type
   {
   public:
      Type(void) : code(DTunknown) { };
      Type(const char* typeString) : type(typeString), code(DTunknown) { }
      Type(FString& typeString) : type(typeString), code(DTunknown) { }
      Type(const FString& typeString) : type(typeString), code(DTunknown) { }
      Type(const Type& rhs)
         {
         type.aliasTo(rhs.type.f_str(), rhs.type.length());
         code = rhs.code;
         }
      Type& operator= (const Type& rhs)
         {
         type.aliasTo(rhs.type.f_str(), rhs.type.length());
         code = rhs.code;
         return *this;
         }

      void initFrom(MessageData& messageData)
         {
         messageData >> type;
         }
      void writeTo(MessageData& messageData) const
         {
         messageData << type;
         }
      unsigned int memorySize(void) const
         {
         return protocol::memorySize(type);
         }

      FString getUnits(void) const {return getAttribute("unit");}
      bool isArray(void) const {return (getAttribute("array") == "T");}
      void setArray(bool isArray);
      DataTypeCode getCode(void) const
         {
         if (code == DTunknown)
            determineType();
         return code;
         }
      const FString& getTypeString(void) const {return type;}

      static FString codeToString(DataTypeCode code);
      bool isApsimVariant()
         {
         return (type.find("param1_name") != FString::npos);
         }
   private:
      FString type;
      mutable DataTypeCode code;

      void determineType(void) const;
      FString getAttribute(const char* attributeName) const;
      void setAttribute(const char* attributeName, const char* value);


   };
inline MessageData& operator>> (MessageData& messageData, Type& type)
   {
   type.initFrom(messageData);
   return messageData;
   }
inline MessageData& operator<< (MessageData& messageData, const Type& type)
   {
   type.writeTo(messageData);
   return messageData;
   }
inline unsigned int memorySize(const Type& type)
   {
   return type.memorySize();
   }

inline DataTypeCode dataTypeCodeOf(bool)   {return DTboolean;}
inline DataTypeCode dataTypeCodeOf(int)    {return DTint4;}
inline DataTypeCode dataTypeCodeOf(float)  {return DTsingle;}
inline DataTypeCode dataTypeCodeOf(double) {return DTdouble;}
inline DataTypeCode dataTypeCodeOf(char)   {return DTchar;}
inline DataTypeCode dataTypeCodeOf(char *) {return DTstring;}
inline DataTypeCode dataTypeCodeOf(FString&) {return DTstring;}
inline DataTypeCode dataTypeCodeOf(std::string&) {return DTstring;}

inline DataTypeCode dataTypeCodeOf(const std::vector<int>&) {return DTint4;}
inline DataTypeCode dataTypeCodeOf(const std::vector<float>&) {return DTsingle;}
inline DataTypeCode dataTypeCodeOf(const std::vector<double>&) {return DTdouble;}
inline DataTypeCode dataTypeCodeOf(const std::vector<std::string>&) {return DTstring;}


} // namespace protocol

#endif
