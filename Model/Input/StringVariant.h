//---------------------------------------------------------------------------

#ifndef StringVariantH
#define StringVariantH

struct Value;
namespace protocol {
   struct QueryValueData;
   struct QuerySetValueData;
   class Component;
   }
// ------------------------------------------------------------------
//  Short description:
//     Encapsulates a single string based variant variable
//     The variant has a name, type, and value(s).  When more
//     than one value is present, the values are treated
//     as an array.
//
//     Array indexing is zero based.

//  Notes:

//  Changes:
//    DPH 27/6/2001

// ------------------------------------------------------------------
class EXPORT StringVariant
   {
   public:
      StringVariant(void) : value(NULL), parent(NULL), defaultValue(NULL) { }
      StringVariant(Value* value, protocol::Component* parent, bool asDefault = false);

      void sendVariable(protocol::QueryValueData& queryData, bool useMainValue);
      void setVariable(protocol::QuerySetValueData& setValueData);
      unsigned doRegistration(void);
      float asFloat(void);
      float asInteger(void);
      std::string getName(void);
      void setTemporalValue(Value* value);

   private:
      protocol::Component* parent;
      Value* value;
      Value* defaultValue;

      enum Type {Real, Integer, String, RealArray, IntegerArray, StringArray};
      Type type;
      string typeString;

      void determineType(void);
   };
#endif
