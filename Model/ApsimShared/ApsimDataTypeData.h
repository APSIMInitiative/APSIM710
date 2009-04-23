//---------------------------------------------------------------------------

#ifndef ApsimDataTypeDataH
#define ApsimDataTypeDataH
#include <General/platform.h>

// ------------------------------------------------------------------
//  Short description:
//     This class encapsulates a single data type.  The
//     class exposes different methods to extract information
//     about the data type.

//  Changes:
//    DPH 19/11/2001
// ------------------------------------------------------------------
class EXPORT ApsimDataTypeData
   {
   public:
      ApsimDataTypeData(XMLNode &n) : node(n) { }

      bool isStructure(void) const;
      bool isArray(void) const;
      bool isBuiltIn(void) const;
      bool isArrayElement(void) const;
      bool isMessage(void) const;
      bool isEvent(void) const;
      std::string getName(void) const;
      std::string getKind(void) const;
      std::string getDescription(void) const;
      std::string getUnit(void) const;
      std::string getLowerBound(void) const;
      std::string getUpperBound(void) const;
      unsigned getNumFields(void) const;
      typedef TreeNodeAliasIterator< TreeNodeIterator<XMLNode>, ApsimDataTypeData> iterator;
      iterator begin(void) const ;
      iterator end(void) const  ;
      std::string getTypeString(void) const;
      std::string getType(void) const;
   private:
      XMLNode node;
   };

#endif
