//---------------------------------------------------------------------------

#ifndef ApsimDataTypesFileH
#define ApsimDataTypesFileH
#include <General/platform.h>

class EXPORT ApsimDataTypeData;
//---------------------------------------------------------------------------
// This class encapsulates the apsim types database ie. (apsim\datatypes.interface)
//---------------------------------------------------------------------------
class EXPORT ApsimDataTypesFile
   {
   public:
      ApsimDataTypesFile(void);
      ApsimDataTypesFile(const std::string& ddml);

      // Return a specific data type to caller.  Will throw if that type doesn't
      // exist.
      ApsimDataTypeData getDataType(const std::string& name);

      typedef TreeNodeAliasIterator< TreeNodeIterator<XMLNode>, ApsimDataTypeData> iterator;
      iterator begin() const;
      iterator end() const;

   private:
      std::string fileName;
      XMLDocument xmlDoc;
   };
#endif
