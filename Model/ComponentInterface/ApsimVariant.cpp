//---------------------------------------------------------------------------

#include <stdexcept>
#include "ApsimVariant.h"
#include "DataTypes.h"

//---------------------------------------------------------------------------

std::string EXPORT protocol::DDML(const ApsimVariant& value)
   {
   return DDML(ApsimVariantType());
   }
