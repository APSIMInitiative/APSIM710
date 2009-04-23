//---------------------------------------------------------------------------

#include <stdexcept>
#include "ApsimVariant.h"
#include "DataTypes.h"

//---------------------------------------------------------------------------

#pragma package(smart_init)

std::string EXPORT protocol::DDML(const ApsimVariant& value)
   {
   return DDML(ApsimVariantType());
   }
