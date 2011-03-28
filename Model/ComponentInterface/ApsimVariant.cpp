//---------------------------------------------------------------------------

#include <stdexcept>
#include "ApsimVariant.h"
#include "DataTypes.h"

using namespace protocol;
//---------------------------------------------------------------------------


std::string EXPORT protocol::DDML(const ApsimVariant& value)
   {
   return DDML(ApsimVariantType());
   }

bool protocol::ApsimVariant::get(const FString& variableName, int& value)
   {
   if (getInternal(variableName, dataTypeCodeOf(value), false, value))
      return true;
   else
      {
      value = 0;
      return false;
      }
   }
bool protocol::ApsimVariant::get(const FString& variableName, bool& value)
   {
   if (getInternal(variableName, dataTypeCodeOf(value), false, value))
      return true;
   else
      {
      value = false;
      return false;
      }
   }
bool protocol::ApsimVariant::get(const FString& variableName, float& value)
   {
   if (getInternal(variableName, dataTypeCodeOf(value), false, value))
      return true;
   else
      {
      value = 0.0;
      return false;
      }
   }
bool protocol::ApsimVariant::get(const FString& variableName, double& value)
   {
   if (getInternal(variableName, dataTypeCodeOf(value), false, value))
      return true;
   else
      {
      value = 0.0;
      return false;
      }
   }
bool protocol::ApsimVariant::get(const FString& variableName, FString& value)
   {
   if (getInternal(variableName, dataTypeCodeOf(value), false, value))
      return true;
   else
      {
      value = "";
      return false;
      }
   }
bool protocol::ApsimVariant::get(const FString& variableName, std::string& value)
   {
   return getInternal(variableName, dataTypeCodeOf(value), false, value);
   }
bool protocol::ApsimVariant::get(const FString& variableName, std::vector<int>& value)
   {
   return getInternal(variableName, dataTypeCodeOf(value), true, value);
   }

bool protocol::ApsimVariant::get(const FString& variableName, std::vector<float>& value)
   {
   return getInternal(variableName, dataTypeCodeOf(value), true, value);
   }
bool protocol::ApsimVariant::get(const FString& variableName, std::vector<double>& value)
   {
   return getInternal(variableName, dataTypeCodeOf(value), true, value);
   }
bool protocol::ApsimVariant::get(const FString& variableName, std::vector<std::string>& value)
   {
   return getInternal(variableName, dataTypeCodeOf(value), true, value);
   }
bool protocol::ApsimVariant::get(const FString& variableName, FStrings& value)
   {
   std::vector<string> values;
   bool ok = getInternal(variableName, DTstring, true, values);
   value = values;
   return ok;
   }
