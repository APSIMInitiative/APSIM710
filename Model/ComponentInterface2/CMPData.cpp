#include "CMPData.h"

using namespace std;
void EXPORT getKindAndArray(const std::string& ddml,
                     std::string& kind, bool& isArray)
   {
   isArray = (ddml.find("array=\"T\"") != string::npos);
   unsigned posKind = ddml.find("kind=\"");
   if (posKind != string::npos)
      {
      kind = ddml.substr(posKind + strlen("kind=\""));
      unsigned posQuote = kind.find('\"');
      if (posQuote == string::npos)
         throw runtime_error("Invalid data type string: " + ddml);
      kind.erase(posQuote);
      }
   }

void EXPORT performBoundCheck(const std::string& name, int value, int lower, int upper)
   {
   if (value < lower || value > upper)
      printf("Value of variable is out of bounds.\n"
             "Variable: %s\n"
             "Value: %d\n"
             "Bounds: %d to %d",
             name.c_str(), value, lower, upper);
   }
void EXPORT performBoundCheck(const std::string& name, float value, float lower, float upper)
   {
   if (value < lower || value > upper)
      printf("Value of variable is out of bounds.\n"
             "Variable: %s\n"
             "Value: %16.7f\n"
             "Bounds: %16.7f to %16.7f",
             name.c_str(), value, lower, upper);
   }
void EXPORT performBoundCheck(const std::string& name, double value, double lower, double upper)
   {
   if (value < lower || value > upper)
      printf("Value of variable is out of bounds.\n"
             "Variable: %s\n"
             "Value: %16.7lf\n"
             "Bounds: %16.7lf to %16.7lf",
             name.c_str(), value, lower, upper);
   }

