#include "CMPData.h"

using namespace std;
void EXPORT getKindAndArray(const std::string& ddml,
                     std::string& kind, bool& isArray)
   {
   isArray = (ddml.find("array=\"T\"") != string::npos);
   size_t posKind = ddml.find("kind=\"");
   if (posKind != string::npos)
      {
      kind = ddml.substr(posKind + strlen("kind=\""));
      size_t posQuote = kind.find('\"');
      if (posQuote == string::npos)
         throw runtime_error("Invalid data type string: " + ddml);
      kind.erase(posQuote);
      }
   }

