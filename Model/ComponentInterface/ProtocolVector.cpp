
#include "ProtocolVector.h"
#include <General/string_functions.h>
#include <General/platform.h>
#include <stdexcept>
namespace protocol {
// ------------------------------------------------------------------
//  Short description:
//     Display a "too many items error message".

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void EXPORT tooManyError(unsigned int maxCount)
   {
   char st[500];
   strcpy(st, "Internal Error - Too many items have been inserted in the \n");
   strcat(st, "component interface vector.  Maximum number of items: ");
   strcat(st, itoa(maxCount).c_str());
   throw std::runtime_error(st);
   }

// ------------------------------------------------------------------
//  Short description:
//     Display a "range error message".

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
void EXPORT rangeError(unsigned int index, unsigned int maxCount)
   {
   char st[500];
   strcpy(st, "Internal Error - Invalid item index passed into \n");
   strcat(st, "component interface vector. \n");
   throw std::runtime_error(st);
   }

static const char* integerArrayType = "<type kind=\"integer4\" array=\"T\"/>";
static const char* realArrayType = "<type kind=\"single\" array=\"T\"/>";
static const char* doubleArrayType = "<type kind=\"double\" array=\"T\"/>";

  std::string EXPORT DDML(const protocol::vector<int>& )
     { return(integerArrayType); }
  std::string EXPORT DDML(const protocol::vector<float>& )
     { return(realArrayType); }
  std::string EXPORT DDML(const protocol::vector<double>& )
     { return(doubleArrayType); }

} // namespace protocol

