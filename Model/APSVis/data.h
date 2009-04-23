#include <vector>
#include <string>
using std::vector;
using std::string;

#if defined (BUILDING_DATA)
   #define DATA_EXPORT _export
#elif defined (NO_BUILDING_DATA)
   #define DATA_EXPORT
#else
   #define DATA_EXPORT _import
#endif
