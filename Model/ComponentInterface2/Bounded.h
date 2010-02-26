#ifndef BoundedH
#define BoundedH

#include <string>
#include <vector>
#include <General/platform.h>

// -------------------------------------------------------------------
// bounding function templates + a bounded version of CMPBuiltIn
// -------------------------------------------------------------------
void EXPORT performBoundCheck(const std::string& name, int value, int lower, int upper);
void EXPORT performBoundCheck(const std::string& name, float value, float lower, float upper);
void EXPORT performBoundCheck(const std::string& name, double value, double lower, double upper);
template <class T>
void performBoundCheck(const std::string& name, const std::vector<T> &values, T lower, T upper)
   {
   for (unsigned i = 0; i != values.size(); i++)
      performBoundCheck(name, values[i], lower, upper);
   }



#endif
