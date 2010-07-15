#include "Bounded.h"
#include <General/platform.h>
#include <stdio.h>

using namespace std;

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
