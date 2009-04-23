#ifndef OUTPUTVARIABLE_H
#define OUTPUTVARIABLE_H

#include <string>
#include "Output.h"

class OutputVariable : public Output
//=======================================================================================
   {
   protected:

   public:
      void* Variable;
      OutputVariable(const std::string &Name, const std::string &Units, const std::string &Description, float &Variable);
   };

#endif

