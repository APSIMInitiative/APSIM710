#ifndef OUTPUT_H
#define OUTPUT_H

#include <string>

class Output
//=======================================================================================
   {
   protected:

   public:
      std::string Name;
      std::string Units;
      std::string Description;

      Output(const std::string &Name, const std::string &Units, const std::string &Description);
      virtual ~Output(){};
   };

#endif

