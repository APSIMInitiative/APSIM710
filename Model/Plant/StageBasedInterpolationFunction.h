#ifndef StageBasedInterpolationFunctionH
#define StageBasedInterpolationFunctionH
#include <string>
class ScienceAPI;
class StageBasedInterpolationFunction
   {
   public:
      StageBasedInterpolationFunction(plantInterface& plant, ScienceAPI& scienceAPI, const std::string& Name, const std::string& Units, const std::string& Description);
      float value(void);

   protected:

      interpolationFunction f;
      std::string Name;
      ScienceAPI& scienceAPI;
      plantInterface& plant;                 // The plant we are attached to

   };

#endif
