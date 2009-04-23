#ifndef EnvironmentH
#define EnvironmentH
#include <vector>

#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/Component.h>
#include <ComponentInterface/Type.h>

#include "Utility/PlantUtility.h"
#include "PlantLibrary.h"

class Environment : public plantThing
   {
   public:
      Environment(ScienceAPI& scienceAPI, const std::string& name);

      int dayOfYear() const {return day_of_year;}
      float radn()    const {return NewMet.radn;}
      float maxt()    const {return NewMet.maxt;}
      float mint()    const {return NewMet.mint;}
      float meant()   const {return (NewMet.maxt + NewMet.mint) / 2;}
      float co2()     const;

      float vpdEstimate() const;
      float dayLength(float sun_angle) const;
      float dayLength(int dyoyr, float sun_angle) const;
      float Latitude() const {return latitude;}

   private:
      protocol::NewMetType NewMet;
      float latitude;                                   // latitude (degrees, negative for southern hemisphere)
      int day_of_year;                                  // day of year
      int year;                                         // year
      float svp_fract;                                  // fraction of distance between svp at
                                                        // min temp and svp at max temp where
                                                        // average svp during transpiration
                                                        // lies. (0-1)
      float _co2;
      float co2_default;

      float svp(float temp) const;
      float vpd(float svp_fract, float maxt, float mint) const;
      void OnNewMet(protocol::NewMetType &newmet) ;
      void OnTick(protocol::TimeType &Tick);
   };

#endif
