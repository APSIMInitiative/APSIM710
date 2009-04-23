#include "StdPlant.h"

#include "Delta.h"
#include "Pool.h"

using namespace std;

Delta::Delta(ScienceAPI& API, const std::string& Name, const std::string& PartName,bool DoRegist)
   : scienceAPI(API)
   {
   this->Name = Name;
   this->PartName = PartName;
   Clear();
   if (DoRegist)
      DoRegistrations();
         	
   }

void Delta::DoRegistrations()
   {
   scienceAPI.exposeFunction(PartName+Name+"Wt", "g/m^2", "Change in " + Name + " " + PartName + " dry matter", FloatGetter(&Biomass::DM));
   scienceAPI.exposeFunction(PartName+Name+"N",  "g/m^2", "Change in " + Name + " " + PartName + " nitrogen", FloatGetter(&Biomass::N));
   scienceAPI.exposeFunction(PartName+Name+"P",  "g/m^2", "Change in " + Name + " " + PartName + " phosphorus", FloatGetter(&Biomass::P));

   }
void Delta::Move (Pool& From, Pool& To)
   {
   From = From - *this;
   To = To + *this;
   }

Biomass& Delta::operator = (const Biomass& Biomass2)
   {
   return Biomass::operator=(Biomass2);
   }

