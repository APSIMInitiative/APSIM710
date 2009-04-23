#ifndef PartsH
#define PartsH

#include "CompositePart.h"

class Parts : public CompositePart
   {
   public:
      Parts(XMLNode parameters,
            ScienceAPI& scienceAPI,
            plantInterface *p,
            const string &name);
     
   private:
      void findParts(XMLNode parameters);
      virtual void onInit1(protocol::Component *) {};
      virtual void onPlantEvent(const string &){};
   };

#endif
