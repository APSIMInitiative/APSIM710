#ifndef CompositePoolH
#define CompositePoolH
#include <string>
class ScienceAPI;

#include "Pool.h"
class CompositePool : public Pool
   {
   public:
      CompositePool(plantInterface& plant,ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName);

      virtual void Clear();

      void AddPool(Biomass& Pool);
      void ClearPools();
      virtual float DM() const;
      virtual float N()  const;
      virtual float P()  const;

      virtual float StructuralDM() const;

      virtual float NonStructuralDM() const;

      virtual Biomass& operator = (const Biomass& rhs);

   private:
      std::vector<Biomass*> Pools;

   };

#endif
