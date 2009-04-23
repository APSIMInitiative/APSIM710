#ifndef CompositeDeltaH
#define CompositeDeltaH
#include <string>
class ScienceAPI;

#include "Pool.h"
class CompositeDelta : public Delta
   {
   public:
      CompositeDelta(ScienceAPI& scienceAPI, const std::string& Name, const std::string& PartName);

      virtual void Clear();

      void Add(Delta& Delta);
      void ClearDeltas();
      virtual float DM() const;
      virtual float N()  const;
      virtual float P()  const;
      virtual float StructuralDM() const;
      virtual float NonStructuralDM() const;

      virtual void AddStructuralDM(float amount) {throw runtime_error("cannot add structural DM to composite delta");} 
      virtual void AddN(float amount) {throw runtime_error("cannot add N to composite delta");} 
      virtual void AddP(float amount) {throw runtime_error("cannot add P to composite delta");} 
      virtual void AddNonStructuralDM(float amount)  {throw runtime_error("cannot add Nonstructural DM to composite delta");} 

      virtual void SetStructuralDM(float amount)  {throw runtime_error("cannot set structural DM to composite delta");} 
      virtual void SetN(float amount)   {throw runtime_error("cannot set N to composite delta");} 
      virtual void SetP(float amount)   {throw runtime_error("cannot set P to composite delta");} 


      virtual Biomass& operator = (const Biomass& rhs);

   private:
      std::vector<Delta*> Deltas;

   };

#endif
