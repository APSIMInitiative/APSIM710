#ifndef PoolInterfaceH
#define PoolInterfaceH
#include <string>
class Delta;
class PoolInterface : public Biomass
   {
   public:
      virtual void Clear (void) = 0;
      virtual float DM() = 0;
      virtual float N() = 0;
      virtual float P() = 0;
      interpolationFunction DigestibilityMax;
      interpolationFunction DigestibilityAvg;
      interpolationFunction DigestibilityMin;
      float Pconc() {return divide(P,DM,0.0);};
      float Nconc() {return divide(N,DM,0.0);};
      float NconcPercent() {return divide(N,DM,0.0)*fract2pcnt;};
      float PconcPercent() {return divide(P,DM,0.0)*fract2pcnt;};

      PoolInterface operator + (const PoolInterface& PoolInterface2);
      PoolInterface operator + (const Delta& Dlt);
      PoolInterface operator * (float Fraction);
      PoolInterface operator = (const PoolInterface& PoolInterface2);
      PoolInterface operator - (const Delta& Dlt);

   private:
      std::string PartName;
      std::string Name;
      ScienceAPI* scienceAPI;

   };

#endif