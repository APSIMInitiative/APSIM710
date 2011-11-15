#include "StdPlant.h"

#include "Biomass.h"
#include "Delta.h"
using namespace std;

Biomass::Biomass()
   {
   Clear();
   }

Biomass::Biomass(float sDM, float sN, float sP, float nsDM)
   {
   privateStructuralDM = sDM;
   privateN = sN;
   privateP = sP;

   privateNonStructuralDM = nsDM;

      CheckBounds();

   }

Biomass::Biomass(const Biomass& Biomass2)
   {
   privateNonStructuralDM = Biomass2.NonStructuralDM();
   privateN = Biomass2.N();
   privateP = Biomass2.P();

   privateStructuralDM = Biomass2.StructuralDM();

   CheckBounds();
   }


void Biomass::Clear (void)
   {
   privateNonStructuralDM = 0.0;
   privateN = 0.0;
   privateP = 0.0;

   privateStructuralDM = 0.0;

   }

void Biomass::AddStructuralDM(float amount)
   {
   privateStructuralDM=privateStructuralDM+amount;
      CheckBounds();
   }
void Biomass::AddN(float amount)
   {
   privateN=privateN+amount;
      CheckBounds();
   }
void Biomass::AddP(float amount)
   {
   privateP=privateP+amount;
      CheckBounds();
   }
void Biomass::AddNonStructuralDM(float amount)
   {
   privateNonStructuralDM=privateNonStructuralDM+amount;
      CheckBounds();
   }


Biomass Biomass::operator + (const Biomass& Biomass2)
   {
   Biomass Temp;
   Temp.privateNonStructuralDM = NonStructuralDM() + Biomass2.NonStructuralDM();
   Temp.privateN  = N()  + Biomass2.N();
   Temp.privateP  = P()  + Biomass2.P();

   Temp.privateStructuralDM = StructuralDM() + Biomass2.StructuralDM();

   return Temp;
   }

Biomass Biomass::operator - (const Biomass& Biomass2)
   {
   Biomass Temp;
   Temp.privateNonStructuralDM = NonStructuralDM() - Biomass2.NonStructuralDM();
   Temp.privateN  = N()  - Biomass2.N();
   Temp.privateP  = P()  - Biomass2.P();

   Temp.privateStructuralDM = StructuralDM() - Biomass2.StructuralDM();

   return Temp;
   }

Biomass Biomass::operator * (float Fraction)
   {
   Biomass Temp;
   Temp.privateNonStructuralDM = NonStructuralDM() * Fraction;
   Temp.privateN = N() * Fraction;
   Temp.privateP = P() * Fraction;

   Temp.privateStructuralDM = StructuralDM() * Fraction;

   return Temp;
   }

Biomass& Biomass::operator = (const Biomass& Biomass2)
   {
   privateNonStructuralDM = l_bound(Biomass2.NonStructuralDM(), 0.0);
   privateN = l_bound(Biomass2.N(), 0.0);
   privateP = l_bound(Biomass2.P(), 0.0);

   privateStructuralDM = l_bound(Biomass2.StructuralDM(), 0.0);

   CheckBounds();
   return *this;
   }


