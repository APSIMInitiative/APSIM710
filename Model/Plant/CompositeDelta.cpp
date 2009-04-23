#include "StdPlant.h"

#include "CompositeDelta.h"
using namespace std;

CompositeDelta::CompositeDelta(ScienceAPI& scienceAPI, const std::string& Name,
                             const std::string& PartName)
   : Delta(scienceAPI, Name, PartName, false)
   {
   }

void CompositeDelta::Clear()
   {
   for (unsigned i = 0; i != Deltas.size(); i++)
      Deltas[i]->Clear();
   }
void CompositeDelta::ClearDeltas()
   {
   Deltas.erase(Deltas.begin(), Deltas.end());
   }
void CompositeDelta::Add(Delta& delta)
   {
   if (Deltas.size() == 0)
      DoRegistrations();
   Deltas.push_back(&delta);
   }

float CompositeDelta::DM() const
   {
   float wt = 0.0;
   for (unsigned i = 0; i != Deltas.size(); i++)
      wt += Deltas[i]->DM();
   return wt;
   }
float CompositeDelta::StructuralDM() const
   {
   float wt = 0.0;
   for (unsigned i = 0; i != Deltas.size(); i++)
      wt += Deltas[i]->StructuralDM();
   return wt;
   }
float CompositeDelta::NonStructuralDM() const
   {
   float wt = 0.0;
   for (unsigned i = 0; i != Deltas.size(); i++)
      wt += Deltas[i]->NonStructuralDM();
   return wt;
   }

float CompositeDelta::N()  const
   {
   float n = 0.0;
   for (unsigned i = 0; i != Deltas.size(); i++)
      n += Deltas[i]->N();
   return n;
   }

float CompositeDelta::P()  const
   {
   float p = 0.0;
   for (unsigned i = 0; i != Deltas.size(); i++)
      p += Deltas[i]->P();
   return p;
   }

Biomass& CompositeDelta::operator = (const Biomass&)
   {
   throw runtime_error("Cannot set biomass for a composite Delta"
                       ". Part name = " + PartName +
                       ". Name = " + Name);
   }

