#include "stdafx.h"
using namespace System;
using namespace System::Runtime::InteropServices;

//------ SoilWaterType ------
public ref class SoilWaterType
   {
   private:
      ComponentType^ Comp;
   public:
      SoilWaterType(ComponentType^ c) : Comp(c) { }

      VariableType^ Variable(String^ VariableName)
         {
         return Comp->Variable(VariableName);
         }
         
      void Publish(String^ EventName, ApsimType^ Data)
         {
         Comp->Publish(Comp->Name + "." + EventName, Data);
         }
            
      property array<Single>^ air_dry
         {
         array<Single>^ get()
            {
            return Comp->Variable( "air_dry" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "air_dry" )->Set(value);
            }
         }
      property array<Single>^ air_dry_dep
         {
         array<Single>^ get()
            {
            return Comp->Variable( "air_dry_dep" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "air_dry_dep" )->Set(value);
            }
         }
      property array<Single>^ bd
         {
         array<Single>^ get()
            {
            return Comp->Variable( "bd" )->ToSingleArray();
            }
         }
      property Single cn2_bare
         {
         Single get()
            {
            return Comp->Variable( "cn2_bare" )->ToSingle();
            }
         void set(Single value)
            {
            Comp->Variable( "cn2_bare" )->Set(value);
            }
         }
      property Single cn2_new
         {
         Single get()
            {
            return Comp->Variable( "cn2_new" )->ToSingle();
            }
         }
      property Single cn_cov
         {
         Single get()
            {
            return Comp->Variable( "cn_cov" )->ToSingle();
            }
         void set(Single value)
            {
            Comp->Variable( "cn_cov" )->Set(value);
            }
         }
      property Single cn_red
         {
         Single get()
            {
            return Comp->Variable( "cn_red" )->ToSingle();
            }
         void set(Single value)
            {
            Comp->Variable( "cn_red" )->Set(value);
            }
         }
      property Single cona
         {
         Single get()
            {
            return Comp->Variable( "cona" )->ToSingle();
            }
         void set(Single value)
            {
            Comp->Variable( "cona" )->Set(value);
            }
         }
      property Single cover_surface_runoff
         {
         Single get()
            {
            return Comp->Variable( "cover_surface_runoff" )->ToSingle();
            }
         }
      property array<Single>^ dlayer
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlayer" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "dlayer" )->Set(value);
            }
         }
      property array<Single>^ dlt_dlayer
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_dlayer" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "dlt_dlayer" )->Set(value);
            }
         }
      property array<Single>^ dlt_sw
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_sw" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "dlt_sw" )->Set(value);
            }
         }
      property array<Single>^ dlt_sw_dep
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dlt_sw_dep" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "dlt_sw_dep" )->Set(value);
            }
         }
      property Single drain
         {
         Single get()
            {
            return Comp->Variable( "drain" )->ToSingle();
            }
         }
      property array<Single>^ dul
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dul" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "dul" )->Set(value);
            }
         }
      property array<Single>^ dul_dep
         {
         array<Single>^ get()
            {
            return Comp->Variable( "dul_dep" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "dul_dep" )->Set(value);
            }
         }
      property Single eff_rain
         {
         Single get()
            {
            return Comp->Variable( "eff_rain" )->ToSingle();
            }
         }
      property Single eo
         {
         Single get()
            {
            return Comp->Variable( "eo" )->ToSingle();
            }
         }
      property Single eos
         {
         Single get()
            {
            return Comp->Variable( "eos" )->ToSingle();
            }
         }
      property Single es
         {
         Single get()
            {
            return Comp->Variable( "es" )->ToSingle();
            }
         }
      property Single esw
         {
         Single get()
            {
            return Comp->Variable( "esw" )->ToSingle();
            }
         }
      property array<Single>^ flow
         {
         array<Single>^ get()
            {
            return Comp->Variable( "flow" )->ToSingleArray();
            }
         }
      property array<Single>^ flow_water
         {
         array<Single>^ get()
            {
            return Comp->Variable( "flow_water" )->ToSingleArray();
            }
         }
      property array<Single>^ flux
         {
         array<Single>^ get()
            {
            return Comp->Variable( "flux" )->ToSingleArray();
            }
         }
      property Single infiltration
         {
         Single get()
            {
            return Comp->Variable( "infiltration" )->ToSingle();
            }
         }
      property array<Single>^ ll15
         {
         array<Single>^ get()
            {
            return Comp->Variable( "ll15" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "ll15" )->Set(value);
            }
         }
      property array<Single>^ ll15_dep
         {
         array<Single>^ get()
            {
            return Comp->Variable( "ll15_dep" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "ll15_dep" )->Set(value);
            }
         }
      property Single max_pond
         {
         Single get()
            {
            return Comp->Variable( "max_pond" )->ToSingle();
            }
         void set(Single value)
            {
            Comp->Variable( "max_pond" )->Set(value);
            }
         }
      property Single pond
         {
         Single get()
            {
            return Comp->Variable( "pond" )->ToSingle();
            }
         }
      property Single pond_evap
         {
         Single get()
            {
            return Comp->Variable( "pond_evap" )->ToSingle();
            }
         }
      property Single profile_esw_depth
         {
         Single get()
            {
            return Comp->Variable( "profile_esw_depth" )->ToSingle();
            }
         void set(Single value)
            {
            Comp->Variable( "profile_esw_depth" )->Set(value);
            }
         }
      property Single profile_fesw
         {
         Single get()
            {
            return Comp->Variable( "profile_fesw" )->ToSingle();
            }
         void set(Single value)
            {
            Comp->Variable( "profile_fesw" )->Set(value);
            }
         }
      property Single runoff
         {
         Single get()
            {
            return Comp->Variable( "runoff" )->ToSingle();
            }
         }
      property Single salb
         {
         Single get()
            {
            return Comp->Variable( "salb" )->ToSingle();
            }
         }
      property array<Single>^ sat
         {
         array<Single>^ get()
            {
            return Comp->Variable( "sat" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "sat" )->Set(value);
            }
         }
      property array<Single>^ sat_dep
         {
         array<Single>^ get()
            {
            return Comp->Variable( "sat_dep" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "sat_dep" )->Set(value);
            }
         }

      property array<Single>^ sw
         {
         array<Single>^ get()
            {
            return Comp->Variable( "sw" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "sw" )->Set(value);
            }
         }
      property array<Single>^ sw_dep
         {
         array<Single>^ get()
            {
            return Comp->Variable( "sw_dep" )->ToSingleArray();
            }
         void set(array<Single>^ value)
            {
            Comp->Variable( "sw_dep" )->Set(value);
            }
         }
      property array<Single>^ sws
         {
         array<Single>^ get()
            {
            return Comp->Variable( "sws" )->ToSingleArray();
            }
         }
      property Single t
         {
         Single get()
            {
            return Comp->Variable( "t" )->ToSingle();
            }
         }
      property Single u
         {
         Single get()
            {
            return Comp->Variable( "u" )->ToSingle();
            }
         void set(Single value)
            {
            Comp->Variable( "u" )->Set(value);
            }
         }
      property Single water_table
         {
         Single get()
            {
            return Comp->Variable( "water_table" )->ToSingle();
            }
         }
      property Single wet_soil_depth
         {
         Single get()
            {
            return Comp->Variable( "wet_soil_depth" )->ToSingle();
            }
         void set(Single value)
            {
            Comp->Variable( "wet_soil_depth" )->Set(value);
            }
         }
   };
