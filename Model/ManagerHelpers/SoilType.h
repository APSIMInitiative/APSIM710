#include "stdafx.h"
using namespace System;
using namespace System::Runtime::InteropServices;

//------ SoilType ------
public ref class SoilType
   {
   private:
      ComponentType^ Comp;
   public:
      SoilType(ComponentType^ c) : Comp(c) { }
   
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
      property String^ name
         {
         String^ get()
            {
            return Comp->Variable( "name" )->ToString();
            }       
         } 
      property Single runoff
         {
         Single get()
            {
            return Comp->Variable( "runoff" )->ToSingle();
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
   };
