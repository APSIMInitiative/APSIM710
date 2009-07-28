#include "stdafx.h"
using namespace System;
using namespace System::Runtime::InteropServices;

//------ Wheat ------
public ref class CropType
   {
   private:
      ComponentType^ Comp;
   public:
      CropType(ComponentType^ c) : Comp(c) { }
   
      property Single biomass
         {
         Single get()
            {
            return Comp->Variable[ "biomass" ]->ToSingle();
            }       
         } 
      property Single cover_green
         {
         Single get()
            {
            return Comp->Variable[ "cover_green" ]->ToSingle();
            }       
         } 
      property Single cover_tot
         {
         Single get()
            {
            return Comp->Variable[ "cover_tot" ]->ToSingle();
            }       
         } 
      property Int32 daysaftersowing
         {
         Int32 get()
            {
            return Comp->Variable[ "daysaftersowing" ]->ToInt32();
            }       
         } 
      property array<Single>^ esw_layr
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "esw_layr" ]->ToSingleArray();
            }       
         } 

      property Single grain_no
         {
         Single get()
            {
            return Comp->Variable[ "grain_no" ]->ToSingle();
            }       
         } 
      property Single grain_protein
         {
         Single get()
            {
            return Comp->Variable[ "grain_protein" ]->ToSingle();
            }       
         } 
      property Single grain_size
         {
         Single get()
            {
            return Comp->Variable[ "grain_size" ]->ToSingle();
            }       
         } 
      property Single grain_wt
         {
         Single get()
            {
            return Comp->Variable[ "grain_wt" ]->ToSingle();
            }       
         } 
      property Single grainn
         {
         Single get()
            {
            return Comp->Variable[ "grainn" ]->ToSingle();
            }       
         } 
      property Single grainwt
         {
         Single get()
            {
            return Comp->Variable[ "grainwt" ]->ToSingle();
            }       
         } 
      property Single height
         {
         Single get()
            {
            return Comp->Variable[ "height" ]->ToSingle();
            }       
         } 
      property array<Single>^ kl
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "kl" ]->ToSingleArray();
            }       
         } 
      property Single lai
         {
         Single get()
            {
            return Comp->Variable[ "lai" ]->ToSingle();
            }       
         } 
      property array<Single>^ leaf_area
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "leaf_area" ]->ToSingleArray();
            }       
         } 
      property Single leaf_no
         {
         Single get()
            {
            return Comp->Variable[ "leaf_no" ]->ToSingle();
            }       
         } 
      property array<Single>^ ll
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "ll" ]->ToSingleArray();
            }       
         } 
      property array<Single>^ ll_dep
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "ll_dep" ]->ToSingleArray();
            }       
         } 
      property String^ name
         {
         String^ get()
            {
            return Comp->Variable[ "name" ]->ToString();
            }       
         } 
      property Single plants
         {
         Single get()
            {
            return Comp->Variable[ "plants" ]->ToSingle();
            }       
         } 
      property Single root_depth
         {
         Single get()
            {
            return Comp->Variable[ "root_depth" ]->ToSingle();
            }       
         } 
      property Single stage
         {
         Single get()
            {
            return Comp->Variable[ "stage" ]->ToSingle();
            }       
         } 
      property String^ stagename
         {
         String^ get()
            {
            return Comp->Variable[ "stagename" ]->ToString();
            }       
         } 
      property Single sw_demand
         {
         Single get()
            {
            return Comp->Variable[ "sw_demand" ]->ToSingle();
            }       
         } 
      property array<Single>^ xf
         {
         array<Single>^ get()
            {
            return Comp->Variable[ "xf" ]->ToSingleArray();
            }       
         } 
      property Single yield
         {
         Single get()
            {
            return Comp->Variable[ "yield" ]->ToSingle();
            }       
         } 
      property Single zadok_stage
         {
         Single get()
            {
            return Comp->Variable[ "zadok_stage" ]->ToSingle();
            }       
         } 
   };
