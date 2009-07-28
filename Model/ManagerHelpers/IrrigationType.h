#include "stdafx.h"
using namespace System;
using namespace System::Runtime::InteropServices;

//------ Fertiliser ------
public ref class IrrigationType
   {
   private:
      ComponentType^ Comp;
   public:
      IrrigationType(ComponentType^ c) : Comp(c) { }
   
      property Single irrigation
         {
         Single get()
            {
            return Comp->Variable[ "irrigation" ]->ToSingle();
            }       
         } 
         
      property Single irrig_tot
         {
         Single get()
            {
            return Comp->Variable[ "irrig_tot" ]->ToSingle();
            }       
         }    
         
      property Single allocation
         {
         Single get()
            {
            return Comp->Variable[ "allocation" ]->ToSingle();
            }       
         }                
         
      property Single irrigation_efficiency
         {
         Single get()
            {
            return Comp->Variable[ "irrigation_efficiency" ]->ToSingle();
            }       
         void set(Single value)
            {
            Comp->Variable[ "irrigation_efficiency" ]->Set(value);
            }       
         }
                  
      property String^ name
         {
         String^ get()
            {
            return Comp->Variable[ "name" ]->ToString();
            }       
         }
      void Apply(double Amount) 
         {
         IrrigationApplicationType^ Irrigation = gcnew IrrigationApplicationType();
         Irrigation->Amount = (float) Amount;
         Comp->Publish("Apply2", Irrigation);
         }
   };
