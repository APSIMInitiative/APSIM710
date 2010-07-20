#include "stdafx.h"
using namespace System;
using namespace System::Runtime::InteropServices;

//------ Fertiliser ------
public ref class FertiliserType
   {
   private:
      ComponentType^ Comp;
   public:
      FertiliserType(ComponentType^ c) : Comp(c) { }
   
      VariableType^ Variable(String^ VariableName)
         {
         return Comp->Variable(VariableName);
         }
         
      void Publish(String^ EventName, ApsimType^ Data)
         {
         Comp->Publish(Comp->Name + "." + EventName, Data);
         }   
   
      property Single fertiliser
         {
         Single get()
            {
            return Comp->Variable( "fertiliser" )->ToSingle();
            }       
         } 
      property String^ name
         {
         String^ get()
            {
            return Comp->Variable( "name" )->ToString();
            }       
         }
      void Apply(double Amount, double Depth, String^ Type) 
         {
         FertiliserApplicationType^ Fertiliser = gcnew FertiliserApplicationType();
         Fertiliser->Amount = (float) Amount;
         Fertiliser->Depth = (float) Depth;
         Fertiliser->Type = Type;
         Comp->Publish(Comp->Name + ".Apply", Fertiliser);
         }
   };
