#pragma once
#include "ComponentType.h"
using namespace System;
using namespace System::Collections::Generic;
using namespace System::Text;



   
generic <typename T>
public ref class LayeredList : List<T> 
   {
   /// --------------------------------------------------------------------------
   /// This class allows the developer to index into the phase collection
   /// using a string key.
   /// --------------------------------------------------------------------------
   public:
   property T default[int]
      {
      virtual T get(int Index) new
         {
         return this[Index-1];
         }
      }
   };   

public ref class Instance : NamedItem
   {
   private:
      static ComponentType^ _Paddock;
      String^ MyFQN()
         {
         String^ FQN = "";
         if (Parent != nullptr)
            FQN = Parent->MyFQN();
         FQN += Name;
         return FQN;
         }
      String^ ComponentName;
      ModelFramework::ApsimComponent^ Component;
   
   protected:

         
   public:
      NamedList<NamedItem^>^ Children;
      Instance^ Parent;
      Instance();

      virtual void Add(Instance^ Child)
         {
         Children->Add(Child);
         }
      int IndexOf(String^ Name)
         {
         int PosPeriod = Name->IndexOf('.');
         if (PosPeriod == -1)
            {
            for (int i = 0; i != Children->Count; i++)
               {
               if (Children[i]->Name->ToLower() == Name->ToLower())
                  return i;
               }
            return -1;
            }
         else
            {
            String^ ChildName = Name->Substring(0, PosPeriod);
            String^ Remainder = Name->Substring(PosPeriod+1);
            return ((Instance^) Children[ChildName])->IndexOf(Remainder);
            }
         }      
      Instance^ Find(String^ Name)
         {
         int PosPeriod = Name->IndexOf('.');
         if (PosPeriod == -1)
            return (Instance^) Children[Name];
         else
            {
            String^ ChildName = Name->Substring(0, PosPeriod);
            String^ Remainder = Name->Substring(PosPeriod+1);
            return ((Instance^) Children[ChildName])->Find(Remainder);
            }
         }         
      void Setup(String^ ComponentNam, ModelFramework::ApsimComponent^ component) 
         {
         ComponentName = ComponentNam;
         Component = component;
         }
      property Instance^ Root 
         { 
         virtual Instance^ get() 
            { 
            if (Parent == nullptr)
               return this;
            else
               return Parent->Root;
            }
         }
         
      property ComponentType^ Paddock 
         { 
         // --------------------------------------------------------------------
         // Returns the singleton instance of a reflection class that is
         // capable of returning metadata about the structure of the current
         // paddock that this model is running in.
         // --------------------------------------------------------------------

         ComponentType^ get()
            { 
            if (_Paddock == nullptr)
               {
               String^ ParentName = Root->ComponentName->Substring(0, Root->ComponentName->LastIndexOf('.'));
               _Paddock = gcnew ComponentType(ParentName, Component);
               }
            return _Paddock;
            } 
         }  
      };

