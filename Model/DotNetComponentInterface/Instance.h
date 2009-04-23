#pragma once

using namespace System;
using namespace System::Collections::Generic;
using namespace System::Text;

public ref class NamedItem
   {
   private:
      String^ _Name;
   public: 
      property String^ Name 
         { 
         virtual String^ get()
            { 
            return _Name; 
            } 
         void set (String^ value)
            { 
            _Name = value; 
            } 
         }
   };

generic <typename T> where T : NamedItem
public ref class NamedList : List<T>
   {
   /// --------------------------------------------------------------------------
   /// This class allows the developer to index into the phase collection
   /// using a string key.
   /// --------------------------------------------------------------------------
   public:
      String^ ParentName;
      property T default[String^]
         {
         T get(String^ Name)
            {
            for each (T Obj in this)
               {
               if (Obj->Name->ToLower() == Name->ToLower())
                  return Obj;
               }
            throw gcnew Exception("Cannot find object: " + ParentName + Name);
            }
         }
   };
   
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
      String^ MyFQN()
         {
         String^ FQN = "";
         if (Parent != nullptr)
            FQN = Parent->MyFQN();
         FQN += Name;
         return FQN;
         }
   public:
      NamedList<NamedItem^>^ Children;
      Instance^ Parent;
      Instance();
      virtual void Add(Instance^ Child)
         {
         Children->Add(Child);
         }
      virtual void Setup() { }
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
      };

