#pragma once
using namespace System;
using namespace System::Collections::Generic;

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
      bool Contains(String^ NameToFInd)
         {
         for each (T Obj in this)
            {
            if (Obj->Name->ToLower() == NameToFInd->ToLower())
               return true;
            }
         return false;
         }         
   };


public ref class TypedItem abstract
   {
   public: 
      virtual bool IsOfType(String^ TypeNameToMatch) abstract;
   };
generic <typename T> where T : TypedItem
public ref class TypedList : List<T>
   {
   /// --------------------------------------------------------------------------
   /// This class allows the developer to index into a component collection
   /// using a type.
   /// --------------------------------------------------------------------------
   public:
      property T default[String^]
         {
         T get(String^ TypeName)
            {
            for each (T Obj in this)
               {
               if (Obj->IsOfType(TypeName))
                  return Obj;
               }
            throw gcnew Exception("Cannot find component of type: " + TypeName);
            }
         }
   };
   
generic <typename T> where T : TypedItem
public ref class TypedMultiList : List<T>
   {
   /// --------------------------------------------------------------------------
   /// This class allows the developer to index into a component collection
   /// using a type.
   /// --------------------------------------------------------------------------
   public:
      property List<T>^ default[String^]
         {
         List<T>^ get(String^ TypeName)
            {
            List<T>^ ReturnList = gcnew List<T>();
            for each (T Obj in this)
               {
               if (Obj->IsOfType(TypeName))
                  ReturnList->Add(Obj);
               }
            return ReturnList;
            }
         }
   };    
