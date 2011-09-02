#pragma once
#include "NamedItem.h"
#include "Variable.h"
#include "RuntimeEventHandler.h"
using namespace System;
using namespace System::Collections::Generic;
using namespace System::Collections::Specialized;
using namespace System::Runtime::InteropServices;
using namespace CSGeneral;

[DllImport("ApsimShared.dll", EntryPoint = "getComponentType", CharSet=CharSet::Ansi, CallingConvention=CallingConvention::StdCall)]
void getComponentType(String^ ComponentName, System::Text::StringBuilder^ ComponentType); 

namespace ModelFramework {
   ref class ApsimComponent;  // forward
   
public ref class Component
   {
   protected:
      static System::Text::StringBuilder^ Data = gcnew System::Text::StringBuilder(10000);
      String^ FQN;
      ModelFramework::ApsimComponent^ Comp;

        
      /// <summary>
      /// Static helper function for getting a component type (DLL name) from a fully
      /// qualified component name (eg. .MasterPM.paddock1.wheat)
      /// </summary>
      static String^ ComponentType(String^ fqn)
         {
         getComponentType(fqn, Data);
         return Data->ToString();
         }

      /// <summary>
      /// Static helper function for getting a component name (eg. wheat) from a fully
      /// qualified component name (eg. .MasterPM.paddock1.wheat)
      /// </summary>
      static String^ ComponentName(String^ fqn)
         {
         return fqn->Substring(fqn->LastIndexOf('.') + 1);
         }

   public:
 
   
      /// <summary>
      /// Constructors
      /// </summary>
      Component(String^ _FullName, ModelFramework::ApsimComponent^ _Comp) { FQN = _FullName; Comp = _Comp; }
      Component() { }
         
      /// <summary>
      /// Name of component eg. wheat
      /// </summary>
      property String^ Name { String^ get() { return ComponentName(FQN); } }

      /// <summary>
      /// Fully qualified name of component eg. .MasterPM.paddock1.wheat
      /// </summary>
      property String^ FullName { String^ get() { return FQN; } }

      /// <summary>
      /// Fully qualified name of component eg. .MasterPM.paddock1.wheat
      /// </summary>
      property String^ TypeName { String^ get() { return ComponentType(FQN); } }

      /// <summary>
      /// Returns a reference to a variable.
      /// </summary>
      virtual Variable^ Variable(String^ VariableName);
         
      /// <summary>
      /// Publish a notification event (i.e. one that doesn't have any data 
      /// associated with it) to this component only.
      /// </summary>
      virtual void Publish(String^ EventName);

      /// <summary>
      /// Publish an event that has associated data to this component only.
      /// </summary>
      virtual void Publish(String^ EventName, ApsimType^ Data);

      /// <summary>
      /// Subscribe to a notification event ie. one without any data associated with it.
      /// </summary>
      void Subscribe(String^ EventName, RuntimeEventHandler::NullFunction^ F) ;
         

      
   };

   }