#pragma once
#using <CSGeneral.dll>
using namespace System;
using namespace System::Collections::Generic;
using namespace System::Collections::Specialized;
using namespace System::Text;
using namespace System::Xml;
using namespace System::Reflection;
using namespace CSGeneral;
using namespace System::Runtime::InteropServices;

#include "Instance.h"
#include "FactoryProperty.h"
#include "FactoryEventHandler.h"
#include "FactoryEvent.h"

using namespace ModelFramework;

/// <summary>
/// Represents a [Link] attribute. Main responsability is to 
/// resolve the linkages at runtime of a simulation.
/// e.g. references
///    [Link()] Paddock MyPaddock;                      // refers to the current paddock.
///    [Link()] Component MyComponent;                  // refers to the current component.
///    [Link()] SoilWat MySoil;                         // refers to the component in scope that has the type 'SoilWat'
///    [Link("wheat2")] Wheat W;                        // refers to the sibling component named 'wheat2'
///    [Link(".simulation.paddock1.wheat2")] Wheat W;   // refers to a specific component with the full path '.simulation.paddock1.wheat2'
/// <summary>
public ref class LinkField
   {
   private:
      FieldInfo^ Field;
      Link^ LinkAttr;
      Instance^ In;
      ModelFramework::ApsimComponent^ Comp;
      static System::Text::StringBuilder^ Data = gcnew System::Text::StringBuilder(10000);
      
      String^ FindComponentByType(String^ TypeToFind, String^ OurName);
      String^ FindComponentByName(String^ NameToFind, String^ OurName);
   public:

      // Constructor
      LinkField(Instance^ _In, FieldInfo^ _Field, Link^ _LinkAttr)
         {
         In = _In;
         Field = _Field;
         LinkAttr = _LinkAttr;
         Comp = In->ParentComponent();
         }
      
      void Resolve();

   };