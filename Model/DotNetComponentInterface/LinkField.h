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
/// Represents a [Link]. Main responsability is to 
/// resolve the linkages at runtime of a simulation.
/// Links can be to APSIM components (which are by default matched using type only) or
/// they can be links to Instance objects (which are by default matched using type ane name).
/// e.g. 
///    APSIM linkages:
///    [Link] Paddock MyPaddock;                      // links to the current paddock.
///    [Link] Component MyComponent;                  // links to the current component.
///    [Link] SoilWat MySoil;                         // links to the component in scope that has the type 'SoilWat'
///    [Link("wheat2")] Wheat W;                      // links to the sibling component named 'wheat2'
///    [Link(".simulation.paddock1.wheat2")] Wheat W; // links to a specific component with the full path '.simulation.paddock1.wheat2'
/// e.g. 
///    Instance linkages (cannot specify a link path for instance linkages):
///    [Link] Function ThermalTime;        // links to an object with type Function and name ThermalTime
///    [Link] Leaf Leaf;                   // links to an object with type Leaf and name Leaf
/// <summary>
public ref class LinkField
   {
   private:
      FieldInfo^ Field;
      Link^ LinkAttr;
      Instance^ In;
      ModelFramework::ApsimComponent^ Comp;
      static System::Text::StringBuilder^ Data = gcnew System::Text::StringBuilder(10000);
      
      Object^ FindInstanceObject(String^ NameToFind, String^ TypeToFind);

      bool IsAPSIMType(String^ TypeToFind);
      Object^ CreateDotNetProxy(String^ ProxyTypeString, String^ FQN);
      Object^ FindApsimComponent(String^ NameToFind, String^ TypeToFind);
      Object^ GetSpecificApsimComponent(String^ NameToFind);
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