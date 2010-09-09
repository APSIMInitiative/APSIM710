#pragma once
#using <CSGeneral.dll>
using namespace System;
using namespace System::Collections::Generic;
using namespace System::Collections::Specialized;
using namespace System::Text;
using namespace System::Xml;
using namespace System::Reflection;
using namespace CSGeneral;

#include "Instance.h"
#include "FactoryProperty.h"
#include "FactoryEventHandler.h"
#include "FactoryEvent.h"

public ref class Factory
   {
   public:
      Factory();
      property Instance^ Root { Instance^get() { return _Root; } }
      property List<FactoryProperty^>^ Properties 
         {
         List<FactoryProperty^>^ get() { return RegisteredProperties; }
         }
      property List<EvntHandler^>^ EventHandlers 
         {
         List<EvntHandler^>^ get() { return RegisteredEventHandlers; }
         }
      property List<FactoryEvent^>^ Events 
         {
         List<FactoryEvent^>^ get() { return RegisteredEvents; }
         }
      void Create(String^ Xml, Assembly^ Assembly, ModelFramework::ApsimComponent^ ParentComponent);
      //void Create(XmlNode^ Node, Assembly^ Assembly);
      void PopulateParams(Instance^ Obj, XmlNode^ Node, ModelFramework::ApsimComponent^ ParentComponent);
      void CheckParameters();
   private:
      Instance^ _Root;
      List<FactoryProperty^>^ RegisteredProperties;
      List<EvntHandler^>^ RegisteredEventHandlers;
      List<FactoryEvent^>^ RegisteredEvents;
      Assembly^ CallingAssembly;
      
      Instance^ CreateInstance(XmlNode^ Node, XmlNode^ Parent, Instance^ ParentInstance,
                               ModelFramework::ApsimComponent^ ParentComponent);
      void GetAllProperties(Instance^ Obj, XmlNode^ Parent);
      void GetAllEventHandlers(Instance^ Obj);
      void GetAllEvents(Instance^ Obj);
      void RemoveRegisteredOutput(String^ OuptutName);
      FactoryProperty^ FindProperty(XmlNode^ Node);
      void RemoveShortCuts(XmlNode^ Node);
   };