#pragma once
#using <CSGeneral.dll>
using namespace System;
using namespace System::Collections::Generic;
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
      property List<FactoryEventHandler^>^ EventHandlers 
         {
         List<FactoryEventHandler^>^ get() { return RegisteredEventHandlers; }
         }
      property List<FactoryEvent^>^ Events 
         {
         List<FactoryEvent^>^ get() { return RegisteredEvents; }
         }
      void Create(String^ Xml, Assembly^ Assembly);
      void Create(XmlNode^ Node, Assembly^ Assembly);
      void PopulateParams(Instance^ Obj, XmlNode^ Node);
      void ThrowOnUnInitialisedParameters();
   private:
      Instance^ _Root;
      List<FactoryProperty^>^ RegisteredProperties;
      List<FactoryEventHandler^>^ RegisteredEventHandlers;
      List<FactoryEvent^>^ RegisteredEvents;
      Assembly^ CallingAssembly;
      
      Instance^ CreateInstance(XmlNode^ Node, XmlNode^ Parent);
      void GetAllProperties(Instance^ Obj, XmlNode^ Parent);
      void GetAllEventHandlers(Instance^ Obj);
      void GetAllEvents(Instance^ Obj);
      void RemoveRegisteredOutput(String^ OuptutName);
      FactoryProperty^ FindProperty(XmlNode^ Node);
   };