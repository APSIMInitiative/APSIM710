#include "Factory.h"

Factory::Factory() 
   {
   // --------------------------------------------------------------------
   // Constructor
   // --------------------------------------------------------------------
   
   RegisteredProperties = gcnew List<FactoryProperty^>();
   RegisteredEventHandlers = gcnew List<EvntHandler^>();
   RegisteredEvents = gcnew List<FactoryEvent^>();
   }

void Factory::Create(String^ Xml, 
                     Assembly^ AssemblyWithTypes,
                     ModelFramework::ApsimComponent^ ParentComponent)
   {
   // --------------------------------------------------------------------
   // Create instances (and populate their fields and properies of all 
   // classes as specified by the Xml passed in. The newly created root
   // instance can be retrieved by the 'Root' property.
   // --------------------------------------------------------------------
   CallingAssembly = AssemblyWithTypes;
   XmlDocument^ Doc = gcnew XmlDocument();
   Doc->LoadXml(Xml);
   RemoveShortCuts(Doc->DocumentElement);
   _Root = CreateInstance(Doc->DocumentElement, nullptr, nullptr, ParentComponent);
   }
//void Factory::Create(XmlNode^ Node, Assembly^ AssemblyWithTypes)
//   {
//   // --------------------------------------------------------------------
//   // Create instances (and populate their fields and properies of all 
//   // classes as specified by the Xml passed in. The newly created root
//   // instance can be retrieved by the 'Root' property.
//   // --------------------------------------------------------------------
//   CallingAssembly = AssemblyWithTypes;
//   _Root = CreateInstance(Node, nullptr, nullptr);
//   }
Instance^ Factory::CreateInstance(XmlNode^ Node, 
                                  XmlNode^ Parent, 
                                  Instance^ ParentInstance, 
                                  ModelFramework::ApsimComponent^ ParentComponent)
   {
   // --------------------------------------------------------------------
   // Create an instance of a the 'Instance' class based on the 
   // Node type passed in information. Then populate the instance based
   // on the child XML nodes.
   // --------------------------------------------------------------------
   Type^ ClassType = CallingAssembly->GetType(Node->Name);
   if (ClassType == nullptr)
      throw gcnew Exception("Cannot find a class called: " + Node->Name);
   Instance^ CreatedInstance = dynamic_cast<Instance^> (Activator::CreateInstance(ClassType));

   CreatedInstance->Initialise(XmlHelper::Name(Node), ParentInstance, ParentComponent);
   GetAllProperties(CreatedInstance, Parent);
   GetAllEventHandlers(CreatedInstance);
   GetAllEvents(CreatedInstance);
   PopulateParams(CreatedInstance, Node, ParentComponent);
   CreatedInstance->Initialised();
   return CreatedInstance;
   }
void Factory::GetAllProperties(Instance^ Obj, XmlNode^ Parent)
   {
   // --------------------------------------------------------------------
   // Go through all reflected fields and properties that are tagged
   // with a 'Param' or 'Input' attribute and add them to our list
   // of registered properties.
   // --------------------------------------------------------------------
   for each (FieldInfo^ Property in Obj->GetType()->GetFields(BindingFlags::FlattenHierarchy | BindingFlags::Instance | BindingFlags::Public | BindingFlags::NonPublic))
      {
      bool AddProperty = false;
      bool IsOutput = false;
      array<Object^>^ Attributes = Property->GetCustomAttributes(false);
      for each (Object^ Attr in Attributes)
         {
         IsOutput = (IsOutput || dynamic_cast<Output^>(Attr) != nullptr);
         if (dynamic_cast<Param^>(Attr) != nullptr || 
             dynamic_cast<Input^>(Attr) != nullptr || 
             dynamic_cast<Output^>(Attr) != nullptr)
             AddProperty = true;
         }
      if (AddProperty)
         {
         FactoryProperty^ NewProperty = gcnew FactoryProperty(gcnew ReflectedField(Property, Obj), Parent);
         if (IsOutput)
            RemoveRegisteredOutput(NewProperty->OutputName);
         RegisteredProperties->Add(NewProperty);
         }
      }
   for each (PropertyInfo^ Property in Obj->GetType()->GetProperties(BindingFlags::FlattenHierarchy | BindingFlags::Instance | BindingFlags::Public | BindingFlags::NonPublic))
      {
      bool AddProperty = false;
      bool IsOutput = false;
      array<Object^>^ Attributes = Property->GetCustomAttributes(false);
      for each (Object^ Attr in Attributes)
         {
         IsOutput = (IsOutput || dynamic_cast<Output^>(Attr) != nullptr);
         if (dynamic_cast<Param^>(Attr) != nullptr || 
             dynamic_cast<Input^>(Attr) != nullptr || 
             dynamic_cast<Output^>(Attr) != nullptr)
            AddProperty = true;
         }
      if (AddProperty)
         {
         FactoryProperty^ NewProperty = gcnew FactoryProperty(gcnew ReflectedProperty(Property, Obj), Parent);
         if (IsOutput)
            RemoveRegisteredOutput(NewProperty->OutputName);
         RegisteredProperties->Add(NewProperty);
         }
      }
   }
void Factory::RemoveRegisteredOutput(String^ OutputName)
   {
   // --------------------------------------------------------------------
   // Remove the specified [output] from the list of registered properties.
   // Duplicates can happen when an [output] in a base class is
   // overridden in a derived class [output]. In this case we want the last
   // duplicate and it superseeds previous ones (base classes)
   // --------------------------------------------------------------------
   for (int i = 0; i != RegisteredProperties->Count; i++)
      {
      if (RegisteredProperties[i]->IsOutput && 
          String::Compare(RegisteredProperties[i]->OutputName, OutputName) == 0)
         {
         RegisteredProperties->RemoveAt(i);
         return;
         }
      }
   }   
void Factory::GetAllEventHandlers(Instance^ Obj)
   {
   // --------------------------------------------------------------------
   // Goes through the model looking for all event handlers that start
   // with 'On' and are marked 'EventHandler'
   // --------------------------------------------------------------------
   
   for each (MethodInfo^ Method in Obj->GetType()->GetMethods(BindingFlags::Instance | BindingFlags::Public | BindingFlags::NonPublic))
      {
      array<Object^>^ Attributes = Method->GetCustomAttributes(false);
      for each (Object^ Attr in Attributes)
         {
         if (dynamic_cast<::EventHandler^>(Attr) != nullptr && 
             Method->Name->Length > 2 &&
             Method->Name->Substring(0, 2) == "On")
            RegisteredEventHandlers->Add(gcnew FactoryEventHandler(Method, Obj));
         }
      }
   }
void Factory::GetAllEvents(Instance^ Obj)
   {
   // --------------------------------------------------------------------
   // Goes through the model looking for all events
   // --------------------------------------------------------------------
   for each (EventInfo^ Event in Obj->GetType()->GetEvents(BindingFlags::Instance | BindingFlags::Public | BindingFlags::NonPublic))
      {
      array<Object^>^ Attributes = Event->GetCustomAttributes(false);
      for each (Object^ Attr in Attributes)
         {
         if (dynamic_cast<::Event^>(Attr) != nullptr)
            RegisteredEvents->Add(gcnew FactoryEvent(Event, Obj));
         }
      }
   }   

   void Factory::PopulateParams(Instance^ Obj, XmlNode^ Node,ModelFramework::ApsimComponent^ ParentComponent)
      {
      // --------------------------------------------------------------------
      // Go through all child XML nodes for the node passed in and set
      // the corresponding property values in the Obj instance passed in.
      // --------------------------------------------------------------------      
      for each (XmlNode^ Child in Node->ChildNodes)
         {
         if (dynamic_cast<XmlComment^>(Child) == nullptr)
            {
            Type^ t = CallingAssembly->GetType(Child->Name);
            if (t != nullptr)
               {
               // Create a child instance - indirect recursion.
               Instance^ ChildInstance = CreateInstance(Child, Child, Obj, ParentComponent);
               Obj->Add(ChildInstance);   
               }
            else if (!Child->HasChildNodes && Child->InnerText == "")
               throw gcnew Exception("Cannot have a blank value for property: " + Child->Name);
            else if (Child->HasChildNodes)
               {
               FactoryProperty^ Parameter = FindProperty(Child);
               if (Parameter == nullptr)
                  throw gcnew Exception("Cannot set value of property: " + Child->Name + ". The property must have either a [Param] or [Input] attribute.");
               Parameter->Name = XmlHelper::Name(Child);
               bool IsXmlText = (dynamic_cast<XmlText^>(Child->ChildNodes[0]) != nullptr);
               if (IsXmlText)
                  Parameter->Set(Child->InnerText);  // set the value of the simple property.
               else
                  Parameter->SetFromXML(Child);      // assume structure and set all fields.
               }
            }
         }
      }
   FactoryProperty^ Factory::FindProperty(XmlNode^ Child)
      {
      // --------------------------------------------------------------------
      // Go through all our registered properties and look for the one that
      // has the specified name. Returns null if not found.
      // --------------------------------------------------------------------
      String^ FQN = CalcParentName(Child);
      for each (FactoryProperty^ Property in RegisteredProperties)
         {
         if (Property->FQN == FQN)
            return Property;
         }
      return nullptr;
      }
      
void Factory::ThrowOnUnInitialisedParameters()
   {
	// -----------------------------------------------
	// Check for parameters in the model that
	// haven't been given a value and throw if any 
	// are found.
	// -----------------------------------------------
	
	String^ Errors = "";
	for (int i = 0; i != RegisteredProperties->Count; i++)
	   {
	   FactoryProperty^ Property = RegisteredProperties[i];
	   if (Property->IsParam && !Property->HasAsValue)
	      {
	      if (Errors != "")
	         Errors += ", ";
	      Errors += Property->FQN;
         }	
      }
   if (Errors != "")
      throw gcnew Exception("The following parameters haven't been initialised: " + Errors);
	}
      
void Factory::RemoveShortCuts(XmlNode^ Node)
   {
   // -----------------------------------------------
   // Remove any shortcut nodes in the children of
   // the specified node.
	// -----------------------------------------------
	
   String^ ShortCutPath = XmlHelper::Attribute(Node, "shortcut");
   if (ShortCutPath != "")
      {
      // Shortcut strings will be a full path e.g. /FrenchBean/Model/Plant/Phenology/ThermalTime
      // But our Node->OwnerDocument->DocumentElemen is <Plant> 
      // So we need to find /Plant/ and remove everything on the path before that. This way
      // we'll end up with a relative path e.g. Plant/Phenology/ThermalTime
      int PosPlant = ShortCutPath->IndexOf("/Plant/");
      if (PosPlant == -1)
         throw gcnew Exception("Invalid shortcut path: " + ShortCutPath);
      ShortCutPath = ShortCutPath->Remove(0, PosPlant+7); // Get rid of the /Plant/ string.
         
      XmlNode^ ReferencedNode = XmlHelper::Find(Node->OwnerDocument->DocumentElement, ShortCutPath);
      if (ReferencedNode == nullptr)
         throw gcnew Exception("Cannot find short cut node: " + ShortCutPath);
      Node->ParentNode->ReplaceChild(ReferencedNode->CloneNode(true), Node);	      
      }
	
	for (int i = 0; i < Node->ChildNodes->Count; i++)
	   RemoveShortCuts(Node->ChildNodes[i]);
   }
      