#include "Component.h"
#include "ApsimComponent.h"
using namespace ModelFramework;

/// <summary>
/// Returns a reference to a variable.
/// </summary>
ModelFramework::Variable^ ModelFramework::Component::Variable(String^ VariableName)
   {
   return gcnew ModelFramework::Variable(Comp, FullName, VariableName);
   }

/// <summary>
/// Publish a notification event (i.e. one that doesn't have any data 
/// associated with it) to this component only.
/// </summary>
void ModelFramework::Component::Publish(String^ EventName)
   {
   Comp->Publish(FullName + "." + EventName, gcnew NullType());
   }

/// <summary>
/// Publish an event that has associated data to this component only.
/// </summary>
void ModelFramework::Component::Publish(String^ EventName, ApsimType^ Data)
   {
   Comp->Publish(FullName + "." + EventName, Data);
   }

/// <summary>
/// Subscribe to a notification event ie. one without any data associated with it.
/// </summary>
void ModelFramework::Component::Subscribe(String^ EventName, RuntimeEventHandler::NullFunction^ F) 
   {
   RuntimeEventHandler^ Event = gcnew RuntimeEventHandler(EventName, F);
   Comp->Subscribe(Event);
   }         
