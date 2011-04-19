#include "stdafx.h"
#include "Paddock.h"
#include "ApsimComponent.h"
using namespace ModelFramework;

/// <summary>
/// Publish a notification event i.e. one that doesn't have any data 
/// associated with it. This event is broadcast to all components within scope.
/// </summary>
void Paddock::Publish(String^ EventName)
   {
   Comp->Publish(EventName, gcnew NullType());
   }

/// <summary>
/// Publish an event that has associated data. This event is broadcast to all components within scope.
/// </summary>
void Paddock::Publish(String^ EventName, ApsimType^ Data)
   {
   Comp->Publish(EventName, Data);
   }


/// <summary>
///  Create a component of the specified name
/// </summary>
ModelFramework::Component^ Paddock::CreateComponent(String^ ComponentName)
   {
   Type^ T = Types::GetProbeInfoAssembly()->GetType("ModelFramework." + ComponentType(ComponentName));
   if (T == nullptr)
      return gcnew Component(ComponentName, Comp);
   else
      {
      array<Object^>^ Parameters = gcnew array<Object^>(2);
      Parameters[0] = ComponentName;
      Parameters[1] = Comp;
      return (Component^)Activator::CreateInstance(T, Parameters);
      }
   }
