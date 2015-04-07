//---------------------------------------------------------------------------
#ifndef FunctionTakingValueH
#define FunctionTakingValueH

#include <string>
#include <ComponentInterface/Interfaces.h>
#include <ComponentInterface/Variable.h>
#include "DataTypes.h"

namespace protocol {

template <class T>
class EXPORT FunctionTakingValue : public INamedData
   // --------------------------------------------------------------
   // Encapsulates a function: void f(const std::string&, const T& value)
   // --------------------------------------------------------------
   {
   public:
      typedef std::function<void(const std::string&, const T &)> Function;
      FunctionTakingValue<T>(Function  f)
         : F(f) { }
      virtual FunctionTakingValue<T>* Clone() const
         {
         FunctionTakingValue<T>* NewF = new FunctionTakingValue<T>(F);
         NewF->SetName(Name);
         return NewF;
         }
		virtual void Pack(protocol::MessageData& message)
		   {throw runtime_error("Cannot pack a SetFunction");}
		virtual void Unpack(protocol::MessageData & message)
		   {
		   message >> value;
		   F(Name, value);
		   }
		virtual std::string DDML()
		   {return protocol::DDML(value);}
      virtual unsigned Size()
         {return memorySize(value);}
      virtual void SetName(const std::string& Name)
         {this->Name = Name;}

   private:
      T value;
      Function F;
      std::string Name;
   };

}
#endif
