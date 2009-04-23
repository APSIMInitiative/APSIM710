//---------------------------------------------------------------------------
#ifndef FunctionReturningValueH
#define FunctionReturningValueH

#include <string>
#include <ComponentInterface/Interfaces.h>
#include <boost/function.hpp>
#include <ComponentInterface/Variable.h>

namespace protocol {

class EXPORT FunctionReturningValue : public INamedData
   // --------------------------------------------------------------
   // This class wraps a getter function for a component. It is also
   // an IData meaning that the result from the getter function can
   // be transported between modules.
   // --------------------------------------------------------------
   {
   public:

      typedef boost::function1<Variable, const std::string&> Function;
      FunctionReturningValue(Function  f)
         {
         F = f;
         }
      virtual FunctionReturningValue* Clone() const
         {
         FunctionReturningValue* NewFunction = new FunctionReturningValue(F);
         NewFunction->SetName(VariableName);
         return NewFunction;
         }
		virtual void Pack(protocol::MessageData& message)
         {
         F(VariableName).Pack(message);
         }
		virtual void Unpack(protocol::MessageData & message)
         {
         throw runtime_error("Cannot unpack a getfunction");
         }
		virtual std::string DDML()
         {
         return F(VariableName).DDML();
         }
      virtual unsigned Size()
         {
         return F(VariableName).Size();
         }
      virtual void SetName(const std::string& Name)
         {this->VariableName = Name;}

   private:
      Function F;
      std::string VariableName;
   };
}
#endif
