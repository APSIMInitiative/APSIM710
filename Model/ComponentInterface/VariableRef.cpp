#include "VariableRef.h"
#include "DataTypes.h"

using namespace protocol;

template <typename T>
class WrapBuiltInRef : public IData
   {
   T& value;
   public:
      WrapBuiltInRef(T& v) : value(v) { }
      virtual WrapBuiltInRef<T>* Clone() const
         {return new WrapBuiltInRef<T>(value);}
		virtual void Pack(protocol::MessageData& message)
         {message << value;}
		virtual void Unpack(protocol::MessageData & message)
         {message >> value;}
		virtual std::string DDML()
         {return protocol::DDML(value);}
      virtual unsigned Size()
         {return memorySize(value);}
   };


VariableRef::VariableRef(bool& value)
   // -------------------------------------------------
   // Constructor for a bool.
   // -------------------------------------------------
   {
   data = new WrapBuiltInRef<bool>(value);
   }

VariableRef::VariableRef(int& value)
   // -------------------------------------------------
   // Constructor for a int.
   // -------------------------------------------------
   {
   data = new WrapBuiltInRef<int>(value);
   }

VariableRef::VariableRef(float& value)
   // -------------------------------------------------
   // Constructor for a float.
   // -------------------------------------------------
   {
   data = new WrapBuiltInRef<float>(value);
   }

VariableRef::VariableRef(double& value)
   // -------------------------------------------------
   // Constructor for a double.
   // -------------------------------------------------
   {
   data = new WrapBuiltInRef<double>(value);
   }

VariableRef::VariableRef(std::string& value)
   // -------------------------------------------------
   // Constructor for a string.
   // -------------------------------------------------
   {
   data = new WrapBuiltInRef<string>(value);
   }

VariableRef::VariableRef(std::vector<bool>& value)
   // -------------------------------------------------
   // Constructor for a vector of bools.
   // -------------------------------------------------
   {
   data = new WrapBuiltInRef<std::vector<bool> >(value);
   }

VariableRef::VariableRef(const VariableRef& rhs)
   // -------------------------------------------------
   // copy constructor
   // -------------------------------------------------
   {
   data = rhs.data->Clone();
   }

VariableRef& VariableRef::operator=(const VariableRef& rhs)
   // -------------------------------------------------
   // assignment operator
   // -------------------------------------------------
   {
   delete data;
   data = rhs.data->Clone();
   return *this;
   }
VariableRef* VariableRef::Clone() const
   // -------------------------------------------------
   // Clone this object and return a pointer.
   // -------------------------------------------------
   {
   return new VariableRef(*this);
   }
void VariableRef::Pack(protocol::MessageData& message)
   // -------------------------------------------------
   // pack this VariableRef into the specified message.
   // -------------------------------------------------
   {
   data->Pack(message);
   }
void VariableRef::Unpack(protocol::MessageData & message)
   // -------------------------------------------------
   // unpack this VariableRef from the specified message.
   // -------------------------------------------------
   {
   data->Unpack(message);
   }
std::string VariableRef::DDML()
   // -------------------------------------------------
   // assignment operator
   // -------------------------------------------------
   {
   return data->DDML();
   }

unsigned VariableRef::Size()
   // -------------------------------------------------
   // assignment operator
   // -------------------------------------------------
   {
   return data->Size();
   }

