#pragma hdrstop

#include "Variable.h"
#include "DataTypes.h"

using namespace protocol;

template <typename T>
class WrapBuiltIn : public IData
   {
   T value;
   public:
      WrapBuiltIn(const T& v) : value(v) { }
      virtual WrapBuiltIn<T>* Clone() const
         {return new WrapBuiltIn<T>(value);}
		virtual void Pack(protocol::MessageData& message)
         {message << value;}
		virtual void Unpack(protocol::MessageData & message)
         {message >> value;}
		virtual std::string DDML()
         {return protocol::DDML(value);}
      virtual unsigned Size()
         {return memorySize(value);}
   };


Variable::Variable(bool value)
   // -------------------------------------------------
   // Constructor for a bool.
   // -------------------------------------------------
   {
   data = new WrapBuiltIn<bool>(value);
   }

Variable::Variable(int value)
   // -------------------------------------------------
   // Constructor for a int.
   // -------------------------------------------------
   {
   data = new WrapBuiltIn<int>(value);
   }

Variable::Variable(float value)
   // -------------------------------------------------
   // Constructor for a float.
   // -------------------------------------------------
   {
   data = new WrapBuiltIn<float>(value);
   }

Variable::Variable(double value)
   // -------------------------------------------------
   // Constructor for a double.
   // -------------------------------------------------
   {
   data = new WrapBuiltIn<double>(value);
   }

Variable::Variable(const std::string& value)
   // -------------------------------------------------
   // Constructor for a string.
   // -------------------------------------------------
   {
   data = new WrapBuiltIn<string>(value);
   }

Variable::Variable(const std::vector<bool>& value)
   // -------------------------------------------------
   // Constructor for a vector of bools.
   // -------------------------------------------------
   {
   data = new WrapBuiltIn<vector<bool> >(value);
   }

Variable::Variable(const Variable& rhs)
   // -------------------------------------------------
   // copy constructor
   // -------------------------------------------------
   {
   data = rhs.data->Clone();
   }

Variable& Variable::operator=(const Variable& rhs)
   // -------------------------------------------------
   // assignment operator
   // -------------------------------------------------
   {
   delete data;
   data = rhs.data->Clone();
   return *this;
   }
Variable* Variable::Clone() const
   // -------------------------------------------------
   // Clone this object and return a pointer.
   // -------------------------------------------------
   {
   return new Variable(*this);
   }
void Variable::Pack(protocol::MessageData& message)
   // -------------------------------------------------
   // pack this variable into the specified message.
   // -------------------------------------------------
   {
   data->Pack(message);
   }
void Variable::Unpack(protocol::MessageData & message)
   // -------------------------------------------------
   // unpack this variable from the specified message.
   // -------------------------------------------------
   {
   data->Unpack(message);
   }
std::string Variable::DDML()
   // -------------------------------------------------
   // assignment operator
   // -------------------------------------------------
   {
   return data->DDML();
   }

unsigned Variable::Size()
   // -------------------------------------------------
   // assignment operator
   // -------------------------------------------------
   {
   return data->Size();
   }

