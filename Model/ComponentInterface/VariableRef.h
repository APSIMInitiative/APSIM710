//---------------------------------------------------------------------------
#ifndef VariableRefH
#define VariableRefH

#include <string>
#include <vector>
#include <General/platform.h>
#include <ComponentInterface/Interfaces.h>

namespace protocol {


class EXPORT VariableRef : public IData
   // --------------------------------------------------------------
   // This class wraps a VariableRef from an APSIM module.
   // --------------------------------------------------------------
   {
   public:
      VariableRef() : data(NULL) { }
      VariableRef(bool& value);
      VariableRef(int& value);
      VariableRef(float& value);
      VariableRef(double& value);
      VariableRef(std::string& value);
      VariableRef(std::vector<bool>& value);

      ~VariableRef() {delete data;}
      VariableRef(const VariableRef& rhs);
      VariableRef& operator=(const VariableRef& rhs);

      virtual VariableRef* Clone() const;
		virtual void Pack(protocol::MessageData& message);
		virtual void Unpack(protocol::MessageData & message);
		virtual std::string DDML();
      virtual unsigned Size();

   private:
      IData* data;


   };
}
#endif
