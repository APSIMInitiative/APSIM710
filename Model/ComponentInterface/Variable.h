//---------------------------------------------------------------------------
#ifndef VariableH
#define VariableH

#include <string>
#include <vector>
#include <General/platform.h>
#include <ComponentInterface/Interfaces.h>

namespace protocol {


class EXPORT Variable : public IData
   // --------------------------------------------------------------
   // This class wraps a variable from an APSIM module.
   // --------------------------------------------------------------
   {
   public:
      Variable() : data(NULL) { }
      Variable(bool value);
      Variable(int value);
      Variable(float value);
      Variable(double value);
      Variable(const std::string& value);
      Variable(const std::vector<bool>& value);

      ~Variable() {delete data;}
      Variable(const Variable& rhs);
      Variable& operator=(const Variable& rhs);

      virtual Variable* Clone() const;
		virtual void Pack(protocol::MessageData& message);
		virtual void Unpack(protocol::MessageData & message);
		virtual std::string DDML();
      virtual unsigned Size();

   private:
      IData* data;


   };
}
#endif
