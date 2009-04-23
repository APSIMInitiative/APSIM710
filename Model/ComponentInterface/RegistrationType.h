//---------------------------------------------------------------------------
#ifndef RegistrationTypeH
#define RegistrationTypeH
#include <general/platform.h>

#include <string>
//---------------------------------------------------------------------------
// Encapsulates a registration type.
//---------------------------------------------------------------------------
class EXPORT RegistrationType
   {
   public:
      enum Type {get=1,         respondToGet=2,
                 set=9,         respondToSet=3,
                 event=5,       respondToEvent=6,
                                respondToGetSet=4,
                                invalid=100};

      RegistrationType(void) : regType(invalid) {}
      RegistrationType(Type t) : regType(t) { }
      RegistrationType(const std::string& st);

      std::string asString();
      RegistrationType opposite();
      RegistrationType::Type type() const {return regType;}
      bool isPassive();

      bool operator ==(const RegistrationType& rhs)
         {return rhs.regType == regType;}
      bool operator !=(const RegistrationType& rhs)
         {return rhs.regType != regType;}
   private:
      Type regType;
   };

#endif
