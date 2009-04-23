//---------------------------------------------------------------------------
#ifndef ConverterBase_H
#define ConverterBase_H

#include <math.h>
#include <string>
#include <sstream>
#include <vector>
#include <iomanip.h>

#include <general/string_functions.h>
#include <general/stl_functions.h>
#include <ApsimShared/FStringExt.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/ApsimVariant.h>

#include <ComponentInterface/Component.h>
#include <ComponentInterface/DataTypes.h>

// ------------------------------------------------------------------
class ConverterBase : public protocol::Component
   {
   public:
      ConverterBase(void);
      ConverterBase(protocol::Component *system);
      virtual ~ConverterBase(void);
      virtual void doInit1(const protocol::Init1Data&) = 0;
      virtual void doInit2(void) = 0;
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData) = 0;
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant) = 0;

   protected:
      protocol::Component *system;

   };

#endif
