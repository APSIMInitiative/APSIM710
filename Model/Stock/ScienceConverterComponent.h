//---------------------------------------------------------------------------
#ifndef ScienceConverterComponent_H
#define ScienceConverterComponent_H
#include <General/pch.h>

#include <math.h>
#include <string>
#include <strstream>
#include <vector>
#include <iomanip.h>

#include <boost/function.hpp>
#include <General/string_functions.h>
#include <General/stl_functions.h>
#include <ApsimShared/FStringExt.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/ApsimVariant.h>

#include <ComponentInterface/Component.h>
#include <ComponentInterface/DataTypes.h>

#include "HerbageConverter.h"
#include "NonHerbageConverter.h"

// ------------------------------------------------------------------
class ScienceConverterComponent : public protocol::Component
   {
   public:
      ScienceConverterComponent(void);
      virtual ~ScienceConverterComponent(void);
      virtual void doInit1(const protocol::Init1Data&);
      virtual void doInit2(void);
      virtual void respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData);
      virtual void respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant);

   private:

      ConverterBase *conversion;
      string conversion_model;
      unsigned endRunID;
   };

//class PlantPoolTypeC
//{
//}

#endif
