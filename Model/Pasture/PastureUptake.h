//---------------------------------------------------------------------------
#ifndef PastureUptakeH
#define PastureUptakeH


#include <General/pch.h>
#include <boost/function.hpp>
#pragma hdrstop

#include <math.h>
#include <strstream>
#include <string>
#include <vector>

#include <iomanip.h>

#include <General/string_functions.h>
#include <General/stl_functions.h>
#include <ApsimShared/FStringExt.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/ApsimVariant.h>

#include <ComponentInterface/Component.h>
#include <ComponentInterface/DataTypes.h>


// Maximum number of layers in soil
#define max_layer 100

// ------------------------------------------------------------------
// ------------------------------------------------------------------
class PastureUptake : public protocol::Component
   {
   public:
      PastureUptake(protocol::Component *system);
      PastureUptake(protocol::Component *system, string uptakeNam, string deltaNam, string unitNam);
      ~PastureUptake(void);
      void doInit1(const protocol::Init1Data&);
      void doInit2(void);
      void doUptake(void);

   private:
      void readParameters ( void );

      unsigned elementUptakeID;
      unsigned dltElementID;
      string uptakeName;
      string deltaName;
      string unitName;

      string cDebug;

      protocol::Component *system;

   };

#endif
