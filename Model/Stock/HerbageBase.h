//---------------------------------------------------------------------------
#ifndef HerbageBase_H
#define HerbageBase_H

#include <math.h>
#include <string>
#include <vector>

#include <General/string_functions.h>
#include <General/stl_functions.h>
#include <ApsimShared/FStringExt.h>
#include <ComponentInterface/MessageDataExt.h>
#include <ComponentInterface/ApsimVariant.h>

#include <ComponentInterface/Component.h>
#include <ComponentInterface/DataTypes.h>


//#include "HerbagePool.h"

#define min(A,B) ((A)<(B)?(A):(B))
#define max(A,B) ((A)>(B)?(A):(B))
// Maximum number of layers in soil

std::string ftoa(double Float);
std::string ftoa(double Float, char *fmtwidth);
std::string itoa(int value, int width);

      const int maxDmdPoolsVeg = 20;
      const int maxDmdPoolsSeed = 2;
      const int maxSeedClasses = 2;

// ------------------------------------------------------------------
class HerbageBase : public protocol::Component
   {
   public:
      HerbageBase(void);
      HerbageBase(protocol::Component *system);
      virtual ~HerbageBase(void);

    virtual void doInit1(const protocol::Init1Data&) = 0;
    virtual void doInit2(void) = 0;
    virtual void doGrazed(protocol::RemoveHerbageType &grazed) = 0;
    virtual void doDigestibility(void) = 0;

    virtual float dmTotalVeg(void) = 0;
    virtual int numDmdPoolsVeg(void) = 0;
    virtual float dmTotVeg(int pool) = 0;
    virtual float dmdValueVeg(int pool) = 0;
    virtual float cpConcVeg(int pool) = 0;
    virtual float pConcVeg(int pool) = 0;
    virtual float sConcVeg(int pool) = 0;
    virtual float protDgVeg(int pool) = 0;
    virtual float ashAlkVeg(int pool) = 0;
    virtual float heightRatioVeg(void) = 0;
    virtual float bD(void) = 0;
    virtual float hHeight(void) = 0;

    virtual float dmTotalSeed(void) = 0;
    virtual int numDmdPoolsSeed(void) = 0;
    virtual float dmTotSeed(int pool) = 0;
    virtual float dmdValueSeed(int pool) = 0;
    virtual float cpConcSeed(int pool) = 0;
    virtual float pConcSeed(int pool) = 0;
    virtual float sConcSeed(int pool) = 0;
    virtual float protDgSeed(int pool) = 0;
    virtual float ashAlkSeed(int pool) = 0;
    virtual float heightRatioSeed(void) = 0;
    virtual void getStage(void) = 0;
    virtual float trampling(void) = 0;

    virtual float proportionGreen(void) = 0;
    virtual float proportionLegume(void) = 0;
    virtual float selectionFactor(void) = 0; // ??
    virtual int seedClass(int pool) = 0;
    virtual int seedMaturity(void) = 0;

    protected:
        protocol::Component *system;

   };


#endif
