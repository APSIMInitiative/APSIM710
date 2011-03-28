//---------------------------------------------------------------------------

#ifndef ScienceAPIH
#define ScienceAPIH

#include <string>
#include <vector>
#include <General/platform.h>
#include <boost/function.hpp>
#include "DataTypes.h"
#include <ComponentInterface/ApsimVariant.h>

namespace protocol {
   class Component;
   };

class DeletableThing
   {
   public:
      virtual ~DeletableThing() {}
   };

class EXPORT ScienceAPI
   {
   public:
      ScienceAPI(protocol::Component* component);
      ~ScienceAPI();

      protocol::Component* getComponent() {return component;}   // get rid of this ASAP.

      void write(const std::string& msg);
      void warning(const std::string& msg);

      // read methods.
      bool read(const std::string& name, int& data, int lower, int upper);
      bool read(const std::string& name, float& data, float lower, float upper);
      bool read(const std::string& name, double& data, double lower, double upper);
      bool read(const std::string& name, std::string& data);

      bool read(const std::string& name, std::vector<int>& data, int lower, int upper);
      bool read(const std::string& name, std::vector<float>& data, float lower, float upper);
      bool read(const std::string& name, float data[], int& numVals, float lower, float upper);
      bool read(const std::string& name, std::vector<double>& data, float lower, float upper);
      bool read(const std::string& name, std::vector<std::string>& data);

      bool readFiltered(const std::string& filterName, std::vector<std::string> &values);
      bool readAll(const std::string& section, std::vector<std::string>& names, std::vector<std::string> &values);

      bool readOptional(const std::string& name, int& data, int lower, int upper);
      bool readOptional(const std::string& name, float& data, float lower, float upper);
      bool readOptional(const std::string& name, double& data, double lower, double upper);
      bool readOptional(const std::string& name, std::string& data);

      bool readOptional(const std::string& name, std::vector<int>& data, int lower, int upper);
      bool readOptional(const std::string& name, std::vector<float>& data, float lower, float upper);
      bool readOptional(const std::string& name, float data[], int& numVals, float lower, float upper);
      bool readOptional(const std::string& name, std::vector<double>& data, float lower, float upper);
      bool readOptional(const std::string& name, std::vector<std::string>& data);

      void setClass1(const std::string& class1) {currentClass1 = class1;}
      void setClass2(const std::string& class2) {currentClass2 = class2;}

      // get methods
      bool get(const std::string& name, const std::string& units, float& data, float lower, float upper);
      bool get(const std::string& name, const std::string& units, std::vector<float>& data, float lower, float upper);
      bool getOptional(const std::string& name, const std::string& units, float& data, float lower, float upper);
      bool getOptional(const std::string& name, const std::string& units, std::vector<float>& data, float lower, float upper);

      // expose variables
      #define IntGetterType boost::function0<int>
      #define IntGetter(address) boost::function0<int>(boost::bind(address, this))
      #define FloatGetterType boost::function0<float>
      #define FloatGetter(address) boost::function0<float>(boost::bind(address, this))
      #define StringGetterType boost::function0<std::string>
      #define StringGetter(address) boost::function0<std::string>(boost::bind(address, this))
      void expose(const std::string& name, const std::string& units, const std::string& description, int& variable);
      void expose(const std::string& name, const std::string& units, const std::string& description, float& variable);
      void expose(const std::string& name, const std::string& units, const std::string& description, string& variable);
      void expose(const std::string& name, const std::string& units, const std::string& description, const std::vector<float>& variable);
      void exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function0<int> fn);
      void exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function0<float> fn);
      void exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function0<std::string> fn);

      // set methods
      void set(const std::string& name, const std::string& units, std::vector<float>& data);

      #define FloatSetter(address) boost::function1<void, float>(boost::bind(address, this, _1))
      void exposeWritable(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, float> fn);

      #define StringSetter(address) boost::function1<void, const std::string&>(boost::bind(address, this, _1))
      void exposeWritable(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, const std::string&> fn);

      // event handlers
      #define NullFunctionType boost::function0<void>
      #define NullFunction(address) NullFunctionType(boost::bind(address, this))
      void subscribe(const std::string& name, NullFunctionType handler);
      void publish(const std::string& name);


      #define FloatFunctionType boost::function1<void, float>
      #define FloatFunction(address) boost::function1<void, float>(boost::bind(address, this, _1))
      void subscribe(const std::string& name, FloatFunctionType handler);

      #define NullFunctionWithNameType boost::function1<void, const std::string&>
      #define NullFunctionWithName(address) NullFunctionWithNameType(boost::bind(address, this, _1))
      void subscribe(const std::string& name, NullFunctionWithNameType handler);

      #define SowFunctionType boost::function1<void, protocol::SowType& >
      #define SowFunction(address) SowFunctionType(boost::bind(address, this, _1))
      void subscribe(const std::string& name, SowFunctionType handler);

      #define HarvestFunctionType boost::function1<void, protocol::HarvestType& >
      #define HarvestFunction(address) HarvestFunctionType(boost::bind(address, this, _1))
      void subscribe(const std::string& name, HarvestFunctionType handler);

      #define KillStemFunctionType boost::function1<void, protocol::KillStemType& >
      #define KillStemFunction(address) KillStemFunctionType(boost::bind(address, this, _1))
      void subscribe(const std::string& name, KillStemFunctionType handler);

      #define TimeFunctionType boost::function1<void, protocol::TimeType& >
      #define TimeFunction(address) TimeFunctionType(boost::bind(address, this, _1))
      void subscribe(const std::string& name, TimeFunctionType handler);

      #define NewMetFunctionType boost::function1<void, protocol::NewMetType& >
      #define NewMetFunction(address) NewMetFunctionType(boost::bind(address, this, _1))
      void subscribe(const std::string& name, NewMetFunctionType handler);

      #define KillCropFunctionType boost::function1<void, protocol::KillCropType&>
      #define KillCropFunction(address) KillCropFunctionType(boost::bind(address, this, _1))
      void subscribe(const std::string& name, KillCropFunctionType handler);

      #define NewProfileFunctionType boost::function1<void, protocol::NewProfileType&>
      #define NewProfileFunction(address) NewProfileFunctionType(boost::bind(address, this, _1))
      void subscribe(const std::string& name, NewProfileFunctionType handler);

      #define ExternalMassFlowFunctionType boost::function1<void, protocol::ExternalMassFlowType&>
      #define ExternalMassFlowFunction(address) ExternalMassFlowFunctionType(boost::bind(address, this, _1))
      void publish(const std::string& name, protocol::ExternalMassFlowType& value);

      #define NewCanopyFunctionType boost::function1<void, protocol::NewCanopyType&>
      #define NewCanopyFunction(address) NewCanopyFunctionType(boost::bind(address, this, _1))
      void publish(const std::string& name, protocol::NewCanopyType& value);

      #define NewCropFunctionType boost::function1<void, protocol::NewCropType&>
      #define NewCropFunction(address) NewCropFunctionType(boost::bind(address, this, _1))
      void publish(const std::string& name, protocol::NewCropType& value);

      #define NewPotentialGrowthFunctionType boost::function1<void, protocol::NewPotentialGrowthType&>
      #define NewPotentialGrowthFunction(address) NewPotentialGrowthFunctionType(boost::bind(address, this, _1))
      void publish(const std::string& name, protocol::NewPotentialGrowthType& value);

      #define CanopyWaterBalanceFunctionType boost::function1<void, protocol::CanopyWaterBalanceType&>
      #define CanopyWaterBalanceFunction(address) CanopyWaterBalanceFunctionType(boost::bind(address, this, _1))
      void subscribe(const std::string& name, CanopyWaterBalanceFunctionType handler);

      #define RemoveCropDmFunctionType boost::function1<void, protocol::RemoveCropDmType&>
      #define RemoveCropDmFunction(address) RemoveCropDmFunctionType(boost::bind(address, this, _1))
      void subscribe(const std::string& name, RemoveCropDmFunctionType handler);

      #define BiomassRemovedFunctionType boost::function1<void, protocol::BiomassRemovedType&>
      #define BiomassRemovedFunction(address) BiomassRemovedFunctionType(boost::bind(address, this, _1))
      void publish(const std::string& name, protocol::BiomassRemovedType& value);

      #define FOMLayerFunctionType boost::function1<void, protocol::FOMLayerType&>
      #define FOMLayerFunction(address) FOMLayerFunctionType(boost::bind(address, this, _1))
      void publish(const std::string& name, protocol::FOMLayerType& value);

      #define ApsimVariantFunctionType boost::function1<void, protocol::ApsimVariant&>
      #define ApsimVariantFunction(address) ApsimVariantFunctionType(boost::bind(address, this, _1))
      void publish(const std::string& name, protocol::ApsimVariant& value);
      void subscribe(const std::string& name, ApsimVariantFunctionType handler);

   private:
      std::string currentClass1;
      std::string currentClass2;
      protocol::Component* component;
      std::vector<DeletableThing*> stuffToDelete;

      std::string readFromSection(const std::string& section, const std::string& name);

   };

std::string EXPORT addUnitsToDDML(const string& ddml, const string& units);
std::string EXPORT addDescToDDML(const string& ddml, const string& desc);

#endif
