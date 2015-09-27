//---------------------------------------------------------------------------

#ifndef ScienceAPIH
#define ScienceAPIH

#include <string>
#include <vector>
#include <General/platform.h>
#include <General/string_functions.h>
#include <functional>
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
      #define IntGetterType std::function<int()>
      #define IntGetter(address) std::function<int()>(std::bind(address, this))
      #define FloatGetterType std::function<float()>
      #define FloatGetter(address) std::function<float()>(std::bind(address, this))
      #define StringGetterType std::function<std::string()>
      #define StringGetter(address) std::function<std::string()>(std::bind(address, this))
      void expose(const std::string& name, const std::string& units, const std::string& description, int& variable);
      void expose(const std::string& name, const std::string& units, const std::string& description, float& variable);
      void expose(const std::string& name, const std::string& units, const std::string& description, string& variable);
      void expose(const std::string& name, const std::string& units, const std::string& description, const std::vector<float>& variable);
      void exposeFunction(const std::string& name, const std::string& units, const std::string& description, std::function<int()> fn);
      void exposeFunction(const std::string& name, const std::string& units, const std::string& description, std::function<float()> fn);
      void exposeFunction(const std::string& name, const std::string& units, const std::string& description, std::function<std::string()> fn);

      void exposeRWArray(const std::string& name, const std::string& units, const std::string& description, const std::vector<float>& variable,
                               std::function<void(std::vector<float>) > handler);

      // set methods
      void set(const std::string& name, const std::string& units, std::vector<float>& data);

      #define IntSetter(address) std::function<void(int)>(std::bind(address, this, _1))
      void exposeWritable(const std::string& name, const std::string& units, const std::string& description, std::function<void(int)> fn);
      
      #define FloatSetter(address) std::function<void(float)>(std::bind(address, this, _1))
      void exposeWritable(const std::string& name, const std::string& units, const std::string& description, std::function<void(float)> fn);

      #define StringSetter(address) std::function<void(const std::string&)>(std::bind(address, this, _1))
      void exposeWritable(const std::string& name, const std::string& units, const std::string& description, std::function<void(const std::string&)> fn);

      // event handlers
      #define NullFunctionType std::function<void()>
      #define NullFunction(address) NullFunctionType(std::bind(address, this))
      void subscribe(const std::string& name, NullFunctionType handler);
      void publish(const std::string& name);


      #define FloatFunctionType std::function<void(float)>
      #define FloatFunction(address) std::function<void(float)>(std::bind(address, this, _1))
      void subscribe(const std::string& name, FloatFunctionType handler);

      #define NullFunctionWithNameType std::function<void(const std::string&)>
      #define NullFunctionWithName(address) NullFunctionWithNameType(std::bind(address, this, _1))
      void subscribe(const std::string& name, NullFunctionWithNameType handler);

      #define SowFunctionType std::function<void(protocol::SowType&) >
      #define SowFunction(address) SowFunctionType(std::bind(address, this, _1))
      void subscribe(const std::string& name, SowFunctionType handler);

      #define HarvestFunctionType std::function<void(protocol::HarvestType&) >
      #define HarvestFunction(address) HarvestFunctionType(std::bind(address, this, _1))
      void subscribe(const std::string& name, HarvestFunctionType handler);

      #define KillStemFunctionType std::function<void(protocol::KillStemType&) >
      #define KillStemFunction(address) KillStemFunctionType(std::bind(address, this, _1))
      void subscribe(const std::string& name, KillStemFunctionType handler);

      #define TimeFunctionType std::function<void(protocol::TimeType&) >
      #define TimeFunction(address) TimeFunctionType(std::bind(address, this, _1))
      void subscribe(const std::string& name, TimeFunctionType handler);

      #define NewMetFunctionType std::function<void(protocol::NewMetType&) >
      #define NewMetFunction(address) NewMetFunctionType(std::bind(address, this, _1))
      void subscribe(const std::string& name, NewMetFunctionType handler);

      #define KillCropFunctionType std::function<void(protocol::KillCropType&)>
      #define KillCropFunction(address) KillCropFunctionType(std::bind(address, this, _1))
      void subscribe(const std::string& name, KillCropFunctionType handler);

      #define NewProfileFunctionType std::function<void(protocol::NewProfileType&)>
      #define NewProfileFunction(address) NewProfileFunctionType(std::bind(address, this, _1))
      void subscribe(const std::string& name, NewProfileFunctionType handler);

      #define ExternalMassFlowFunctionType std::function<void(protocol::ExternalMassFlowType&)>
      #define ExternalMassFlowFunction(address) ExternalMassFlowFunctionType(std::bind(address, this, _1))
      void publish(const std::string& name, protocol::ExternalMassFlowType& value);

      #define NewCanopyFunctionType std::function<void(protocol::NewCanopyType&)>
      #define NewCanopyFunction(address) NewCanopyFunctionType(std::bind(address, this, _1))
      void publish(const std::string& name, protocol::NewCanopyType& value);

      #define NewCropFunctionType std::function<void(protocol::NewCropType&)>
      #define NewCropFunction(address) NewCropFunctionType(std::bind(address, this, _1))
      void publish(const std::string& name, protocol::NewCropType& value);

      #define NewPotentialGrowthFunctionType std::function<void(protocol::NewPotentialGrowthType&)>
      #define NewPotentialGrowthFunction(address) NewPotentialGrowthFunctionType(std::bind(address, this, _1))
      void publish(const std::string& name, protocol::NewPotentialGrowthType& value);

      #define CanopyWaterBalanceFunctionType std::function<void(protocol::CanopyWaterBalanceType&)>
      #define CanopyWaterBalanceFunction(address) CanopyWaterBalanceFunctionType(std::bind(address, this, _1))
      void subscribe(const std::string& name, CanopyWaterBalanceFunctionType handler);

      #define RemoveCropBiomassFunctionType std::function<void(protocol::RemoveCropBiomassType&)>
      #define RemoveCropBiomassFunction(address) RemoveCropBiomassFunctionType(std::bind(address, this, _1))
      void subscribe(const std::string& name, RemoveCropBiomassFunctionType handler);

      #define BiomassRemovedFunctionType std::function<void(protocol::BiomassRemovedType&)>
      #define BiomassRemovedFunction(address) BiomassRemovedFunctionType(std::bind(address, this, _1))
      void publish(const std::string& name, protocol::BiomassRemovedType& value);

      #define FOMLayerFunctionType std::function<void(protocol::FOMLayerType&)>
      #define FOMLayerFunction(address) FOMLayerFunctionType(std::bind(address, this, _1))
      void publish(const std::string& name, protocol::FOMLayerType& value);

      #define AddSurfaceOMFunctionType std::function<void(protocol::AddSurfaceOMType&)>
      #define AddSurfaceOMFunction(address) AddSurfaceOMFunctionType(std::bind(address, this, _1))
      void subscribe(const std::string& name, AddSurfaceOMFunctionType handler);

      #define ApsimVariantFunctionType std::function<void(protocol::ApsimVariant&)>
      #define ApsimVariantFunction(address) ApsimVariantFunctionType(std::bind(address, this, _1))
      void publish(const std::string& name, protocol::ApsimVariant& value);
      void subscribe(const std::string& name, ApsimVariantFunctionType handler);

      #define RemovedByAnimalSetter(address) std::function<void(const protocol::RemovedByAnimalType&)>(std::bind(address, this, _1))
      void exposeWritable(const std::string& name, const std::string& units, const std::string& description, std::function<void(const protocol::RemovedByAnimalType&)> fn);

      #define NitrogenChangedFunctionType std::function<void(protocol::NitrogenChangedType&)>
      #define NitrogenChangedunction(address) NitrogenChangedFunctionType(std::bind(address, this, _1))
      void publish(const std::string& name, protocol::NitrogenChangedType& value);
      
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
