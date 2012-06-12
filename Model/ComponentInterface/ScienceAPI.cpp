//---------------------------------------------------------------------------
#include <stdexcept>
#include <General/string_functions.h>
#include <ApsimShared/ApsimRegistry.h>
#include "Component.h"
#include "ScienceAPI.h"


template <class T>
bool StringConverter(protocol::Component* component, const string& name,
                     const string& from, T& to, double lower, double upper)
   {
   // 1. Convert it
   try {to = boost::lexical_cast<T> (from); }
   catch(boost::bad_lexical_cast &e)
      {
      string msg = string("Problem converting variable to ") +
                   typeid(T).name() + " type.\n"
                   "Parameter name = " + name + "\n"
                   "Value          = '" + from + "'";
      throw std::runtime_error(msg);
      }

   // 2. Check bounds
   if (to < lower || to > upper)
      {
      string msg = "Bound check warning while reading parameter.\n"
                   "Variable  : " + name + "\n"
                   "Condition : " + ftoa(lower, 2) + " <= " +
                   from + " <= " + ftoa(upper, 2);
      component->error(msg, false);
      return false;
      }

   return true;
   }
template <class T>
bool StringConverter(protocol::Component* component, const string& name,
                     const string& from, vector<T>& to, double lower, double upper)
   {
   to.erase(to.begin(), to.end());

   // 1. Convert it
   std::vector <string> strings;
   splitIntoValues (from, " ", strings);
   for (unsigned i = 0; i != strings.size(); i++)
     {
     T value;
     try {value = boost::lexical_cast<T> (strings[i]); }
     catch(boost::bad_lexical_cast &e)
        {
        string msg = string("Problem converting variable to vector<") +
                     typeid(T).name() + "> type.\n"
                     "Parameter name = " + name + "\n"
                     "Value          = '" + strings[i] + "'";
        throw std::runtime_error(msg);
        }
     to.push_back(value);

     // 2. Check bounds
     if (value < lower || value > upper)
        {
        string msg = "Bound check warning while reading parameter.\n"
                     "Variable  : " + name + "(" + itoa(i+1) + ")\n"
                     "Condition : " + ftoa(lower, 2) + " <= " +
                     strings[i] + " <= " + ftoa(upper, 2);
        component->error(msg, false);
        }
     }

   return (to.size() > 0);
   }



ScienceAPI::ScienceAPI(protocol::Component* c)
   {
   component = c;
   }
ScienceAPI::~ScienceAPI()
   {
   for (unsigned i = 0; i != stuffToDelete.size(); i++)
      delete stuffToDelete[i];
   stuffToDelete.erase(stuffToDelete.begin(), stuffToDelete.end());
   }

void ScienceAPI::write(const std::string& msg)
   {
   component->writeString(msg.c_str());
   }
void ScienceAPI::warning(const std::string& msg)
   {
   component->error(msg.c_str(), false);
   }
bool ScienceAPI::read(const std::string& name, int& data, int lower, int upper)
   {
   string valueAsString;
   if (ScienceAPI::read(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::read(const std::string& name, float& data, float lower, float upper)
   {
   string valueAsString;
   if (ScienceAPI::read(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::read(const std::string& name, double& data, double lower, double upper)
   {
   string valueAsString;
   if (ScienceAPI::read(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::read(const std::string& name, std::string& data)
   {
   if (!readOptional(name, data))
      throw std::runtime_error("Cannot find a value for parameter: " + name);

   return (data != "");
   }



string ScienceAPI::readFromSection(const std::string& section, const std::string& name)
   {
   // -------------------------------------------------
   // Read the specified parameter name from the
   // specified section name.
   // -------------------------------------------------
   return component->getProperty(section, name);
   }

bool ScienceAPI::read(const std::string& name, std::vector<int>& data, int lower, int upper)
   {
   string valueAsString;
   if (ScienceAPI::read(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::read(const std::string& name, std::vector<float>& data, float lower, float upper)
   {
   string valueAsString;
   if (ScienceAPI::read(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }
bool ScienceAPI::read(const std::string& name, float data[], int& numVals, float lower, float upper)
   {
   vector<float> values;
   if (ScienceAPI::read(name, values, lower, upper))
      {
      numVals = values.size();
      for (int i = 0; i != numVals; i++)
         data[i] = values[i];
      return true;
      }
   else
      return false;
   }
bool ScienceAPI::read(const std::string& name, std::vector<double>& data, float lower, float upper)
   {
   string valueAsString;
   if (ScienceAPI::read(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::read(const std::string& name, std::vector<std::string>& data)
   {
   string valueAsString;
   if (ScienceAPI::read(name, valueAsString))
      {
      splitIntoValues (valueAsString, " ", data);
      return true;
      }
   else
      return false;
   }

bool ScienceAPI::readFiltered(const std::string& name, std::vector<std::string>& values)
   {
   if (name.find('/') != string::npos)
      {
      string value = component->findProperty(name);
      if (value != "")
         values.push_back(value);
      }
   else
      {
      if (currentClass1 != "")
         component->getMultipleProperties(currentClass1, name, values);
      if (values.size() == 0 && currentClass2 != "")
         component->getMultipleProperties(currentClass2, name, values);
      if (values.size() == 0)
         component->getMultipleProperties("parameters", name, values);
      }
   return (values.size() > 0);
   }
bool ScienceAPI::readAll(const std::string& section, std::vector<std::string>& names, std::vector<std::string> &values)
   {
   component->getProperties(section, names, values);
   return (values.size() > 0);
   }

// -------------------------------------------------------------
// Optional methods.
// -------------------------------------------------------------
bool ScienceAPI::readOptional(const std::string& name, int& data, int lower, int upper)
   {
   string valueAsString;
   if (readOptional(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::readOptional(const std::string& name, float& data, float lower, float upper)
   {
   string valueAsString;
   if (readOptional(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::readOptional(const std::string& name, double& data, double lower, double upper)
   {
   string valueAsString;
   if (readOptional(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::readOptional(const std::string& name, std::string& data)
   {
   data = "";

   if (data == "" && currentClass1 != "")
      data = readFromSection(currentClass1, name);
   if (data == "" && currentClass2 != "")
      data = readFromSection(currentClass2, name);
   if (data == "")
      data = readFromSection("parameters", name);

   // remove any Units specifier "(..)":
   splitOffBracketedValue(data, '(', ')');

   // And any whitespace...
   stripLeadingTrailing(data, " \t");

   return (data != "");
   }

bool ScienceAPI::readOptional(const std::string& name, std::vector<int>& data, int lower, int upper)
   {
   string valueAsString;
   if (readOptional(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::readOptional(const std::string& name, std::vector<float>& data, float lower, float upper)
   {
   string valueAsString;
   if (readOptional(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }
bool ScienceAPI::readOptional(const std::string& name, float data[], int& numVals, float lower, float upper)
   {
   vector<float> values;
   if (readOptional(name, values, lower, upper))
      {
      numVals = values.size();
      for (int i = 0; i != numVals; i++)
         data[i] = values[i];
      return true;
      }
   else
      return false;
   }

bool ScienceAPI::readOptional(const std::string& name, std::vector<double>& data, float lower, float upper)
   {
   string valueAsString;
   if (readOptional(name, valueAsString))
      return StringConverter(component, name, valueAsString, data, lower, upper);
   else
      return false;
   }

bool ScienceAPI::readOptional(const std::string& name, std::vector<std::string>& data)
   {
   string valueAsString;
   if (readOptional(name, valueAsString))
      {
      splitIntoValues (valueAsString, " ", data);
      return true;
      }
   else
      return false;
   }


std::string EXPORT addUnitsToDDML(const string& ddml, const string& units)
   {
   string returnString = ddml;
   unsigned pos = returnString.find("/>");
   if (units != "" && pos != string::npos)
      returnString = returnString.substr(0, pos) + " unit=\"" + units + "\"/>";
   return returnString;
   }
std::string EXPORT addDescToDDML(const string& ddml, const string& desc)
   {
   string returnString = ddml;
   unsigned pos = returnString.find("/>");
   if (desc != "" && pos != string::npos)
      returnString = returnString.substr(0, pos) + " description=\"" + desc + "\"/>";
   return returnString;
   }
// -------------------------------------------------------------
// GET methods.
// -------------------------------------------------------------
bool ScienceAPI::get(const std::string& name, const std::string& units, float& data, float lower, float upper)
   {
   if (!getOptional(name, units, data, lower, upper))
      {
      string st = "The module " + string(component->getName()) + " has asked for the value of the variable " + name;
      st += ".\nIt received no responses.";
      throw std::runtime_error(st);
      }
     return true;
   }
bool ScienceAPI::get(const std::string& name, const std::string& units, std::vector<float>& data, float lower, float upper)
   {
   if (!getOptional(name, units, data, lower, upper))
      {
      string st = "The module " + string(component->getName()) + " has asked for the value of the variable " + name;
      st += ".\nIt received no responses.";
      throw std::runtime_error(st);
      }
     return true;
   }
bool ScienceAPI::getOptional(const std::string& name, const std::string& units, float& data, float lower, float upper)
   {
   string ddml = protocol::DDML(1.0f);
   addUnitsToDDML(ddml, units);
   int destID;
   string regName;
   ApsimRegistry::getApsimRegistry().unCrackPath(component->getId(), name, destID, regName);
   unsigned id = component->addRegistration(::get, destID, regName, ddml);
   return component->getVariable(id, data, lower, upper, true);
   }
bool ScienceAPI::getOptional(const std::string& name, const std::string& units, std::vector<float>& data, float lower, float upper)
   {
   string ddml = protocol::DDML(vector<float>());
   addUnitsToDDML(ddml, units);
   int destID;
   string regName;
   ApsimRegistry::getApsimRegistry().unCrackPath(component->getId(),name, destID, regName);
   unsigned id = component->addRegistration(::get, destID, regName, ddml);
   return component->getVariable(id, data, lower, upper, true);
   }

// -------------------------------------------------------------
// Expose methods.
// -------------------------------------------------------------
// -------------------------------------------------------------------
// A wrapper class for CMP getters
// -------------------------------------------------------------------
template <class FT, class T>
class CMPGetter : public DeletableThing
   {
   private:
      FT getter;
      T dummy;
      std::string ddml;
   public:
      CMPGetter(FT& fn)
         {
         getter = fn;
         ddml = protocol::DDML(dummy);
         }
      void CMPFunction(protocol::Component* component, protocol::QueryValueData &qd)
         {
         component->sendVariable(qd, getter());
         }
      const char* DDML() {return ddml.c_str();}

   };

void ScienceAPI::expose(const std::string& name, const std::string& units, const std::string& description, float& variable)
   {
   component->addGettableVar(name.c_str(), variable, units.c_str(), description.c_str());
   }
void ScienceAPI::expose(const std::string& name, const std::string& units, const std::string& description, int& variable)
   {
   component->addGettableVar(name.c_str(), variable, units.c_str(), description.c_str());
   }
void ScienceAPI::expose(const std::string& name, const std::string& units, const std::string& description, const std::vector<float>& variable)
   {
   component->addGettableVar(name.c_str(), variable, units.c_str(), description.c_str());
   }
void ScienceAPI::expose(const std::string& name, const std::string& units, const std::string& description, string& variable)
   {
   component->addGettableVar(name.c_str(), variable, units.c_str(), description.c_str());
   }
void ScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function0<int> handler)
   {
   typedef CMPGetter<IntGetterType, int> WrapperType;
   WrapperType* wrapper = new WrapperType (handler);
   stuffToDelete.push_back(wrapper);
   boost::function2<void, protocol::Component*, protocol::QueryValueData &> fn;
   fn = boost::bind(&WrapperType::CMPFunction, wrapper, _1, _2);
   component->addGettableVar(name.c_str(), protocol::DTint4, false, fn, units.c_str(), description.c_str());
   }
void ScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function0<float> handler)
   {
   typedef CMPGetter<FloatGetterType, float> WrapperType;
   WrapperType* wrapper = new WrapperType (handler);
   stuffToDelete.push_back(wrapper);
   boost::function2<void, protocol::Component*, protocol::QueryValueData &> fn;
   fn = boost::bind(&WrapperType::CMPFunction, wrapper, _1, _2);
   component->addGettableVar(name.c_str(), protocol::DTsingle, false, fn, units.c_str(), description.c_str());
   }
void ScienceAPI::exposeFunction(const std::string& name, const std::string& units, const std::string& description, boost::function0<std::string> handler)
   {
   typedef CMPGetter<StringGetterType, std::string> WrapperType;
   WrapperType* wrapper = new WrapperType (handler);
   stuffToDelete.push_back(wrapper);
   boost::function2<void, protocol::Component*, protocol::QueryValueData &> fn;
   fn = boost::bind(&WrapperType::CMPFunction, wrapper, _1, _2);
   component->addGettableVar(name.c_str(), protocol::DTstring, false, fn, units.c_str(), description.c_str());
   }


// -------------------------------------------------------------
// SET methods.
// -------------------------------------------------------------
void ScienceAPI::set(const std::string& name, const std::string& units, std::vector<float>& data)
   {
   string ddml = protocol::DDML(std::vector<float>());
   addUnitsToDDML(ddml, units);
   int destID;
   string regName;
   ApsimRegistry::getApsimRegistry().unCrackPath(component->getId(), name, destID, regName);
   unsigned id = component->addRegistration(::set, destID, regName, ddml);
   bool ok =  component->setVariable(id, data);
   if (!ok)
      throw std::runtime_error("Cannot set the value of variable: " + name);
   }


// -------------------------------------------------------------------
// A wrapper class for CMP setters
// -------------------------------------------------------------------
template <class FT, class T>
class CMPSetter : public DeletableThing
   {
   private:
      FT setter;
      T dummy;
      std::string name;
      std::string ddml;
      protocol::DataTypeCode typeCode;
   public:
      CMPSetter(FT& fn, const string& n, protocol::DataTypeCode dt)
         {
         name = n;
         setter = fn;
         typeCode = dt;
         ddml = protocol::DDML(dummy);
         }
      bool CMPFunction(protocol::Component* component, protocol::QuerySetValueData &v)
         {
         protocol::TypeConverter* converter = NULL;
         getTypeConverter(name.c_str(),
                              v.variant.getType().getCode(),
                              typeCode,
                              v.variant.getType().isArray(),
                              false,
                               converter);
         v.variant.unpack(converter, NULL, dummy);
         setter(dummy);
         if (converter) delete converter;
         return true;
         }
      const char* DDML() {return ddml.c_str();}

   };

// -------------------------------------------------------------
// Exposing a SETtable variable
// -------------------------------------------------------------
void ScienceAPI::exposeWritable(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, int> handler)
   {
   typedef CMPSetter<boost::function1<void, int>, int> WrapperType;
   WrapperType* wrapper = new WrapperType (handler, name, protocol::DTint4);
   stuffToDelete.push_back(wrapper);
   boost::function2<bool, protocol::Component*, protocol::QuerySetValueData &> fn;
   fn = boost::bind(&WrapperType::CMPFunction, wrapper, _1, _2);

   component->addSettableVar(name.c_str(), protocol::DTint4, wrapper->DDML(), fn, units.c_str(), description.c_str());
   }
void ScienceAPI::exposeWritable(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, float> handler)
   {
   typedef CMPSetter<boost::function1<void, float>, float> WrapperType;
   WrapperType* wrapper = new WrapperType (handler, name, protocol::DTsingle);
   stuffToDelete.push_back(wrapper);
   boost::function2<bool, protocol::Component*, protocol::QuerySetValueData &> fn;
   fn = boost::bind(&WrapperType::CMPFunction, wrapper, _1, _2);

   component->addSettableVar(name.c_str(), protocol::DTsingle, wrapper->DDML(), fn, units.c_str(), description.c_str());
   }
void ScienceAPI::exposeWritable(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, const std::string&> handler)
   {
   typedef CMPSetter<boost::function1<void, const std::string&>, string> WrapperType;
   WrapperType* wrapper = new WrapperType (handler, name, protocol::DTstring);
   stuffToDelete.push_back(wrapper);
   boost::function2<bool, protocol::Component*, protocol::QuerySetValueData &> fn;
   fn = boost::bind(&WrapperType::CMPFunction, wrapper, _1, _2);

   component->addSettableVar(name.c_str(), protocol::DTstring, wrapper->DDML(), fn, units.c_str(), description.c_str());
   }
void ScienceAPI::exposeWritable(const std::string& name, const std::string& units, const std::string& description, boost::function1<void, const protocol::RemovedByAnimalType&> handler)
   {
   typedef CMPSetter<boost::function1<void, const protocol::RemovedByAnimalType&>, protocol::RemovedByAnimalType> WrapperType;
   WrapperType* wrapper = new WrapperType (handler, name, protocol::DTunknown);
   stuffToDelete.push_back(wrapper);
   boost::function2<bool, protocol::Component*, protocol::QuerySetValueData &> fn;
   fn = boost::bind(&WrapperType::CMPFunction, wrapper, _1, _2);

   component->addSettableVar(name.c_str(), protocol::DTunknown, wrapper->DDML(), fn, units.c_str(), description.c_str());
   }

// -------------------------------------------------------------------
// A wrapper class for CMP events, gets and sets that take a single
// data item as an arguemnt.
// -------------------------------------------------------------------
template <class FT, class T>
class CMPMethod1 : public DeletableThing
   {
   private:
      FT setter;
      T dummy;
      std::string ddml;
   public:
      CMPMethod1(FT& fn)
         {
         setter = fn;
         ddml = protocol::DDML(dummy);
         }
      void invoke(unsigned &, unsigned &, protocol::Variant& variant)
         {
         if (variant.isApsimVariant())
            {
            protocol::ApsimVariant ApsimVar(variant);
            unpack(ApsimVar, dummy);
            }
         else
            variant.unpack(NULL, NULL,  dummy);
         setter(dummy);
         }
      const char* DDML() {return ddml.c_str();}

   };

// -------------------------------------------------------------------
// A wrapper class for CMP events that take a simple built in type 
// e.g. float as an arguemnt.
// -------------------------------------------------------------------
template <class FT, class T>
class CMPMethodBuiltIn : public DeletableThing
   {
   private:
      FT setter;
      T dummy;
      std::string ddml;
   public:
      CMPMethodBuiltIn(FT& fn)
         {
         setter = fn;
         ddml = protocol::DDML(dummy);
         }
      void invoke(unsigned &, unsigned &, protocol::Variant& variant)
         {
         variant.unpack(NULL, NULL,  dummy);
         setter(dummy);
         }
      const char* DDML() {return ddml.c_str();}

   };

// -------------------------------------------------------------------
// A wrapper class for CMP events, gets and sets that take a single
// data item as an arguemnt.
// -------------------------------------------------------------------
template <class FT>
class CMPMethod0 : public DeletableThing
   {
   private:
      FT setter;
   public:
      CMPMethod0(FT& fn)
         {
         setter = fn;
         }
      void invoke(unsigned &, unsigned &, protocol::Variant& variant)
         {
         setter();
         }
      const char* DDML() {return nullTypeDDML;}

   };

// -------------------------------------------------------------------
// A wrapper class for CMP events, gets and sets that take a single
// data item as an arguemnt.
// -------------------------------------------------------------------
template <class FT>
class CMPMethod0WithName : public DeletableThing
   {
   private:
      FT setter;
      std::string name;
   public:
      CMPMethod0WithName(FT& fn, const std::string& n)
         {
         setter = fn;
         name = n;
         }
      void invoke(unsigned &, unsigned &, protocol::Variant& variant)
         {
         setter(name);
         }
      const char* DDML() {return nullTypeDDML;}

   };

// -------------------------------------------------------------------
// A wrapper class for ApsimVariants
// -------------------------------------------------------------------
class ApsimVariantWrapper : public DeletableThing
   {
   private:
      ApsimVariantFunctionType setter;
      protocol::Component* comp;
   public:
      ApsimVariantWrapper(ApsimVariantFunctionType& fn, protocol::Component* component)
         {
         setter = fn;
         comp = component;
         }
      void invoke(unsigned &, unsigned &, protocol::Variant& variant)
         {
         protocol::ApsimVariant ApsimVar(variant);
         setter(ApsimVar);
         }
      const char* DDML()
         {
         protocol::ApsimVariantType dummy;
         return protocol::DDML(dummy).c_str();
         }

   };

// -------------------------------------------------------------
// Event handlers.
// -------------------------------------------------------------
void ScienceAPI::subscribe(const std::string& name, NullFunctionType handler)
   {
   typedef CMPMethod0<NullFunctionType> WrapperType;
   WrapperType* wrapper = new WrapperType (handler);
   stuffToDelete.push_back(wrapper);

   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;
   fn = boost::bind(&WrapperType::invoke, wrapper, _1, _2, _3);
   component->addEvent(name.c_str(),
                       fn, wrapper->DDML());
   }
void ScienceAPI::subscribe(const std::string& name, NullFunctionWithNameType handler)
   {
   typedef CMPMethod0WithName<NullFunctionWithNameType> WrapperType;
   WrapperType* wrapper = new WrapperType (handler, name);
   stuffToDelete.push_back(wrapper);

   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;
   fn = boost::bind(&WrapperType::invoke, wrapper, _1, _2, _3);
   component->addEvent(name.c_str(),
                       fn, wrapper->DDML());
   }
void ScienceAPI::subscribe(const std::string& name, FloatFunctionType handler)
   {
   typedef CMPMethodBuiltIn<FloatFunctionType, float> WrapperType;
   WrapperType* wrapper = new WrapperType (handler);
   stuffToDelete.push_back(wrapper);

   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;
   fn = boost::bind(&WrapperType::invoke, wrapper, _1, _2, _3);
   component->addEvent(name.c_str(),
                       fn, wrapper->DDML());
   }
void ScienceAPI::subscribe(const std::string& name, SowFunctionType handler)
   {
   typedef CMPMethod1<SowFunctionType, protocol::SowType> WrapperType;
   WrapperType* wrapper = new WrapperType (handler);
   stuffToDelete.push_back(wrapper);

   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;
   fn = boost::bind(&WrapperType::invoke, wrapper, _1, _2, _3);
   component->addEvent(name.c_str(),
                       fn, wrapper->DDML());
   }
void ScienceAPI::subscribe(const std::string& name, HarvestFunctionType handler)
   {
   typedef CMPMethod1<HarvestFunctionType, protocol::HarvestType> WrapperType;
   WrapperType* wrapper = new WrapperType (handler);
   stuffToDelete.push_back(wrapper);

   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;
   fn = boost::bind(&WrapperType::invoke, wrapper, _1, _2, _3);
   component->addEvent(name.c_str(),
                       fn, wrapper->DDML());
   }
void ScienceAPI::subscribe(const std::string& name, KillStemFunctionType handler)
   {
   typedef CMPMethod1<KillStemFunctionType, protocol::KillStemType> WrapperType;
   WrapperType* wrapper = new WrapperType (handler);
   stuffToDelete.push_back(wrapper);

   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;
   fn = boost::bind(&WrapperType::invoke, wrapper, _1, _2, _3);
   component->addEvent(name.c_str(),
                       fn, wrapper->DDML());
   }
void ScienceAPI::subscribe(const std::string& name, TimeFunctionType handler)
   {
   typedef CMPMethod1<TimeFunctionType, protocol::TimeType> WrapperType;
   WrapperType* wrapper = new WrapperType (handler);
   stuffToDelete.push_back(wrapper);

   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;
   fn = boost::bind(&WrapperType::invoke, wrapper, _1, _2, _3);
   component->addEvent(name.c_str(),
                       fn, wrapper->DDML());
   }

void ScienceAPI::subscribe(const std::string& name, NewMetFunctionType handler)
   {
   typedef CMPMethod1<NewMetFunctionType, protocol::NewMetType> WrapperType;
   WrapperType* wrapper = new WrapperType (handler);
   stuffToDelete.push_back(wrapper);

   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;
   fn = boost::bind(&WrapperType::invoke, wrapper, _1, _2, _3);
   component->addEvent(name.c_str(),
                       fn, wrapper->DDML());
   }

void ScienceAPI::subscribe(const std::string& name, KillCropFunctionType handler)
   {
   typedef CMPMethod1<KillCropFunctionType, protocol::KillCropType> WrapperType;
   WrapperType* wrapper = new WrapperType (handler);
   stuffToDelete.push_back(wrapper);

   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;
   fn = boost::bind(&WrapperType::invoke, wrapper, _1, _2, _3);
   component->addEvent(name.c_str(),
                       fn, wrapper->DDML());
   }

void ScienceAPI::subscribe(const std::string& name, NewProfileFunctionType handler)
   {
   typedef CMPMethod1<NewProfileFunctionType, protocol::NewProfileType> WrapperType;
   WrapperType* wrapper = new WrapperType (handler);
   stuffToDelete.push_back(wrapper);

   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;
   fn = boost::bind(&WrapperType::invoke, wrapper, _1, _2, _3);
   component->addEvent(name.c_str(),
                       fn, wrapper->DDML());
   }

void ScienceAPI::subscribe(const std::string& name, CanopyWaterBalanceFunctionType handler)
   {
   typedef CMPMethod1<CanopyWaterBalanceFunctionType, protocol::CanopyWaterBalanceType> WrapperType;
   WrapperType* wrapper = new WrapperType (handler);
   stuffToDelete.push_back(wrapper);

   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;
   fn = boost::bind(&WrapperType::invoke, wrapper, _1, _2, _3);
   component->addEvent(name.c_str(),
                       fn, wrapper->DDML());
   }
void ScienceAPI::subscribe(const std::string& name, RemoveCropBiomassFunctionType handler)
   {
   typedef CMPMethod1<RemoveCropBiomassFunctionType, protocol::RemoveCropBiomassType> WrapperType;
   WrapperType* wrapper = new WrapperType (handler);
   stuffToDelete.push_back(wrapper);

   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;
   fn = boost::bind(&WrapperType::invoke, wrapper, _1, _2, _3);
   component->addEvent(name.c_str(), fn, wrapper->DDML());
   }
void ScienceAPI::subscribe(const std::string& name, ApsimVariantFunctionType handler)
   {
   ApsimVariantWrapper* wrapper = new ApsimVariantWrapper(handler, component);
   stuffToDelete.push_back(wrapper);

   boost::function3<void, unsigned &, unsigned &, protocol::Variant &> fn;
   fn = boost::bind(&ApsimVariantWrapper::invoke, wrapper, _1, _2, _3);
   protocol::ApsimVariantType dummy;
   string ddml = protocol::DDML(dummy);
   component->addEvent(name.c_str(),
                       fn, ddml.c_str());
   }


// -------------------------------------------------------------
// Publish methods
// -------------------------------------------------------------
void ScienceAPI::publish(const std::string& name)
   {
   string ddml = nullTypeDDML;
   int destID;
   string regName;
   ApsimRegistry::getApsimRegistry().unCrackPath(component->getId(), name, destID, regName);
   unsigned id = component->addRegistration(::event, destID, regName, ddml);
   int dummy;
   component->publish(id, dummy);
   }
void ScienceAPI::publish(const std::string& name, protocol::ExternalMassFlowType& value)
   {
   string ddml = protocol::DDML(value);
   int destID;
   string regName;
   ApsimRegistry::getApsimRegistry().unCrackPath(component->getId(), name, destID, regName);
   unsigned id = component->addRegistration(::event, destID, regName, ddml);
   component->publish(id, value);
   }
void ScienceAPI::publish(const std::string& name, protocol::NewCanopyType& value)
   {
   string ddml = protocol::DDML(value);
   int destID;
   string regName;
   ApsimRegistry::getApsimRegistry().unCrackPath(component->getId(), name, destID, regName);
   unsigned id = component->addRegistration(::event, destID, regName, ddml, "");
   component->publish(id, value);
   }
void ScienceAPI::publish(const std::string& name, protocol::NewCropType& value)
   {
   string ddml = protocol::DDML(value);
   int destID;
   string regName;
   ApsimRegistry::getApsimRegistry().unCrackPath(component->getId(), name, destID, regName);
   unsigned id = component->addRegistration(::event, destID, regName, ddml, "");
   component->publish(id, value);
   }
void ScienceAPI::publish(const std::string& name, protocol::NewPotentialGrowthType& value)
   {
   string ddml = protocol::DDML(value);
   int destID;
   string regName;
   ApsimRegistry::getApsimRegistry().unCrackPath(component->getId(), name, destID, regName);
   unsigned id = component->addRegistration(::event, destID, regName, ddml, "");
   component->publish(id, value);
   }
void ScienceAPI::publish(const std::string& name, protocol::BiomassRemovedType& value)
   {
   string ddml = protocol::DDML(value);
   int destID;
   string regName;
   ApsimRegistry::getApsimRegistry().unCrackPath(component->getId(), name, destID, regName);
   unsigned id = component->addRegistration(::event, destID, regName, ddml, "");
   component->publish(id, value);
   }
void ScienceAPI::publish(const std::string& name, protocol::FOMLayerType& value)
   {
   string ddml = protocol::DDML(value);
   int destID;
   string regName;
   ApsimRegistry::getApsimRegistry().unCrackPath(component->getId(), name, destID, regName);
   unsigned id = component->addRegistration(::event, destID, regName, ddml, "");
   component->publish(id, value);
   }
void ScienceAPI::publish(const std::string& name, protocol::ApsimVariant& value)
   {
   string ddml = protocol::DDML(value);
   int destID;
   string regName;
   ApsimRegistry::getApsimRegistry().unCrackPath(component->getId(), name, destID, regName);
   unsigned id = component->addRegistration(::event, destID, regName, ddml, "");
   component->publish(id, value);
   }
