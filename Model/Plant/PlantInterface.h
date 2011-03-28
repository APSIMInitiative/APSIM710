//---------------------------------------------------------------------------
#ifndef PlantIfaceH
#define PlantIfaceH

// Maximum size_of of tables
#define max_table 30

////////////////////////
// array size settings
// Maximum number of layers in soil
#define max_layer 100

// Forward definitions..
namespace protocol {
  class Component;
  struct QuerySetValueData;
  struct ApsimGetQueryData;
};
class Environment;
class Phenology;
class Co2Modifier;
class CompositePart;
class Phenology;
class RootBase;
class Arbitrator;
class Population;
class Fixation;
class Leaf;
class Stem;

class pheno_stress_t {
   public:
    float swdef;
    float nfact;
    float swdef_flower;
    float swdef_grainfill;
    float remove_biom_pheno;
};

// An abstract plant interface, as seen from protocol::Component (outside)
class IPlant {
 public:
   virtual ~IPlant() {};
   virtual void onInit1() = 0;
   virtual void onInit2() = 0;
};

//      crop status names
typedef enum {out, dead, alive} status_t;

// Abstact plant interface, as seen from plant things (inside)
class plantInterface {
   public:
      virtual ~plantInterface() {};

      virtual Phenology& phenology() = 0;
      virtual Environment& environment() = 0;
      virtual RootBase& root() = 0;
      virtual Arbitrator& arbitrator() = 0;
      virtual Population& population() = 0;
      virtual Fixation& fixation() = 0;
      virtual Leaf& leaf() = 0;
      virtual Stem& stem() = 0;
      virtual CompositePart& All() = 0;
      virtual CompositePart& Tops() = 0;
      virtual CompositePart& Grain() = 0;

      virtual float getPeswSeed(void) = 0;
      virtual float getFaswSeed(void) = 0;
      virtual float getLeafNo (void) = 0;           // Leaf number (leaves/m^2)
      virtual std::string Name() = 0;

      virtual float getTempStressPhoto(void) = 0;
      virtual float getNfactPhoto(void) = 0;
      virtual float getNfactGrainConc(void) = 0;
      virtual float getOxdefPhoto(void) = 0;
      virtual float getPfactPhoto(void) = 0;
      virtual float getSwdefPhoto(void) = 0;
      virtual float getCumSwdefPheno(void) = 0;
      virtual float getCumSwdefPhoto(void) = 0;
      virtual bool phosphorusAware(void) = 0;       // Whether P is present in system
      virtual bool removeBiomassReport(void) = 0;
      virtual void doPlantEvent(const string& oldStageName, const string& newStageName, bool phenologyRewound) = 0;      // Something is asking the plant to do something
      virtual status_t Status() = 0;
      virtual void SetStatus(status_t NewStatus) = 0;

      virtual const Co2Modifier *getCo2Modifier(void) = 0;
      virtual const string & getCropType(void) = 0;
      virtual protocol::Component *getComponent(void) = 0;
      virtual float getSwDefPheno() = 0;
      virtual float getNFactPheno() = 0;
      virtual float getPFactPheno() = 0;

};

// Something that plugs into a plant
class plantThing {
   protected:
      ScienceAPI& scienceAPI;
      std::string name;
   public:
     plantThing(ScienceAPI& api, const std::string& nam)
       : scienceAPI(api), name(nam) {};
     virtual ~plantThing() {};
     std::string getName() const {return name;}
     virtual void onInit1(protocol::Component *) {};
     virtual void readConstants (protocol::Component *, const string &) {};
     virtual void readSpeciesParameters (protocol::Component *, vector<string> &) {};
     virtual void readCultivarParameters (protocol::Component *, const string &) {};
     virtual void read() { }
     virtual void onPlantEvent(const string &) {};
     virtual void update(void) {};
     virtual void onClassChange() {};

     virtual void zeroAllGlobals(void) {};
     virtual void zeroDeltas(void) {};
};



#define setupGetFunction(s,name,type,length,address,units,desc) {\
   boost::function2<void, protocol::Component *, protocol::QueryValueData &> fn;\
   fn = boost::bind(address, this, _1, _2); \
   s->addGettableVar(name, type, length, fn, units, desc);\
   }

#endif
