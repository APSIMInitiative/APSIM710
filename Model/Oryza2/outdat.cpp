// Replacement outdat() and outarr() routines. These are called by oryza
// at the end of each day to store output. This implementation registers it
// by name with the APSIM infrastructure and stores the value for later when
// the infrastructre asks for it.

// This is built with the g++ bundled with gfortran. Linking MSC object files into 
// the oryza dll is just too hard. Unfortunately, that rules out using the 
// componentInterface support routines.
#include <string.h>
#include <string>
#include <map>
#include <iostream>
#include <stdexcept>
#include <General/platform.h>

using namespace std;

// C routines from fortran infrastructure 
extern "C" void EXPORT STDCALL ExposeReal(const char* Name, const char* Units, const char* Description, int* Writable, float* Data,
                                          unsigned NameLength, unsigned UnitsLength, unsigned DescriptionLength);
extern "C" void EXPORT STDCALL ExposeRealArray(const char* Name, const char* Units, const char* Description, 
                                               int* Writable, float* Data, int* NumValues, int* MaxValues,
                                               unsigned NameLength, unsigned UnitsLength, unsigned DescriptionLength);

class floatArray {
   public:
     float *value;
     int n;
     string description;
     string units;
     floatArray(const char *_description, const char *_units, int _n) {
       n = _n; value = new float [n]; clear();
       description = _description; units = _units;
     }
     ~floatArray() { delete [] value; }
     void clear(void) {for (int i = 0; i < n; i++) {value[i] = 0.0;} }
     void set(float *_value, int _n) {for (int i = 0; i < n; i++) {value[i] = _value[i];} }
};

// case-independent (ci) compare_less binary function
struct ci_less : std::binary_function<std::string, std::string, bool>
  {
    struct nocase_compare : public std::binary_function<unsigned char,unsigned char,bool> 
    {
      bool operator() (const unsigned char& c1, const unsigned char& c2) const {
          return tolower (c1) < tolower (c2); 
      }
    };
    bool operator() (const std::string & s1, const std::string & s2) const {
      return std::lexicographical_compare 
        (s1.begin (), s1.end (),   // source range
        s2.begin (), s2.end (),   // dest range
        nocase_compare ());  // comparison
    }
  };

// A class encapsulating oryza variables. Oryza sets these variables at the end of each day,
// we expose them to the system for reporting. They are registered with the system at init2.
// NB. Scalar variables are arrays of length 1.
class Variables {
   private:
     typedef std::map<string, floatArray *, ci_less> VMap;
     typedef std::pair<string, floatArray *> VPair;
     VMap myVariables;
     void add(const string &name, floatArray *variable);
   public:
     Variables();
     ~Variables();
     void set(char *, float *, int nelem);
     void clear(void);
};
										  
Variables *outVariables = NULL;
Variables::Variables() {
   myVariables.insert(VPair("DVS", new floatArray("Developmental Stage", "0-2", 1)));
   myVariables.insert(VPair("RDD", new floatArray("Daily shortwave radiation", "J/m2/d", 1)));
   myVariables.insert(VPair("TMIN", new floatArray("Daily minimum temperature ","oC", 1)));
   myVariables.insert(VPair("TMAX", new floatArray("Daily maximum temperature ","oC", 1)));
   myVariables.insert(VPair("DTR", new floatArray("Daily shortwave radiation", "J/m2/d", 1)));
   myVariables.insert(VPair("RAPCDT", new floatArray("Absorbed PAR","W/m2", 1)));
   myVariables.insert(VPair("PARCUM", new floatArray("Cumulative Absorbed PAR", "W/m2", 1)));
   myVariables.insert(VPair("NFLV", new floatArray("N fraction in the leaves", "g/N/m2", 1)));
   myVariables.insert(VPair("SLA", new floatArray("Specific Leaf Area", "ha/kg", 1)));
   myVariables.insert(VPair("SLASIM", new floatArray("Specific Leaf Area", "ha/kg", 1)));
   myVariables.insert(VPair("LESTRS", new floatArray("Leaf expansion stress factor", "", 1)));
   myVariables.insert(VPair("LRSTRS", new floatArray("Leaf rolling stress factor", "", 1)));
   myVariables.insert(VPair("LDSTRS", new floatArray("Leaf death stress factor", "", 1)));
   myVariables.insert(VPair("PCEW", new floatArray("Reduction in potential transpiration rate", "", 1)));
   myVariables.insert(VPair("NSP", new floatArray("Number of spikelets", "", 1)));
   myVariables.insert(VPair("LAI", new floatArray("Leaf area index", "m2/m2", 1)));
   myVariables.insert(VPair("WAGT", new floatArray("Weight of Tops", "kg/ha", 1)));
   myVariables.insert(VPair("WST",  new floatArray("Weight of Stems", "kg/ha", 1)));
   myVariables.insert(VPair("WLVG",  new floatArray("Weight of Green Leaves", "kg/ha", 1)));
   myVariables.insert(VPair("WLVD",  new floatArray("Weight of Dead Leaves", "kg/ha", 1)));
   myVariables.insert(VPair("WLV",   new floatArray("Weight of Green and Dead Leaves", "kg/ha", 1)));
   myVariables.insert(VPair("WSO",   new floatArray("Weight of Storage Organs", "kg/ha", 1)));
   myVariables.insert(VPair("WRR14", new floatArray("Weight rough rice with 14% moisture", "kg/ha", 1)));
   myVariables.insert(VPair("ZRT", new floatArray("Root Depth", "m", 1)));
   myVariables.insert(VPair("wrt", new floatArray("Weight of Roots", "kg/ha", 1)));
   myVariables.insert(VPair("wrr", new floatArray("Weight of Rough rice", "", 1)));
   myVariables.insert(VPair("rnstrs", new floatArray("N stress reduction factor for RGRL", "", 1)));
   myVariables.insert(VPair("ssga", new floatArray("Specific Stem green area", "", 1)));
   myVariables.insert(VPair("dvr", new floatArray("Development rate of the crop", "d", 1)));
   myVariables.insert(VPair("hu", new floatArray("Heat units", "oC/d", 1)));
   myVariables.insert(VPair("trc", new floatArray("Potential Transpiration of rice in main field", "mm", 1)));
   myVariables.insert(VPair("gcr", new floatArray("Growth rate of crop", "kg/ha", 1)));
   myVariables.insert(VPair("gnsp", new floatArray("Rate of increase in spikelet number", "n/ha", 1)));
   myVariables.insert(VPair("spgf", new floatArray("Spikelet growth factor", "n/kg", 1)));
   myVariables.insert(VPair("sf1", new floatArray("Spikelet fertility factor due to low temperatures", "", 1)));
   myVariables.insert(VPair("sf2", new floatArray("Spikelet fertility factor due to high temperatures ", "", 1)));
   myVariables.insert(VPair("spfert", new floatArray("Spikelet fertility", "", 1)));
   myVariables.insert(VPair("fso", new floatArray("Fraction of growth allocated to storage organs", "kg/ha", 1)));
   myVariables.insert(VPair("gso", new floatArray("Growth rate of storage organs", "kg/ha", 1)));
   myVariables.insert(VPair("gngr", new floatArray("Rate of increase in grain number", "no/ha", 1)));
   myVariables.insert(VPair("ggr", new floatArray("Rate of increase in grain weight", "kg/ha", 1)));
   myVariables.insert(VPair("ngr", new floatArray("Number of grains", "no/ha", 1)));
   myVariables.insert(VPair("wgrmx", new floatArray("Maximum individual grain weight", "kg/grain", 1)));
   myVariables.insert(VPair("rlv", new floatArray("Root length density", "", 1)));
   myVariables.insert(VPair("trwl", new floatArray("Transpiration by layer", "mm", 10)));
   myVariables.insert(VPair("TRW", new floatArray("Transpiration", "mm", 1)));
   myVariables.insert(VPair("ROOTM", new floatArray("Root mass", "kg/ha", 10)));
   myVariables.insert(VPair("ROOTL", new floatArray("Root length density", "cm/ha/m", 10)));
   //myVariables.insert(VPair("ROOTM1", new floatArray("Root mass layer 1", "kg/ha", 1)));
   //myVariables.insert(VPair("ROOTL1", new floatArray("Root length density layer 1", "cm/ha/m", 1)));
   //myVariables.insert(VPair("ROOTM2", new floatArray("Root mass layer 2", "kg/ha", 1)));
   //myVariables.insert(VPair("ROOTL2", new floatArray("Root length density layer 2", "cm/ha/m", 1)));
   //myVariables.insert(VPair("ROOTM3", new floatArray("Root mass layer 3", "kg/ha", 1)));
   //myVariables.insert(VPair("ROOTL3", new floatArray("Root length density layer 3", "cm/ha/m", 1)));
   //myVariables.insert(VPair("ROOTM4", new floatArray("Root mass layer 4", "kg/ha", 1)));
   //myVariables.insert(VPair("ROOTL4", new floatArray("Root length density layer 4", "cm/ha/m", 1)));
   //myVariables.insert(VPair("ROOTM5", new floatArray("Root mass layer 5", "kg/ha", 1)));
   //myVariables.insert(VPair("ROOTL5", new floatArray("Root length density layer 5", "cm/ha/m", 1)));
   //myVariables.insert(VPair("ROOTM6", new floatArray("Root mass layer 6", "kg/ha", 1)));
   //myVariables.insert(VPair("ROOTL6", new floatArray("Root length density layer 6", "cm/ha/m", 1)));
   //myVariables.insert(VPair("ROOTM7", new floatArray("Root mass layer 7", "kg/ha", 1)));
   //myVariables.insert(VPair("ROOTL7", new floatArray("Root length density layer 7", "cm/ha/m", 1)));
   //myVariables.insert(VPair("ROOTM8", new floatArray("Root mass layer 8", "kg/ha", 1)));
   //myVariables.insert(VPair("ROOTL8", new floatArray("Root length density layer 8", "cm/ha/m", 1)));
   //myVariables.insert(VPair("ROOTM9", new floatArray("Root mass layer 9", "kg/ha", 1)));
   //myVariables.insert(VPair("ROOTL9", new floatArray("Root length density layer 9", "cm/ha/m", 1)));
   myVariables.insert(VPair("nacr", new floatArray("Actual N uptake rate by the crop", "kg/ha", 1)));
   myVariables.insert(VPair("NSLLV", new floatArray("Stress factor for leaf death caused by N stress", "", 1)));
   myVariables.insert(VPair("NUPP", new floatArray("N Uptake", "kg/ha", 1)));
   myVariables.insert(VPair("ANCR", new floatArray("Amount of N in crop", "kg/ha", 1)));
   myVariables.insert(VPair("ANLV", new floatArray("Amount of N in leaves", "kg/ha", 1)));
   myVariables.insert(VPair("ANLD", new floatArray("Amount of N in dead leaves", "kg/ha", 1)));
   myVariables.insert(VPair("ANST", new floatArray("Amount of N in stems", "kg/ha", 1)));
   myVariables.insert(VPair("ANSO", new floatArray("Amount of N in Storage Organs", "kg/ha", 1)));
   myVariables.insert(VPair("NMAXL", new floatArray("Maximum N fraction in leaves on a weight basis", "kg/ha", 1)));
   myVariables.insert(VPair("FNLV", new floatArray("Fraction of N in leaves on a weight basis", "", 1)));
   myVariables.insert(VPair("SNH4", new floatArray("Supply of NH4", "kg/ha", 10)));
   myVariables.insert(VPair("SNO3", new floatArray("Supply of NO3", "kg/ha", 10)));
   myVariables.insert(VPair("SPNO3", new floatArray("Supply of NO3 limited by sminno3", "kg/ha", 10)));
   myVariables.insert(VPair("SPNH4", new floatArray("Supply of NH4 limited by sminnh4", "kg/ha", 10)));

   for (VMap::iterator i = myVariables.begin(); i != myVariables.end(); i++) 
     add(i->first, i->second);
}

Variables::~Variables() {
    for (VMap::iterator i = myVariables.begin(); i != myVariables.end(); i++)
       delete i->second;
}

void Variables::add(const string &name, floatArray *variable)
{
   int writeable = 0;
     if (variable->n > 1)
       ExposeRealArray(name.c_str(), variable->units.c_str(), variable->description.c_str(), 
                       &writeable, variable->value, &variable->n, &variable->n, 
                       name.size(), variable->units.size(), variable->description.size());
     else
       ExposeReal(name.c_str(), variable->units.c_str(), variable->description.c_str(), 
                  &writeable, variable->value, 
                  name.size(), variable->units.size(), variable->description.size());
}

void Variables::set(char *name, float *_value, int numvals)
{
   if (myVariables.find(name) == myVariables.end()) {
     cout << "Oryza2: new variable " << name << " - add it to outdat.cpp" << endl;
     myVariables.insert(VPair(name, new floatArray(" ", " ", numvals)));
   }
   myVariables[name]->set(_value, numvals);
}

void Variables::clear(void)
{
    for (VMap::iterator i = myVariables.begin(); i != myVariables.end(); i++)
      i->second->clear();
}

// These routines are called from fortran to set up, clear and close a variable mapper
extern "C" void EXPORT STDCALL oryza2createoutputs(void)
{
   if (outVariables != NULL) {
      outVariables->clear();
      delete outVariables;
   }
   outVariables = new Variables();
}
extern "C" void EXPORT STDCALL oryza2clearoutputs(void)
{
    if (outVariables != NULL)
      outVariables->clear();
}

extern "C" void EXPORT STDCALL oryza2closeoutputs(void)
{
    if (outVariables != NULL) 
      {
      delete outVariables;
      outVariables = NULL; 
      }
}

extern "C" void EXPORT STDCALL outdat(int *, int *, char *fname, float *value, unsigned int fnamelen)
{  
   if (outVariables == NULL) {throw runtime_error("outVariables is NULL");}
   char name[fnamelen + 1];
   memcpy(name, fname, fnamelen);
   name[fnamelen] = ' ';
   for (; name[fnamelen] == ' '; fnamelen--) { name[fnamelen] = '\0'; }
   outVariables->set(name, value, 1);
}

extern "C" void EXPORT STDCALL outarr(int *, int *, char *fname, float *value, int *numvalues, unsigned int fnamelen)
{
   if (outVariables == NULL) {throw runtime_error("outVariables is NULL");}
   if (*numvalues <= 0) return;
   char name[fnamelen + 1];
   memcpy(name, fname, fnamelen);
   name[fnamelen] = ' ';
   for (; name[fnamelen] == ' '; fnamelen--) { name[fnamelen] = '\0'; }

   outVariables->set(name, value, *numvalues);
}

