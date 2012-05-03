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

void nameToDescription (const char * name, char ** desc, char ** units) {
   *desc = "<undefined>"; *units = "";
   if (strcasecmp(name, "DVS") == 0) { *desc = "Developmental Stage"; *units = "0-2"; }
   else if (strcasecmp(name, "RDD") == 0) { *desc = "Daily shortwave radiation"; *units = "J/m2/d"; }
   else if (strcasecmp(name, "TMIN") == 0) { *desc = "Daily minimum temperature "; *units = "oC"; }
   else if (strcasecmp(name, "TMAX") == 0) { *desc = "Daily maximum temperature "; *units = "oC"; }
   else if (strcasecmp(name, "DTR") == 0) { *desc = "Daily shortwave radiation"; *units = "J/m2/d"; }
   else if (strcasecmp(name, "RAPCDT") == 0) { *desc = "Absorbed PAR"; *units = "W/m2"; }
   else if (strcasecmp(name, "PARCUM   ") == 0) { *desc = "Cumulative Absorbed PAR"; *units = "W/m2"; }
   else if (strcasecmp(name, "NFLV") == 0) { *desc = "N fraction in the leaves"; *units = "g/N/m2"; }
   else if (strcasecmp(name, "SLA") == 0) { *desc = "Specific Leaf Area"; *units = "ha/kg"; }
   else if (strcasecmp(name, "SLASIM") == 0) { *desc = "Specific Leaf Area"; *units = "ha/kg"; }
   else if (strcasecmp(name, "LESTRS") == 0) { *desc = "Leaf expansion stress factor"; *units = ""; }
   else if (strcasecmp(name, "LRSTRS") == 0) { *desc = "Leaf rolling stress factor"; *units = ""; }
   else if (strcasecmp(name, "LDSTRS") == 0) { *desc = "Leaf death stress factor"; *units = ""; }
   else if (strcasecmp(name, "PCEW") == 0) { *desc = "Reduction in potential transpiration rate"; *units = ""; }
   else if (strcasecmp(name, "NSP") == 0) { *desc = "Number of spikelets"; *units = ""; }
   else if (strcasecmp(name, "LAI") == 0) { *desc = "Leaf area index"; *units = "m2/m2"; }
   else if (strcasecmp(name, "WAGT") == 0) { *desc = "Weight of Tops"; *units = "kg/ha"; }
   else if (strcasecmp(name, "WST") == 0) { *desc = "Weight of Stems"; *units = "kg/ha"; }
   else if (strcasecmp(name, "WLVG") == 0) { *desc = "Weight of Green Leaves"; *units = "kg/ha"; }
   else if (strcasecmp(name, "WLVD") == 0) { *desc = "Weight of Dead Leaves"; *units = "kg/ha"; }
   else if (strcasecmp(name, "WLV") == 0) { *desc = "Weight of Green and Dead Leaves"; *units = "kg/ha"; }
   else if (strcasecmp(name, "WSO   ") == 0) { *desc = "Weight of Storage Organs"; *units = "kg/ha"; }
   else if (strcasecmp(name, "WRR14 ") == 0) { *desc = "Weight rough rice with 14% moisture"; *units = "kg/ha"; }
   else if (strcasecmp(name, "ZRT") == 0) { *desc = "Root Depth"; *units = "m"; }
   else if (strcasecmp(name, "wrt") == 0) { *desc = "Weight of Roots"; *units = "kg/ha"; }
   else if (strcasecmp(name, "wrr") == 0) { *desc = "Weight of Rough rice"; *units = ""; }
   else if (strcasecmp(name, "rnstrs") == 0) { *desc = "N stress reduction factor for RGRL"; *units = ""; }
   else if (strcasecmp(name, "ssga") == 0) { *desc = "Specific Stem green area"; *units = ""; }
   else if (strcasecmp(name, "dvr") == 0) { *desc = "Development rate of the crop"; *units = "d"; }
   else if (strcasecmp(name, "hu") == 0) { *desc = "Heat units"; *units = "oC/d"; }
   else if (strcasecmp(name, "trc") == 0) { *desc = "Potential Transpiration of rice in main field"; *units = "mm"; }
   else if (strcasecmp(name, "gcr") == 0) { *desc = "Growth rate of crop"; *units = "kg/ha"; }
   else if (strcasecmp(name, "gnsp") == 0) { *desc = "Rate of increase in spikelet number"; *units = "n/ha/d"; }
   else if (strcasecmp(name, "spgf") == 0) { *desc = "Spikelet growth factor"; *units = "n/kg"; }
   else if (strcasecmp(name, "sf1") == 0) { *desc = "Spikelet fertility factor due to low temperatures"; *units = ""; }
   else if (strcasecmp(name, "sf2") == 0) { *desc = "Spikelet fertility factor due to high temperatures "; *units = ""; }
   else if (strcasecmp(name, "spfert") == 0) { *desc = "Spikelet fertility"; *units = ""; }
   else if (strcasecmp(name, "fso") == 0) { *desc = "Fraction of growth allocated to storage organs"; *units = "kg/ha"; }
   else if (strcasecmp(name, "gso") == 0) { *desc = "Growth rate of storage organs"; *units = "kg/ha/d"; }
   else if (strcasecmp(name, "gngr") == 0) { *desc = "Rate of increase in grain number"; *units = "no/ha/d"; }
   else if (strcasecmp(name, "ggr") == 0) { *desc = ""; *units = ""; }
   else if (strcasecmp(name, "ngr") == 0) { *desc = ""; *units = ""; }
   else if (strcasecmp(name, "wgrmx") == 0) { *desc = "Maximum individual grain weight"; *units = "kg/grain"; }
   else if (strcasecmp(name, "rlv") == 0) { *desc = "Root length density"; *units = ""; }
   else if (strcasecmp(name, "trwl") == 0) { *desc = "Transpiration by layer"; *units = "mm"; }

}

class floatArray {
   public:
     float *value;
     int n;
     floatArray(int _n) {n = _n; value = new float [n]; clear();}
     ~floatArray() { delete [] value; }
     void clear(void) {for (int i = 0; i < n; i++) {value[i] = 0.0;} }
};

// A class encapsulating oryza variables. Oryza sets these variables at the end of each day,
// we expose them to the system for reporting.
class Variables {
   private:
     typedef std::map<string, float*> FMap;
     typedef std::map<string, floatArray *> VMap;
     typedef std::pair<string, float*> FPair;
     typedef std::pair<string, floatArray *> VPair;
     FMap myVariables;
     VMap myArrays;
     float * addFloat(char *name);
     floatArray * addFloatArray(char * name, int);
   public:
     Variables() {};
     ~Variables();
     void set(char *, float);
     void setArray(char *, float *, int nelem);
     void clear(void);
};

										  
Variables *outVariables = NULL;
Variables::~Variables() {
    for (FMap::iterator i = myVariables.begin(); i != myVariables.end(); i++)
       delete (*i).second;
    for (VMap::iterator i = myArrays.begin(); i != myArrays.end(); i++)
       delete (*i).second;
}

float * Variables::addFloat(char *name)
{
   float *v = new float;
   *v = 0.0;
   int writeable = 0;
   char * desc; char *units;
   nameToDescription (name, &desc, &units);
   ExposeReal(name, units, desc, &writeable, v, strlen(name), strlen(units), strlen(desc));
   return v;
}

void Variables::set(char *name, float _value)
{
   if (myVariables.find(name) == myVariables.end()) {
     myVariables.insert(FPair(name, addFloat(name)));
   }
   *(myVariables[name]) = _value;
}

floatArray * Variables::addFloatArray(char *name, int numvals)
{
   floatArray *v = new floatArray(numvals);
   int writeable = 0;
   char * desc; char *units;
   nameToDescription (name, &desc, &units);
   if (numvals <= 0)  {throw runtime_error(string(name) + " cannot have zero length");}

   ExposeRealArray(name, units, desc, &writeable, v->value, &v->n, &v->n, strlen(name), strlen(units), strlen(desc));
   return v;
}
void Variables::setArray(char *name, float * _value, int numvals)
{
   if (myArrays.find(name) == myArrays.end()) {
     myArrays.insert(VPair(name, addFloatArray(name, numvals)));
   }
   floatArray *arr = myArrays[name];
   if (arr->n != numvals)  {throw runtime_error(string(name) + " array size has changed");}
   for (int i = 0; i < numvals; i++) arr->value[i] = _value[i];
}

void Variables::clear(void)
{
    for (FMap::iterator i = myVariables.begin(); i != myVariables.end(); i++)
      *((*i).second) = 0.0;
    for (VMap::iterator i = myArrays.begin(); i != myArrays.end(); i++)
      (*i).second->clear();
}

// These routines are called from fortran to set up, clear and close a variable mapper
extern "C" void EXPORT STDCALL oryza2createoutputs(void)
{
   if (outVariables != NULL) {throw std::runtime_error("createoutputs called twice");}
   outVariables = new Variables();
}
extern "C" void EXPORT STDCALL oryza2clearoutputs(void)
{
    if (outVariables == NULL) {throw runtime_error("outVariables is NULL");}
    outVariables->clear();
}

extern "C" void EXPORT STDCALL oryza2closeoutputs(void)
{
    if (outVariables == NULL) {throw runtime_error("outVariables already deleted");}
    delete outVariables;
    outVariables = NULL;
}

extern "C" void EXPORT STDCALL outdat(int *, int *, char *fname, float *value, unsigned int fnamelen)
{  
   if (outVariables == NULL) {throw runtime_error("outVariables is NULL");}
   char name[fnamelen + 1];
   memcpy(name, fname, fnamelen);
   name[fnamelen] = ' ';
   for (; name[fnamelen] == ' '; fnamelen--) { name[fnamelen] = '\0'; }
   outVariables->set(name, *value);
   //cout << "   else if (strcasecmp(name, \"" <<name <<"\") == 0) { *desc = \"\"; *units = \"\"; }"<<endl;
}

extern "C" void EXPORT STDCALL outarr(int *, int *, char *fname, float *value, int *numvalues, unsigned int fnamelen)
{
   if (*numvalues <= 0) return;
   char name[fnamelen + 1];
   memcpy(name, fname, fnamelen);
   name[fnamelen] = ' ';
   for (; name[fnamelen] == ' '; fnamelen--) { name[fnamelen] = '\0'; }

   outVariables->setArray(name, value, *numvalues);
   //cout << "outarr:" << name<< " = " << *numvalues << endl;
}

