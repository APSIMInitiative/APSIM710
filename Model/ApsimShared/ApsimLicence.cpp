#include <general/pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ApsimLicence.h"
#include "ApsimDirectories.h"
#include <fstream>
#include <sstream>
#include <general/string_functions.h>

static const char* indent = "<IND x=\"90\">";
static const char* lf = "<BR>";

struct Key_struct
   {
   #define MAX_NUM_MODULES 200
   char name[100];
   char organisation[100];
   char applications[500];                // eg. APSFront APSGraph WinEdit ...
   char moduleList[MAX_NUM_MODULES * 9]; // eg. sorg sorgsat accum ...
   char buildType[100];                  // eg. library OR executable
   bool moduleSource[MAX_NUM_MODULES];
   bool moduleLicensed[MAX_NUM_MODULES];
   bool moduleReleased[MAX_NUM_MODULES];
   long numModules;                      // number of modules
   char location[100];                   // location string ie CARMASAT
   unsigned long checksum;
   } keyInfo;

// ------------------------------------------------------------------
// Constructor
// ------------------------------------------------------------------
ApsimLicence::ApsimLicence(const string& filename)
   : fileName(filename)
   {
   keyInfo.numModules = 0;
   strcpy(keyInfo.moduleList, "");
   strcpy(keyInfo.applications, "");
   }

// ------------------------------------------------------------------
// The binary image consists of the ASCII image with mild encryption
// applied and a 4 byte checksum appended.  The size of the ASCII
// image is taken to be 'Size_of_binary_image' - 4.  This function
// transforms all but the last 4 bytes of the binary image into an
// ASCII image.  The the last 4 bytes of the binary image are compared to a
// checksum of the resulting ASCII image.  TRUE is returned, if the
// checksum is correct, otherwise FALSE.
// ------------------------------------------------------------------
bool ApsimLicence::convertToASCII(unsigned char *binaryImage,
                                  long sizeOfBinaryImage)
   {
   const long CYC_INITL_VAL=123456L;
   const long CYC_ADD=1L;
   const long CYC_MUL=293L;
   const long CYC_MOD=7158260L;

   long cyc;
   int ch_offset;
   int ch_asc, ch_bin;
   long asc_size;
   long i_asc;
   unsigned char *csum;
   int i_csum;
   bool is_sum_ok;

   asc_size = sizeOfBinaryImage - 4;

/* Convert ascii bytes. */
   cyc = CYC_INITL_VAL;
   for (i_asc=0; i_asc<asc_size; i_asc++)
   {   ch_offset = cyc % 256;    // cyc & 255 is faster, but not in F77.
      ch_bin = binaryImage[i_asc];
      ch_asc = (ch_bin + 256 - ch_offset) % 256;
      cyc = (cyc * CYC_MUL) + CYC_ADD + ch_asc;
      cyc = cyc % CYC_MOD;
      binaryImage[i_asc] = (char) ch_asc;
   }

/* Checkout the checksum. */
/* This is done with pointers here.  To be done with an EQUIVALENCE in F77. */
   is_sum_ok = true;
   csum = (unsigned char*) &cyc;
   for (i_csum=0; i_csum<4; i_csum++)
   {   if (binaryImage[asc_size + i_csum] != csum[i_csum])
         is_sum_ok = false;
   }

   return is_sum_ok;
   }

// ------------------------------------------------------------------
// The binary image consists of the ASCII image with mild encryption
// applied and a 4 byte checksum appended.  The size of the ASCII
// image is taken to be 'Size_of_binary_image' - 4.  This function
// transforms the ASCII image 'ASCII_image' into a binary image.
// ------------------------------------------------------------------
void ApsimLicence::convertToBinary(unsigned char *ASCIIImage, long sizeOfBinaryImage)
   {
   const long CYC_INITL_VAL=123456L;
   const long CYC_ADD=1L;
   const long CYC_MUL=293L;
   const long CYC_MOD=7158260L;

   long cyc;
   int ch_offset;
   int ch_asc, ch_bin;
   long asc_size;
   long i_asc;
   unsigned char *csum;
   int i_csum;

   asc_size = sizeOfBinaryImage - 4;

/* Convert ascii bytes. */
   cyc = CYC_INITL_VAL;
   for (i_asc=0; i_asc<asc_size; i_asc++)
   {   ch_offset = cyc % 256;    // cyc & 255 is faster, but not in F77.
      ch_asc = ASCIIImage[i_asc];
      ch_bin = (ch_asc + ch_offset) % 256;
      cyc = (cyc * CYC_MUL) + CYC_ADD + ch_asc;
      cyc = cyc % CYC_MOD;
      ASCIIImage[i_asc] = (char) ch_bin;
   }

/* Write in the checksum. */
/* This is done with pointers here.  To be done with an EQUIVALENCE in F77. */
   csum = (unsigned char*) &cyc;
   for (i_csum=0; i_csum<4; i_csum++)
      ASCIIImage[asc_size + i_csum] = csum[i_csum];
   }

// ------------------------------------------------------------------
// Read the license file.  Will throw if licence is invalid.
// ------------------------------------------------------------------
void ApsimLicence::read(void) throw(runtime_error)
   {
   ifstream in(fileName.c_str(), ios::binary);
   in.read( (char*) &keyInfo, sizeof(keyInfo));
   if (!convertToASCII((unsigned char*) &keyInfo, sizeof(keyInfo)))
      throw runtime_error("Not a valid APSIM licence: " + fileName);
   }
// ------------------------------------------------------------------
// Write the license file
// ------------------------------------------------------------------
void ApsimLicence::write (void)
   {
   ofstream out(fileName.c_str(), ios::binary);
   convertToBinary((unsigned char*) &keyInfo, sizeof(keyInfo));
   out.write((char*) &keyInfo, sizeof(keyInfo));
   }
// ------------------------------------------------------------------
// Return true if the specified module is licenced in this key.
// ------------------------------------------------------------------
bool ApsimLicence::isLicenced(const std::string& module)
   {
   vector<string> modules;
   string modulesString = keyInfo.moduleList;
   Split_string (modulesString, " ", modules);
   vector<string>::iterator i = find(modules.begin(),
                                     modules.end(),
                                     module);
   if (i != modules.end())
      return keyInfo.moduleLicensed[i-modules.begin()];
   else
      return false;
   }
// ------------------------------------------------------------------
// Return true if the specified module is a source in this key.
// ------------------------------------------------------------------
bool ApsimLicence::isSource(const std::string& module)
   {
   vector<string> modules;
   string modulesString = keyInfo.moduleList;
   Split_string (modulesString, " ", modules);
   vector<string>::iterator i = find(modules.begin(),
                                     modules.end(),
                                     module);
   if (i != modules.end())
      return keyInfo.moduleSource[i-modules.begin()];
   else
      return false;
   }
// ------------------------------------------------------------------
// Return licensed APSUITE user name to caller.
// ------------------------------------------------------------------
string ApsimLicence::getName(void)
   {
   return keyInfo.name;
   }
// ------------------------------------------------------------------
// Return licensed APSUITE organisation name to caller.
// ------------------------------------------------------------------
string ApsimLicence::getOrganisation (void)
   {
   return keyInfo.organisation;
   }
// ------------------------------------------------------------------
// Return the location string to caller.
// ------------------------------------------------------------------
string ApsimLicence::getLocation(void)
   {
   return keyInfo.location;
   }
// ------------------------------------------------------------------
// Set the name part of the key
// ------------------------------------------------------------------
void ApsimLicence::setName(const string& name)
   {
   strncpy(keyInfo.name, name.c_str(), sizeof(keyInfo.name));
   }
// ------------------------------------------------------------------
// Set the organisation part of the key
// ------------------------------------------------------------------
void ApsimLicence::setOrganisation(const string& organisation)
   {
   strncpy (keyInfo.organisation, organisation.c_str(), sizeof(keyInfo.organisation));
   }
// ------------------------------------------------------------------
// Set the location in the key.
// ------------------------------------------------------------------
void ApsimLicence::setLocation(const string& location)
   {
   strncpy(keyInfo.location, location.c_str(), sizeof(keyInfo.location));
   }
// ------------------------------------------------------------------
// Add a module to the key.
// ------------------------------------------------------------------
void ApsimLicence::addModule(const string& moduleName, bool source, bool licenced) throw(runtime_error)
   {
   if (keyInfo.numModules < MAX_NUM_MODULES)
      {
      strcat(keyInfo.moduleList, " ");
      strcat(keyInfo.moduleList, moduleName.c_str());
      keyInfo.moduleSource[keyInfo.numModules] = source;
      keyInfo.moduleLicensed[keyInfo.numModules] = licenced;
      keyInfo.moduleReleased[keyInfo.numModules] = 1;
      keyInfo.numModules++;
      }
   else
      throw runtime_error("Too many modules in key.");
   }
// ------------------------------------------------------------------
// return licence details (name & organisation) as a HTML formatted string.
// ------------------------------------------------------------------
string ApsimLicence::getDetails(void)
   {
   ostringstream details;

   // write name.
   details << "<B>Name:</B>" << indent << getName() << lf;

   // write organisation.
   details << "<B>Organisation:</B>" << indent << getOrganisation() << lf;
   return details.str();
   }
// ------------------------------------------------------------------
// return all licence details as a HTML formatted string.
// ------------------------------------------------------------------
string ApsimLicence::getAllDetails(void)
   {
   ostringstream details;
   // write location
   details << "<B>Location:</B>" << indent << getLocation() << lf;

   // write modules
   details << "<B>Modules:</B>";
   vector<string> modules;
   Split_string(keyInfo.moduleList, " ", modules);
   for (vector<string>::iterator moduleI = modules.begin();
                                 moduleI != modules.end();
                                 moduleI++)
      {
      details << indent << *moduleI;
      if (isSource(*moduleI))
         details << "(source)";
      details << lf;
      }
   return getDetails() + details.str();
   }
// ------------------------------------------------------------------
// Return the user's apsim licence.
// ------------------------------------------------------------------
ApsimLicence getApsimKey(void) throw(runtime_error)
   {
   ApsimLicence key(getApsimDirectory() + "\\apsim.key");
   key.read();
   return key;
   }

