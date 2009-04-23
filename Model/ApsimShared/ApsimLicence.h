#ifndef ApsimLicence_H
#define ApsimLicence_H

#include <string>
#include <vector>

// ------------------------------------------------------------------
// Class to read and write a licence
// ------------------------------------------------------------------
class __declspec(dllexport) ApsimLicence
   {
   protected:
      std::string fileName;
      bool isValid;

      bool convertToASCII(unsigned char *binaryImage, long sizeOfBinaryImage);
      void convertToBinary(unsigned char *ASCIIImage, long sizeOfBinaryImage);

   public:
      ApsimLicence(const std::string& filename);
      void read(void) throw(std::runtime_error);
      void write(void);
      bool isLicenced(const std::string& module);
      bool isSource(const std::string& module);

      std::string getName(void);
      std::string getOrganisation(void);
      std::string getLocation(void);
      void setName(const std::string& name);
      void setOrganisation(const std::string& organisation);
      void setLocation(const std::string& location);
      void addModule(const string& moduleName, bool source, bool licenced) throw(std::runtime_error);

      std::string getDetails(void);
      std::string getAllDetails(void);
   };

// ------------------------------------------------------------------
// Return the user's apsim licence.
// ------------------------------------------------------------------
ApsimLicence getApsimKey(void) throw(std::runtime_error);
#endif

