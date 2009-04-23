//---------------------------------------------------------------------------
#ifndef ApsimDataFileWriterH
#define ApsimDataFileWriterH
#include <map>
#include <vector>
#include <fstream>
#include <boost/date_time/gregorian/gregorian.hpp>
#include "ApsimDataFile.h" // to pull in Value
//---------------------------------------------------------------------------
// This class encapsulates all writing to an APSIM data file.
// A data file consists of 2 sections, a constants section and a temporal
// data section. Comments begin with an exclamation character '!'
//
// The constants section consists of lines like: keyword = value (units).
// e.g. Latitude =  -27.11
//      tav =  19.57 (oC)
// Units are optional. There is no limit to the number of constants.
// A constant can be an array of values:
// e.g. sw = 0.9 0.2 0.5 (mm/mm)
//
// The temporal data section contains fields (columns) of data for
// specific dates. Each field has a field name and units on separate lines:
// e.g. site year   day  radn   maxt   mint   rain    evap
//        ()   ()    () (MJ/m2) (oC)   (oC)   (mm)    (mm)
//      DALB 1988     1 20.74   33.0   17.4    0.2    7.41
// Each field must have a name and unit. There is no limit to the number
// or types of fields.
// Arrays are supported:
// sw(1)  sw(2)  sw(3)
//  (mm)   (mm)   (mm)
//   0.8    0.6   0.55
// Each record has a date column in the ISO 8601 extended format: YYYY-MM-DD
//---------------------------------------------------------------------------
class __declspec(dllexport) ApsimDataFileWriter
   {
   public:
      typedef std::vector<Value> Values;

      //---------------------------------------------------------------------------
      // constructor.
      //---------------------------------------------------------------------------
      ApsimDataFileWriter(void) { }
      ~ApsimDataFileWriter(void);

      //---------------------------------------------------------------------------
      // Open the data file for writing. All data is overwritten.
      // Section name should NOT have [ ] characters around it.
      //---------------------------------------------------------------------------
      void open(const std::string& fileName,
                const std::string& sectionName);

      //---------------------------------------------------------------------------
      // Close the data file for writing
      //---------------------------------------------------------------------------
      void close(void);

      //---------------------------------------------------------------------------
      // add a constant to the data file.
      //---------------------------------------------------------------------------
      void addConstant(const Value& value);

      //---------------------------------------------------------------------------
      // add a record of temporal data to the file.
      //---------------------------------------------------------------------------
      void addTemporalRecord(boost::gregorian::date& date, const Values& value);

   private:
      std::string fileName;
      Values constants;
      Values temporalData;
      bool haveWritenHeadings;

      std::ofstream out;
   };
#endif
