//---------------------------------------------------------------------------
#ifndef ApsimDataFileH
#define ApsimDataFileH
#include <vector>
#include <General/platform.h>
#include <General/string_functions.h>
#include <boost/date_time/gregorian/gregorian.hpp>
#include <fstream>
//---------------------------------------------------------------------------
// This class a single 'value' whether it be a constant or a value for
// a specific date. NB A 'value' can have multiple values ie an array.
//---------------------------------------------------------------------------
struct Value
   {
   std::string name;
   std::string units;
   std::vector<std::string> values;
   std::string comment;
   Value(void) { }
   Value(const std::string& n, const std::string& u,
         const std::vector<std::string>& v, const std::string& c)
      : name(n), units(u), values(v), comment(c) { }
   Value(const std::string& n, const std::string& u,
         const std::string& v, const std::string& c)
      : name(n), units(u), comment(c) {values.push_back(v);}
   std::string getName(void) {return name;}
   std::string getValue(void) {return values[0];}
   bool operator== (const std::string& rhsName)
     {return (Str_i_Eq(name, rhsName));}
   };

//---------------------------------------------------------------------------
// This class encapsulates all reading from an APSIM data file.
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
// Each record must have a date. The date can be represented as either:
//     year, month and day columns
//     year and day columns where day is day of year
//     date column in the ISO 8601 extended format: YYYY-MM-DD
//---------------------------------------------------------------------------
class EXPORT ApsimDataFile
   {
   public:
      typedef std::vector<Value> Values;
      typedef Values::iterator iterator;

      //---------------------------------------------------------------------------
      // constructor.
      //---------------------------------------------------------------------------
      ApsimDataFile(void) { }
      ~ApsimDataFile(void) {close();}

      //---------------------------------------------------------------------------
      // Open the data file.
      //---------------------------------------------------------------------------
      void open(const std::string& fileName);
      void close(void);

      //---------------------------------------------------------------------------
      // constant iterators
      //---------------------------------------------------------------------------
      iterator constantsBegin() {return constants.begin();}
      iterator constantsEnd()   {return constants.end();}

      //---------------------------------------------------------------------------
      // temporal iterators
      //---------------------------------------------------------------------------
      iterator fieldsBegin() {return temporalData.begin();}
      iterator fieldsEnd()   {return temporalData.end();}

      //---------------------------------------------------------------------------
      // return the date on the current record.
      //---------------------------------------------------------------------------
      boost::gregorian::date getDate(void) ;

      //---------------------------------------------------------------------------
      // advance the met file to a specified date. Throws is can't find date.
      //---------------------------------------------------------------------------
      void gotoDate(boost::gregorian::date dateToFind);

      // position at first record.
      bool first(void);

      // advance to the next record.
      bool next(void);

      // advance to the last record.
      bool last(void);

      // return true if at end of file.
      bool eof(void);

   private:
      std::string fileName;
      Values constants;
      Values temporalData;
      std::vector<unsigned> columnIndexes;

      std::ifstream in;
      unsigned firstRecordPos;
      iterator yearI;
      iterator monthI;
      iterator dayI;
      iterator dateI;
      iterator domI;
      bool haveFoundDate;
      bool endOfFile;

      std::istream& getline(std::string& line);

      void readAndStoreFields(const std::string& filename) throw(std::runtime_error);
      void readAndStoreRecords(const std::string& filename) throw (std::runtime_error);
      void readApsimHeader() throw(std::runtime_error);
      bool readNextRecord() throw(std::runtime_error);
      void lookForDateField(void) ;
      void standardiseUnits(std::vector<std::string>& words);

   };
#endif
