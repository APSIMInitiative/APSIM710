//---------------------------------------------------------------------------
#include <General/pch.h>
#pragma hdrstop

#include "ApsimDataFileWriter.h"
#include <iomanip>
#include <boost/lexical_cast.hpp>
using namespace std;
using namespace boost;
using namespace boost::gregorian;
//---------------------------------------------------------------------------
// destructor
//---------------------------------------------------------------------------
ApsimDataFileWriter::~ApsimDataFileWriter(void)
   {
   close();
   }
//---------------------------------------------------------------------------
// Open the data file for writing. All data is overwritten.
// Section name should NOT have [ ] characters around it.
//---------------------------------------------------------------------------
void ApsimDataFileWriter::open(const std::string& file,
                               const std::string& sectionName)
   {
   fileName = file;
   out.open(fileName.c_str());
   out << "[" << sectionName << "]" << endl;;
   haveWritenHeadings = false;
   }
//---------------------------------------------------------------------------
// Close the data file for writing.
//---------------------------------------------------------------------------
void ApsimDataFileWriter::close(void)
   {
   if (out.is_open())
      {
      if (!haveWritenHeadings)
         {
         out << "date" << endl;
         out << "  ()" << endl;
         }

      out.close();
      }
   }
//---------------------------------------------------------------------------
// add a constant to the data file.
//---------------------------------------------------------------------------
void ApsimDataFileWriter::addConstant(const Value& value)
   {
   out << value.name << " = ";
   copy(value.values.begin(), value.values.end(),
        ostream_iterator<string>(out, " "));
   out << value.units;
   if (value.comment != "")
      out << "   ! " << value.comment;
   out << endl;
   }
//---------------------------------------------------------------------------
// for_each functor for writing a line to a stream.
//---------------------------------------------------------------------------
struct WriteLine
   {
   ostream& out;
   WriteLine(ostream& o) : out(o) { }
   void operator() (const string& st)
      {
      out << setw(15) << st;
      }
   };
//---------------------------------------------------------------------------
// add a record of temporal data to the file.
//---------------------------------------------------------------------------
void ApsimDataFileWriter::addTemporalRecord(boost::gregorian::date& recordDate,
                                            const Values& fields)
   {
   WriteLine writeLine(out);
   if (!haveWritenHeadings)
      {
      vector<string> headings, units;
      headings.push_back("date");
         
      units.push_back("()");
      for (Values::const_iterator field = fields.begin();
                                  field != fields.end();
                                  field++)
         {
         if (field->values.size() == 1)
            {
            headings.push_back(field->name);
            units.push_back(field->units);
            }
         else
            {
            for (unsigned array = 0; array != field->values.size(); array++)
               {
               headings.push_back(field->name + "(" + lexical_cast<string>(array) + ")");
               units.push_back(field->units);
               }
            }
         }
      for_each(headings.begin(), headings.end(),  writeLine);
      out << endl;
      for_each(units.begin(), units.end(),  writeLine);
      out << endl;
      haveWritenHeadings = true;
      }
   vector<string> values;

   string dateValue = to_iso_extended_string(recordDate);

   values.push_back(dateValue);
   for (Values::const_iterator field = fields.begin();
                               field != fields.end();
                               field++)
      {
      copy(field->values.begin(), field->values.end(),
           back_inserter(values));
      }
   for_each(values.begin(), values.end(),  writeLine);
   out << endl;
   }
