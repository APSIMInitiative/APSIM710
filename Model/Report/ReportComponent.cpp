#pragma hdrstop
#include <sstream>

#include <ApsimShared/ApsimVersion.h>
#include <ComponentInterface2/ScienceAPI.h>
#include <General/math_functions.h>
#include <General/stl_functions.h>
#include <General/string_functions.h>
#include <General/date_class.h>
#include <General/path.h>
#include <General/StringTokenizer.h>

#include "ReportComponent.h"
using namespace std;

// ------------------------------------------------------------------
// Field constructor
// ------------------------------------------------------------------
Field::Field (ScienceAPI& scienceAPI,
              const std::string& fqn,
              const std::string& ddml,
              const std::string& alias,
              const std::string& nastring,
              const std::string& format,
              bool csv,
              const std::string& unitsToOutput)
   : scienceAPI(&scienceAPI)
   {
   this->fqn = fqn;
   this->units = getAttributeFromXML(ddml, "unit");
   this->kind =  getAttributeFromXML(ddml, "kind");
   this->alias = alias;
   this->nastring = nastring;
   this->format = format;
   this->csv = csv;
   this->unitsToOutput = unitsToOutput;

   if (this->units[0] != '(')
      this->units = "(" + this->units + ")";

   if (this->unitsToOutput == "")
      this->unitsToOutput = this->units;

   if (this->unitsToOutput != "" && this->unitsToOutput[0] != '(')
      this->unitsToOutput = "(" + this->unitsToOutput + ")";

   if (this->format == "")
      {
      if (this->unitsToOutput == "(g/m^2)")
         this->format = "2";
      else if (this->unitsToOutput == "(kg/ha)")
         this->format = "1";
      else
         this->format = "3";
      }
   }

// ------------------------------------------------------------------
// write this field's heading to the specified output stream.
// ------------------------------------------------------------------
void Field::writeHeadings(ostream& out)
   {
   getValues();

   for (unsigned i = 0; i != values.size(); i++)
      {
      if (i > 0 && csv)
         out << ',';

      // Work out a heading.
      string heading;
      if (alias == "")
         {
         if (fqn.find('.') != string::npos)
            heading = fqn.substr(fqn.find('.')+1);
         else
            heading = fqn;
         }
      else
         heading = alias;

      // If we have more than 1 value then we have an array. Add array index.
      if (values.size() > 1)
         heading += "(" + itoa(i+1) + ")";

      // Now calculate a field width.
      int fieldWidth = max((unsigned)15, heading.length() + 1);
      fieldWidth = max((unsigned)fieldWidth, values[i].length() + 1);
      fieldWidth = max((unsigned)fieldWidth, units.length() + 1);
      widths.push_back(fieldWidth);

      writeValueTo(out, heading, fieldWidth);
      }
   }
// ------------------------------------------------------------------
// write this field's units to the specified output stream.
// ------------------------------------------------------------------
void Field::writeUnits(ostream& out)
   {
   for (unsigned i = 0; i != values.size(); i++)
      {
      if (i > 0 && csv)
         out << ',';
      writeValueTo(out, unitsToOutput,  widths[i]);
      }
   }


// ------------------------------------------------------------------
// Go get a variable from system.
// ------------------------------------------------------------------
void Field::getValues()
   {
   values.erase(values.begin(), values.end());
   try
      {
      scienceAPI->get(fqn, "", true, values);
      }
   catch (const std::exception& err)
		{
      string msg = "Error getting value from system.\nVariable=";
      msg += fqn;
      msg += "\n";
      msg += err.what();
		throw std::runtime_error(msg);
		}

   if (values.size() == 0)
      values.push_back(nastring);
   else
      {
      applyUnitConversion();
      formatValues();
      }
   }

// ------------------------------------------------------------------
// Format the values according to the format string. Format string
// can be a date format (e.g. dd/mm/yyyy or a precision number)
// ------------------------------------------------------------------
void Field::formatValues(void)
   {
   if (format != "")
      {
      for (unsigned i = 0; i != values.size(); i++)
         {
         if (Is_numerical(format.c_str()))
            {
            if (kind != "integer4")
               {
               int precision = atoi(format.c_str());
               char* endptr;
               double value = strtod(values[i].c_str(), &endptr);
               if (*endptr == '\0')
                  values[i] = ftoa(value, precision);
               }
            }
         else
            {
            GDate d;
            d.Set(atoi(values[i].c_str()));
            d.Set_write_format(format.c_str());
            d.Write(values[i]);
            }
         }
      }
   }

// ------------------------------------------------------------------
// Apply a simple unit conversion to all values if necessary
// ------------------------------------------------------------------
void Field::applyUnitConversion(void)
   {
   if (unitsToOutput != units)
      {
      for (unsigned i = 0; i != values.size(); i++)
         {
         if (Is_numerical(values[i].c_str()))
            {
            char* endptr;
            double value = strtod(values[i].c_str(), &endptr);
            if (*endptr == '\0')
               {
               bool error = false;
               if (unitsToOutput == "(g/m^2)")
                  {
                  if (units == "(kg/ha)")
                     value /= 10;
                  else if (units == "(t/ha)")
                     value *= 100;
                  else
                     error = true;
                  }
               else if (unitsToOutput == "(kg/ha)")
                  {
                  if (units == "(g/m^2)")
                     value *= 10;
                  else if (units == "(t/ha)")
                     value *= 1000;
                  else
                     error = true;
                  }
               else if (unitsToOutput == "(t/ha)")
                  {
                  if (units == "(g/m^2)")
                     value /= 100;
                  else if (units == "(kg/ha)")
                     value /= 1000;
                  else
                     error = true;
                  }
               else
                  error = true;

               if (error)
                  throw runtime_error("Invalid unit conversion from " + units + " to " + unitsToOutput
                        + "\nCan only convert between (g/m^2), (kg/ha) and (t/ha)"
                        + "\nError occurred while outputting variable " + fqn);
               values[i] = ftoa(value, 5);
               }
            }
         }
      }
   }
// ------------------------------------------------------------------
// write this field to summary file
// ------------------------------------------------------------------
void Field::writeValueTo(ostream& out, const std::string& value, unsigned fieldWidth)
   {
   if (csv)
      out << value;
   else
      {
      out.width(fieldWidth);
      if (value.length() >= fieldWidth)
         out << value.substr(0, fieldWidth-1);
      else
         out << value;
      }
   }

// ------------------------------------------------------------------
// write this field to specified output stream.
// ------------------------------------------------------------------
void Field::writeValue(ostream& out)
   {
   getValues();
   for (unsigned i = 0; i != values.size(); i++)
      {
      if (csv && i > 0)
         out << ',';
      unsigned width;
      if (i >= widths.size())
         width = widths[0];
      else
         width = widths[i];
      writeValueTo(out, values[i], width);
      }
   }

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// Create an instance of the REPORT module
// ------------------------------------------------------------------
extern "C" ReportComponent* STDCALL EXPORT createComponent(ScienceAPI& scienceAPI)
   {
   return new ReportComponent(scienceAPI);
   }
extern "C" void STDCALL EXPORT deleteComponent(ReportComponent* component)
   {
   delete component;
   }
// ------------------------------------------------------------------
// initialise the REPORT component.
// ------------------------------------------------------------------
ReportComponent::ReportComponent(ScienceAPI& scienceapi)
   : scienceAPI(scienceapi)
   {
   outputOnThisDay = false;
   scienceAPI.subscribe("init1", nullFunction(&ReportComponent::onInit1));
   csv = false;
   precision = 1000;
   }

// ------------------------------------------------------------------
// initialise the REPORT component - STAGE 1.
// ------------------------------------------------------------------
void ReportComponent::onInit1()
   {
   scienceAPI.subscribe("init2", nullFunction(&ReportComponent::onInit2));
   scienceAPI.subscribe("report", nullFunction(&ReportComponent::onReport));
   scienceAPI.subscribe("do_output", nullFunction(&ReportComponent::onDoOutput));
   scienceAPI.subscribe("do_end_day_output", nullFunction(&ReportComponent::onDoEndDayOutput));
   scienceAPI.expose("days_since_last_report", "day", "The number of days since the last output",
                     false, daysSinceLastReport);
   }

// ------------------------------------------------------------------
// initialise the REPORT component - STAGE 2.
// ------------------------------------------------------------------
void ReportComponent::onInit2(void)
   {
   cout << endl;
   cout << "------- " << scienceAPI.name() << " Initialisation ";
   cout.width(79-24-scienceAPI.name().length());
   cout.fill('-');
   cout << '-' << endl;
   cout.fill(' ');

   // work out a filename.
   string fileName;
   if (!scienceAPI.read("outputfile", "", true, fileName))
      fileName = calcFileName();

   file.open(fileName.c_str());
   if (!file)
      throw runtime_error("Cannot open output file (sharing violation?): " + fileName);

   // get output frequencies
   std::vector<string> frequencies;
   if (scienceAPI.read("outputfrequency", "", true, frequencies))
      {
      if (frequencies.size() > 0)
         cout << "     Output frequency:" << endl;
      for (unsigned f = 0; f != frequencies.size(); f++)
         {
         stripLeadingTrailing(frequencies[f], " ");
         string name = "        " + frequencies[f];
         cout << name << endl;
         scienceAPI.subscribe(frequencies[f], nullFunction(&ReportComponent::onFrequency));
         }
      }

   if (!scienceAPI.read("NAString", "", true, nastring))
      nastring = "?";

   // get format specifier.
   string csvString;
   if (scienceAPI.read("format", "", true, csvString))
      csv = Str_i_Eq(csvString, "csv");

   // get precision.
   if (!scienceAPI.read("precision", "", true, precision, 0, 15))
      {
      if (csv)
         precision = 6;
      }

   // enumerate through all output variables
   // and create a field for each.
   cout << "     Output variables:" << endl;
   scienceAPI.readFiltered("variable", variableLines);
   for (unsigned i = 0; i != variableLines.size(); i++)
      cout << "        " + variableLines[i] << endl;

   cout << endl;

   daysSinceLastReport = 1;

   // write out all initial conditions.
   string msg = "     Output file = " + fileName;
   cout << msg << endl;
   msg = "     Format = ";
   if (csv)
      msg += "csv";
   else
      msg += "normal";
   cout << msg << endl;
   haveWrittenHeadings = false;
   }

// ------------------------------------------------------------------
// parese the variable line and create a variable object.
// ------------------------------------------------------------------
void ReportComponent::createVariable(const string& name)
   {
   vector<string> tokens;
   SplitStringHonouringQuotes(name, " ", tokens);

   string keyword, variable, format, alias, units;
   unsigned currTok = 0;
   variable = tokens[currTok++];
   stripLeadingTrailing(variable, "\" ");
   while (currTok < tokens.size())
      {
      keyword = tokens[currTok++];
      if (keyword == "format")
         {
         format = tokens[currTok++];
         To_upper(format);
         }
      else if (keyword == "units")
         units = tokens[currTok++];
      else if (keyword == "as")
         alias = tokens[currTok++];
      }

   // find out who owns what out there in the simulation
   vector<QueryMatch> matches;
   if (variable.find('.') == string::npos)
      scienceAPI.query("*." + variable, matches);
   else
      scienceAPI.query(variable, matches);

   if (format == "" && precision != 1000)
      format = itoa(precision);

   if (matches.size() == 0)
      {
      fields.push_back(Field(scienceAPI,
                             variable,
                             "",
                             alias,
                             nastring, format, csv, units));
      }
   else
      {
      for (unsigned i = 0; i != matches.size(); i++)
         {
         string thisAlias = alias;
         if (alias == "" && matches.size() > 1)
            thisAlias = matches[i].name;
         string matchName = matches[i].name;
         string matchDdml = matches[i].ddml;
         // The match returned from the system will be lower case -
         // try to keep it "pretty" as the user described
         fields.push_back(Field(scienceAPI,
                                (matches.size() > 1) ? matchName : variable,
                                matchDdml,
                                thisAlias,
                                nastring, format, csv, units));
         }
      }
   }

// ------------------------------------------------------------------
// Calculate a file name based on simulation title and PM name.
// ------------------------------------------------------------------
string ReportComponent::calcFileName()
   {
   string FQName;

   scienceAPI.get("title", "", true, title);
   string fileName = title;

   FQName = scienceAPI.FQName();   // eg. ".masterpm.paddock.outputfile"
   string parent = FQName.substr(0,FQName.rfind('.'));
   parent = parent.substr(1+parent.rfind('.'));

   if (!Str_i_Eq(parent, "paddock") && !Str_i_Eq(parent, "masterpm"))
      {
      if (fileName != "")
         fileName += " ";
      fileName += parent;
      }

   if (!Str_i_Eq(scienceAPI.name(), "outputfile"))
      {
      if (fileName != "")
         fileName += " ";
      fileName += scienceAPI.name();
      }
   fileName += ".out";
   title = Path(fileName).Get_name();

   return fileName;
   }

// ------------------------------------------------------------------
// Handle the report event.
// ------------------------------------------------------------------
void ReportComponent::onReport()
   {
   daysSinceLastReport++;

   if (outputOnThisDay)
      writeLineOfOutput();
   outputOnThisDay = false;
   }

// ------------------------------------------------------------------
// Handle one of the frequency events.
// ------------------------------------------------------------------
void ReportComponent::onFrequency()
   {
   writeLineOfOutput();
   }

// ------------------------------------------------------------------
// Handle the doOutput event.
// ------------------------------------------------------------------
void ReportComponent::onDoOutput()
   {
   writeLineOfOutput();
   }

// ------------------------------------------------------------------
// Handle the doEndDayOutput event.
// ------------------------------------------------------------------
void ReportComponent::onDoEndDayOutput()
   {
   outputOnThisDay = true;
   }

// ------------------------------------------------------------------
// write a line of output to output file.
// ------------------------------------------------------------------
void ReportComponent::writeLineOfOutput(void)
   {
   if (!haveWrittenHeadings)
      {
      for (unsigned i = 0; i != variableLines.size(); i++)
         createVariable(variableLines[i]);

      haveWrittenHeadings = true;
      writeHeadings();
      }

   for (vector<Field>::iterator f = fields.begin();
                                f != fields.end();
                                f++)
      {
      if (csv && f != fields.begin())
         file << ',';
      (*f).writeValue(file);
      }

   file << endl;

   daysSinceLastReport = 0;
   scienceAPI.publish("reported");
   }

// ------------------------------------------------------------------
// write out headings and units.
// ------------------------------------------------------------------
void ReportComponent::writeHeadings(void)
   {
   // write out headings and units.
   ostringstream headingLine;
   ostringstream unitLine;
   for (vector<Field>::iterator f = fields.begin();
                                f != fields.end();
                                f++)
      {
      if (csv && f != fields.begin())
         {
         headingLine << ',';
         unitLine << ',';
         }
      f->writeHeadings(headingLine);
      f->writeUnits(unitLine);
      }

   // output header
   file << "ApsimVersion = " << getApsimVersion() << endl;

   // write out all constants
   vector<string> names, values;
   scienceAPI.readAll(names, values);
   bool ConstantsFound = false;
   for (unsigned i = 0; i != names.size(); i++)
      {
      if (!Str_i_Eq(names[i], "variable") &&
          !Str_i_Eq(names[i], "outputfrequency") &&
          !Str_i_Eq(names[i], "outputfile") &&
          !Str_i_Eq(names[i], "nastring") &&
          !Str_i_Eq(names[i], "format") &&
          !Str_i_Eq(names[i], "precision"))
         {
         ConstantsFound = true;
         file << names[i] << " = " << values[i] << endl;
         }
      }

   // output title if no other constants found.
   if (!ConstantsFound)
      {
      file << "Title = " << title << endl;
      }

   // output headings and units
   file << headingLine.str() << endl;
   file << unitLine.str() << endl;
   }

