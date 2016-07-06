#include <sstream>

#include <General/math_functions.h>
#include <General/stl_functions.h>
#include <General/string_functions.h>
#include <General/date_class.h>
#include <General/path.h>
#include <General/StringTokenizer.h>
#include <ApsimShared/ApsimVersion.h>
#include <ApsimShared/ApsimSettings.h>
#include <ComponentInterface2/ScienceAPI2.h>

#include "ReportComponent.h"
#ifdef __WIN32__
#include "Windows.h"
#endif
using namespace std;

// ------------------------------------------------------------------
// Field constructor
// ------------------------------------------------------------------
Field::Field (ScienceAPI2& scienceAPI,
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

   //fix for VS2010 which no longer null terminates strings. JF 3/5/11
   if (this->units.empty())
	   this->units = "()";

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
   if (this->units == "()" && this->format != "" && !Is_numerical(this->format.c_str()))
      {
      this->units = "(" + this->format + ")";
      this->unitsToOutput = "(" + this->format + ")";
      }
   upperCaseFormat = format;
   To_upper(upperCaseFormat);
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
      size_t fieldWidth = max((size_t)15, heading.length() + 1);
      fieldWidth = max(fieldWidth, values[i].length() + 1);
      fieldWidth = max(fieldWidth, units.length() + 1);
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
            d.Set_write_format(upperCaseFormat.c_str());
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
void Field::writeValueTo(ostream& out, const std::string& value, size_t fieldWidth)
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
      size_t width;
      if (i >= widths.size())
         width = widths[0];
      else
         width = widths[i];
      writeValueTo(out, values[i], width);
      }
   }

// ------------------------------------------------------------------
// Create an instance of the REPORT module
// ------------------------------------------------------------------
extern "C" EXPORT ReportComponent* STDCALL createComponent(ScienceAPI2& scienceAPI)
   {
   return new ReportComponent(scienceAPI);
   }
extern "C" void  EXPORT STDCALL deleteComponent(ReportComponent* component)
   {
   delete component;
   }
// ------------------------------------------------------------------
// initialise the REPORT component.
// ------------------------------------------------------------------
ReportComponent::ReportComponent(ScienceAPI2& scienceapi)
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
   if (scienceAPI.name().length() < (79-24))
   {
	   cout.width(79-24-scienceAPI.name().length());
	   cout.fill('-');
	   cout << '-' << endl;
	   cout.fill(' ');
   }

   // find a filename.
   // Workaround. A readParameter call will strip out any titles with () in them - it thinks they
   // are units strings to be discarded. Read the raw string..
   std::vector<string> scratch; 
   string fileName;
   scienceAPI.readFiltered("outputfile",  scratch);
   if (scratch.size() > 0) fileName = scratch[0]; else throw runtime_error("No output file specified");

   scratch.clear();
   scienceAPI.readFiltered("title",  scratch);
   if (scratch.size() > 0) title = scratch[0];

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
      if (Str_i_Eq(keyword, "format"))
         format = tokens[currTok++];
      else if (Str_i_Eq(keyword, "units"))
         units = tokens[currTok++];
      else if (Str_i_Eq(keyword, "as"))
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
   string versionString = getApsimVersion();
   std::string IncludeBuildNumber;
   ApsimSettings settings;
   settings.read("apsimui|IncludeBuildNumberInOutSumFile", IncludeBuildNumber, false);
#ifdef __WIN32__
   TCHAR buffer[MAX_COMPUTERNAME_LENGTH + 1];
   unsigned long size;
   if (!GetComputerName(buffer, &size) || !_strnicmp(buffer, "bob", 4))
       IncludeBuildNumber = "No";
#endif
   if (IncludeBuildNumber == "Yes")
       versionString += " r" + getApsimBuildNumber();
   file << "ApsimVersion = " << versionString << endl;

   // write out all constants
   vector<string> names, values;
   XMLDocument* simScript = new XMLDocument(scienceAPI.getInitData(), XMLDocument::xmlContents);
   XMLNode::iterator initData = find_if(simScript->documentElement().begin(),
                                        simScript->documentElement().end(),
                                        EqualToName<XMLNode>("initdata"));
   for (XMLNode::iterator i = initData->begin();
                          i != initData->end();
                          i++)
      {
      if (i->getName() == "constant")
         {
         names.push_back(i->getAttribute("name"));
         values.push_back(i->getValue());
         }
      }
   for (unsigned i = 0; i != names.size(); i++)
      file << names[i] << " = " << values[i] << endl;

   file << "Title = " << title << endl;

   // output headings and units
   file << headingLine.str() << endl;
   file << unitLine.str() << endl;
   }

