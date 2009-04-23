#include <fstream>
#include <stdexcept>

#include <math.h>
#include <boost/date_time/gregorian/gregorian.hpp>

#include <general/string_functions.h>
#include <general/date_class.h>
#include <general/platform.h>

#include <ApsimShared/FStringExt.h>
#include <ApsimShared/ApsimDataFile.h>
#include <ApsimShared/ApsimSettings.h>

#include <ComponentInterface/Component.h>


#include "StringVariant.h"
#include "InputComponent.h"


using namespace std;
using namespace boost;
using namespace boost::gregorian;

static const char* dayLengthType =
   "<type name=\"daylength\" kind=\"single\" unit=\"hours\"/>";
static const char* vpType =
   "<type kind=\"single\"/>";
static const char* startDateType =
   "<type name=\"startDate\" kind=\"integer4\" unit=\"julian days\"/>";
static const char* endDateType =
   "<type name=\"endDate\" kind=\"integer4\" unit=\"julian days\"/>";
static const char* hasDataTodayTypeDDML =
   "<type name=\"hasDataToday\" kind=\"boolean\"/>";

namespace protocol {
  std::string EXPORT DDML(const std::vector<NewMetType>&)
     {
     return (string(
         "<type name=\"newmet\" array=\"T\">"
         "   <field name=\"today\" kind=\"double\"/>"
         "   <field name=\"radn\" kind=\"single\"/>"
         "   <field name=\"maxt\" kind=\"single\"/>"
         "   <field name=\"mint\" kind=\"single\"/>"
         "   <field name=\"rain\" kind=\"single\"/>"
         "   <field name=\"vp\" kind=\"single\"/>"
         "</type>"));
     }
}
// ------------------------------------------------------------------
//  Short description:
//     Return a blank string when requested to indicate that we
//     don't need a wrapper DLL.

//  Notes:

//  Changes:
//    DPH 7/6/2001

// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL wrapperDLL(char* wrapperDll)
   {
   strcpy(wrapperDll, "");
   }
extern "C" void STDCALL getDescriptionInternal(char* initScript,
                                                 char* description);
// ------------------------------------------------------------------
// Return component description info.
// ------------------------------------------------------------------
extern "C" EXPORT void STDCALL getDescription(char* initScript, char* description)
   {
   getDescriptionInternal(initScript, description);
   }
// ------------------------------------------------------------------
// createComponent
// ------------------------------------------------------------------
protocol::Component* createComponent(void)
   {
   return new InputComponent;
   }

// ------------------------------------------------------------------
// constructor
// ------------------------------------------------------------------
InputComponent::InputComponent(void)
   : todaysDate(pos_infin), fileDate(pos_infin),
     startDate(pos_infin), endDate(pos_infin)
   {
   }

// ------------------------------------------------------------------
// initialise the REPORT component.
// ------------------------------------------------------------------
InputComponent::~InputComponent(void)
   {
   }
// ------------------------------------------------------------------
// INIT1 method handler.
// ------------------------------------------------------------------
void InputComponent::doInit1(const protocol::Init1Data& init1Data)
   {
   try
      {
      protocol::Component::doInit1(init1Data);

      static const char* getDataDDML = "<type kind=\"string\" array=\"T\"/>";
      static const char* stringDDML = "<type kind=\"string\"/>";

      // register a few things.
      tickID = addRegistration(::respondToEvent, -1, "tick", DDML(protocol::TimeType()).c_str());
      preNewmetID = addRegistration(::event, -1, "preNewmet", DDML(protocol::NewMetType()).c_str());
      newmetID = addRegistration(::event, -1, "newmet", DDML(protocol::NewMetType()).c_str());
      hasDataTodayID = addRegistration(::respondToGet, -1, "hasDataToday", hasDataTodayTypeDDML);
      getDataMethodID = addRegistration(::respondToEvent, -1, "getData", getDataDDML);
      haveReadTodaysDataID = addRegistration(::event, -1, "HaveReadTodaysData", nullTypeDDML);

      iAmMet = (Str_i_Cmp(getName(), "met") == 0);
      if (iAmMet)
         {
         daylengthID = addRegistration(::respondToGet, -1, "day_length", dayLengthType);
         vpID = addRegistration(::respondToGet, -1, "vp", vpType);
         }
      else
         daylengthID = 0;

      string dateName = getName();
      dateName += "_start_date";
      startDateID = addRegistration(::respondToGet, -1, dateName.c_str(), startDateType);
      dateName = getName();
      dateName += "_end_date";
      endDateID = addRegistration(::respondToGet, -1, dateName.c_str(), endDateType);

      dateName = string(getName()) + "_start_date_string";
      startDateStringID = addRegistration(::respondToGet, -1, dateName.c_str(), stringDDML);
      dateName = string(getName()) + "_end_date_string";
      endDateStringID = addRegistration(::respondToGet, -1, dateName.c_str(), stringDDML);

      openInputFile();
      registerAllVariables();
      checkForSparseData();
      }
   catch (const runtime_error& err)
      {
      error(string(err.what()), true);
      }
   }
// ------------------------------------------------------------------
// Open the input file associtated with this module.
// ------------------------------------------------------------------
void InputComponent::openInputFile(void)
   {
   fileName = componentData->getProperty("parameters", "filename");
   if (fileName == "")
      throw "Cannot find a filename parameter for module: " + string(getName());

   data.open(fileName);
   }
// ------------------------------------------------------------------
// INIT 2 - temporary
// ------------------------------------------------------------------
void InputComponent::doInit2(void)
   {
   if (allowSparseData)
      writeString("Sparse data is allowed");
   else
      writeString("Sparse data is not allowed");
   string msg = "INPUT File name: " + fileName;
#ifdef __WIN32__
         // Convert unix style paths to native DOS format
         Replace_all(msg, "/", "\\");
#endif
   ApsimSettings::addMacro(msg);
   writeString(msg.c_str());
   }
// ------------------------------------------------------------------
// add a variable to our list and register it.
// ------------------------------------------------------------------
void InputComponent::addVariable(Value& value)
   {
   if (!Str_i_Eq(value.name, "year")  &&
       !Str_i_Eq(value.name, "day")   &&
       !Str_i_Eq(value.name, "month") &&
       !Str_i_Eq(value.name, "date"))
      {
      Variables::iterator i = findVariable(value.name);
      if (i != variables.end())
         i->second.setTemporalValue(&value);
      else
         {
         StringVariant variable(&value, this);
         variables.insert(make_pair(variable.doRegistration(), variable));
         }
      }
   }
// ------------------------------------------------------------------
// read in all constants from file.
// ------------------------------------------------------------------
void InputComponent::registerAllVariables(void)
   {
   for_each(data.constantsBegin(), data.constantsEnd(),
            bind(&InputComponent::addVariable, this, _1));

   for_each(data.fieldsBegin(), data.fieldsEnd(),
            bind(&InputComponent::addVariable, this, _1));
   }
// ------------------------------------------------------------------
// Check to see if we need to handle sparse data or not.
// ------------------------------------------------------------------
void InputComponent::checkForSparseData(void)
   {
   ApsimDataFile::iterator i = find(data.constantsBegin(),
                                    data.constantsEnd(),
                                    "allow_sparse_data");
   allowSparseData = (i != data.constantsEnd()
                      && i->values.size() == 1
                      && (Str_i_Eq(i->values[0], "true") || Str_i_Eq(i->values[0], "yes")));
   }
// ------------------------------------------------------------------
// Advance the file to todays date. Returns the date the file is
// positioned at.
// ------------------------------------------------------------------
date InputComponent::advanceToTodaysData(void)
   {
   try
      {
      while (!data.eof() && data.getDate() < todaysDate)
         data.next();
      if (data.eof())
         return date(pos_infin);
      else
         return data.getDate();
      }
   catch (const exception& err)
      {
      string msg = err.what();
      msg +=". This error occurred while trying to read from input file " + fileName;
      error(msg, true);
      return date(pos_infin);
      }
   }
// ------------------------------------------------------------------
// return a variable to caller.
// ------------------------------------------------------------------
void InputComponent::respondToGet(unsigned int& fromID, protocol::QueryValueData& queryData)
   {
   if (queryData.ID == daylengthID)
      sendVariable(queryData, calcDayLength());
   else if (queryData.ID == vpID)
      sendVariable(queryData, calcVP(getVariableValue("mint")));
   else if (queryData.ID == startDateID)
      {
      getStartEndDate();
      sendVariable(queryData, (int) startDate.julian_day());
      }

   else if (queryData.ID == endDateID)
      {
      getStartEndDate();
      sendVariable(queryData, (int) endDate.julian_day());
      }

   else if (queryData.ID == startDateStringID)
      {
      getStartEndDate();
      ostringstream out;
      out << startDate.day() << '/' << startDate.month() << '/' << startDate.year();
      string st = out.str();
      sendVariable(queryData, st);
      }

   else if (queryData.ID == endDateStringID)
      {
      getStartEndDate();
      ostringstream out;
      out << endDate.day() << '/' << endDate.month() << '/' << endDate.year();
      string st = out.str();
      sendVariable(queryData, st);
      }

   else if (queryData.ID == hasDataTodayID)
      {
      sendVariable(queryData, (todaysDate == fileDate));
      }

   else
      variables[queryData.ID].sendVariable(queryData, (todaysDate == fileDate));
   }
// ------------------------------------------------------------------
// set the value of one of our variables.
// ------------------------------------------------------------------
bool InputComponent::respondToSet(unsigned int& fromID, protocol::QuerySetValueData& setValueData)
   {
   variables[setValueData.ID].setVariable(setValueData);
   return true;
   }
// ------------------------------------------------------------------
// Event handler.
// ------------------------------------------------------------------
void InputComponent::respondToEvent(unsigned int& fromID, unsigned int& eventID, protocol::Variant& variant)
   {
   if (eventID == tickID)
      {
      protocol::TimeType tick;
      variant.unpack(tick);
      todaysDate = date(tick.startday);
      fileDate = advanceToTodaysData();
      int dummy = 0;
      publish(haveReadTodaysDataID, dummy);
      if (fileDate != todaysDate && !allowSparseData)
         {
         string msg = "Cannot find data in INPUT file for date ";
         msg += to_simple_string(todaysDate).c_str();
         error(msg, true);
         }
      else
         publishNewMetEvent();
      }
   else if (eventID == getDataMethodID)
      {
      data.first();

      vector<protocol::NewMetType> newmets;
      vector<string> dataDates;
      variant.unpack(dataDates);
      for (unsigned i = 0; i != dataDates.size(); i++)
         {
         date dataDate(from_string(dataDates[i]));
         try
            {
            data.gotoDate(dataDate);

            protocol::NewMetType newmet;
            newmet.today = dataDate.julian_day();
            newmet.maxt = getVariableValue("maxt");
            newmet.mint = getVariableValue("mint");
            newmet.radn = getVariableValue("radn");
            newmet.rain = getVariableValue("rain");
            newmet.vp = getVariableValue("vp");
            newmets.push_back(newmet);
            }
         catch (const exception&)
            {
            string msg = "Cannot find patch data in INPUT file for date ";
            msg += to_simple_string(dataDate).c_str();
            error(msg, false);
            }
         }


      unsigned returnDataMethodID = addRegistration(::event,
                                                    fromID,
                                                    "returnData",
                                                    protocol::DDML(std::vector<protocol::NewMetType>()));
      publish(returnDataMethodID, newmets);

      // reposition the data file to todays date.
      data.first();
      data.gotoDate(todaysDate);
      }
   }
// ------------------------------------------------------------------
// Find a value and return it's numerical value.  Returns value if
// found or zero otherwise.
// ------------------------------------------------------------------
float InputComponent::getVariableValue(const string& name)
   {
   Variables::iterator i = findVariable(name);
   if (i != variables.end())
      return i->second.asFloat();
   else
      return 0.0;
   }
// ------------------------------------------------------------------
// Find a variable in our variables list. Return variables.end() if
// not found.
// ------------------------------------------------------------------
InputComponent::Variables::iterator InputComponent::findVariable(const std::string& name)
   {
   for (Variables::iterator i = variables.begin();
                            i != variables.end();
                            i++)
      {
      if (Str_i_Eq(i->second.getName(), name))
         return i;
      }
   return variables.end();
   }
// ------------------------------------------------------------------
// Calculate and return day length.
// ------------------------------------------------------------------
float InputComponent::calcDayLength(void)
   {
   float latitude = getVariableValue("latitude");
   if (latitude != 0.0)
      {
      // Twilight is defined as the interval between sunrise or sunset and the
      // time when the true centre of the sun is 6 degrees below the horizon.
      // Sunrise or sunset is defined as when the true centre of the sun is 50'
      // below the horizon.
      float twligt = -6.0;
      int dayOfYear = date_duration(todaysDate - date(todaysDate.year(), 1, 1)).days()+1;
      return dayLength(dayOfYear, latitude, twligt);
      }
   else
      return 0.0;
   }

// ------------------------------------------------------------------
// Calculate vp
// ------------------------------------------------------------------
float InputComponent::calcVP(float temp_arg)
   {
   return 6.1078 * exp(17.269*temp_arg / (237.3 + temp_arg));
   }
// ------------------------------------------------------------------
// Publish a newmet event.
// ------------------------------------------------------------------
void InputComponent::publishNewMetEvent(void)
   {
   if (iAmMet)
      {
      // send out a preNewMet Event.
      protocol::NewMetType newmet;
      newmet.today = todaysDate.julian_day();
      newmet.maxt = getVariableValue("maxt");
      newmet.mint = getVariableValue("mint");
      newmet.radn = getVariableValue("radn");
      newmet.rain = getVariableValue("rain");
      newmet.vp = getVariableValue("vp");
      if (newmet.vp == 0.0)
         newmet.vp = calcVP(newmet.mint);
      publish(preNewmetID, newmet);

      newmet.today = todaysDate.julian_day();
      newmet.maxt = getVariableValue("maxt");
      newmet.mint = getVariableValue("mint");
      newmet.radn = getVariableValue("radn");
      newmet.rain = getVariableValue("rain");
      newmet.vp = getVariableValue("vp");
      if (newmet.vp == 0.0)
         newmet.vp = calcVP(newmet.mint);
      publish(newmetID, newmet);

      }
   }
// ------------------------------------------------------------------
// Transfer of sign - from FORTRAN.
// The result is of the same type and kind as a. Its value is the abs(a) of a,
// if b is greater than or equal positive zero; and -abs(a), if b is less than
// or equal to negative zero.
// Example a = sign (30,-2) ! a is assigned the value -30
// ------------------------------------------------------------------
float sign(float a, float b)
   {
   if (b >= 0)
      return fabs(a);
   else
      return -fabs(a);
   }
// ------------------------------------------------------------------
// constrains a variable within bounds of lower and upper
//    Returns "lower", if "var" is less than "lower".  Returns "upper"
//    if "var" is greater than "upper".  Otherwise returns "var".
// ------------------------------------------------------------------
float bound(float var, float lower, float upper)
   {
   if (var < lower)
      return lower;
   else if (var > upper)
      return upper;
   else
      return var;
   }
// ------------------------------------------------------------------
// return the time elasped in hours between the specified sun angle
// from 90 deg in am and pm. +ve above the horizon, -ve below the horizon.
// NB There is a small err in cos (90), thus a special
// case is made for this.
// ------------------------------------------------------------------
float InputComponent::dayLength(int dyoyr, float lat, float sun_angle)
   {
   float aeqnox = 82.25;               // equinox
   float pi =  3.14159265359;
   float dg2rdn = (2.0*pi) / 360.0;
   float decsol = 23.45116 * dg2rdn;   // amplitude of declination of sun
                                       //   - declination of sun at solstices.
                                       // cm says here that the maximum
                                       // declination is 23.45116 or 23 degrees
                                       // 27 minutes.
                                       // I have seen else_where that it should
                                       // be 23 degrees 26 minutes 30 seconds -
                                       // 23.44167
   float dy2rdn = (2.0*pi) /365.25;    // convert days to radians
   float rdn2hr = 24.0/(2.0*pi);       // convert radians to hours

   float alt;                          // twilight altitude limited to max/min
                                       //   sun altitudes end of twilight
                                       //   - altitude of sun. (radians)
   float altmn;                        // altitude of sun at midnight
   float altmx;                        // altitude of sun at midday
   float clcd;                         // cos of latitude * cos of declination
   float coshra;                       // cos of hour angle - angle between the
                                       //   sun and the meridian.
   float dec;                          // declination of sun in radians - this
                                       //   is the angular distance at solar
                                       //   noon between the sun and the equator.
   float hrangl;                       // hour angle - angle between the sun
                                       //   and the meridian (radians).
   float hrlt;                         // day_length in hours
   float latrn;                        // latitude in radians
   float slsd;                         // sin of latitude * sin of declination
   float sun_alt;                      // angular distance between
                                       // sunset and end of twilight - altitude
                                       // of sun. (radians)
                                       // Twilight is defined as the interval
                                       // between sunrise or sunset and the
                                       // time when the true centre of the sun
                                       // is 6 degrees below the horizon.
                                       // Sunrise or sunset is defined as when
                                       // the true centre of the sun is 50'
                                       // below the horizon.

   sun_alt = sun_angle * dg2rdn;

   // calculate daylangth in hours by getting the
   // solar declination (radians) from the day of year, then using
   // the sin and cos of the latitude.

   // declination ranges from -.41 to .41 (summer and winter solstices)

   dec = decsol*sin (dy2rdn* (dyoyr - aeqnox));

   // get the max and min altitude of sun for today and limit
   // the twilight altitude between these.

   if (fabs(lat) == 90.0)
      coshra = sign (1.0, -dec) * sign (1.0, lat);
   else
      {
      latrn = lat*dg2rdn;
      slsd = sin(latrn)*sin(dec);
      clcd = cos(latrn)*cos(dec);

      altmn = asin (bound (slsd - clcd, -1.0, 1.0));
      altmx = asin (bound (slsd + clcd, -1.0, 1.0));
      alt = bound (sun_alt, altmn, altmx);

      // get cos of the hour angle
      coshra = (sin (alt) - slsd) /clcd;
      coshra = bound (coshra, -1.0, 1.0);
      }

   // now get the hour angle and the hours of light
   hrangl = acos (coshra);
   hrlt = hrangl*rdn2hr*2.0;
   return hrlt;
   }
// ------------------------------------------------------------------
// If we haven't already, get the input file start and end dates.
// ------------------------------------------------------------------
void InputComponent::getStartEndDate(void)
   {
   if (startDate.is_infinity())
      {
      data.last();
      endDate = data.getDate();
      data.first();
      startDate = data.getDate();
      }
   }

