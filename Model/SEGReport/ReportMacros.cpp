//---------------------------------------------------------------------------
#include <general\pch.h>
#include <vcl.h>
#pragma hdrstop

#include "ReportMacros.h"
#include <general\stringtokenizer.h>
#include <general\string_functions.h>
#include <generalvcl\vcl_functions.h>
#include <general\date_functions.h>
#pragma package(smart_init)

using namespace std;
using namespace boost::gregorian;


//---------------------------------------------------------------------------
// Evaluate the specified macro.
//---------------------------------------------------------------------------
string ReportMacros::resolve(TComponent* owner, const string& macro)
   {
   string returnValue = macro;

   unsigned posMacro = returnValue.find('$');
   while (posMacro != string::npos)
      {
      unsigned posOpenBracket = returnValue.find('(', posMacro);
      unsigned posCloseBracket = matchBracket(returnValue, '(', ')', posOpenBracket);
      if (posOpenBracket != string::npos && posCloseBracket != string::npos)
         {
         string macroName = returnValue.substr(posMacro, posOpenBracket-posMacro);
         string argumentString = returnValue.substr(posOpenBracket+1, posCloseBracket-posOpenBracket-1);
         if (macroName != "$")
            argumentString = resolve(owner, argumentString);
         vector<string> arguments;
         splitIntoValues(argumentString, ",", arguments);

         string value;
         try
            {
            if (macroName == "$property" && (arguments.size() == 1 || arguments.size() == 2))
               {
               int recNo = 0;
               if (arguments.size() == 2)
                  recNo = StrToInt(arguments[1].c_str());
               value = resolveComponentPropertyMacro(owner, arguments[0].c_str(), recNo).c_str();
               }
            else if (macroName == "$decplaces" && arguments.size() == 2)
               {
               double doubleValue = StrToFloat(arguments[0].c_str());
               value = ftoa(doubleValue, StrToInt(arguments[1].c_str()));
               }
            else if (macroName == "$today")
               {
               date today(day_clock::local_day());
               value = to_dmy(today);
               }
            else if (macroName == "$dayofyeartodate" && arguments.size() == 1)
               {
               int numDays = atoi(arguments[0].c_str());
               date d(day_clock::local_day().year(), 1, 1);
               if (numDays > 0)
                  d = d + date_duration(numDays - 1);
               value = to_dmy(d);
               }
            else if (macroName == "$formatshortdate" && arguments.size() == 1)
               {
               date d(fromDmyString(arguments[0]));
               ostringstream out;
               out << d.day() << '-' << getShortMonthString(d.month());
               value = out.str();
               }
            else if (macroName == "$adddaystodate" && arguments.size() == 2)
               {
               date d(fromDmyString(arguments[0]));
               d = d + date_duration(atoi(arguments[1].c_str()));
               value = to_dmy(d);
               }
            else if (macroName == "$")
               value = argumentString;
            }
         catch (Exception& err)
            {
            value = "?";
            }
         catch (exception& err)
            {
            value = "?";
            }

         returnValue.replace(posMacro, posCloseBracket-posMacro+1, value);
         }

      posMacro = returnValue.find('$', posMacro+1);
      }

   return returnValue;
   }

