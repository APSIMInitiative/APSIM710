#include <vector>
#include <map>
#include <string>
#include <fstream>
#include <stdexcept>
#include <iostream>

#include <General/StringTokenizer.h>
#include <General/string_functions.h>
#include <General/path.h>
#include <General/date_class.h>
#include <General/IniFile.h>
#include <General/stristr.h>

#include <boost/lexical_cast.hpp>
#include <boost/bind.hpp>

#include <ApsimShared/ApsimControlFile.h>
#include <ApsimShared/ApsimDirectories.h>
#include <ApsimShared/ApsimSettings.h>

#include "ControlFileConverter.h"
using namespace std;
using namespace boost;

// ------------------------------------------------------------------
// evaluate an expression and return result.
// ------------------------------------------------------------------
string evaluateExpression(const string& value1, const string& value2, const string& oper)
   {
   if (value2 == "" || oper == "")
      return value1;
   else if (!Is_numerical(value1.c_str()) || !Is_numerical(value2.c_str()))
      return "";
   else
      {
      double value;
      double number1 = lexical_cast<double>(value1);
      double number2 = lexical_cast<double>(value2);

      if (oper == "+")
         value = number1 + number2;
      else if (oper == "-")
         value = number1 - number2;
      else if (oper == "*")
         value = number1 * number2;
      else if (oper == "/")
         value = number1 / number2;
      return ftoa(value, 2);
      }
   }

//---------------------------------------------------------------------------
// convert the specified control file using the version number of
// APSIM and the version number in the control file.
// Throws an exception if a problem was encountered.
// If callback is not null, then it will be called for every section
// in con file being converter.
//---------------------------------------------------------------------------
void ControlFileConverter::go(const string& fileName)
   {
   vector<XMLNode> scriptNodes;
   ApsimSettings settings;
   string toVersionString;
   settings.getConversionNodes(ApsimControlFile::getVersionNumber(fileName),
                               scriptNodes, toVersionString);
   if (scriptNodes.size() > 0)
      {
      cout << "The APSIM control file: " + fileName << endl;
      cout << "will be automatically upgraded to the new version of APSIM." << endl;
      cout << "The original con/par and man files will be given new extensions of " << endl;
      cout << "oldcon, oldpar and oldman." << endl << endl;

      bool somethingConverted = false;
      for (unsigned f = 0; f != scriptNodes.size(); f++)
         somethingConverted = convert(fileName, scriptNodes[f], true)
                              || somethingConverted;

      ApsimControlFile::setVersionNumber(fileName, toVersionString);
      }
   }
//---------------------------------------------------------------------------
// convert the specified control file using the commands under the specified
// script node.  Throws an exception if a problem was encountered.
//---------------------------------------------------------------------------
bool ControlFileConverter::convert(const string& fileName,
                                   XMLNode script,
                                   bool writeToStdOut)
   {   string savedDirectory = getCurrentDirectory();
   string fullFileName = fileName;
   if (fullFileName.find_first_of("\\/") == string::npos)
      {
      // no directory supplied - add current working directory.
      fullFileName = getCurrentDirectory() + "/" + fileName;
      }
   typedef map<string, vector<string> > Output;
   Output output;

   bool somethingWasConverted = false;
   con = new ApsimControlFile(fullFileName);

   // Make the working directory the same directory as the where the
   // control file resides.
   setCurrentDirectory(fileDirName(fullFileName));

   vector<string> controlFileSections;
   con->getAllSectionNames(controlFileSections);
   for (unsigned section = 0; section != controlFileSections.size(); section++)
      {
      conSection = controlFileSections[section];
      if (con->isValid(controlFileSections[section]))
         {
         // Loop through all lines in script and perform required actions
         for (XMLNode::iterator conversionNode = script.begin();
                                conversionNode != script.end();
                                conversionNode++)
            {
            if (conversionNode->getName() == "conversion")
               {
               bool ok = convertSection(*conversionNode);
               if (ok)
                  {
                  somethingWasConverted = true;
                  XMLNode description = findNode(*conversionNode, "description");
                  if (description.isValid())
                     output[conSection].push_back(description.getValue());
                  else
                     output[conSection].push_back("Unknown conversion");
                  }
               }
            }
         }
      }
   delete con;
   if (somethingWasConverted && writeToStdOut)
      {
      for (Output::iterator out = output.begin(); out != output.end(); out++)
         {
         cout << "Converting section: " << out->first << endl;
         for (unsigned i = 0; i != out->second.size(); i++)
            cout << "   " << out->second[i] << endl;
         cout << endl;
         }
      }

   // restore the original directory
   setCurrentDirectory(savedDirectory);
   return somethingWasConverted;
   }

//---------------------------------------------------------------------------
// convert the control file using the commands in the specified node.
// Throws an exception if a problem was encountered.
// Returns true on success.
//---------------------------------------------------------------------------
bool ControlFileConverter::convertSection(XMLNode& conversionNode)
   {
   bool ok = false;
   for (XMLNode::iterator commandNode = conversionNode.begin();
                          commandNode != conversionNode.end();
                          commandNode++)
      {
      if (commandNode->getName() == "command")
         {
         string command = commandNode->getValue();

         // extract the routine name
         unsigned posOpenBracket = command.find('(');
         if (posOpenBracket == string::npos)
            throw runtime_error("Bad control file converter command: " + command);
         string routineName = command.substr(0, posOpenBracket);

         // extract the arguments inside the brackets.
         unsigned posCloseBracket = command.rfind(')');
         if (posCloseBracket == string::npos)
            throw runtime_error("Bad control file converter command: " + command);
         string arguments = command.substr(posOpenBracket+1, posCloseBracket-posOpenBracket-1);
         stripLeadingTrailing(arguments, " ");

         // call the appropriate routine to do the conversion.
         if (routineName == "SetParameterValue")
            ok = executeSetParameterValue(arguments) || ok;
         else if (routineName == "RenameParameter")
            ok = executeRenameParameter(arguments) || ok;
         else if (routineName == "DeleteParameter")
            ok = executeDeleteParameter(arguments) || ok;
         else if (routineName == "ChangeInstantiation")
            ok = executeChangeInstantiation(arguments) || ok;
         else if (routineName == "RemoveReportOutputSwitch")
            ok = executeRemoveReportOutputSwitch(arguments) || ok;
         else if (routineName == "MoveParameter")
            ok = executeMoveParameter(arguments) || ok;
         else if (routineName == "NewFormatReportVariables")
            ok = executeNewFormatReportVariables(arguments) || ok;
         else if (routineName == "MoveParametersOutOfCon")
            ok = executeMoveParametersOutOfCon(arguments) || ok;
         else if (routineName == "RemoveSumAvgToTracker")
            ok = executeRemoveSumAvgToTracker(arguments) || ok;
         else if (routineName == "RemoveTrackerDefault")
            ok = executeRemoveTrackerDefault(arguments) || ok;
         else if (routineName == "SearchReplaceReportVariables")
            ok = executeSearchReplaceReportVariables(arguments) || ok;
         else if (routineName == "AddParamFileToModule")
            ok = executeAddParamFileToModule(arguments) || ok;
         else if (routineName == "RemovePeriodsInReportAndTracker")
            ok = removePeriodsInReportAndTracker(arguments) || ok;
         else if (routineName == "ReworkTrackerVariables")
            ok = ReworkTrackerVariables(arguments) || ok;
         else if (routineName == "RenameModule")
            ok = executeRenameModule(arguments) || ok;
         else if (routineName == "SearchReplace")
            ok = executeSearchReplace(arguments) || ok;
         else if (routineName == "SetManagerActionParameter")
            ok = executeSetManagerActionParameter(arguments) || ok;
         else if (routineName == "DeleteManagerActionParameter")
            ok = executeDeleteManagerActionParameter(arguments) || ok;
         else if (routineName == "FindModuleLocalIniFile")
            ok = executeFindModuleLocalIniFile(arguments) || ok;
         else if (routineName == "RemoveReportVariable")
            ok = executeRemoveReportVariable(arguments) || ok;
         else if (routineName == "DeleteModule")
            ok = executeDeleteModule(arguments) || ok;
         else if (routineName == "IniToXml")
            ok = executeIniToXml(arguments) || ok;
         else if (routineName == "SearchReplaceCon")
            ok = executeSearchReplaceCon(arguments) || ok;
         else if (routineName == "SearchReplaceConRaw")
            ok = executeSearchReplaceConRaw(arguments) || ok;
         }
      }
   return ok;
   }
//---------------------------------------------------------------------------
// Evalulate the specified expression and return a value.  Returns true on
// success.
//---------------------------------------------------------------------------
bool ControlFileConverter::evaluate(const string& expression, string& value) const throw(runtime_error)
   {
   value = expression;

   // look for a macro.
   if (value.find("%controlfilenamebase%") != string::npos)
      {
      Replace_all(value, "%controlfilenamebase%",
                  Path(con->getFileName()).Get_name_without_ext().c_str());
      return true;
      }

   // look for date function(
   unsigned posFunction = value.find("date(");
   if (posFunction != string::npos)
      {
      // extract the arguments inside the brackets.
      unsigned posCloseBracket = value.rfind(')');
      if (posCloseBracket == string::npos)
         throw runtime_error("Invalid date function usage: " + value);
      posFunction += strlen("date(");
      string arguments = value.substr(posFunction, posCloseBracket-posFunction);
      return evaluateDate(arguments, value);
      }
   else
      {
      StringTokenizer tokenizer(value, " ");
      string value1 = tokenizer.nextToken();
      string oper = tokenizer.nextToken();
      string value2 = tokenizer.nextToken();
      resolveVariableRef(value1);
      resolveVariableRef(value2);
      value = evaluateExpression(value1, value2, oper);
      }
   return true;
   }
// ------------------------------------------------------------------
// resolve a module.variable to a value if necessary.
// ------------------------------------------------------------------
void ControlFileConverter::resolveVariableRef(string& value) const
   {
   // check for a module.name
   unsigned posPeriod = value.find('.');
   if (posPeriod != string::npos)
      {
      string moduleName = value.substr(0, posPeriod);
      string parameterName = value.substr(posPeriod+1);
      vector<string> instances;
      con->getInstances(conSection, moduleName, instances);
      for (unsigned i = 0; i != instances.size(); i++)
         {
         string st = con->getParameterValue(conSection, instances[i], parameterName);
         if (st != "")
            {
            value = st;
            splitOffBracketedValue(value, '(', ')');
            stripLeadingTrailing(value, " ");
            return;
            }
         }
      }
   }
// ------------------------------------------------------------------
// evaluate the date arguments passed in and return a date.
// ------------------------------------------------------------------
bool ControlFileConverter::evaluateDate(const string& arguments, string& value) const throw(runtime_error)
   {
   // This function expects 2 values: day_of_year and year
   StringTokenizer tokenizer(arguments, ", ");
   string arg1 = tokenizer.nextToken();
   string arg2 = tokenizer.nextToken();
   if (arg1 == "" || arg2 == "")
      throw runtime_error("Invalid arguments to date function: " + arguments);

   string dayString;
   string yearString;
   if (!evaluate(arg1, dayString) || !evaluate(arg2, yearString))
      return false;
   int day = atoi(dayString.c_str());
   int year = atoi(yearString.c_str());
   if (day == 0 || year == 0)
      return false;

   GDate date;
   date.Set(day, year);
   ostringstream buffer;
   date.Write(buffer);
   buffer << "     ! dd/mm/yyyy";
   value = buffer.str();
   return true;
   }
//---------------------------------------------------------------------------
// Execute the SetParameterValue command.  Returns true on success.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeSetParameterValue(const string& arguments) throw(runtime_error)
   {
   vector<string> args;
   SplitStringHonouringQuotes(arguments, ",", args);
   if (args.size() != 2 && args.size() != 3)
      throw runtime_error("Bad arguments in call to setParameterValue: " + arguments);

   stripLeadingTrailing(args[0], "\" ");
   stripLeadingTrailing(args[1], "\" ");
   bool alwaysCreate = true;
   if (args.size() == 3)
      {
      stripLeadingTrailing(args[2], "\" ");
      if (Str_i_Eq(args[2], "OnlyWhenNecessary"))
         alwaysCreate = false;
      else
         throw runtime_error("Invalid 3rd argument to SetParameterValue : " + args[2]);
      }

   unsigned posPeriod = args[0].find('.');
   if (posPeriod == string::npos)
      throw runtime_error("Bad 1st argument to SetParameterValue - " + args[0]);
   string moduleName = args[0].substr(0, posPeriod);
   string parameterName = args[0].substr(posPeriod+1);

   string value;
   if (evaluate(args[1], value))
      {
      vector<string> instanceNames;
      con->getInstances(conSection, moduleName, instanceNames);
      if (alwaysCreate || instanceNames.size() > 0)
         {
         con->setParameterValue(conSection, moduleName, parameterName, value);
         return true;
         }
      }
   return false;
   }
//---------------------------------------------------------------------------
// Execute the RenameParameter command.  Returns true on success.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeRenameParameter(const string& arguments) throw(runtime_error)
   {
   unsigned posComma = arguments.find(',');
   if (posComma == string::npos)
      throw runtime_error("Bad arguments in call to RenameParameter: " + arguments);

   string arg1 = arguments.substr(0, posComma);
   string arg2 = arguments.substr(posComma+1, arguments.length()-posComma-1);
   stripLeadingTrailing(arg1, " ");
   stripLeadingTrailing(arg2, " ");
   unsigned posPeriod = arg1.find('.');
   string moduleName = arg1.substr(0, posPeriod);
   string parameterName = arg1.substr(posPeriod+1);
   return con->renameParameter(conSection, moduleName, parameterName, arg2);
   }
//---------------------------------------------------------------------------
// Execute the DeleteParameter command.  Returns true on success.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeDeleteParameter(const string& arguments) throw(runtime_error)
   {
   unsigned posPeriod = arguments.find('.');
   string moduleName = arguments.substr(0, posPeriod);
   string parameterName = arguments.substr(posPeriod+1);
   return con->deleteParameter(conSection, moduleName, parameterName);
   }
//---------------------------------------------------------------------------
// Execute the ChangeInstantiation command.  Returns true on success.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeChangeInstantiation(const string& arguments) throw(runtime_error)
   {
   unsigned posComma = arguments.find(',');
   if (posComma == string::npos)
      throw runtime_error("Bad arguments in call to ChangeInstantiation: " + arguments);

   string arg1 = arguments.substr(0, posComma);
   string arg2 = arguments.substr(posComma+1, arguments.length()-posComma-1);
   stripLeadingTrailing(arg1, " ");
   stripLeadingTrailing(arg2, " ");

   return con->changeModuleName(conSection, arg1, arg2);
   }
// ------------------------------------------------------------------
// Execute the newFormatReportVariables function call
// ------------------------------------------------------------------
class RemoveReportSwitch
   {
   public:
      RemoveReportSwitch(const string& pName, bool& m)
         : paramName(pName), modified(m) { }

      void callback(IniFile* par, const string& section)
         {
         string value;
         par->read(section, paramName, value);
         unsigned posOverwrite = value.find("/overwrite");
         if (posOverwrite != string::npos)
            {
            value.erase(posOverwrite);
            stripLeadingTrailing(value, " ");
            par->write(section, paramName, value);
            modified = true;
            }
         }

   private:
      const string& paramName;
      bool& modified;
   };

bool ControlFileConverter::executeRemoveReportOutputSwitch(const string& arguments) throw(runtime_error)
   {
   bool modified = false;
   RemoveReportSwitch removeSwitch(arguments, modified);
   con->enumerateParameters(conSection, "report", false,
                            boost::bind(&RemoveReportSwitch::callback, &removeSwitch, _1, _2));
   return modified;
   }
//---------------------------------------------------------------------------
// Execute the MoveParameter command.  Returns true on success.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeMoveParameter(const string& arguments) throw(runtime_error)
   {
   unsigned posComma = arguments.find(',');
   if (posComma == string::npos)
      throw runtime_error("Bad arguments in call to MoveParameter: " + arguments);

   string arg1 = arguments.substr(0, posComma);
   string arg2 = arguments.substr(posComma+1, arguments.length()-posComma-1);
   stripLeadingTrailing(arg1, " ");
   stripLeadingTrailing(arg2, " ");
   unsigned posPeriod = arg1.find('.');
   string moduleName = arg1.substr(0, posPeriod);
   string parameterName = arg1.substr(posPeriod+1);
   if (arg2 == "" && Str_i_Eq(parameterName, "title"))
      {
      string title;
      evaluate("report.title", title);
      con->deleteParameter(conSection, moduleName, parameterName);
      con->setTitle(conSection, title);
      return true;
      }
   else
      return con->moveParameter(conSection, moduleName, parameterName, arg2);
   }

// ------------------------------------------------------------------
// Execute the NewFormatReportVariables command. Returns true on success
// ------------------------------------------------------------------
class FormatReportVariables
   {
   public:
      FormatReportVariables(bool& m)
         : modified(m) { }

      void callback(IniFile* par, const string& section)
         {
         // Get the module_names, variable_names and variable_alias lines for all
         // instances of REPORT.  Keep in mind that there may be multiple variable 'blocks'
         // for each section.

         vector<string> moduleLines, variableLines, aliasLines;
         par->read(section, "module_names", moduleLines);
         par->read(section, "variable_names", variableLines);
         par->read(section, "variable_alias", aliasLines);

         if (moduleLines.size() != variableLines.size()
             || moduleLines.size() != aliasLines.size())
            throw runtime_error("Control file converter error: Invalid report variables section in "
                                "parameter file.  The number of values in the module_names, "
                                "variable_names and variable_alias lines must be the same. ");

         // For each variable on each variable line, create a new variable.
         vector<string> newVariables;
         for (unsigned variableLineI = 0;
                       variableLineI != variableLines.size();
                       variableLineI++)
            {
            vector<string> modules, variables, aliases;
            Split_string(moduleLines[variableLineI], " ", modules);
            Split_string(variableLines[variableLineI], " ", variables);
            Split_string(aliasLines[variableLineI], " ", aliases);
            if (modules.size() != variables.size() || modules.size() != aliases.size())
               throw runtime_error("Control file converter error: Invalid report variables section in "
                                   "parameter file.  The number of values in the module_names, "
                                   "variable_names and variable_alias lines must be the same. "
                                   "Variable_names: " + variableLines[variableLineI]);
            for (unsigned int variable = 0; variable < variables.size(); variable++)
               {
               string variableName = modules[variable] + "." + variables[variable];
               if (aliases[variable] != "-")
                  variableName += " as " + aliases[variable];
               newVariables.push_back(variableName);
               }
            }

         // write all new variables.
         if (newVariables.size() > 0)
            {
            par->write(section, "variable", newVariables);
            modified = true;
            }
         }

   private:
      bool& modified;
   };

bool ControlFileConverter::executeNewFormatReportVariables(const std::string& arguments) throw(runtime_error)
   {
   bool modified = false;
   FormatReportVariables formatReportVariables(modified);
   con->enumerateParameters(conSection, "report", false,
                            boost::bind(&FormatReportVariables::callback, &formatReportVariables, _1, _2));
   return modified;
   }
//---------------------------------------------------------------------------
// move all parameters out of the control file and into a parameter file.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeMoveParametersOutOfCon(const std::string arguments) throw(runtime_error)
   {
   if (con->hasParametersInCon(conSection))
      {
      string parFileToUse = arguments;
      if (parFileToUse == "")
         {
         parFileToUse = con->getFileName() + ".par";
         }
      return con->moveParametersOutOfCon(conSection, parFileToUse);
      }
   return false;
   }
// ------------------------------------------------------------------
// Execute the RemoveSumAvgToTracker command. Returns true on success
// ------------------------------------------------------------------
class RemoveSumAvg
   {
   public:
      RemoveSumAvg(ApsimControlFile* c, const string& section, bool& m)
         : con(c), conSection(section), modified(m), trackerNum(1) { }

      void callback(IniFile* par, const string& section)
         {
         unsigned posFirstPeriod = section.find('.');
         unsigned posSecondPeriod = section.find('.', posFirstPeriod+1);
         string instanceName = section.substr(posFirstPeriod+1, posSecondPeriod-posFirstPeriod-1);

         string trackerInstanceName = string("tracker") + itoa(trackerNum);

         vector<string> trackerVariables;
         vector<string> variables;
         par->read(section, "variable", variables);

         vector<string> newVariables;
         bool found = false;
         for (unsigned v = 0; v != variables.size(); v++)
            {
            if ((variables[v].find("sum@") != string::npos ||
                 variables[v].find("avg@") != string::npos)
                 && variables[v].find("tracker") == string::npos)
               {
               found = true;
               modified = true;
               StringTokenizer tokenizer(variables[v], ".@");
               string moduleName = tokenizer.nextToken();
               string functionName = tokenizer.nextToken();
               string realVariableName = tokenizer.nextToken();

               string alias;
               unsigned posAlias = realVariableName.find(" as ");
               if (posAlias != string::npos)
                  {
                  alias = realVariableName.substr(posAlias+4);
                  realVariableName.erase(posAlias);
                  }

               string variableName = realVariableName;
               Replace_all(variableName, "(", "[");
               Replace_all(variableName, ")", "]");
               string reportVariable = trackerInstanceName + "."
                                     + functionName +  "@"
                                     + moduleName + "." + variableName;
               if (alias != "")
                  reportVariable += " as " + alias;
               variables[v] = reportVariable;

               string trackerFunctionName = functionName;
               if (Str_i_Eq(trackerFunctionName, "avg"))
                  trackerFunctionName = "average";

               // set the tracker variable.
               string trackerVariable = trackerFunctionName + " of " + moduleName + "."
                      + realVariableName + " since " + instanceName
                      + ".reported as ";
               trackerVariable += functionName + "@" + moduleName + "." + variableName;
               trackerVariable += " on post";
               trackerVariables.push_back(trackerVariable);
               }
            }

         // write all new variables.
         if (found)
            {
            par->write(section, "variable", variables);

            con->setParameterValues(conSection, trackerInstanceName, "variable", "", trackerVariables);
            con->changeModuleName(conSection, trackerInstanceName, "tracker(" + trackerInstanceName + ")");

            trackerNum++;
            }
         }

   private:
      ApsimControlFile* con;
      const string& conSection;
      bool& modified;
      int trackerNum;
   };
bool ControlFileConverter::executeRemoveSumAvgToTracker(const std::string& arguments) throw(runtime_error)
   {
   bool modified = false;
   RemoveSumAvg removeSumAvg(con, conSection, modified);
   con->enumerateParameters(conSection, "report", false, boost::bind(&RemoveSumAvg::callback, &removeSumAvg, _1, _2));
   return modified;
   }
//---------------------------------------------------------------------------
// Execute the executeRemoveTrackerDefault command.  Returns true on success.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeRemoveTrackerDefault(const string& arguments) throw(runtime_error)
   {
   vector<string> instanceNames;
   con->getInstances(conSection, "tracker", instanceNames);
   bool someHaveChanged = false;
   for (unsigned i = 0; i != instanceNames.size(); i++)
      {
      vector<string> variables;
      con->getParameterValues(conSection, instanceNames[i], "variable", variables);
      for (unsigned v = 0; v != variables.size(); v++)
         {
         if (variables[v].find(" on ") == string::npos)
            {
            variables[v] += " on process";
            someHaveChanged = true;
            }
         }
      if (someHaveChanged)
         {
         con->setParameterValues(conSection, instanceNames[i], "variable",
                                 "", variables);
         }
      }
   return someHaveChanged;
   }
//---------------------------------------------------------------------------
// To a search and replace on all report variables
//---------------------------------------------------------------------------
bool ControlFileConverter::executeSearchReplaceReportVariables(const string& arguments) throw(runtime_error)
   {
   unsigned posComma = arguments.find(',');
   if (posComma == string::npos)
      throw runtime_error("Bad arguments in call to SearchReplaceReportVariables: " + arguments);

   string arg1 = arguments.substr(0, posComma);
   string arg2 = arguments.substr(posComma+1, arguments.length()-posComma-1);
   stripLeadingTrailing(arg1, " ");
   stripLeadingTrailing(arg2, " ");

   vector<string> instanceNames;
   con->getInstances(conSection, "report", instanceNames);
   bool someHaveChanged = false;
   for (unsigned i = 0; i != instanceNames.size(); i++)
      {
      vector<string> variables;
      con->getParameterValues(conSection, instanceNames[i], "variable", variables);
      for (unsigned v = 0; v != variables.size(); v++)
         someHaveChanged = (replaceAll(variables[v], arg1, arg2) || someHaveChanged);

      if (someHaveChanged)
         {
         con->setParameterValues(conSection, instanceNames[i], "variable",
                                 "", variables);
         }
      }
   return someHaveChanged;
   }
//---------------------------------------------------------------------------
// Add a parameter file reference to all instances of a module in con file.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeAddParamFileToModule(const string& arguments) throw(runtime_error)
   {
   unsigned posComma = arguments.find(',');
   if (posComma == string::npos)
      throw runtime_error("Bad arguments in call to AddParamFileToModule: " + arguments);

   string arg1 = arguments.substr(0, posComma);
   string arg2 = arguments.substr(posComma+1, arguments.length()-posComma-1);
   stripLeadingTrailing(arg1, " ");
   stripLeadingTrailing(arg2, " ");
   posComma = arg2.find(',');
   string arg3 = arg2.substr(posComma+1);
   arg2.erase(posComma);

   return con->addParameterFileReference(conSection, arg1, arg2, arg3);
   }
//---------------------------------------------------------------------------
// Remove any period characters in tracker variables.
//---------------------------------------------------------------------------
bool ControlFileConverter::removePeriodsInReportAndTracker(const string& arguments) throw(runtime_error)
   {
   // change report variables first.
   vector<string> instanceNames;
   con->getInstances(conSection, "report", instanceNames);
   bool someHaveChanged = false;
   for (unsigned i = 0; i != instanceNames.size(); i++)
      {
      vector<string> variables;
      con->getParameterValues(conSection, instanceNames[i], "variable", variables);
      for (unsigned v = 0; v != variables.size(); v++)
         {
         unsigned posPeriod = variables[v].find('.');
         someHaveChanged = (replaceAll(variables[v], posPeriod+1, ".", "_") || someHaveChanged);
         }

      if (someHaveChanged)
         {
         con->setParameterValues(conSection, instanceNames[i], "variable",
                                 "", variables);
         }
      }

   // change tracker variables.
   bool someHaveChanged2 = false;
   instanceNames.erase(instanceNames.begin(), instanceNames.end());
   con->getInstances(conSection, "tracker", instanceNames);
   for (unsigned i = 0; i != instanceNames.size(); i++)
      {
      vector<string> variables;
      con->getParameterValues(conSection, instanceNames[i], "variable", variables);
      for (unsigned v = 0; v != variables.size(); v++)
         {
         unsigned posAs = variables[v].find(" as ");
         someHaveChanged2 = (replaceAll(variables[v], posAs, ".", "_") || someHaveChanged);
         }

      if (someHaveChanged2)
         {
         con->setParameterValues(conSection, instanceNames[i], "variable",
                                 "", variables);
         }
      }


   return (someHaveChanged || someHaveChanged2);
   }
//---------------------------------------------------------------------------
// Rework the tracker variables to new format.
//---------------------------------------------------------------------------
bool ControlFileConverter::ReworkTrackerVariables(const string& arguments) throw(runtime_error)
   {
   // change tracker variables.
   vector<string> instanceNames;
   con->getInstances(conSection, "tracker", instanceNames);
   for (unsigned i = 0; i != instanceNames.size(); i++)
      {
      vector<string> variables;
      con->getParameterValues(conSection, instanceNames[i], "variable", variables);
      for (unsigned v = 0; v != variables.size(); v++)
         {
         if (variables[v].find(" from ") == string::npos
             && variables[v].find(" last ") == string::npos)
            {
            StringTokenizer tokenizer(variables[v], " \t\n");
            string stat = tokenizer.nextToken();
            string word = tokenizer.nextToken();
            string variable = tokenizer.nextToken();
            string eventName, startPeriod, endPeriod, as;
            word = tokenizer.nextToken();
            while (word != "")
               {
               if (Str_i_Eq(word, "since"))
                  {
                  startPeriod = tokenizer.nextToken();
                  endPeriod = "now";
                  }
               else if (Str_i_Eq(word, "between"))
                  {
                  startPeriod = tokenizer.nextToken();
                  string andjunk = tokenizer.nextToken();
                  endPeriod = tokenizer.nextToken();
                  }
               else if (Str_i_Eq(word, "on"))
                  eventName = tokenizer.nextToken();
               else if (Str_i_Eq(word, "as"))
                  as = tokenizer.nextToken();
               word = tokenizer.nextToken();
               }
            if (Str_i_Eq(eventName, "prepare"))
               eventName = "start_of_day";
            if (Str_i_Eq(eventName, "post"))
               eventName = "end_of_day";

            variables[v] = stat + " of " + variable;
            if (eventName != "")
               variables[v] += " on " + eventName;
            if (startPeriod != "")
               variables[v] += " from " + startPeriod + " to " + endPeriod;
            if (as != "")
               variables[v] += " as " + as;
            }
         }

      if (variables.size() > 0)
         con->setParameterValues(conSection, instanceNames[i], "variable",
                                 "", variables);
      }

   return (instanceNames.size() > 0);
   }
//---------------------------------------------------------------------------
// Rename a module in the control file.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeRenameModule(const string& arguments) throw(runtime_error)
   {
   unsigned posComma = arguments.find(',');
   if (posComma == string::npos)
      throw runtime_error("Bad arguments in call to RenameModule: " + arguments);

   string arg1 = arguments.substr(0, posComma);
   string arg2 = arguments.substr(posComma+1, arguments.length()-posComma-1);
   stripLeadingTrailing(arg1, " ");
   stripLeadingTrailing(arg2, " ");

   return con->renameModule(conSection, arg1, arg2);
   }
void TurnIntoRegEx(string& st)
   {
   string ReturnString;
   string LowerCaseSt = st;
   string UpperCaseSt = st;
   To_lower(LowerCaseSt);
   To_upper(UpperCaseSt);
   for (unsigned i = 0; i != st.length(); i++)
      {
      if ((st[i] >= 'A' && st[i] <= 'Z') ||
          (st[i] >= 'a' && st[i] <= 'z'))
         ReturnString += string("[") + UpperCaseSt[i] + LowerCaseSt[i] + "]";
      else
         ReturnString += st[i];
      }
   st = ReturnString;
   }
//---------------------------------------------------------------------------
// Perform a search and replace on a param file section
//---------------------------------------------------------------------------
bool ControlFileConverter::executeSearchReplace(const string& arguments) throw(runtime_error)
   {
   vector<string> args;
   SplitStringHonouringQuotes(arguments, ",", args);
   if (args.size() != 3)
      throw runtime_error("Bad arguments in call to SearchReplace: " + arguments);

   stripLeadingTrailing(args[1], " ");
   stripLeadingTrailing(args[2], " ");

   stripLeadingTrailing(args[0], "\"");
   stripLeadingTrailing(args[1], "\"");
   stripLeadingTrailing(args[2], "\"");
   string StringToFind = args[1];
   TurnIntoRegEx(StringToFind);
   replaceAll(StringToFind, "(", "\\~");
   replaceAll(StringToFind, "~", "(");

   replaceAll(StringToFind, ")", "\\~");
   replaceAll(StringToFind, "~", ")");
   StringToFind = "(\\W)" + StringToFind + "(\\W)";
   string ReplacementString = "$1" + args[2] + "$2";
   return con->searchReplace(conSection, args[0], StringToFind, ReplacementString);
   }
//---------------------------------------------------------------------------
// Set a manager action parameter.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeSetManagerActionParameter(const string& arguments) throw(runtime_error)
   {
   vector<string> args;
   SplitStringHonouringQuotes(arguments, ",", args);
   if (args.size() != 3)
      throw runtime_error("Bad arguments in call to SetManagerActionParameter: " + arguments);

   stripLeadingTrailing(args[0], "\" ");
   stripLeadingTrailing(args[1], "\" ");
   stripLeadingTrailing(args[2], "\" ");

   managerActionNewParameter = args[1];
   StringTokenizer tokenizer(args[2], " ");
   managerActionValue1 = tokenizer.nextToken();
   managerActionOper = tokenizer.nextToken();
   managerActionValue2 = tokenizer.nextToken();

   return con->enumerateManagerActionLines(conSection, args[0],
                                           boost::bind(&ControlFileConverter::SetManagerActionCallback, this, _1, _2));
   }
//---------------------------------------------------------------------------
// Callback for SetmanagerActionParameter.
//---------------------------------------------------------------------------
void ControlFileConverter::SetManagerActionCallback(std::vector<ApsimControlFile::ManagerActionParameter>& parameters,
                                                    bool& modified)
   {
   // go resolve all manager parameters.
   for (unsigned p = 0; p != parameters.size(); p++)
      {
      if (Str_i_Eq(managerActionValue1, parameters[p].name))
         managerActionValue1 = parameters[p].value;
      if (Str_i_Eq(managerActionValue2, parameters[p].name))
         managerActionValue2 = parameters[p].value;
      }
   ApsimControlFile::ManagerActionParameter newParam;
   newParam.name = managerActionNewParameter;
   newParam.value = evaluateExpression(managerActionValue1, managerActionValue2,
                                       managerActionOper);
   parameters.push_back(newParam);
   modified = true;
   }
//---------------------------------------------------------------------------
// Delete a manager action parameter.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeDeleteManagerActionParameter(const string& arguments) throw(runtime_error)
   {
   vector<string> args;
   SplitStringHonouringQuotes(arguments, ",", args);
   if (args.size() != 2)
      throw runtime_error("Bad arguments in call to DeleteManagerActionParameter: " + arguments);

   stripLeadingTrailing(args[0], "\" ");
   stripLeadingTrailing(args[1], "\" ");

   managerActionValue1 = args[1];

   return con->enumerateManagerActionLines(conSection, args[0],
                                           boost::bind(&ControlFileConverter::DeleteManagerActionCallback, this, _1, _2));
   }
//---------------------------------------------------------------------------
// Callback for DeleteManagerActionParameter.
//---------------------------------------------------------------------------
void ControlFileConverter::DeleteManagerActionCallback(std::vector<ApsimControlFile::ManagerActionParameter>& parameters,
                                                       bool& modified)
   {
   // go resolve all manager parameters.
   for (unsigned p = 0; p != parameters.size(); p++)
      {
      if (Str_i_Eq(managerActionValue1, parameters[p].name))
         {
         parameters.erase(parameters.begin()+p);
         modified = true;
         return;
         }
      }
   modified = false;
   }
//---------------------------------------------------------------------------
// Find a module.ini file. Return true if found.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeFindModuleLocalIniFile(const string& arguments)
   {
   vector<string> args;
   SplitStringHonouringQuotes(arguments, ",", args);
   if (args.size() != 1)
      throw runtime_error("Invalid arguments to FindModuleLocalIniFile: " + arguments);

   stripLeadingTrailing(args[0], "\" ");

   string apsimDirectory = getApsimDirectory();

   vector<string> instanceNames;
   con->getInstances(conSection, args[0], instanceNames);
   for (unsigned i = 0; i != instanceNames.size(); i++)
      {
      string iniFile = con->getIniFileForInstance(conSection, instanceNames[i]);
      if (stristr(fileDirName(iniFile).c_str(), apsimDirectory.c_str()) == NULL)
         return true;
      }
   return false;
   }

//---------------------------------------------------------------------------
// Remove report variable. Return true if found.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeRemoveReportVariable(const string& arguments)
   {
   vector<string> args;
   SplitStringHonouringQuotes(arguments, ",", args);
   if (args.size() != 1)
      throw runtime_error("Invalid arguments to RemoveReportVariable: " + arguments);

   stripLeadingTrailing(args[0], "\" ");
   unsigned posPeriod = args[0].find('.');
   string moduleToFind;
   string variableToFind;
   if (posPeriod != string::npos)
      {
      moduleToFind = args[0].substr(0, posPeriod);
      variableToFind = args[0].substr(posPeriod+1);
      }
   else
      variableToFind = args[0];

   vector<string> instanceNames;
   con->getInstances(conSection, "report", instanceNames);

   bool someHaveChanged = false;
   for (unsigned i = 0; i != instanceNames.size(); i++)
      {
      vector<IniFile*> parFiles;
      vector<string> sectionNames;
      con->getParameterSections(conSection, instanceNames[i], parFiles, sectionNames);

      for (unsigned p = 0; p != parFiles.size(); p++)
         {
         vector<string> newVariables;
         vector<string> variables;
         parFiles[p]->read(sectionNames[p], "variable", variables);
         bool found = false;
         for (unsigned v = 0; v != variables.size(); v++)
            {
            string Variable = variables[v];
            if (Variable.find(' ') != string::npos)
               Variable.erase(Variable.find(' '));
            string VariableModule;
            posPeriod = Variable.find('.');
            if (posPeriod != string::npos)
               {
               VariableModule = Variable.substr(0, posPeriod);
               Variable.erase(0, Variable.find('.')+1);
               }
            if (Str_i_Eq(variableToFind, Variable) &&
                moduleToFind == "" || VariableModule == "" ||
                Str_i_Eq(moduleToFind, VariableModule))
               found = true;
            else
               newVariables.push_back(variables[v]);
            }

         if (found)
            {
            parFiles[p]->write(sectionNames[p], "variable", newVariables);
            someHaveChanged = true;
            }
         }

      someHaveChanged = someHaveChanged;
      }
   return someHaveChanged;
   }

//---------------------------------------------------------------------------
// Delete a module in the control file.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeDeleteModule(const string& arguments) throw(runtime_error)
   {
   return con->deleteModule(conSection, arguments);
   }
//---------------------------------------------------------------------------
// Rename an .ini file to .xml file.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeIniToXml(const string& arguments) throw(runtime_error)
   {
   return con->iniToXml(conSection);
   }

//---------------------------------------------------------------------------
// Add an escape char to the regex string.
//---------------------------------------------------------------------------
void escape(string& st, char charToEscape)
   {
   unsigned pos = st.find(charToEscape);
   while (pos != string::npos)
      {
      st.insert(pos, 1, '\\');
      pos = st.find(charToEscape, pos+2);
      }
   }

//---------------------------------------------------------------------------
// Perform a search and replace on a control file.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeSearchReplaceCon(const string& arguments) throw(runtime_error)
   {
   vector<string> args;
   SplitStringHonouringQuotes(arguments, ",", args);

   vector<vector<string> > twoLists;
   twoLists.push_back(vector<string>());
   twoLists.push_back(vector<string>());

   unsigned j = 0;
   for (unsigned i = 0; i != args.size(); i++)
      {
      stripLeadingTrailing(args[i], " ");
      stripLeadingTrailing(args[i], "\"");
      if (args[i] != "\n")
         {
         twoLists[j].push_back(args[i]);
         j++;
         if (j == 2) j = 0;
         }
      }
   if (twoLists[0].size() != twoLists[1].size())
      throw runtime_error("Bad arguments in call to executeSearchReplaceCon: " + arguments);

   for (unsigned i = 0; i != twoLists[0].size(); i++)
      {
      string stringToFind = twoLists[0][i];
      TurnIntoRegEx(stringToFind);
      escape(stringToFind, '\\');
      escape(stringToFind, '(');
      escape(stringToFind, ')');
      stringToFind = "(\\W)" + stringToFind + "(\\W)";
      twoLists[0][i] = stringToFind;
      }
   for (unsigned i = 0; i != twoLists[1].size(); i++)
      twoLists[1][i] = "$1" + twoLists[1][i] + "$2";

   return con->searchReplaceCon(twoLists[0], twoLists[1]);
   }

//---------------------------------------------------------------------------
// Point .ini/.xml files to a new folder.
//---------------------------------------------------------------------------
bool ControlFileConverter::executeSearchReplaceConRaw(const string& arguments)
   {
   vector<string> args;
   SplitStringHonouringQuotes(arguments, ",", args);

   vector<vector<string> > twoLists;
   twoLists.push_back(vector<string>());
   twoLists.push_back(vector<string>());

   unsigned j = 0;
   for (unsigned i = 0; i != args.size(); i++)
      {
      stripLeadingTrailing(args[i], " ");
      stripLeadingTrailing(args[i], "\"");
      if (args[i] != "\n")
         {
         twoLists[j].push_back(args[i]);
         j++;
         if (j == 2) j = 0;
         }
      }
   if (twoLists[0].size() != twoLists[1].size())
      throw runtime_error("Bad arguments in call to SearchReplaceConRaw: " + arguments);

   return con->searchReplaceCon(twoLists[0], twoLists[1]);
   }
