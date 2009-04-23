//---------------------------------------------------------------------------
#ifndef ControlFileConverterH
#define ControlFileConverterH

class XMLNode;
#include <fstream>

//---------------------------------------------------------------------------
// This class converts a control file from 1 format to another.
//---------------------------------------------------------------------------
class ControlFileConverter
   {
   public:
      // ------------------------------------------------------------------
      //Converts the specified control file if necessary.
      // ------------------------------------------------------------------
      void go(const std::string& fileName);
      bool convert(const std::string& fileName, XMLNode script, bool writeToStdOut);



   private:
      ApsimControlFile* con;
      std::string conSection;
      std::string parFileToUse;
      std::ofstream log;
      std::string managerActionValue1;
      std::string managerActionOper;
      std::string managerActionValue2;
      std::string managerActionNewParameter;


      bool convertSection(XMLNode& conversionNode);

      //---------------------------------------------------------------------------
      // Evalulate the specified expression and return a value.  Returns true on
      // success.
      //---------------------------------------------------------------------------
      bool evaluate(const std::string& expression, std::string& value) const throw(std::runtime_error);

      // ------------------------------------------------------------------
      // resolve a module.variable to a value if necessary.
      // ------------------------------------------------------------------
      void resolveVariableRef(std::string& value) const;

      // ------------------------------------------------------------------
      // evaluate the date arguments passed in and return a date.
      // ------------------------------------------------------------------
      bool evaluateDate(const std::string& arguments, std::string& value) const throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // Execute the SetParameterValue command.  Returns true on success.
      //---------------------------------------------------------------------------
      bool executeSetParameterValue(const std::string& arguments) throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // Execute the RenameParameter command.  Returns true on success.
      //---------------------------------------------------------------------------
      bool executeRenameParameter(const std::string& arguments) throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // Execute the DeleteParameter command.  Returns true on success.
      //---------------------------------------------------------------------------
      bool executeDeleteParameter(const std::string& arguments) throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // Execute the ChangeInstantiation command.  Returns true on success.
      //---------------------------------------------------------------------------
      bool executeChangeInstantiation(const std::string& arguments) throw(std::runtime_error);

      // ------------------------------------------------------------------
      // Execute the newFormatReportVariables function call
      // ------------------------------------------------------------------
      bool executeRemoveReportOutputSwitch
         (const std::string& arguments) throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // Execute the MoveParameter command.  Returns true on success.
      //---------------------------------------------------------------------------
      bool executeMoveParameter(const std::string& arguments) throw(std::runtime_error);

      // ------------------------------------------------------------------
      // Execute the NewFormatReportVariables command. Returns true on success
      // ------------------------------------------------------------------
      bool executeNewFormatReportVariables(const std::string& arguments) throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // move all parameters out of the control file and into a parameter file.
      //---------------------------------------------------------------------------
      bool executeMoveParametersOutOfCon(const std::string arguments) throw(std::runtime_error);

      // ------------------------------------------------------------------
      // Execute the RemoveSumAvgToTracker command. Returns true on success
      // ------------------------------------------------------------------
      bool executeRemoveSumAvgToTracker(const std::string& arguments) throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // Execute the executeRemoveTrackerDefault command.  Returns true on success.
      //---------------------------------------------------------------------------
      bool executeRemoveTrackerDefault(const std::string& arguments) throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // To a search and replace on all report variables
      //---------------------------------------------------------------------------
      bool executeSearchReplaceReportVariables(const std::string& arguments) throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // Add a parameter file reference to all instances of a module in con file.
      //---------------------------------------------------------------------------
      bool executeAddParamFileToModule(const std::string& arguments) throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // Remove any period characters in tracker variables.
      //---------------------------------------------------------------------------
      bool removePeriodsInReportAndTracker(const std::string& arguments) throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // Rework the tracker variables to new format.
      //---------------------------------------------------------------------------
      bool ReworkTrackerVariables(const std::string& arguments) throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // Add a parameter file reference to all instances of a module in con file.
      //---------------------------------------------------------------------------
      bool executeRenameModule(const std::string& arguments) throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // Perform a search and replace on a param file section
      //---------------------------------------------------------------------------
      bool executeSearchReplace(const std::string& arguments) throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // Set a manager action parameter.
      //---------------------------------------------------------------------------
      bool executeSetManagerActionParameter(const std::string& arguments) throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // Callback for SetmanagerActionParameter.
      //---------------------------------------------------------------------------
      void SetManagerActionCallback(std::vector<ApsimControlFile::ManagerActionParameter>& parameters,
                                    bool& modified);

      //---------------------------------------------------------------------------
      // Delete a manager action parameter.
      //---------------------------------------------------------------------------
      bool executeDeleteManagerActionParameter(const std::string& arguments) throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // Find a module.ini file. Return true if found.
      //---------------------------------------------------------------------------
      bool executeFindModuleLocalIniFile(const std::string& arguments);

      //---------------------------------------------------------------------------
      // Remove report variable. Return true if found.
      //---------------------------------------------------------------------------
      bool executeRemoveReportVariable(const std::string& arguments);

      //---------------------------------------------------------------------------
      // Callback for DeleteManagerActionParameter.
      //---------------------------------------------------------------------------
      void DeleteManagerActionCallback(std::vector<ApsimControlFile::ManagerActionParameter>& parameters,
                                       bool& modified);

      //---------------------------------------------------------------------------
      // Delete a module from the control file.
      //---------------------------------------------------------------------------
      bool executeDeleteModule(const std::string& arguments) throw(std::runtime_error);


      bool executeIniToXml(const std::string& arguments) throw(std::runtime_error);

      //---------------------------------------------------------------------------
      // Perform a search and replace on a control file
      //---------------------------------------------------------------------------
      bool executeSearchReplaceCon(const std::string& arguments) throw(std::runtime_error);

      bool executeSearchReplaceConRaw(const std::string& arguments);

   };

// ------------------------------------------------------------------
// Returns true if the specified control file needs converting
// ------------------------------------------------------------------
bool needToConvertControlFile(const std::string& fileName);

//---------------------------------------------------------------------------
// convert the specified control file.
// Throws an error if a problem was encountered.
//---------------------------------------------------------------------------
void convertControlFile(const std::string& fileName) throw (std::runtime_error);

#endif

