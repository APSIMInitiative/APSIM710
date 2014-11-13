using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using ModelFramework;
using ApsimFile;
using System.IO;
using System.Data;
using System.Collections;
using System.Text.RegularExpressions;
using System.Globalization;
using CSGeneral;

public class PatchData
{
    /// <summary>A private class for patching a single variable</summary>
    public class PatchVariable
    {
        /// <summary>Gets or sets the data.</summary>
        private DataTable data;

        /// <summary>The name of the column to pull out of 'data'</summary>
        private string dataColumnName;

        /// <summary>
        /// Gets or sets a value indicating whether this <see cref="PatchVariable"/> is longterm.
        /// </summary>
        private bool longterm;

        /// <summary>The variable name to set</summary>
        private string variableNameToSet;

        /// <summary>From date</summary>
        private DateTime fromDate;

        /// <summary>To date</summary>
        private DateTime toDate;

        /// <summary>The value</summary>
        private string value;

        /// <summary>Link to our parent paddock</summary>
        private ModelFramework.Component paddock = null;

        /// <summary>Initializes a new instance of the <see cref="PatchVariable"/> class.</summary>
        /// <param name="variableName">Name of the variable.</param>
        /// <param name="longterm">if set to <c>true</c> [longterm].</param>
        /// <param name="fromDate">From date.</param>
        /// <param name="toDate">To date.</param>
        /// <param name="data">The data.</param>
        public PatchVariable(string variableName, string dataColumnName, string value, bool longterm, DateTime fromDate, DateTime toDate, DataTable data, ModelFramework.Component paddock)
        {
            this.variableNameToSet = variableName;
            this.dataColumnName = dataColumnName;
            this.value = value;
            this.longterm = longterm;
            this.fromDate = fromDate;
            this.toDate = toDate;
            this.data = data;
            this.value = value;
            this.paddock = paddock;

            // For convienence of user, sometimes the data column name may have a met. in front
            // of the name. Strip this off.
            if (this.dataColumnName != null)
            {
                int posPeriod = this.dataColumnName.LastIndexOf('.');
                if (posPeriod != -1)
                    this.dataColumnName = this.dataColumnName.Substring(posPeriod + 1);
            }
        }

        /// <summary>Applies the patch if we have data for the specified date.</summary>
        /// <param name="dateTime">The date</param>
        internal void ApplyPatch(DateTime date)
        {
            bool useDates = this.fromDate != DateTime.MinValue && this.toDate != DateTime.MinValue;

            bool inDateRange = !useDates || dateInRange(date);
            if (inDateRange)
            {
                string newVariableValue = value;
                if (newVariableValue == null)
                {
                    if (data == null)
                        throw new Exception("Cannot find values for patch variable: " + this.dataColumnName);

                    // Find the row that has the month we're looking for.
                    var row = data.AsEnumerable().FirstOrDefault
                        (r => ((DateTime)r["Date"]).Day == date.Day &&
                              ((DateTime)r["Date"]).Month == date.Month);

                    if (row != null)
                        newVariableValue = row[this.dataColumnName].ToString();
                }
                if (newVariableValue != null && newVariableValue != string.Empty)
                {
                    double doubleValue = Convert.ToDouble(newVariableValue);
                    if (!paddock.Set(this.variableNameToSet, doubleValue))
                        throw new Exception("Cannot set variable: " + this.variableNameToSet);
                }
            }
        }

        /// <summary>Returns true if specified date is in range</summary>
        /// <param name="date">The date to check</param>
        /// <returns>True if date is in range</returns>
        private bool dateInRange(DateTime date)
        {
            if (longterm)
            {
                DateTime firstDate = new DateTime(date.Year, this.fromDate.Month, this.fromDate.Day);
                int numYearsInWindow = this.toDate.Year - this.fromDate.Year;
                DateTime lastDate = new DateTime(firstDate.Year + numYearsInWindow, this.toDate.Month, this.toDate.Day);
                return (date >= firstDate && date <= lastDate);
            }
            else
                return (date >= this.fromDate && date <= this.toDate);
        }
    }

    /// <summary>The file we're reading from</summary>
    private DataTable data = null;

    /// <summary>The variables that will do the patching.</summary>
    private List<PatchVariable> patchVariables = null;

    /// <summary>The data table from input</summary>
    private DataTable dataTableFromInput;

    /// <summary>True when we are requesting data from INPUT</summary>
    private bool weAreRequestingData = false;

    /// <summary>Link to our parent paddock</summary>
    [Link]
    private ModelFramework.Component Paddock = null;

    /// <summary>The event delegate for the 'getData' event</summary>
    /// <param name="dateStrings">The date strings.</param>
    public delegate void getDataDelegate(StringArrayType dateStrings);

    /// <summary>Invoked when we need data from the INPUT model</summary>
    [Event]
    public event getDataDelegate getData;

    /// <summary>The file name read from the parameter file.</summary>
    [Param]
    public string FileName { get; set; }

    /// <summary>On initialisation, set up this component</summary>
    [EventHandler]
    public void OnInitialised()
    {
        // Open the file, read in all constants and data and then close the file.
        APSIMInputFile file = new APSIMInputFile();

        if (!File.Exists(FileName))
            throw new Exception("Cannot find file: " + FileName);

        try
        {
            file.Open(FileName);
        }
        catch (Exception)
        {
        }
        data = file.ToTable();
        file.Close();
        Console.WriteLine("      Reading patch data from: " + FileName);

        // Convert all constants found into PatchVariables.
        patchVariables = new List<PatchVariable>();
        foreach (APSIMConstant constant in file.Constants)
            ProcessConstant(constant);
    }

    /// <summary>Input module has sent out a PreNewMet event.</summary>
    [EventHandler]
    public void OnPreNewMet(NewMetType newMetData)
    {
        if (patchVariables != null)
        {
            foreach (PatchVariable variable in patchVariables)
            {
                variable.ApplyPatch(DateUtility.JulianDayNumberToDateTime((int)Math.Truncate(newMetData.today)));
            }
        }
    }

    /// <summary>Process the raw constant straight from the input file</summary>
    /// <remarks>
    /// The objective of this method is to parse the text in the 
    /// specified constant and create a representative 'PatchVariable'
    /// </remarks>
    private void ProcessConstant(APSIMConstant constant)
    {
        // Parse examples:
        //    longterm rain = 0
        //    longterm rain = patch_rain
        //    longterm rain = rain from 2014-04-01 to 2014-07-31

        if (constant.Name != "Title")
        {
            string descriptor = constant.Name + " = " + constant.Value;
            Console.WriteLine("      " + descriptor);

            string pattern = @"((?<longterm>longterm)\s+)?" +
                             @"(?<variable>\S+)" +
                             @"\s+=\s+" +
                             @"(?<value>\S+)" +
                             @"(" +
                               @"(\s+from\s+)(?<date1>\S+)" +
                               @"(\s+to\s+)(?<date2>\S+)" +
                             @")?";

            Regex r = new Regex(pattern);
            Match m = r.Match(descriptor);
            if (!m.Success)
                throw new Exception("Invalid report variable found: " + descriptor);
            GroupCollection c = r.Match(descriptor).Groups;

            bool longterm = c["longterm"].Value == "longterm";
            string variableNameToSet = c["variable"].Value;
            string dataColumnName = c["value"].Value;
            string date1String = c["date1"].Value;
            string date2String = c["date2"].Value;
            DateTime date1;
            DateTime.TryParse(date1String, CultureInfo.InvariantCulture, DateTimeStyles.None, out date1);
            DateTime date2;
            DateTime.TryParse(date2String, CultureInfo.InvariantCulture, DateTimeStyles.None, out date2);
            string value = null;

            DataTable dataForConstant;
            if (data.Rows.Count > 0 && data.Columns.Contains(dataColumnName))
                dataForConstant = data;
            else if (date1 != DateTime.MinValue && date2 != DateTime.MinValue)
                dataForConstant = getDataFromInput(date1, date2);
            else
            {
                // assume the data column name is actually a value.
                value = dataColumnName;
                dataColumnName = null;
                dataForConstant = null;
            }

            patchVariables.Add(new PatchVariable(variableNameToSet, dataColumnName, value, longterm, date1, date2, dataForConstant, Paddock));
        }
    }

    /// <summary>Gets the data from input/met model</summary>
    /// <param name="date1">The first date to get data for</param>
    /// <param name="date2">The last date to get data for</param>
    /// <returns>The data returned from INPUT</returns>
    /// <exception cref="System.Exception">Cannot get data from INPUT or MET</exception>
    private DataTable getDataFromInput(DateTime date1, DateTime date2)
    {
        // See if we can use data that we've already requested.
        if (this.dataTableFromInput != null && this.dataTableFromInput.Rows.Count > 0)
        {
            DateTime firstDateOfData = (DateTime) this.dataTableFromInput.Rows[0][0];
            DateTime lastDateOfData = (DateTime)this.dataTableFromInput.Rows[this.dataTableFromInput.Rows.Count-1][0];
            if (date1 == firstDateOfData && date2 == lastDateOfData)
                return this.dataTableFromInput;
        }

        // Get the data we need from INPUT by calling Input.getData
        List<string> dateStrings = new List<string>();
        for (DateTime date = date1; date <= date2; date = date.AddDays(1))
            dateStrings.Add(date.ToString("yyyy/MM/dd"));
        if (getData == null)
            throw new Exception("Cannot get data from INPUT or MET");
        StringArrayType strings = new StringArrayType(dateStrings.ToArray());
        weAreRequestingData = true;
        dataTableFromInput = null;
        getData.Invoke(strings);
        weAreRequestingData = false;

        if (dataTableFromInput.Rows.Count == 0)
            dataTableFromInput = null;
        return dataTableFromInput;
    }

    /// <summary>Input model has sent out data</summary>
    /// <param name="dailyData">The daily data that we asked for</param>
    [EventHandler]
    public void OnReturnData(NewMetArrayType t)
    {
        if (weAreRequestingData)
        {
            dataTableFromInput = new DataTable();
            dataTableFromInput.Columns.Add("Date", typeof(DateTime));
            dataTableFromInput.Columns.Add("MaxT", typeof(double));
            dataTableFromInput.Columns.Add("MinT", typeof(double));
            dataTableFromInput.Columns.Add("Radn", typeof(double));
            dataTableFromInput.Columns.Add("Rain", typeof(double));
            dataTableFromInput.Columns.Add("VP", typeof(double));
            if (t.element != null)
            {
                foreach (NewMetArrayelementType metData in t.element)
                {
                    DataRow newRow = dataTableFromInput.NewRow();
                    newRow["Date"] = DateUtility.JulianDayNumberToDateTime((int)Math.Truncate(metData.today));
                    newRow["MaxT"] = metData.maxt;
                    newRow["MinT"] = metData.mint;
                    newRow["Radn"] = metData.radn;
                    newRow["Rain"] = metData.rain;
                    newRow["VP"] = metData.vp;
                    dataTableFromInput.Rows.Add(newRow);
                }
            }
        }
         
    }

}

