using System;
using System.Collections.Generic;
using System.Text;
using System.Data;
using CMPServices;

namespace outputComp
{
    //============================================================================
    /// <summary>
    /// AusFarm Data Module.
    /// Ancestor class to a data module that could be an interface to a Firebird, 
    /// Interbase, MSAccess, SQL Server or any other database.
    /// </summary>
    //============================================================================
    public abstract class TAFDataModule
    {
        /// <summary>
        /// The attribute type for a record attribute.
        /// </summary>
        public static int RECORD_NODE = 999;
        /// <summary>
        /// The attribute type for an array node.
        /// </summary>
        public static int ARRAY_NODE  = 99;

        /// <summary>
        /// Constant used to identify which type of database to use.
        /// </summary>
        public static string DB_PARADOX = "Paradox";
        /// <summary>
        /// Constant used to identify which type of database to use.
        /// </summary>
        /// <summary>
        /// Constant used to identify which type of database to use.
        /// </summary>
        public static string DB_MSACCESS = "MS Access";
        /// <summary>
        /// Constant used to identify which type of database to use.
        /// </summary>
        public static string DB_INTERBASE = "Interbase";
        /// <summary>
        /// Constant used to identify which type of database to use.
        /// </summary>
        public static string DB_FIREBIRD = "Interbase";

        /// <summary>
        /// The attribute id used for a date column/field.
        /// </summary>
        public static int TIMECOL = 999999;
        /// <summary>
        /// The name of the date column.
        /// </summary>
        public static string TIMECOLNAME = "Date";   //refer to defs.DATETIMECOLNAME

        //============================================================================
        /// <summary>
        /// Output array for each timestep. Filled with data from a blob field.
        /// Defined for use in AusFarm.
        /// </summary>
        //============================================================================
        public static string typeOutput = "<type name=\"result\" array=\"F\">" +
                                            "<field name=\"real\" array=\"T\">" +
                                              "<element>" +
                                                "<field name=\"id\"    kind=\"integer2\"/>" +
                                                "<field name=\"value\" kind=\"single\"/>" +
                                              "</element>" +
                                            "</field>" +
                                            "<field name=\"string\" array=\"T\">" +
                                            "<element>" +
                                             "<field name=\"id\"    kind=\"integer2\"/>" +
                                             "<field name=\"value\" kind=\"string\"/>" +
                                            "</element>" +
                                          "</field>" +
                                        "</type>";

        private string FDBName;
        private TDDMLValue resultArray;
        //============================================================================
        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="sDBName">Full path to the database.</param>
        //============================================================================
        public TAFDataModule(string sDBName)
        {
            DBName = sDBName;
            resultArray = new TDDMLValue(typeOutput, "");
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        //============================================================================
        public virtual bool createTables()
        {
            return true;
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="SQLtext"></param>
        /// <returns>Rows</returns>
        //============================================================================
        public abstract int RunSQL(string SQLtext);
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="SQLtext"></param>
        /// <returns>Rows affected if using INSERT, UPDATE or DELETE</returns>
        //============================================================================
        public abstract int ExecSQL(string SQLtext);
        //============================================================================
        /// <summary>
        /// Reads the next row of the result set.
        /// </summary>
        //============================================================================
        public abstract Boolean Read();
        //============================================================================
        /// <summary>
        /// Close the reader after a reading process is complete.
        /// </summary>
        //============================================================================
        public abstract void CloseReader();
        //============================================================================
        /// <summary>
        /// Close the database connection.
        /// </summary>
        //============================================================================
        public abstract void closeConnection();
        //============================================================================
        /// <summary>
        /// Add an attribute record to the Attributes table.
        /// </summary>
        /// <param name="iAttrId"></param>
        /// <param name="iParentID"></param>
        /// <param name="iIndex"></param>
        /// <param name="iTypeCode"></param>
        /// <param name="iDecPl"></param>
        /// <param name="sFullName"></param>
        /// <param name="sAlias"></param>
        /// <param name="sAggr"></param>
        /// <param name="sUnits"></param>
        /// <returns>Rows affected</returns>
        //============================================================================
        public virtual int AddAttribute(int iAttrId, int iParentID, int iIndex, int iTypeCode, int iDecPl, string sFullName, string sAlias, string sAggr, string sUnits)
        {
            string s = "INSERT INTO Attribute (attr_id, attr_name, parent_id, attr_index, attr_type, attr_alias, attr_aggreg, decplaces, units) " +
                        "VALUES (" + iAttrId.ToString() + "," +
                        "'" + sFullName + "'," +
                        iParentID.ToString() + "," +
                        iIndex.ToString() + "," +
                        iTypeCode.ToString() + "," +
                        "'" + sAlias + "'," +
                        "'" + sAggr + "'," +
                        iDecPl.ToString() + "," +
                        "'" + sUnits + "')";
            return ExecSQL(s);
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="RunID"></param>
        /// <param name="eventTime"></param>
        /// <param name="data"></param>
        /// <param name="line"></param>
        //============================================================================
        public virtual void AddAttrValueRecord(int RunID, DateTime eventTime, Byte[] data, String line)
        {
        }
        //============================================================================
        /// <summary>
        /// Get the value at the column as a string.
        /// </summary>
        /// <param name="col">Column number 0->n</param>
        /// <returns>String value for this column.</returns>
        //============================================================================
        public abstract string ColAsString(int col);
        //============================================================================
        /// <summary>
        /// Get the value at the column as a string.
        /// </summary>
        /// <param name="col">Column number 0->n</param>
        /// <returns></returns>
        //============================================================================
        public abstract int ColAsInt(int col);
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="col">Column number 0->n</param>
        /// <returns></returns>
        //============================================================================
        public abstract double ColAsFloat(int col);
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="col">Column number 0->n</param>
        /// <returns></returns>
        //============================================================================
        public abstract DateTime ColAsDate(int col);
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="col"></param>
        /// <returns></returns>
        //============================================================================
        public abstract Byte[] ColAsBytes(int col);
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        //============================================================================
        public string DBName {
            get {return FDBName;}
            set {FDBName = value;}
        }
        //============================================================================
        /// <summary>
        /// Return the string representation of the bool value
        /// </summary>
        /// <param name="truth"></param>
        /// <returns>T/F</returns>
        //============================================================================
        public virtual string BoolAsString(bool truth)
        {
            return truth.ToString();
        }
        //============================================================================
        /// <summary>
        /// Return a date string.
        /// </summary>
        /// <param name="when"></param>
        /// <returns>yyyy-mm-dd hh:nn:ss</returns>
        //============================================================================
        public virtual string DateAsString(DateTime when)
        {
            return String.Format("{0, 4:d4}-{1, 02:d2}-{2, 02:d2} {3, 02:d2}:{4, 02:d2}:{5, 02:d2}",
                                 when.Year, when.Month, when.Day, when.Hour, when.Minute, when.Second);
        }
        //============================================================================
        /// <summary>
        /// The quotation character used in sql statements.
        /// </summary>
        //============================================================================
        public abstract Char QuotationMark
        {
            get;
        }

    }
}
