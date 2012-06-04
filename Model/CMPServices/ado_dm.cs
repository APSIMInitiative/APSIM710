using System;
using System.Collections.Generic;
using System.Text;
using System.Data.SqlClient;
using System.Data;
using System.Data.OleDb;
using System.IO;
using System.Threading;

namespace outputComp
{
    //============================================================================
    /// <summary>
    /// Class for reading a Microsoft Access database used  by AusFarm.
    /// </summary>
    //============================================================================
    public class TADODataModule : TAFDataModule
    {
        private OleDbConnection sqlDbConn;
        private SqlDataAdapter sqlDataAdapt;
        private OleDbDataReader reader;
        

        //============================================================================
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="sDBName">Full path to the database.</param>
        //============================================================================
        public TADODataModule(string sDBName)
            : base(sDBName)
        {
            CreateDBComponents();
            reader = null;
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        //============================================================================
        private void CreateDBComponents()
        {
            string connectionStr = "Provider=Microsoft.Jet.OLEDB.4.0;" +
                                   "Password=\"\";" +
                                   "User ID=Admin;" +
                                   "Data Source=" + DBName + ";" +
                                   "Mode=Share Deny Read|Share Deny Write;Persist Security Info=True";
            sqlDbConn = new OleDbConnection(connectionStr);
            sqlDataAdapt = new SqlDataAdapter();
        }
        //============================================================================
        /// <summary>
        /// Use ADOX to create the Access database
        /// </summary>
        /// <param name="dbName"></param>
        /// <returns></returns>
        //============================================================================
        protected bool CreateBlankDB(string dbName)
        {
            bool created = false;
            if (!File.Exists(dbName))
            {
/*                ADOX.CatalogClass cat = new ADOX.CatalogClass();

                cat.Create("Provider=Microsoft.Jet.OLEDB.4.0;" +
                       "Data Source=" + dbName + ";" +
                       "Jet OLEDB:Engine Type=5");
                
                cat = null; */
                created = true;
            }
            sqlDbConn.Close();
            return created;
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        //============================================================================
        public void openConnection()
        {
            if (sqlDbConn.State != ConnectionState.Open) 
                sqlDbConn.Open();
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        //============================================================================
        public override void closeConnection()
        {
            if (reader != null)
                reader.Close();
            sqlDbConn.Close();
            OleDbConnection.ReleaseObjectPool();
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="SQLtext"></param>
        /// <returns></returns>
        //============================================================================
        public override int ExecSQL(string SQLtext)
        {
            OleDbCommand command = new OleDbCommand(SQLtext, sqlDbConn);
            OleDbTransaction transaction = null;
            // Start a local transaction with ReadCommitted isolation level.
            transaction = sqlDbConn.BeginTransaction(IsolationLevel.ReadCommitted);
            command.Transaction = transaction;

            try
            {
                openConnection();
                command.ExecuteNonQuery();
                transaction.Commit();
            }
            catch (Exception excep)
            {
                throw (new ApplicationException(excep.Message));
            }

            return 0;
        }
        //============================================================================
        /// <summary>
        /// Execute a SELECT query on the database ready for using Read() and then
        /// each field can be accessed using ColAsString().
        /// </summary>
        /// <param name="SQLtext">SELECT statement</param>
        /// <returns>Number of rows returned.</returns>
        //============================================================================
        public override int RunSQL(string SQLtext)
        {
            OleDbCommand command = new OleDbCommand(SQLtext, sqlDbConn);
            openConnection();
            OleDbTransaction transaction = sqlDbConn.BeginTransaction();
            command.Transaction = transaction;

            reader = command.ExecuteReader();
            transaction.Commit();

            return reader.HasRows ? 1 : 0;
        }
        //============================================================================
        /// <summary>
        /// Close the reader after a reading process is complete.
        /// </summary>
        //============================================================================
        public override void CloseReader()
        {
            reader.Close();
        }
        //============================================================================
        /// <summary>
        /// Executes a Read. Must be used to advance the reader to the first record.
        /// Returns true if there are more records.
        /// </summary>
        /// <returns>True if there are more records.</returns>
        //============================================================================
        public override Boolean Read()
        {
            Boolean result = false;
            if (reader != null)
                result = reader.Read();
            return result;
        }
        //============================================================================
        /// <summary>
        /// Get the string value of this column.
        /// </summary>
        /// <param name="col">Column index 0->n</param>
        /// <returns>The value. Empty string if not found.</returns>
        //============================================================================
        public override string ColAsString(int col)
        {
            string result = "";
            if (reader != null)
            {
                if (reader.FieldCount >= col + 1)
                    result = reader.GetString(col);
            }
            return result;
        }
        //============================================================================
        /// <summary>
        /// Get the integer value of this column.
        /// </summary>
        /// <param name="col">Column index 0->n</param>
        /// <returns>The value. 0 if not found.</returns>
        //============================================================================
        public override int ColAsInt(int col)
        {
            int result = 0;
            if (reader != null)
                if (reader.FieldCount >= col + 1)
                    result = reader.GetInt32(col);
            return result;
        }
        //============================================================================
        /// <summary>
        /// Get the double value of this column.
        /// </summary>
        /// <param name="col">Column index 0->n</param>
        /// <returns>The value. 0 if not found.</returns>
        //============================================================================
        public override double ColAsFloat(int col)
        {
            double result = 0;
            if (reader != null)
            {
                if (reader.FieldCount >= col + 1)
                    result = reader.GetDouble(col); 
            }
            return result;
        }
        //============================================================================
        /// <summary>
        /// Return the value of this column as a DateTime.
        /// </summary>
        /// <param name="col">Column index 0->n</param>
        /// <returns>The date value. DateTime.MinValue if not found.</returns>
        //============================================================================
        public override DateTime ColAsDate(int col)
        {
            DateTime result = DateTime.MinValue;
            if (reader != null)
            {
                if (reader.FieldCount >= col + 1)
                    result = reader.GetDateTime(col);
            }
            return result;
        }
        //============================================================================
        /// <summary>
        /// Return the value of this column as a byte array.
        /// </summary>
        /// <param name="col">Column index 0->n</param>
        /// <returns>The byte array. byte[1] if not found.</returns>
        //============================================================================
        public override Byte[] ColAsBytes(int col)
        {
            Byte[] result = new Byte[1];
            if (reader != null)
            {
                if (reader.FieldCount >= col + 1)
                {
                    long size = reader.GetBytes(col, 0, null, 0, 0);
                    result = new Byte[size];
                    reader.GetBytes(col, 0, result, 0, (int)size);
                }
            }
            return result;
        }
        //============================================================================
        /// <summary>
        /// Access the number of fields in the result set.
        /// </summary>
        /// <returns>The count of fields.</returns>
        //============================================================================
        public int ColCount()
        {
            return reader.FieldCount;
        }
        //============================================================================
        /// <summary>
        /// Create the tables for the database.
        /// </summary>
        /// <returns>True if the process succeeds.</returns>
        //============================================================================
        public override bool createTables()
        {
            string simulation = "CREATE TABLE Simulation " +
                                "(" +
                                " sim_name Text (255) CONSTRAINT simnam PRIMARY KEY," + // Text field 255 characters long (primary key)
                                " sim_file Text (255)," +
                                " appl_exe_name Text (255)," +
                                " execute_date DATETIME," + // DateTime field
                                " tactical YesNo" +         // Boolean field
                                ")";

            string simrun = "CREATE TABLE SimRun " +
                            "(" +
                            " run_id Integer," + // long int, primary key     CONSTRAINT runid PRIMARY KEY
                            " run_descr Text (255), " +
                            " sim_name Text (255)," +  // foreign key
                            " start_date DATETIME," +  // DateTime field
                            " end_date DATETIME, " +   // DateTime field
                            " CONSTRAINT runid PRIMARY KEY (run_id)," +
                            " CONSTRAINT smsr FOREIGN KEY (sim_name) REFERENCES Simulation (sim_name)" +
                            ")";

            string attrib = "CREATE TABLE Attribute " +
                            "(" +
                            " attr_id Integer CONSTRAINT attrid PRIMARY KEY," + // long int, primary key
                            " attr_name Text (255)," +   // Text field 255 characters long
                            " parent_id Integer," +      // long int
                            " attr_index Integer," +     // long int
                            " attr_type Integer," +      // long int
                            " attr_alias Text (255)," +  // Text field 255 characters long
                // ' full_name Text (255),' +   // Text field 255 characters long
                            " attr_aggreg Text (6)," +   // Text field 6 characters long
                            " decplaces Integer," +       // long int
                            " units Text (20)" +
                            ")";

            string attribval = "CREATE TABLE AttrValues " +
                               "(" +
                               " run_id Integer," +        // long int
                               " event_time DATETIME," +   // DateTime field
#if DEBUG_WITH_STRINGS
                               " string_val Memo," +       //debug with strings
#endif
                               " blob_val OLEObject," +     //TTypedValue
                               " CONSTRAINT runid FOREIGN KEY (run_id) REFERENCES SimRun (run_id)" +
                               ")";

            try
            {
                CreateBlankDB(DBName);
                openConnection();
                ExecSQL(simulation);
                ExecSQL(simrun);
                ExecSQL(attrib);
                ExecSQL(attribval);
                sqlDbConn.Close();
            }
            catch (Exception excep)
            {
                throw (new ApplicationException(excep.Message));
            }

            return true;
        }
        //============================================================================
        /// <summary>
        /// Return the quotation mark used in sql queries.
        /// </summary>
        //============================================================================
        public override Char QuotationMark
        {
            get { return '#'; }
        }
        //============================================================================
        /// <summary>
        /// Write the attribute values record in the AttrValues table.
        /// </summary>
        /// <param name="RunID"></param>
        /// <param name="eventTime"></param>
        /// <param name="data"></param>
        /// <param name="line"></param>
        //============================================================================
        public override void AddAttrValueRecord(int RunID, DateTime eventTime, Byte[] data, String line)
        {
            //NEEDS TO BE IMPLEMENTED!!!!!!!!!!
        }
    }
}
