using System;
using System.Collections.Generic;
using System.Text;
using System.Data;
using System.IO;
using FirebirdSql.Data.FirebirdClient;
using FirebirdSql.Data;
using CMPServices;

namespace outputComp
{
    //============================================================================
    /// <summary>
    /// Firebird database interface.
    /// </summary>
    //============================================================================
    public class TFBDataModule : TAFDataModule
    {
        private const string SERV_STANDALONE = "ServerType = 0";
        private const string SERV_EMBEDED = "ServerType = 1";

        private string FServerType;
        private FbConnection fbConn;
        private FbDataReader reader;

        //============================================================================
        /// <summary>
        /// Constructor.
        /// </summary>
        /// <param name="sDBName">The full path to the database.</param>
        //============================================================================
        public TFBDataModule(string sDBName)
            : base(sDBName)
        {
            FServerType = SERV_EMBEDED;
            CreateDBComponents();
            reader = null;
        }
        //============================================================================
        /// <summary>
        /// Sets up the connection string and creates the connection object.
        /// </summary>
        //============================================================================
        private void CreateDBComponents()
        {

            string connectionString =
                FServerType + ";" +            //1=embeded server 0=standalone server
                "User=SYSDBA;" +
                "Password=masterkey;" +
                "Database=" + DBName + ";" +
                "DataSource=localhost;" +
                "Port=3050;" +
                "Dialect=3;" +
                "Charset=NONE;" +
                "Role=;" +
                "Connection lifetime=15;" +
                "Pooling=true;" +
                "Packet Size=8192";

            fbConn = new FbConnection(connectionString);
        }
        //============================================================================
        /// <summary>
        /// Create the Firebird database
        /// </summary>
        /// <param name="dbName"></param>
        /// <returns>True if it was created</returns>
        //============================================================================
        protected bool CreateBlankDB(string dbName)
        {
            bool created = false;
            if (!File.Exists(dbName))
            {
                FbConnection.CreateDatabase("Database=" + dbName + ";" + FServerType);
                FbConnection.ClearAllPools();
                created = true;
            }
            return created;
        }
        //============================================================================
        /// <summary>
        /// Open the connection if it is not already open.
        /// </summary>
        //============================================================================
        public void openConnection()
        {
            if (fbConn.State != ConnectionState.Open)
                fbConn.Open();
        }
        //============================================================================
        /// <summary>
        /// Closes the reader and the connection.
        /// </summary>
        //============================================================================
        public override void closeConnection()
        {
            if (reader != null)
                reader.Close();
            fbConn.Close();
            FbConnection.ClearAllPools();
        }
        //============================================================================
        /// <summary>
        /// Executes an INSERT, DELETE or UPDATE.
        /// </summary>
        /// <param name="SQLtext"></param>
        /// <returns>Number of rows affected.</returns>
        //============================================================================
        public override int ExecSQL(string SQLtext)
        {
            FbTransaction transaction = fbConn.BeginTransaction();
            FbCommand command = new FbCommand(SQLtext, fbConn, transaction);

            openConnection();
            int rows = command.ExecuteNonQuery();
            transaction.Commit();
            return rows;
        }
        //============================================================================
        /// <summary>
        /// Used for SELECT queries.
        /// </summary>
        /// <param name="SQLtext"></param>
        /// <returns>Rows read</returns>
        //============================================================================
        public override int RunSQL(string SQLtext)
        {
            FbTransaction transaction = fbConn.BeginTransaction();
            FbCommand command = new FbCommand(SQLtext, fbConn, transaction);

            openConnection();
            reader = command.ExecuteReader();
            transaction.Commit();
            return reader.RecordsAffected;
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
        /// Create the tables for the database.
        /// </summary>
        /// <returns>True if the process works.</returns>
        //============================================================================
        public override bool createTables()
        {
            string simulation = "CREATE TABLE Simulation " +
                  "(" +
                  "sim_name VARCHAR(255) NOT NULL, " +  // Text field 255 characters long
                  "sim_file VARCHAR(255), " +           // Text field 255 characters long
                  "appl_exe_name VARCHAR(255), " +      // Text field 255 characters long
                  "execute_date TIMESTAMP, " +          // DateTime field
                  "tactical SMALLINT, " +
                  "CONSTRAINT SIMNAME PRIMARY KEY (sim_name)" +
                  ")";

            string simrun = "CREATE TABLE SimRun " +
                  "(" +
                  "run_id SMALLINT NOT NULL, " +    // int
                  "run_descr VARCHAR(255), " +
                  "sim_name VARCHAR(255), " +       // Text field 40 characters
                  "start_date TIMESTAMP, " +        // DateTime field
                  "end_date TIMESTAMP, " +          // DateTime field
                  "CONSTRAINT RUNID PRIMARY KEY (run_id)" +
                  ")";

            string attr = "CREATE TABLE Attribute " +
                  "(" +
                  "attr_id SMALLINT NOT NULL, " + // int
                  "attr_name VARCHAR(255), " +    // Text field 255 characters long
                  "parent_id SMALLINT, " +        // int
                  "attr_index SMALLINT, " +       // int
                  "attr_type SMALLINT, " +        // int
                  "attr_alias VARCHAR(255), " +   // Text field 255 characters long
                  "attr_aggreg VARCHAR(6), " +    // Text field 6 characters long
                  "decplaces SMALLINT, " +        // int
                  "units VARCHAR(20), " +         // Text field 20 characters long
                  "CONSTRAINT ATTRID PRIMARY KEY (attr_id)" +
                  ")";

            string attrval = "CREATE TABLE AttrValues " +
                  "(" +
                  "run_id SMALLINT NOT NULL, " +          // int
                  "event_time TIMESTAMP NOT NULL, " +     // DateTime field
#if DEBUG_WITH_STRINGS
                  "string_val VARCHAR(255), " +     //debug with strings
#else
                  "blob_val BLOB, " +                     // TTypedValue
#endif
                  "CONSTRAINT EVRUN PRIMARY KEY (run_id, event_time)" +
                  ")";

            try
            {
                bool created = CreateBlankDB(DBName);
                closeConnection();
                openConnection();
                if (!created)   //if not created then remove old tables
                {
                    ExecSQL("drop table Simulation;");
                    ExecSQL("drop table SimRun;");
                    ExecSQL("drop table Attribute;");
                    ExecSQL("drop table AttrValues;");
                }
                //create tables
                ExecSQL(simulation);
                ExecSQL(simrun);
                ExecSQL(attr);
                ExecSQL(attrval);
                closeConnection();
            }
            catch (Exception excep)
            {
                throw (new ApplicationException(excep.Message));
            }

            return true;
        }
        //============================================================================
        /// <summary>
        /// Reads the next row of the result set.
        /// </summary>
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
        /// Get the value at the column as a string.
        /// </summary>
        /// <param name="col">Column number 0->n</param>
        /// <returns>Value for this column and current record.</returns>
        //============================================================================
        public override string ColAsString(int col)
        {
            string result = "";
            if (reader != null)
                result = reader.GetString(col);
            return result;
        }
        //============================================================================
        /// <summary>
        /// Get the value at the column as an integer
        /// </summary>
        /// <param name="col">Column number 0->n</param>
        /// <returns>Value for this column and current record.</returns>
        //============================================================================
        public override int ColAsInt(int col)
        {
            int result = 0;
            if (reader != null)
                result = reader.GetInt32(col);
            return result;
        }
        //============================================================================
        /// <summary>
        /// 
        /// </summary>
        /// <param name="col"></param>
        /// <returns></returns>
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
        /// 
        /// </summary>
        /// <param name="col"></param>
        /// <returns></returns>
        //============================================================================
        public override byte[] ColAsBytes(int col)
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
        /// 
        /// </summary>
        /// <param name="col"></param>
        /// <returns></returns>
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
        /// Return a string representaion of the bool value.
        /// </summary>
        /// <param name="truth"></param>
        /// <returns>true=1 false=0</returns>
        //============================================================================
        public override string BoolAsString(bool truth)
        {
            if (truth)
                return "1";
            else
                return "0";
        }
        //============================================================================
        /// <summary>
        /// Adds the value record to the AttrValues table for this timestep. 
        /// </summary>
        /// <param name="RunID">Simulation run id.</param>
        /// <param name="eventTime">Record event time.</param>
        /// <param name="data">Byte array containing the TTypedValue data.</param>
        /// <param name="line">The values as a text string. Only used for DEBUG_WITH_STRINGS</param>
        //============================================================================
        public override void AddAttrValueRecord(int RunID, DateTime eventTime, Byte[] data, String line)
        {
            // This operation should be as fast as possible.
            try
            {
                FbDataAdapter valueDA = new FbDataAdapter();
                FbTransaction txn = fbConn.BeginTransaction();

                valueDA.MissingSchemaAction = MissingSchemaAction.AddWithKey;

#if DEBUG_WITH_STRINGS
                valueDA.InsertCommand = new FbCommand("INSERT INTO AttrValues (run_id, event_time, string_val) " +
                                                        "VALUES (?, ?, ?)");
#else
                    valueDA.InsertCommand = new FbCommand("INSERT INTO AttrValues (run_id, event_time, string_val) " +
                                                            "VALUES (?, ?, ?)");
#endif
                valueDA.InsertCommand.Parameters.Add("@runid", FbDbType.Integer, 4, "run_id").Value = RunID;
                valueDA.InsertCommand.Parameters.Add("@eventtime", FbDbType.Date, 8, "event_time").Value = eventTime;
#if DEBUG_WITH_STRINGS
                valueDA.InsertCommand.Parameters.Add("@stringval", FbDbType.VarChar, line.Length, "string_val").Value = line;
#else
                valueDA.InsertCommand.Parameters.Add("@blobval", FbDbType.Binary, data.Length, "blob_val").Value = data;
#endif
                //now write the values
                valueDA.InsertCommand.Connection = fbConn;
                valueDA.InsertCommand.Transaction = txn;
                openConnection();

                valueDA.InsertCommand.ExecuteNonQuery();
                txn.Commit();
            }
            catch (Exception excep)
            {
                throw (new ApplicationException(excep.Message));
            }
        }
        //============================================================================
        /// <summary>
        /// The quotation mark to use in the sql statements.
        /// </summary>
        //============================================================================
        public override char QuotationMark
        {
            get { return '\"'; }
        }
    }
}


