using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
//using ModelFramework;
using System.Runtime.InteropServices;
using System.Xml;
using CSGeneral;
using System.IO;
#if __MonoCS__
using Mono.Data.Sqlite;
#else
using System.Data.SQLite;
#endif
using System.Data;
using ModelFramework;

public class Variables
{
    [Param]
    public string[] Variable = null;
}

public class ReportDb
{
#if __MonoCS__
    private SqliteConnection Connection;
    private SqliteCommand InsertCommand;
    private SqliteTransaction InsertTransaction = null;
#else
    private SQLiteConnection Connection;
    private SQLiteCommand InsertCommand;
    private SQLiteTransaction InsertTransaction = null;
#endif
    private string FileName = "Output.db";
    private int SimulationID = -1;

    [Input]
    private DateTime Today;

    [Input(IsOptional=true)]
    string Title = null;

    [Link]
    Variables Variables = null;
    //[Param]
    //string[] OutputFrequencys = null;

    [Link]
    Paddock Paddock = null;

    /// <summary>
    /// Constructor
    /// </summary>
    public ReportDb()
    {

    }

    /// <summary>
    /// Destructor. Close our DB connection.
    /// </summary>
    ~ReportDb()
    {
        if (Connection != null)
        {
            if (InsertTransaction != null)
                InsertTransaction.Commit();
            Connection.Close();
        }
    }

    /// <summary>
    /// We have been initialised - create .db file.
    /// </summary>
    [EventHandler]
    public void OnInitialised()
    {
#if __MonoCS__
        Connection = new SqliteConnection("Data Source=" + FileName + ";Version=3;New=False;Compress=True;");
        Connection.Open();
        SqliteCommand sql_cmd = new SqliteCommand(Connection);
#else
        Connection = new SQLiteConnection("Data Source=" + FileName + ";Version=3;New=False;Compress=True;");
        Connection.Open();
        SQLiteCommand sql_cmd = new SQLiteCommand(Connection);
#endif
        if (Title == null)
        {
            Component Simulation = Paddock.LinkByType("Simulation") as Component;
            if (Simulation != null)
                Simulation.Get("Title", out Title);
        }

        using (SQLiteTransaction dbTrans = Connection.BeginTransaction())
        {
            // Make sure we have a Simulations table.
            if (!TableNames.Contains("Simulations"))
            {
                sql_cmd = Connection.CreateCommand();
                sql_cmd.CommandText = "CREATE TABLE Simulations (ID INTEGER PRIMARY KEY ASC, Title TEXT)"; ;
                sql_cmd.ExecuteNonQuery();
            }
            dbTrans.Commit();
        }

        using (SQLiteTransaction dbTrans = Connection.BeginTransaction())
        {
            // Make sure we have a Data table.
            if (!TableNames.Contains("Data"))
            {
                string Cmd = "CREATE TABLE Data (SimulationID INTEGER)";
                sql_cmd = Connection.CreateCommand();
                sql_cmd.CommandText = Cmd;
                sql_cmd.ExecuteNonQuery();
            }
            dbTrans.Commit();
        }

        // Try and get our SimulationID. If it's not in the Simulations table
        // then add it. The title is used as the simulation name.
        SimulationID = GetSimulationID();
        if (SimulationID == -1)
        {
            sql_cmd.CommandText = "INSERT INTO [Simulations] (Title) VALUES ('" + Title + "')";
            sql_cmd.ExecuteNonQuery();
            SimulationID = GetSimulationID();
        }

        // Remove existing data for this simulation.
        sql_cmd.CommandText = "DELETE FROM [Data] WHERE SimulationID = " + SimulationID.ToString();
        sql_cmd.ExecuteNonQuery();
        SimulationID = GetSimulationID();

        // Give Sqlite some commands to speed up the adding of data.
        sql_cmd = Connection.CreateCommand();
        sql_cmd.CommandText = "PRAGMA synchronous=OFF";
        sql_cmd.ExecuteNonQuery();
    }

    /// <summary>
    /// Get a simulation ID for our Title. 
    /// </summary>
    /// <returns></returns>
    private int GetSimulationID()
    {
#if __MonoCS__
        SqliteCommand Command = new SqliteCommand(Connection);
        Command.CommandText = "SELECT ID FROM Simulations WHERE Title='" + Title + "'";
        SqliteDataReader Reader = Command.ExecuteReader();
#else
        SQLiteCommand Command = new SQLiteCommand(Connection);
        Command.CommandText = "SELECT ID FROM Simulations WHERE Title='" + Title + "'";
        SQLiteDataReader Reader = Command.ExecuteReader();
#endif
        if (Reader.Read())
            return Reader.GetInt32(0);
        else
            return -1;
    }

    /// <summary>
    /// Returns a list of table names.
    /// </summary>
    public List<string> TableNames
    {
        get
        {
#if __MonoCS__
            SqliteCommand Command = new SqliteCommand(Connection);
            Command.CommandText = "SELECT name FROM sqlite_master WHERE type='table'";
            SqliteDataReader Reader = Command.ExecuteReader();
#else
            SQLiteCommand Command = new SQLiteCommand(Connection);
            Command.CommandText = "SELECT name FROM sqlite_master WHERE type='table'";
            SQLiteDataReader Reader = Command.ExecuteReader();
#endif
            List<string> TableNames = new List<string>();
            while (Reader.Read())
            {
                TableNames.Add(Reader.GetString(0));
            }
            return TableNames;
        }
    }

    /// <summary>
    /// Returns a list of table names.
    /// </summary>
    public SortedSet<string> DataFieldNames
    {
        get
        {
#if __MonoCS__
            SqliteCommand Command = new SqliteCommand(Connection);
            Command.CommandText = "PRAGMA table_info (Data)";
            SqliteDataReader Reader = Command.ExecuteReader();
#else
            SQLiteCommand Command = new SQLiteCommand(Connection);
            Command.CommandText = "PRAGMA table_info (Data)";
            SQLiteDataReader Reader = Command.ExecuteReader();
#endif
            SortedSet<string> FieldNames = new SortedSet<string>(StringComparer.CurrentCultureIgnoreCase);
            while (Reader.Read())
            {
                FieldNames.Add(Reader.GetString(1));
            }
            return FieldNames;
        }
    }

    /// <summary>
    /// Return an SQLite data type string for the specified value.
    /// </summary>
    private string GetDataType(object Value)
    {
        string ColumnType = null;
        if (Value == null || Value.GetType().ToString() == "System.Double")
            ColumnType = "real";
        else if (Value.GetType().ToString() == "System.DateTime")
            ColumnType = "date";
        else if (Value.GetType().ToString() == "System.Int32")
            ColumnType = "integer";
        else if (Value.GetType().ToString() == "System.Single")
            ColumnType = "real";
        else
            ColumnType = "char(50)";
        return ColumnType;
    }

    /// <summary>
    ///  Go prepare an insert into query and return the query.
    /// </summary>
    private void PrepareInsertCommandForDataTable(List<KeyValuePair<string, object>> Values)
    {
        if (InsertCommand == null || InsertCommand.Parameters.Count != Values.Count)
        {
#if __MonoCS__
            InsertCommand = new SqliteCommand(Connection);
#else
            InsertCommand = new SQLiteCommand(Connection);
#endif
            string Cmd = "INSERT INTO [" + "Data" + "] (";
            for (int i = 0; i < Values.Count; i++)
            {
                if (i > 0)
                    Cmd += ",";
                Cmd += "[" + Values[i].Key + "]";
#if __MonoCS__
                InsertCommand.Parameters.Add(new SqliteParameter(Values[i].Key));
#else
                InsertCommand.Parameters.Add(new SQLiteParameter(Values[i].Key));
#endif
            }
            Cmd += ") VALUES (";

            for (int i = 0; i < Values.Count; i++)
            {
                if (i > 0)
                    Cmd += ",";
                Cmd += "?";
            }
            Cmd += ")";
            InsertCommand.CommandText = Cmd;
        }
    }

    /// <summary>
    ///  Bind all parameters values to the specified query and execute the query.
    /// </summary>
    public void BindParametersAndRunQuery(List<KeyValuePair<string, object>> Variables)
    {
        for (int i = 0; i < Variables.Count; i++)
        {
            KeyValuePair<string, object> Variable = Variables[i];
            if (Variable.Value == null || Variable.Value.GetType().ToString() != "System.DateTime")
                InsertCommand.Parameters[i].Value = Variable.Value;

            else
            {
                DateTime d = (DateTime)Variable.Value;
                InsertCommand.Parameters[i].Value = d.ToString("yyyy-MM-dd hh:mm:ss");
            }
        }
        InsertCommand.ExecuteNonQuery();
    }

    /// <summary>
    /// Retrieve all values of all variables.
    /// </summary>
    private List<KeyValuePair<string, object>> GetValues()
    {
        List<KeyValuePair<string, object>> Values = new List<KeyValuePair<string, object>>();

        Values.Add(new KeyValuePair<string, object>("SimulationID", SimulationID));
        Values.Add(new KeyValuePair<string, object>("Date", Today));
        for (int i = 0; i < Variables.Variable.Length; i++)
        {
            string Value;
            Paddock.Get(Variables.Variable[i], out Value);
            ConvertVariableToValues(Variables.Variable[i], Value, Values);
        }
        return Values;
    }

    private void ConvertVariableToValues(string VariableName, string StringValue, List<KeyValuePair<string, object>> Values)
    {
        if (StringValue != "")
        {
            string[] Bits = StringValue.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            List<double> DoubleValues = new List<double>();
            foreach (string Bit in Bits)
            {
                double D;
                if (!Double.TryParse(Bit, out D))
                {
                    // Must be a string.
                    Values.Add(new KeyValuePair<string, object>(VariableName, StringValue));
                    return;
                }
                DoubleValues.Add(D);
            }

            // If we got this far then we have either a scalar double or an array of scalars.
            if (DoubleValues.Count == 1)
                Values.Add(new KeyValuePair<string, object>(VariableName, DoubleValues[0]));
            else
            {
                // Array of doubles. Create an entry in Values for each array member.
                for (int i = 0; i < DoubleValues.Count; i++)
                {
                    string VariableNameFull = VariableName + "(" + (i + 1).ToString() + ")";
                    Values.Add(new KeyValuePair<string, object>(VariableNameFull, DoubleValues[i]));
                }
            }
        }
        else
            Values.Add(new KeyValuePair<string, object>(VariableName, null));
    }

    private void EnsureDataTableHasCorrectFields(List<KeyValuePair<string, object>> Variables)
    {
#if __MonoCS__
        SqliteTransaction AlterTransaction = null;
#else
        SQLiteTransaction AlterTransaction = null;
#endif

        // Work out which fields we need to add to the Data table.
        SortedSet<string> ExistingFieldNames = DataFieldNames;
        foreach (KeyValuePair<string, object> Variable in Variables)
        {
            if (!ExistingFieldNames.Contains(Variable.Key))
            {
                if (AlterTransaction == null)
                {
                    if (InsertTransaction != null)
                    {
                        InsertTransaction.Commit();
                        InsertTransaction = null;
                    }
                    AlterTransaction = Connection.BeginTransaction();
                }

                string SQL = "ALTER TABLE Data ADD [" + Variable.Key + "] " + GetDataType(Variable.Value);
#if __MonoCS__
                SqliteCommand Command = Connection.CreateCommand();
#else
                SQLiteCommand Command = Connection.CreateCommand();
#endif
                Command.CommandText = SQL;
                Command.ExecuteNonQuery();
            }
        }

        if (AlterTransaction != null)
            AlterTransaction.Commit();
    }

    /// <summary>
    /// Daily timestep handler.
    /// </summary>
    [EventHandler]
    public void OnHarvesting()
    {
        List<KeyValuePair<string, object>> Values = GetValues();

        EnsureDataTableHasCorrectFields(Values);

        if (InsertTransaction == null)
            InsertTransaction = Connection.BeginTransaction();

        PrepareInsertCommandForDataTable(Values);

        BindParametersAndRunQuery(Values);
    }



}
