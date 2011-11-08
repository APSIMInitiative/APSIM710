using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Data;
using CSGeneral;

class Report
   {
   [Param] private string FileName = "";

   [Input] private ModelAPIInterface ScienceAPI = null;
   private SQLite Connection;
   private IntPtr Query;
   [Param] private string[] Variables = null;

   /// <summary>
   /// Destructor. Close our DB connection.
   /// </summary>
   ~Report()
      {
      if (Connection != null)
         {
         Connection.Finalize(Query);
         if (Connection.IsOpen)
            {
            Connection.ExecuteNonQuery("COMMIT");
            Connection.CloseDatabase();
            }
         }
      }

   /// <summary>
   ///  Go create a SQLite file with appropriate tables.
   /// </summary>
   private void CreateTable(SQLite Connection, string TableName, string[] Names, List<object> Values)
      {
      string Cmd = "CREATE TABLE " + TableName + "(";

      for (int i = 0; i < Names.Length; i++)
         {
         string ColumnType = null;
         if (Values[i].GetType().ToString() == "System.DateTime")
            ColumnType = "date";
         else if (Values[i].GetType().ToString() == "System.Int32")
            ColumnType = "integer";
         else if (Values[i].GetType().ToString() == "System.Single")
            ColumnType = "real";
         else if (Values[i].GetType().ToString() == "System.Double")
            ColumnType = "real";
         else
            ColumnType = "char(50)";

         if (i > 0)
            Cmd += ",";
         Cmd += "[" + Names[i] + "] " + ColumnType;
         }
      Cmd += ")";
      Connection.ExecuteNonQuery(Cmd);
      }

   /// <summary>
   ///  Go prepare an insert into query and return the query.
   /// </summary>
   private IntPtr PrepareInsertIntoTable(SQLite Connection, string TableName, string[] Names)
      {
      string Cmd = "INSERT INTO " + TableName + "(";

      for (int i = 0; i < Names.Length; i++)
         {
         if (i > 0)
            Cmd += ",";
         Cmd += "[" + Names[i] + "]";
         }
      Cmd += ") VALUES (";

      for (int i = 0; i < Names.Length; i++)
         {
         if (i > 0)
            Cmd += ",";
         Cmd += "?";
         }
      Cmd += ")";
      return Connection.Prepare(Cmd);
      }

   [EventHandler]
   public void OnReport()
      {
      // Get all variable values.
      List<object> OutputValues = new List<object>();
      foreach (string VariableName in Variables)
         OutputValues.Add(ScienceAPI.Get(VariableName));
      
      if (Connection == null)
         {
         if (File.Exists(FileName))
            File.Delete(FileName);

         Connection = new SQLite();
         Connection.OpenDatabase(FileName);
         Connection.ExecuteNonQuery("PRAGMA synchronous=OFF");
         Connection.ExecuteNonQuery("BEGIN");
         CreateTable(Connection, "test", Variables, OutputValues);
         Query = PrepareInsertIntoTable(Connection, "test", Variables);
         }
      Connection.BindParametersAndRunQuery(Query, OutputValues);
      }
   }
