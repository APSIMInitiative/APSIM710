using System;
using System.Text;
using System.Threading;
using System.Collections;
using System.Runtime.InteropServices;
using System.Data;
using System.Collections.Generic;


   /// <summary>
   /// A .NET wrapper around the SQLite library
   /// </summary>

   public class SQLiteException : Exception
      {
      public SQLiteException(string message) :
         base(message)
         {

         }
      }

   public class SQLite
      {
      const int SQLITE_OK = 0;
      const int SQLITE_ROW = 100;
      const int SQLITE_DONE = 101;
      const int SQLITE_INTEGER = 1;
      const int SQLITE_FLOAT = 2;
      const int SQLITE_TEXT = 3;
      const int SQLITE_BLOB = 4;
      const int SQLITE_NULL = 5;

      #region Externals
      [DllImport("sqlite3.dll", EntryPoint = "sqlite3_open", CallingConvention = CallingConvention.Cdecl)]
      static extern int sqlite3_open(string filename, out IntPtr db);

      [DllImport("sqlite3.dll", EntryPoint = "sqlite3_close", CallingConvention = CallingConvention.Cdecl)]
      static extern int sqlite3_close(IntPtr db);

      [DllImport("sqlite3.dll", EntryPoint = "sqlite3_prepare_v2", CallingConvention = CallingConvention.Cdecl)]
      static extern int sqlite3_prepare_v2(IntPtr db, string zSql,
          int nByte, out IntPtr ppStmpt, IntPtr pzTail);

      [DllImport("sqlite3.dll", EntryPoint = "sqlite3_step", CallingConvention = CallingConvention.Cdecl)]
      static extern int sqlite3_step(IntPtr stmHandle);

      [DllImport("sqlite3.dll", EntryPoint = "sqlite3_finalize", CallingConvention = CallingConvention.Cdecl)]
      static extern int sqlite3_finalize(IntPtr stmHandle);

      [DllImport("sqlite3.dll", EntryPoint = "sqlite3_errmsg", CallingConvention = CallingConvention.Cdecl)]
      static extern string sqlite3_errmsg(IntPtr db);

      [DllImport("sqlite3.dll", EntryPoint = "sqlite3_column_count", CallingConvention = CallingConvention.Cdecl)]
      static extern int sqlite3_column_count(IntPtr stmHandle);

      [DllImport("sqlite3.dll", EntryPoint = "sqlite3_column_origin_name", CallingConvention = CallingConvention.Cdecl)]
      static extern string sqlite3_column_origin_name(
          IntPtr stmHandle, int iCol);

      [DllImport("sqlite3.dll", EntryPoint = "sqlite3_column_type", CallingConvention = CallingConvention.Cdecl)]
      static extern int sqlite3_column_type(IntPtr stmHandle, int iCol);

      [DllImport("sqlite3.dll", EntryPoint = "sqlite3_column_int", CallingConvention = CallingConvention.Cdecl)]
      static extern int sqlite3_column_int(IntPtr stmHandle, int iCol);

      [DllImport("sqlite3.dll", EntryPoint = "sqlite3_column_text", CallingConvention = CallingConvention.Cdecl)]
      static extern string sqlite3_column_text(IntPtr stmHandle, int iCol);

      [DllImport("sqlite3.dll", EntryPoint = "sqlite3_column_double", CallingConvention = CallingConvention.Cdecl)]
      static extern double sqlite3_column_double(IntPtr stmHandle, int iCol);

      [DllImport("sqlite3.dll", CallingConvention = CallingConvention.Cdecl)]
      public static extern string sqlite3_bind_double(IntPtr Query, int ParameterNumber, double Value);

      [DllImport("sqlite3.dll", CallingConvention = CallingConvention.Cdecl)]
      public static extern string sqlite3_bind_int(IntPtr Query, int ParameterNumber, int Value);

      [DllImport("sqlite3.dll", CallingConvention = CallingConvention.Cdecl)]
      public static extern string sqlite3_bind_text(IntPtr Query, int ParameterNumber, string Value, int n, IntPtr CallBack);

      [DllImport("sqlite3.dll", CallingConvention = CallingConvention.Cdecl)]
      public static extern int sqlite3_reset(IntPtr Query);
      #endregion

      private IntPtr _db; //pointer to SQLite database
      private bool _open; //whether or not the database is open

      /// <summary>
      /// Property to return true if the database is open.
      /// </summary>
      public bool IsOpen { get { return _open; } }

      /// <summary>
      /// Opens or creates SQLite database with the specified path
      /// </summary>
      /// <param name="path">Path to SQLite database</param>
      public void OpenDatabase(string path)
         {
         if (sqlite3_open(path, out _db) != SQLITE_OK)
            throw new SQLiteException("Could not open database file: " + path);

         _open = true;
         }

      /// <summary>
      /// Closes the SQLite database
      /// </summary>
      public void CloseDatabase()
         {
         if (_open)
            sqlite3_close(_db);

         _open = false;
         }

      /// <summary>
      /// Executes a query that returns no results
      /// </summary>
      /// <param name="query">SQL query to execute</param>
      public void ExecuteNonQuery(string query)
         {
         if (!_open)
            throw new SQLiteException("SQLite database is not open.");

         //prepare the statement
         IntPtr stmHandle = Prepare(query);

         if (sqlite3_step(stmHandle) != SQLITE_DONE)
            throw new SQLiteException("Could not execute SQL statement.");

         Finalize(stmHandle);
         }

      /// <summary>
      /// Executes a query and stores the results in 
      /// a DataTable
      /// </summary>
      /// <param name="query">SQL query to execute</param>
      /// <returns>DataTable of results</returns>
      public DataTable ExecuteQuery(string query)
         {
         if (!_open)
            throw new SQLiteException("SQLite database is not open.");

         //prepare the statement
         IntPtr stmHandle = Prepare(query);

         //get the number of returned columns
         int columnCount = sqlite3_column_count(stmHandle);

         //create datatable and columns
         DataTable dTable = new DataTable();
         for (int i = 0; i < columnCount; i++)
            dTable.Columns.Add(sqlite3_column_origin_name(stmHandle, i));

         //populate datatable
         while (sqlite3_step(stmHandle) == SQLITE_ROW)
            {
            object[] row = new object[columnCount];
            for (int i = 0; i < columnCount; i++)
               {
               switch (sqlite3_column_type(stmHandle, i))
                  {
                  case SQLITE_INTEGER:
                     row[i] = sqlite3_column_int(stmHandle, i);
                     break;
                  case SQLITE_TEXT:
                     row[i] = sqlite3_column_text(stmHandle, i);
                     break;
                  case SQLITE_FLOAT:
                     row[i] = sqlite3_column_double(stmHandle, i);
                     break;
                  }
               }

            dTable.Rows.Add(row);
            }

         Finalize(stmHandle);

         return dTable;
         }

      /// <summary>
      /// Prepares a SQL statement for execution
      /// </summary>
      /// <param name="query">SQL query</param>
      /// <returns>Pointer to SQLite prepared statement</returns>
      public IntPtr Prepare(string query)
         {
         IntPtr stmHandle;

         if (sqlite3_prepare_v2(_db, query, query.Length,
               out stmHandle, IntPtr.Zero) != SQLITE_OK)
            throw new SQLiteException(sqlite3_errmsg(_db));

         return stmHandle;
         }

      /// <summary>
      /// Finalizes a SQLite statement
      /// </summary>
      /// <param name="stmHandle">
      /// Pointer to SQLite prepared statement
      /// </param>
      public void Finalize(IntPtr stmHandle)
         {
         if (sqlite3_finalize(stmHandle) != SQLITE_OK)
            throw new SQLiteException("Could not finalize SQL statement.");
         }

      /// <summary>
      ///  Bind all parameters values to the specified query and execute the query.
      /// </summary>
      public void BindParametersAndRunQuery(IntPtr Query, List<object> Values)
         {
         for (int i = 0; i < Values.Count; i++)
            {
            if (Values[i].GetType().ToString() == "System.DateTime")
               {
               DateTime d = (DateTime)Values[i];
               sqlite3_bind_text(Query, i + 1, d.ToString("yyyy-MM-dd hh:mm:ss"), -1, IntPtr.Zero);
               }
            else if (Values[i].GetType().ToString() == "System.Int32")
               {
               int integer = (int)Values[i];
               sqlite3_bind_int(Query, i + 1, integer);
               }
            else if (Values[i].GetType().ToString() == "System.Single")
               {
               float f = (float)Values[i];
               sqlite3_bind_double(Query, i + 1, f);
               }
            else if (Values[i].GetType().ToString() == "System.Double")
               {
               double d = (double)Values[i];
               sqlite3_bind_double(Query, i + 1, d);
               }
            else
               {
               string s = (string)Values[i];
               sqlite3_bind_text(Query, i + 1, s, -1, IntPtr.Zero);
               }

            }

         if (sqlite3_step(Query) != SQLITE_DONE)
            throw new SQLiteException("Could not execute SQL statement.");
         sqlite3_reset(Query);
         }
      }

