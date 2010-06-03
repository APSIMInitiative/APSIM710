using System;
using System.Data;
using System.Collections.Generic;

namespace CSGeneral
	{
	//------------------------------------------
	// Some utilities for loading and unloading
	// a DataTable
	// -----------------------------------------
   public class DataTableUtility
      {
      // ---------------------------------------------------
      // Add a value to the specified data table
      // ---------------------------------------------------
      static public void AddValue(DataTable Table, string ColumnName, string Value, int StartRow, int Count)
         {
         string[] Values = new string[Count];
         for (int i = 0; i != Count; i++)
            Values[i] = Value;
         AddColumn(Table, ColumnName, Values, StartRow, Count);
         }

      // ---------------------------------------------------
      // Add a value to the specified data table
      // ---------------------------------------------------
      static public void AddValue(DataTable Table, string ColumnName, double Value, int StartRow, int Count)
         {
         string[] Values = new string[Count];
         for (int i = 0; i != Count; i++)
            {
            if (Value == MathUtility.MissingValue)
               Values[i] = "";
            else
               Values[i] = Value.ToString();
            }
         AddColumn(Table, ColumnName, Values, StartRow, Count);
         }


      // ---------------------------------------------------
      // Add a column of values to the specified data table
      // ---------------------------------------------------
      static public void AddColumn(DataTable Table, string ColumnName, double[] Values, int StartRow, int Count)
         {
         if (Table.Columns.IndexOf(ColumnName) == -1)
            Table.Columns.Add(ColumnName, typeof(double));

         // Make sure there are enough values in the table.
         while (Table.Rows.Count < Values.Length + StartRow)
            Table.Rows.Add(Table.NewRow());

         int Row = StartRow;
         for (int Index = 0; Index != Values.Length; Index++)
            {
            if (Values[Index] != MathUtility.MissingValue)
               Table.Rows[Row][ColumnName] = Values[Index];
            else
               Table.Rows[Row][ColumnName] = DBNull.Value;
            Row++;
            }
         }

      // ---------------------------------------------------
      // Add a column of values to the specified data table
      // ---------------------------------------------------
      static public void AddColumn(DataTable Table, string ColumnName, double[] Values)
         {
         AddColumn(Table, ColumnName, Values, 0, Values.Length);
         }

      // ---------------------------------------------------
      // Add a column of values to the specified data table
      // ---------------------------------------------------
      static public void AddColumn(DataTable Table, string ColumnName, string[] Values)
         {
         AddColumn(Table, ColumnName, Values, 0, Values.Length);
         }

      // ---------------------------------------------------
      // Add a column of values to the specified data table
      // ---------------------------------------------------
      static public void AddColumn(DataTable Table, string ColumnName, string[] Values, int StartRow, int Count)
         {
         if (Table.Columns.IndexOf(ColumnName) == -1)
            Table.Columns.Add(ColumnName, typeof(string));

         // Make sure there are enough values in the table.
         while (Table.Rows.Count < Values.Length + StartRow)
            Table.Rows.Add(Table.NewRow());

         int Row = StartRow;
         for (int Index = 0; Index != Values.Length; Index++)
            {
            if (Values[Index] != "")
               Table.Rows[Row][ColumnName] = Values[Index];
            Row++;
            }
         }


      // ---------------------------------------------------
      // Get a column of values from the specified data table
      // ---------------------------------------------------
      static public double[] GetColumnAsDoubles(DataTable Table, string ColumnName)
         {
         return GetColumnAsDoubles(Table, ColumnName, Table.Rows.Count);
         }
      static public double[] GetColumnAsDoubles(DataTable Table, string ColumnName, int NumValues)
         {
         double[] Values = new double[NumValues];
         for (int Row = 0; Row != Table.Rows.Count && Row != NumValues; Row++)
            {
            if (Table.Rows[Row][ColumnName].ToString() == "")
               Values[Row] = MathUtility.MissingValue;
            else
               Values[Row] = Convert.ToDouble(Table.Rows[Row][ColumnName]);
            }
         return Values;
         }
      static public double[] GetColumnAsDoubles(DataTable Table, string ColumnName, int NumValues, int StartRow)
         {
         double[] Values = new double[NumValues];
         int Index = 0;
         for (int Row = StartRow; Row != Table.Rows.Count && Index != NumValues; Row++)
            {
            if (Table.Rows[Row][ColumnName].ToString() == "")
               Values[Index] = MathUtility.MissingValue;
            else
               {
               try
                  {
                  Values[Index] = Convert.ToDouble(Table.Rows[Row][ColumnName]);
                  }
               catch (Exception)
                  {
                  throw new Exception("Invalid number found: " + Table.Rows[Row][ColumnName].ToString() +
                                 ". Row: " + Row.ToString() + ". Column name: " + ColumnName);
                  }
               }
            Index++;
            }
         return Values;
         }

      // ---------------------------------------------------
      // Get a column of values from the specified data table
      // ---------------------------------------------------
      static public string[] GetColumnAsStrings(DataTable Table, string ColumnName)
         {
         return GetColumnAsStrings(Table, ColumnName, Table.Rows.Count);
         }
      static public string[] GetColumnAsStrings(DataTable Table, string ColumnName, int NumValues)
         {
         string[] Values = new string[NumValues];
         for (int Row = 0; Row != Table.Rows.Count && Row != NumValues; Row++)
            Values[Row] = Convert.ToString(Table.Rows[Row][ColumnName]);
         return Values;
         }
      static public string[] GetColumnAsStrings(DataTable Table, string ColumnName, int NumValues, int StartRow)
         {
         string[] Values = new string[NumValues];
         int Index = 0;
         for (int Row = StartRow; Row != Table.Rows.Count && Index != NumValues; Row++)
            {
            Values[Index] = Convert.ToString(Table.Rows[Row][ColumnName]);
            Index++;
            }
         return Values;
         }

      // ---------------------------------------------------
      // Get a list of column names
      // ---------------------------------------------------
      static public string[] GetColumnNames(DataTable Table)
         {
         string[] ColumnNames = new string[Table.Columns.Count];
         for (int Col = 0; Col != Table.Columns.Count; Col++)
            ColumnNames[Col] = Table.Columns[Col].ColumnName;
         return ColumnNames;
         }


      // ---------------------------------------------------------------------
      // Get number of non blank values in column of the specified data table
      // ---------------------------------------------------------------------
      static public int GetNumberOfNonBlankRows(DataTable Table, string ColumnName)
         {
         for (int Row = Table.Rows.Count - 1; Row >= 0; Row--)
            {
            if (Table.Rows[Row][ColumnName].ToString() != "")
               return Row + 1;
            }
         return Table.Rows.Count;
         }

      static public DateTime GetDateFromRow(DataRow Row)
         {
         // ---------------------------------------------------------------------
         // Try and return a date for the specified row in the specified table.
         // Will throw if there is no date found.
         // ---------------------------------------------------------------------
         int Year = 0;
         int Month = 0;
         int Day = 0;
         for (int Col = 0; Col != Row.Table.Columns.Count; Col++)
            {
            string ColumnName = Row.Table.Columns[Col].ColumnName.ToLower();
            if (ColumnName == "date")
               {
               if (Row.Table.Columns[Col].DataType == typeof(DateTime))
                  return (DateTime) Row[Col];
               else
                  return DateTime.Parse(Row[Col].ToString());
               }
            else if (ColumnName == "year")
               Year = Convert.ToInt32(Row[Col]);
            else if (ColumnName == "month")
               Month = Convert.ToInt32(Row[Col]);
            else if (ColumnName == "day")
               Day = Convert.ToInt32(Row[Col]);
            }
         if (Year > 0)
            {
            if (Day > 0)
               return new DateTime(Year, 1, 1).AddDays(Day - 1);
            else
               Day = 1;
            if (Month == 0)
               Month = 1;
            return new DateTime(Year, Month, Day);
            }
         throw new Exception("Cannot find a date columns. " +
                             "There must be one of the following combinations of columns: " +
                             "[a date column] OR " +
                             "[a year and day column] OR" +
                             "[a year, month and day column]");
         }

      static public DataView FilterTableForYear(DataTable Table, int StartYear, int EndYear)
         {
         // ---------------------------------------------------------------------
         // Filter the specified data table for the specified year range.
         // ---------------------------------------------------------------------
         DataView View = new DataView();
         View.Table = Table;
         if (Table.Columns.IndexOf("year") != -1)
            View.RowFilter = "Year >= " + StartYear.ToString() + " and Year <= " + EndYear;

         else if (Table.Columns.IndexOf("date") != -1)
            {
            // This uses system locale to decode a date string, we should really
            // be using the units attribute instead.
            DateTime d1 = new DateTime(StartYear, 1, 1);
            String filter = String.Format(System.Globalization.CultureInfo.InvariantCulture.DateTimeFormat, "Date >= #{0}#", d1);
            DateTime d2 = new DateTime(EndYear, 12, 31);
            filter += String.Format(System.Globalization.CultureInfo.InvariantCulture.DateTimeFormat, "AND Date <= #{0}#", d2);
            View.RowFilter = filter;
            }
         else
            throw new Exception("Cannot find a date column in data");
         return View;
         }


      static public List<string> GetDistinctValues(DataTable Table, string ColumnName)
         {
         // ---------------------------------------------------------------------
         // Return a list of unique values for the specified column in the
         // specified table.
         // ---------------------------------------------------------------------
         List<string> Values = new List<string>();

         foreach (DataRow Row in Table.Rows)
            {
            if (Values.IndexOf(Row[ColumnName].ToString()) == -1)
               Values.Add(Row[ColumnName].ToString());
            }
         return Values;
         }
      static public double[] ColumnValues(DataView View, string ColumnName)
         {
         double[] Values = new double[View.Count];
         for (int Row = 0; Row != View.Count; Row++)
            Values[Row] = Convert.ToDouble(View[Row][ColumnName]);
         return Values;
         }

      static public DataTable MonthlySums(DataView View)
         {
         // ----------------------------------------------------------------------------------
         // From the daily data in the Metfile object, calculate monthly sums of all variables
         // ----------------------------------------------------------------------------------
         DataTable MonthlyData = new DataTable();
         MonthlyData.TableName = "MonthlyData";

         if (View.Table.Columns.IndexOf("Date") == -1)
            MonthlyData.Columns.Add("Date", Type.GetType("System.DateTime"));

         foreach (DataColumn Column in View.Table.Columns)
            MonthlyData.Columns.Add(Column.ColumnName, Column.DataType);

         int PreviousMonth = 0;
         DataRow MonthRow = null;
         for (int Row = 0; Row != View.Count; Row++)
            {
            DateTime RowDate = DataTableUtility.GetDateFromRow(View[Row].Row);
            if (PreviousMonth != RowDate.Month)
               {
               MonthRow = MonthlyData.NewRow();
               MonthlyData.Rows.Add(MonthRow);
               MonthRow["Date"] = RowDate;
               PreviousMonth = RowDate.Month;
               }

            foreach (DataColumn Column in View.Table.Columns)
               {
               if (Convert.IsDBNull(MonthRow[Column.ColumnName]))
                  MonthRow[Column.ColumnName] = View[Row][Column.ColumnName];
               else if (Column.DataType.ToString() == "System.Single" || Column.DataType.ToString() == "System.Double")
                  MonthRow[Column.ColumnName] = Convert.ToDouble(MonthRow[Column.ColumnName]) + 
                                                Convert.ToDouble(View[Row][Column.ColumnName]);
               else
                  MonthRow[Column.ColumnName] = View[Row][Column.ColumnName];

               }
            }
         return MonthlyData;
         }

      }
   }
