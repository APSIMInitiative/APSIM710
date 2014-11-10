using System;
using System.Data;
using System.Windows.Forms;
using System.Collections;
using System.Drawing;

namespace CSGeneral
{

    public class GridUtility
    {

        /// <summary>
        /// Get a column of double values from the specified grid column. Empty cells are converted to double.NaN.
        /// If the entire column is empty then null will be returned.
        /// </summary>
        public static double[] GetColumnAsDoubles(DataGridView Grid, int ColumnIndex)
        {
            int NumValues = FindLastRow(Grid)+1;
            double[] Values = new double[NumValues];

            int Index = 0;
            for (int RowIndex = 0; RowIndex <= NumValues - 1; RowIndex++)
            {
                if (Grid.Rows[RowIndex].Cells[ColumnIndex].Value != DBNull.Value &&
                    Grid.Rows[RowIndex].Cells[ColumnIndex].FormattedValue.ToString() != "")
                    Values[Index] = Convert.ToDouble(Grid.Rows[RowIndex].Cells[ColumnIndex].Value);
                else
                    Values[Index] = double.NaN;

                Index = Index + 1;
            }
            if (MathUtility.ValuesInArray(Values))
                return Values;
            else
                return null;
        }

        /// <summary>
        /// Get a column of string values from the specified grid column
        /// </summary>
        public static string[] GetColumnAsStrings(DataGridView Grid, int ColumnIndex)
        {
            int NumValues = FindLastRow(Grid)+1;
            string[] Values = new string[NumValues];

            int Index = 0;
            for (int RowIndex = 0; RowIndex < NumValues; RowIndex++)
            {
                if (Grid.Rows[RowIndex].Cells[ColumnIndex].Value == null)
                    Values[Index] = "";
                else
                    Values[Index] = Grid.Rows[RowIndex].Cells[ColumnIndex].Value.ToString();
                Index = Index + 1;
            }
            return Values;
        }

        /// <summary>
        /// Get a column of string values from the specified grid column
        /// </summary>
        public static string[] GetColumnAsStringsUsingCellFormat(DataGridView Grid, int ColumnIndex)
        {
            int NumValues = FindLastRow(Grid) + 1;
            string[] Values = new string[NumValues];

            int Index = 0;
            for (int RowIndex = 0; RowIndex < NumValues; RowIndex++)
            {
                if (Grid.Rows[RowIndex].Cells[ColumnIndex].Value == null)
                    Values[Index] = "";
                else
                    Values[Index] = Grid.Rows[RowIndex].Cells[ColumnIndex].FormattedValue.ToString();
                Index = Index + 1;
            }
            return Values;
        }

        /// <summary>
        /// Convert a grid to a DataTable.
        /// </summary>
        public static DataTable GridToDataTable(DataGridView Grid)
        {
            int NumValues = Grid.RowCount - 1;
            if (!Grid.ReadOnly)
                NumValues--;

            DataTable Data = new DataTable("Data");
            for (int Col = 0; Col < Grid.Columns.Count; Col++)
                Data.Columns.Add(Grid.Columns[Col].HeaderText, DetermineTypeOfColumn(Grid, Col));

            for (int Row = 0; Row < NumValues; Row++)
            {
                DataRow NewRow = Data.NewRow();
                Data.Rows.Add(NewRow);
                for (int Col = 0; Col < Grid.Columns.Count; Col++)
                {
                    if (Grid.Rows[Row].Cells[Col].Value != null)
                        NewRow[Col] = Grid.Rows[Row].Cells[Col].Value.ToString();
                }
            }
            return Data;
        }

        /// <summary>
        /// Determine and return a data type for the specified grid column.
        /// </summary>
        private static Type DetermineTypeOfColumn(DataGridView Grid, int Col)
        {
            for (int Row = 0; Row != Grid.Rows.Count; Row++)
            {
                if (Grid.Rows[Row].Cells[Col].Value != null)
                    return StringManip.DetermineType(Grid.Rows[Row].Cells[Col].Value.ToString(), "");
            }
            return typeof(string);
        }

        /// <summary>
        /// Set the values of a grid column
        /// </summary>
        public static void SetColumnValues(DataGridView Grid, int Col, IList Values)
        {
            if (Values != null)
            {
                Grid.RowCount = Math.Max(Grid.Rows.Count, Values.Count);
                for (int Row = 0; Row < Grid.Rows.Count; Row++)
                    Grid.Rows[Row].Cells[Col].Value = Row < Values.Count ? Values[Row] : "";
            }

        }

        /// <summary>
        /// Add a column to the grid.
        /// </summary>
        public static DataGridViewColumn AddColumn(DataGridView Grid, 
                                                   string ColumnName, 
                                                   IList Values, 
                                                   string Format = null, 
                                                   Color? BackgroundColour = null, 
                                                   Color? ForegroundColour = null,
                                                   bool ReadOnly = false,
                                                   string[] ToolTips = null)
        {
            int Col = Grid.Columns.Add(ColumnName, ColumnName);
            if (Values != null)
            {
                while (Grid.RowCount < Values.Count)
                    Grid.Rows.Add(new DataGridViewRow());

                for (int Row = 0; Row < Values.Count; Row++)
                {
                    if (Values[Row] is double && double.IsNaN((double)Values[Row]))
                        Grid.Rows[Row].Cells[Col].Value = null;
                    else
                        Grid.Rows[Row].Cells[Col].Value = Values[Row];
                    if (ToolTips != null && ToolTips[Row] != "")
                    {
                        Grid.Rows[Row].Cells[Col].ToolTipText = ToolTips[Row];
                        if (!ForegroundColour.HasValue)
                            Grid.Columns[Col].DefaultCellStyle.ForeColor = Color.Blue;
                    }
                }
                if (Format != null)
                    Grid.Columns[Col].DefaultCellStyle.Format = Format;
                if (BackgroundColour.HasValue)
                    Grid.Columns[Col].DefaultCellStyle.BackColor = BackgroundColour.Value;
                if (ForegroundColour.HasValue)
                    Grid.Columns[Col].DefaultCellStyle.ForeColor = ForegroundColour.Value;
                else if (ReadOnly)
                    Grid.Columns[Col].DefaultCellStyle.ForeColor = Color.Gray;
                Grid.Columns[Col].ReadOnly = ReadOnly;
                
            }
            return Grid.Columns[Col];
        }

        /// <summary>
        /// Find the last populated row and return its index.
        /// </summary>
        public static int FindLastRow(DataGridView Grid)
        {
            if (Grid.RowCount == 0)
                return -1;

            int Row = Grid.RowCount - 1;
            do
            {
                foreach (DataGridViewCell Cell in Grid.Rows[Row].Cells)
                {
                    if (Cell.Value != null && !Cell.ReadOnly)
                        return Row;
                }
                Row--;
            }
            while (Row > 0);
            return Row;
        }

        /// <summary>
        /// Get a column of double values from the specified grid column. Empty cells are converted to double.NaN.
        /// If the entire column is empty then null will be returned.
        /// </summary>
        public static string[] GetColumnOfToolTips(DataGridView Grid, int ColumnIndex)
        {
            int NumValues = FindLastRow(Grid) + 1;
            string[] Values = new string[NumValues];

            int Index = 0;
            for (int RowIndex = 0; RowIndex <= NumValues - 1; RowIndex++)
            {
                Values[Index] = Grid.Rows[RowIndex].Cells[ColumnIndex].ToolTipText;
                Index = Index + 1;
            }
            if (MathUtility.ValuesInArray(Values))
                return Values;
            else
                return null;
        }
    }
}