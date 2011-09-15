using System;
using System.Data;
using System.Windows.Forms;

namespace CSGeneral
{

    public class GridUtility
    {

        /// <summary>
        /// Get a column of double values from the specified grid column
        /// </summary>
        public static double[] GetColumnAsDoubles(DataGridView Grid, int ColumnIndex)
        {
            int NumValues = Grid.RowCount;
            if (!Grid.ReadOnly)
                NumValues--;
            double[] Values = new double[NumValues];

            int Index = 0;
            for (int RowIndex = 0; RowIndex <= NumValues - 1; RowIndex++)
            {
                if (Grid.Rows[RowIndex].Cells[ColumnIndex].Value != DBNull.Value)
                {
                    Values[Index] = Convert.ToDouble(Grid.Rows[RowIndex].Cells[ColumnIndex].Value);
                }
                else
                {
                    Values[Index] = 999999.0;
                }

                Index = Index + 1;
            }
            return Values;
        }

        /// <summary>
        /// Get a column of string values from the specified grid column
        /// </summary>
        public static string[] GetColumnAsStrings(DataGridView Grid, int ColumnIndex)
        {
            int NumValues = Grid.RowCount;
            if (!Grid.ReadOnly)
                NumValues--;
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



    }
}