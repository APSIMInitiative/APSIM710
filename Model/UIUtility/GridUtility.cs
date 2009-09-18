
using System;
using FarPoint.Win.Spread;

//This is used in CSUserInterface project in SoilUI, InitWaterUI, InitNitrogenUI, ProfileUI, SampleUI 
//This is used in GraphUserInterface project in FrequencyUI, ChartPageUI 
//This is used in VBUserInterface project in OutputFileDescUI, GenericUI

namespace UIUtility
{

    // ------------------------------------------ 
    // This class contains a few Grid functions 
    // ------------------------------------------ 
    public class GridUtility
    {

        // ------------------------------------------------------ 
        // Populate the specified grid column with double values. 
        // ------------------------------------------------------ 
        public static void SetColumnAsDoubles(SheetView Grid, int ColumnIndex, double[] Values)
        {
            // Make sure the grid has enough rows. 
            if ((Grid.RowCount < Values.Length))
            {
                Grid.RowCount = Values.Length;
            }

            // Add values to column 
            int RowIndex = 0;
            foreach (double Value in Values)
            {
                if (Value != 999999.0)
                {
                    Grid.Cells[RowIndex, ColumnIndex].Value = Value;
                }
                RowIndex = RowIndex + 1;
            }
        }


        // -------------------------------------- 
        // Set the cell of a grid as a double. 
        // -------------------------------------- 
        public static void SetCellAsDouble(SheetView Grid, int ColumnIndex, int RowIndex, double Value)
        {
            if ((Value != 999999.0))
            {
                Grid.Cells[RowIndex, ColumnIndex].Value = Value;
            }
        }


        // ------------------------------------------------------ 
        // Populate the specified grid column with string values. 
        // ------------------------------------------------------ 
        public static void SetColumnAsStrings(SheetView Grid, int ColumnIndex, string[] Values)
        {

            // Make sure the grid has enough rows. 
            if ((Grid.RowCount < Values.Length))
            {
                Grid.RowCount = Values.Length;
            }

            // Add values to column 
            int RowIndex = 0;
            foreach (string Value in Values)
            {
                Grid.Cells[RowIndex, ColumnIndex].Value = Value;
                RowIndex = RowIndex + 1;
            }
        }


        // ---------------------------------------------------------- 
        // Get a column of double values from the specified grid column 
        // ---------------------------------------------------------- 
        public static double[] GetColumnAsDoubles(SheetView Grid, int ColumnIndex, int NumValues)
        {
            int NumValuesToReturn = Math.Min(Grid.RowCount, NumValues);
            double[] Values = new double[NumValuesToReturn];

            int Index = 0;
            for (int RowIndex = 0; RowIndex <= NumValuesToReturn - 1; RowIndex++)
            {
                if (!string.IsNullOrEmpty(Grid.Cells[RowIndex, ColumnIndex].Text))
                {
                    Values[Index] = Convert.ToDouble(Grid.Cells[RowIndex, ColumnIndex].Value);
                }
                else
                {
                    Values[Index] = 999999.0;
                }

                Index = Index + 1;
            }
            return Values;
        }


        // ---------------------------------------------------------- 
        // Get a column of string values from the specified grid column 
        // ---------------------------------------------------------- 
        public static string[] GetColumnAsStrings(SheetView Grid, int ColumnIndex, int NumValues)
        {

            int NumValuesToReturn = Math.Min(Grid.RowCount, NumValues);
            string[] Values = new string[NumValuesToReturn];

            int Index = 0;
            for (int RowIndex = 0; RowIndex <= NumValuesToReturn - 1; RowIndex++)
            {
                Values[Index] = (string)Grid.Cells[RowIndex, ColumnIndex].Value;
                Index = Index + 1;
            }
            return Values;
        }


        // -------------------------------------- 
        // Get the contents of a cell of a grid. 
        // -------------------------------------- 
        public static double GetCellAsDouble(SheetView Grid, int ColumnIndex, int RowIndex)
        {
            if (((Grid.Cells[RowIndex, ColumnIndex].Value != null)))
            {
                return Convert.ToDouble(Grid.Cells[RowIndex, ColumnIndex].Value);
            }
            else
            {
                return 999999.0;
            }
        }


        // -------------------------------------- 
        // Get the contents of a cell of a grid. 
        // -------------------------------------- 
        public static string GetCellAsString(SheetView Grid, int ColumnIndex, int RowIndex)
        {
            if (((Grid.Cells[RowIndex, ColumnIndex].Value != null)))
            {
                return (string)Grid.Cells[RowIndex, ColumnIndex].Value.ToString();
            }
            else
            {
                return "";
            }
        }

        // --------------------------------------------------------------------- 
        // Get number of non blank values in column of the specified data table 
        // --------------------------------------------------------------------- 
        public static int FindFirstBlankCell(SheetView Grid, int ColumnIndex)
        {
            for (int RowIndex = 0; RowIndex <= Grid.RowCount - 1; RowIndex++)
            {
                if (string.IsNullOrEmpty(Grid.Cells[RowIndex, ColumnIndex].Text))
                {
                    return RowIndex;
                }
            }
            return Grid.RowCount;
        }
        // --------------------------------------------------------------------- 
        // Get number of non blank values in column of the specified data table 
        // --------------------------------------------------------------------- 
        public static int[] FindBlankCells(SheetView Grid, int ColumnIndex, int RowCount)
        {
            int count = 0;
            int[] RowIndexs = new int[RowCount];
            for (int RowIndex = 0; RowIndex <= RowCount - 1; RowIndex++)
            {
                if (string.IsNullOrEmpty(Grid.Cells[RowIndex, ColumnIndex].Text))
                {
                    RowIndexs[count] = RowIndex;
                    count += 1;
                }
            }
            Array.Resize(ref RowIndexs, count);
            return RowIndexs;
        }
        // ----------------------- 
        // How many rows in sheet have data 
        // ----------------------- 
        public static int FindRowsInSheet(SheetView Grid)
        {
            int i = 0;
            for (int RowIndex = Grid.RowCount - 1; RowIndex >= 0; RowIndex += -1)
            {
                if (!string.IsNullOrEmpty(Grid.Cells[RowIndex, 0].Text))
                {
                    i += 1;
                }
            }
            return i;
        }
        // -------------------------------------------------------------- 
        // Return the last row in the specified grid that has data in it. 
        // -------------------------------------------------------------- 
        public static int FindLastBlankCell(SheetView Grid)
        {
            for (int RowIndex = Grid.RowCount - 1; RowIndex >= 0; RowIndex += -1)
            {
                if (!string.IsNullOrEmpty(Grid.Cells[RowIndex, 0].Text))
                {
                    return RowIndex;
                }
            }
            return 0;
        }
    }
}