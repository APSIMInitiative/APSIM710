using System;
using System.Data;
using System.IO;
using System.Collections;
using System.Collections.Specialized;
using System.ComponentModel;

using CSGeneral;
using System.Globalization;


// An APSIMInputFile is either a ".met" file or a ".out" file.
// They are both text files that share the same format. 
// These classes are used to read/write these files and create an object instance of them.


namespace ApsimFile
{
    // ---------------------------------------------
    // A simple type for encapsulating a constant
    // ---------------------------------------------
    public class APSIMConstant
    {
        public APSIMConstant(string name, string val, string units, string comm)
        {
            Name = name;
            Value = val;
            Units = units;
            Comment = comm;
        }

        public string Name;
        public string Value;
        public string Units;
        public string Comment;
    }

    /// <summary>
    /// This class encapsulates an APSIM input file providing methods for
    /// reading data.
    /// </summary>
    public class APSIMInputFile
    {
        private string _FileName;
        public StringCollection Headings;
        public StringCollection Units;
        private ArrayList _Constants = new ArrayList();
        private bool CSV = false;
        private StreamReaderRandomAccess In;
        private DateTime FirstDate;
        private DateTime LastDate;
        private int FirstLinePosition;
        private StringCollection Words = new StringCollection();

        public ArrayList Constants
        {
            get
            {
                return _Constants;
            }
        }
        public APSIMConstant Constant(string ConstantName)
        {
            // -------------------------------------
            // Return a given constant to caller
            // -------------------------------------

            foreach (APSIMConstant c in _Constants)
            {
                if (StringManip.StringsAreEqual(c.Name, ConstantName))
                {
                    return c;
                }
            }
            return null;
        }
        public void SetConstant(string ConstantName, string ConstantValue)
        {
            // -------------------------------------
            // Set a given constant's value.
            // -------------------------------------

            foreach (APSIMConstant c in _Constants)
            {
                if (StringManip.StringsAreEqual(c.Name, ConstantName))
                    c.Value = ConstantValue;
            }
        }
        public void ReadFromFile(string FileName, DataTable Data)
        {
            // ------------------------------------------------------------------------
            // Read from the specified file and put all data into specified data table.
            // ------------------------------------------------------------------------
            Open(FileName);
            ReadAllData(In, Data);
            In.Close();
        }
        public void ReadApsimHeaderLines(StreamReaderRandomAccess In,
                                          ref StringCollection ConstantLines,
                                          ref StringCollection HeadingLines)
        {
            string PreviousLine = "";

            string Line = In.ReadLine();
            while (!In.EndOfStream)
            {
                int PosEquals = Line.IndexOf('=');
                if (PosEquals != -1)
                {
                    // constant found.
                    ConstantLines.Add(Line);
                }
                else
                {
                    char[] whitespace = { ' ', '\t' };
                    int PosFirstNonBlankChar = StringManip.IndexNotOfAny(Line, whitespace);
                    if (PosFirstNonBlankChar != -1 && Line[PosFirstNonBlankChar] == '(')
                    {
                        HeadingLines.Add(PreviousLine);
                        HeadingLines.Add(Line);
                        break;
                    }
                }
                PreviousLine = Line;
                Line = In.ReadLine();
            }

        }
        public void AddConstantsToData(DataTable Data, int StartRow)
        {
            foreach (APSIMConstant Constant in Constants)
            {
                if (Data.Columns.IndexOf(Constant.Name) == -1)
                {
                    Type ColumnType = StringManip.DetermineType(Constant.Value, "");
                    Data.Columns.Add(new DataColumn(Constant.Name, ColumnType));
                }
                for (int Row = StartRow; Row < Data.Rows.Count; Row++)
                {
                    double Value;
                    if (Double.TryParse(Constant.Value, NumberStyles.Float, new CultureInfo("en-US"), out Value))
                        Data.Rows[Row][Constant.Name] = Value;
                    else
                        Data.Rows[Row][Constant.Name] = Constant.Value;
                }
            }
        }
        public void ReadApsimHeader(StreamReaderRandomAccess In)
        {
            // ----------------------------------
            // Read in the apsim header.
            // ----------------------------------
            StringCollection ConstantLines = new StringCollection();
            StringCollection HeadingLines = new StringCollection();
            ReadApsimHeaderLines(In, ref ConstantLines, ref HeadingLines);

            bool TitleFound = false;
            foreach (string ConstantLine in ConstantLines)
            {
                string Line = ConstantLine;
                string Comment = StringManip.SplitOffAfterDelimiter(ref Line, "!");
                Comment.Trim();
                int PosEquals = Line.IndexOf('=');
                if (PosEquals != -1)
                {
                    string Name = Line.Substring(0, PosEquals).Trim();
                    if (Name.ToLower() == "title")
                    {
                        TitleFound = true;
                        Name = "Title";
                    }
                    string Value = Line.Substring(PosEquals + 1).Trim();
                    string Unit = StringManip.SplitOffBracketedValue(ref Value, '(', ')');
                    _Constants.Add(new APSIMConstant(Name, Value, Unit, Comment));
                }
            }
            if (HeadingLines.Count >= 2)
            {
                if (CSV)
                {
                    HeadingLines[0] = HeadingLines[0].TrimEnd(',');
                    HeadingLines[1] = HeadingLines[1].TrimEnd(',');
                    Headings = new StringCollection();
                    Units = new StringCollection();
                    Headings.AddRange(HeadingLines[0].Split(",".ToCharArray()));
                    Units.AddRange(HeadingLines[1].Split(",".ToCharArray()));
                    for (int i = 0; i < Headings.Count; i++)
                        Headings[i] = Headings[i].Trim();
                    for (int i = 0; i < Units.Count; i++)
                        Units[i] = Units[i].Trim();
                }
                else
                {
                    Headings = StringManip.SplitStringHonouringQuotes(HeadingLines[0], " \t");
                    Units = StringManip.SplitStringHonouringQuotes(HeadingLines[1], " \t");
                }
                TitleFound = TitleFound || StringManip.IndexOfCaseInsensitive(Headings, "title") != -1;
                if (Headings.Count != Units.Count)
                    throw new Exception("The number of headings and units doesn't match in file: " + _FileName);
            }
            if (!TitleFound)
                _Constants.Add(new APSIMConstant("Title", Path.GetFileNameWithoutExtension(_FileName), "", ""));
        }
        private void ReadAllData(StreamReaderRandomAccess In, DataTable Data)
        {
            StringCollection Words = new StringCollection();
            bool CheckHeadingsExist = true;
            while (GetNextLine(In, ref Words))
            {
                StoreRowInData(In, Words, Data, CheckHeadingsExist);
                CheckHeadingsExist = false;
            }
        }

        private void StoreRowInData(StreamReaderRandomAccess In, StringCollection Words, DataTable Data, bool CheckHeadingsExist)
        {
            DataRow NewMetRow = Data.NewRow();
            for (int w = 0; w != Words.Count; w++)
            {
                int TableColumnNumber = Data.Columns.IndexOf(Headings[w]);
                if (CheckHeadingsExist)
                {
                    if (TableColumnNumber == -1)
                    {
                        Type ColumnType;
                        if (Words[w] == "?" || Words[w] == "*" || Words[w] == "")
                            ColumnType = StringManip.DetermineType(LookAheadForNonMissingValue(In, w), Units[w]);
                        else
                            ColumnType = StringManip.DetermineType(Words[w], Units[w]);
                        Data.Columns.Add(new DataColumn(Headings[w], ColumnType));
                        TableColumnNumber = Data.Columns.Count - 1;
                    }
                }
                Words[w] = Words[w].Trim();
                if (Words[w] != "?" && Words[w] != "*" && Words[w] != "")
                {
                    if (Data.Columns[TableColumnNumber].DataType == typeof(DateTime))
                    {
                        // Need to get a sanitised date e.g. d/M/yyyy 
                        string DateFormat = Units[w].ToLower();
                        DateFormat = StringManip.SplitOffBracketedValue(ref DateFormat, '(', ')');
                        DateFormat = DateFormat.Replace("mmm", "MMM");
                        DateFormat = DateFormat.Replace("mm", "m");
                        DateFormat = DateFormat.Replace("dd", "d");
                        DateFormat = DateFormat.Replace("m", "M");
                        if (DateFormat == "")
                            DateFormat = "d/M/yyyy";
                        try
                        {
                            DateTime Value = DateTime.ParseExact(Words[w], DateFormat, null);
                            NewMetRow[TableColumnNumber] = Value;
                        }
                        catch (Exception)
                        { }
                    }
                    else if (Data.Columns[TableColumnNumber].DataType == typeof(float))
                    {
                        try
                        {
                            NewMetRow[TableColumnNumber] = Convert.ToDouble(Words[w] /*, new CultureInfo("en-US")*/);
                        }
                        catch (Exception e)
                        {
                            string err = "";
                            for(int i=0;i<Words.Count;i++)
                                err += Words[i] + " ";

                            System.Windows.Forms.MessageBox.Show("Error in column " + (w + 1).ToString() + ":\r\n" + err);
                            return;
                        }
                    }
                    else
                        NewMetRow[TableColumnNumber] = Words[w];
                }
            }
            Data.Rows.Add(NewMetRow);
        }

        private bool GetNextLine(StreamReaderRandomAccess In, ref StringCollection Words)
        {
            if (In.EndOfStream)
                return false;

            string Line = In.ReadLine();

            if (Line == null || Line.Length == 0)
                return false;

            if (Line.IndexOf("!") > 0) //used to ignore "!" in a row
                Line = Line.Substring(0, Line.IndexOf("!") - 1);

            if (CSV)
            {
                Words.Clear();
                Line = Line.TrimEnd(',');
                Words.AddRange(Line.Split(",".ToCharArray()));
            }
            else
                Words = StringManip.SplitStringHonouringQuotes(Line, " \t");
            if (Words.Count != Headings.Count)
                throw new Exception("Invalid number of values on line: " + Line + "\r\nin file: " + _FileName);

            // Remove leading / trailing double quote chars.
            for (int i = 0; i < Words.Count; i++)
                Words[i] = Words[i].Trim("\"".ToCharArray());
            return true;
        }
        private string LookAheadForNonMissingValue(StreamReaderRandomAccess In, int w)
        {
            if (In.EndOfStream)
                return "?";

            int Pos = In.Position;

            StringCollection Words = new StringCollection();
            while (GetNextLine(In, ref Words) && Words[w] == "?") ;

            In.Position = Pos;

            if (Words.Count > w)
                return Words[w];
            else
                return "?";
        }

        /// <summary>
        /// Open the file ready for reading.
        /// </summary>
        public void Open(string FileName)
        {
            if (FileName == "")
                return;

            if (!File.Exists(FileName))
                throw new Exception("Cannot find file: " + FileName);

            _FileName = FileName;
            CSV = Path.GetExtension(FileName).ToLower() == ".csv";

            _Constants.Clear();

            In = new StreamReaderRandomAccess(_FileName);
            ReadApsimHeader(In);
            FirstLinePosition = In.Position;

            DataTable Data = new DataTable();

            int SavedPosition = In.Position;

            // Get first date.
            StringCollection Words = new StringCollection();
            GetNextLine(In, ref Words);
            StoreRowInData(In, Words, Data, true);
            try
            {
                In.Seek(0, SeekOrigin.End);
                if (In.Position >= 1000 && In.Position -1000 > SavedPosition)
                {
                    In.Seek(-1000, SeekOrigin.End);
                    In.ReadLine(); // throw away partial line.
                }
                else
                    In.Seek(SavedPosition, SeekOrigin.Begin);
            }
            catch (Exception)
            {
                // The seek can fail on small files.
                In.Seek(SavedPosition, SeekOrigin.Begin);
            }


            while (GetNextLine(In, ref Words)) ;

            if (Words.Count == 0)
                throw new Exception("Cannot find last row of file: " + FileName);
            StoreRowInData(In, Words, Data, false);

            if (Data.Rows.Count == 2)
            {
                try
                {
                    FirstDate = DataTableUtility.GetDateFromRow(Data.Rows[0]);
                    LastDate = DataTableUtility.GetDateFromRow(Data.Rows[1]);
                }
                catch (Exception)
                {
                    // Not all files will have a date.
                }
            }
            In.Seek(FirstLinePosition, SeekOrigin.Begin);
        }
        public void SeekToDate(DateTime Date)
        {
            int NumRowsToSkip = (Date - FirstDate).Days;

            In.Seek(FirstLinePosition, SeekOrigin.Begin);
            while (!In.EndOfStream && NumRowsToSkip > 0)
            {
                In.ReadLine();
                NumRowsToSkip--;
            }
            int SavedPosition = In.Position;
            StringCollection Words = new StringCollection();
            if (GetNextLine(In, ref Words))
            {
                // Make sure we found the date.
                DataTable D = new DataTable();
                StoreRowInData(In, Words, D, true);
                DateTime RowDate = DataTableUtility.GetDateFromRow(D.Rows[0]);
                if (RowDate != Date)
                    throw new Exception("Non consecutive dates found in file: " + _FileName);
            }
            else
                throw new Exception("End of file reached while trying to find date " + Date.ToShortDateString() +
                                    " in file " + _FileName);

            // All ok - restore position.
            In.Seek(SavedPosition, SeekOrigin.Begin);
        }

        public void GetNextLineOfData(DataTable Table)
        {
            Words.Clear();
            if (GetNextLine(In, ref Words))
                StoreRowInData(In, Words, Table, Table.Rows.Count == 0);
            else
                throw new Exception("End of file reached while reading file: " + _FileName);
        }
    }
}
