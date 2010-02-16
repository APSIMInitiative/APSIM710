using System;
using System.Data;
using System.IO;
using System.Collections;
using System.Collections.Specialized;
using System.ComponentModel;

using CSGeneral;


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
         if (FileName == "")
            return;

         if (!File.Exists(FileName))
            throw new Exception("Cannot find file: " + FileName);

         _FileName = FileName;
         CSV = Path.GetExtension(FileName).ToLower() == ".csv";

         _Constants.Clear();

         StreamReaderRandomAccess In = new StreamReaderRandomAccess(_FileName);
         ReadApsimHeader(In);
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
               Type ColumnType = DetermineType(Constant.Value);
               Data.Columns.Add(new DataColumn(Constant.Name, ColumnType));
               }
            for (int Row = StartRow; Row < Data.Rows.Count; Row++)
               Data.Rows[Row][Constant.Name] = Constant.Value;
            }
         }
      public void GetFirstAndLastRecords(string FileName, DataTable Data)
         {
         if (!File.Exists(FileName))
            throw new Exception("Cannot find file: " + FileName);

         _FileName = FileName;
         StreamReaderRandomAccess In = new StreamReaderRandomAccess(FileName);
         ReadApsimHeader(In);

         StringCollection Words = new StringCollection();
         GetNextLine(In, ref Words);
         StoreRowInData(In, Words, Data, true);

         In.Seek(-1000, SeekOrigin.End);
         In.ReadLine(); // throw away partial line.

         while (GetNextLine(In, ref Words)) ;

         if (Words.Count == 0)
            throw new Exception("Cannot find last row of file: " + FileName);
         StoreRowInData(In, Words, Data, false);
         }


      private void ReadApsimHeader(StreamReaderRandomAccess In)
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
               }
            else
               {
               Headings = StringManip.SplitStringHonouringQuotes(HeadingLines[0], " \t");
               Units = StringManip.SplitStringHonouringQuotes(HeadingLines[1], " \t");
               }
            TitleFound = TitleFound || StringManip.IndexOfCaseInsensitive(Headings, "title") != -1;
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
            if (CheckHeadingsExist)
               {
               if (Data.Columns.IndexOf(Headings[w]) == -1)
                  {
                  Type ColumnType;
                  if (Words[w] == "?" || Words[w] == "*" || Words[w] == "")
                     ColumnType = DetermineType(LookAheadForNonMissingValue(In, w));
                  else
                     ColumnType = DetermineType(Words[w]);
                  Data.Columns.Add(new DataColumn(Headings[w], ColumnType));
                  }
               }
            if (Words[w] != "?" && Words[w] != "*" && Words[w] != "")
               NewMetRow[Headings[w]] = Words[w];
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
         return true;
         }
      private string LookAheadForNonMissingValue(StreamReaderRandomAccess In, int w)
         {
         if (In.EndOfStream)
            return "?";

         int Pos = In.Position;

         StringCollection Words = new StringCollection();
         while (GetNextLine(In, ref Words) && Words[w] == "?");

         In.Position = Pos;

         if (Words.Count > w)
            return Words[w];
         else
            return "?";
         }
      private Type DetermineType(string Value)
         {
         Type ColumnType;
         if (Value == "?")
            ColumnType = Type.GetType("System.Byte");

         else if (StringManip.IsNumeric(Value))
            ColumnType = Type.GetType("System.Single");

         else if (StringManip.IsDateTime(Value))
            ColumnType = Type.GetType("System.DateTime");

         else
            ColumnType = Type.GetType("System.String");

         return ColumnType;
         }
      }
   }
