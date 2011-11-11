using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using CSGeneral;

namespace CSGeneral
{
    /// <summary>
    /// Summary description for Class2.
    /// </summary>
    public class StringManip
    {

        public const int MaxStringLength = 50000;


        // This function converts a C string to a vb string by returning everything 
        // up to the null character 
        public static string CStringToVBString(string Cstring)
        {
            string result;
            try
            {
                char NullChar = new char();
                result = Cstring.Substring(0, Cstring.IndexOf(NullChar));
            }
            catch (System.Exception)
            {
                System.Windows.Forms.MessageBox.Show("Error converting string type from CS to VB: " + Cstring);
                result = "";
            }

            return result;
        }



        // ------------------------------------------------------- 
        // A version of IndexOf that is case insensitive. 
        // ------------------------------------------------------- 
        public static int IndexOfCaseInsensitive(string[] Values, string St)
        {
            string StLower = St.ToLower();
            for (int i = 0; (i <= (Values.Length - 1)); i++)
            {
                if ((Values[i].ToLower() == StLower))
                {
                    return i;
                }
            }
            return -1;
        }

        // ------------------------------------------------------- 
        // A version of IndexOf that is case insensitive. 
        // ------------------------------------------------------- 
        public static int IndexOfCaseInsensitive(StringCollection Values, string St)
        {
            string StLower = St.ToLower();
            for (int i = 0; (i <= (Values.Count - 1)); i++)
            {
                if ((Values[i].ToLower() == StLower))
                {
                    return i;
                }
            }
            return -1;

        }

        // ------------------------------------------------------- 
        // A version of IndexOf that is case insensitive. 
        // ------------------------------------------------------- 
        public static int IndexOfCaseInsensitive(List<string> Values, string St)
        {
            string StLower = St.ToLower();
            for (int i = 0; (i <= (Values.Count - 1)); i++)
            {
                if ((Values[i].ToLower() == StLower))
                {
                    return i;
                }
            }
            return -1;

        }


        // ------------------------------------------------------------------
        // This method complements the string function IndexOfAny by
        // providing a NOT version. Returns -1 if non of the specified
        // characters are found in specified string.
        // ------------------------------------------------------------------
        public static int IndexNotOfAny(string Text, char[] Delimiters)
        {
            return IndexNotOfAny(Text, Delimiters, 0);
        }

        // ------------------------------------------------------------------
        // This method complements the string function IndexOfAny by
        // providing a NOT version. Returns -1 if non of the specified
        // characters are found in specified string.
        // ------------------------------------------------------------------
        public static int IndexNotOfAny(string Text, char[] Delimiters, int Pos)
        {
            string DelimitersString = new string(Delimiters);
            for (int i = Pos; i < Text.Length; i++)
            {
                if (DelimitersString.IndexOf(Text[i]) == -1)
                    return i;
            }
            return -1;
        }



        /// ------------------------------------------------------------------
        /// <summary>
        /// This method splits values on a comma but also honours double quotes
        /// ensuring something in double quotes is never split.
        ///     eg: if text = value1, "value 2, 2a", value3
        ///     then: words[0] = value1
        ///           words[1] = value2, 2a
        ///           words[2] = value3
        /// All values returned have been trimmed of spaces and double quotes.
        /// </summary>
        // ------------------------------------------------------------------
        public static StringCollection SplitStringHonouringQuotes(string Text, string Delimiters)
        {
            StringCollection ReturnStrings = new StringCollection();
            if (Text.Trim() == "")
                return ReturnStrings;

            bool InsideQuotes = false;
            int Start = IndexNotOfAny(Text, " ".ToCharArray());
            for (int i = Start; i < Text.Length; i++)
            {
                if (Text[i] == '"')
                    InsideQuotes = !InsideQuotes; // toggle

                else if (!InsideQuotes)
                {
                    if (Delimiters.IndexOf(Text[i]) != -1)
                    {
                        // Found a word - store it.
                        if (Start != i)
                            ReturnStrings.Add(Text.Substring(Start, i - Start).Trim(" ".ToCharArray()));
                        Start = i+1;

                    }
                }
            }
            if (Start != Text.Length)
                ReturnStrings.Add(Text.Substring(Start, Text.Length - Start).Trim(" ".ToCharArray()));

            // remove leading and trailing quote if necessary.
            for (int i = 0; i < ReturnStrings.Count; i++)
            {
                if (ReturnStrings[i][0] == '"' && ReturnStrings[i][ReturnStrings[i].Length - 1] == '"')
                {
                    ReturnStrings[i] = ReturnStrings[i].Substring(1, ReturnStrings[i].Length - 2).Trim();
                    if (ReturnStrings[i] == "")
                    {
                        ReturnStrings.RemoveAt(i);
                        i--;
                    }
                }
            }
            return ReturnStrings;
        }

        public static bool StringsAreEqual(string St1, string St2)
        {
            return St1.ToLower() == St2.ToLower();
        }



        // -----------------------------------------------------
        // Remove, and return everything after the specified
        // delimiter from the specified string. 
        // -----------------------------------------------------
        public static string SplitOffAfterDelimiter(ref string St, string Delimiter)
        {
            string ReturnString = "";
            int PosDelimiter = St.IndexOf(Delimiter);
            if (PosDelimiter != -1)
            {
                ReturnString = St.Substring(PosDelimiter + Delimiter.Length).Trim();
                St = St.Remove(PosDelimiter, St.Length - PosDelimiter);
            }
            return ReturnString;
        }



        // ------------------------------------------------------------------
        // Split off a bracketed value from the end of the specified string.
        // The bracketed value is then returned, without the brackets,
        // or blank if not found.
        // ------------------------------------------------------------------
        public static string SplitOffBracketedValue(ref string St, char OpenBracket, char CloseBracket)
        {
            string ReturnString = "";

            int PosOpenBracket = St.LastIndexOf(OpenBracket);
            int PosCloseBracket = St.LastIndexOf(CloseBracket);
            if (PosOpenBracket != -1 && PosCloseBracket != -1 && PosOpenBracket < PosCloseBracket)
            {
                ReturnString = St.Substring(PosOpenBracket + 1, PosCloseBracket - PosOpenBracket - 1).Trim();
                St = St.Remove(PosOpenBracket, PosCloseBracket - PosOpenBracket + 1).Trim();
            }
            return ReturnString;
        }



        // ------------------------------------------
        // Return true if specified string is numeric
        // ------------------------------------------
        public static bool IsNumeric(string St)
        {
            float Value;
            return Single.TryParse(St, out Value);
        }



        // ------------------------------------------
        // Return true if specified string is a date time.
        // ------------------------------------------
        public static bool IsDateTime(string St)
        {
            DateTime Value;
            return DateTime.TryParse(St, out Value);
        }


        // -------------------------------------------------------
        // Indent the specified string a certain number of spaces.
        // -------------------------------------------------------
        public static string IndentText(string St, int numChars)
        {
            string space = new string(' ', numChars);
            return space + St.Replace("\r\n", "\r\n" + space);
        }

        // -------------------------------------------------------
        // Indent the specified string a certain number of spaces.
        // -------------------------------------------------------
        public static string UnIndentText(string St, int numChars)
        {
            if (St.Length < numChars)
                return St;
            string returnString = St.Remove(0, numChars);

            string space = "\r\n" + new string(' ', numChars);
            return returnString.Replace(space, "\r\n");
        }



        public static string DQuote(string St)
        {
            return "\"" + St + "\"";
        }

        public static Type DetermineType(string Value, string Units)
        {
            Type ColumnType;
            if (Value == "?")
                ColumnType = Type.GetType("System.Byte");

            else if (StringManip.IsNumeric(Value))
                ColumnType = Type.GetType("System.Single");

            else if (Units == "" && StringManip.IsDateTime(Value))
                ColumnType = Type.GetType("System.DateTime");

            else if ((Units.Contains("d") && Units.Contains("/") && Units.Contains("y"))
                      || StringManip.IsDateTime(Value))
                ColumnType = Type.GetType("System.DateTime");

            else
                ColumnType = Type.GetType("System.String");

            return ColumnType;
        }

        public static string[] CreateStringArray(string Value, int NumValues)
        {
            string[] Arr = new string[NumValues];
            for (int i = 0; i < NumValues; i++)
                Arr[i] = Value;
            return Arr;
        }


        public static int FindMatchingClosingBracket(string Contents, int StartPos, char OpenBracket, char CloseBracket)
        {
            char[] CharSet = new char[2] { OpenBracket, CloseBracket };
            int Pos = Contents.IndexOfAny(CharSet, StartPos);

            int Count = 0;
            while (Pos != -1)
            {
                if (Contents[Pos] == OpenBracket)
                    Count++;
                else
                    Count--;
                if (Count == 0)
                    return Pos;

                Pos = Contents.IndexOfAny(CharSet, Pos + 1);
            }

            return -1;
        }

        /// <summary>
        /// Convert a FORTRAN type APSIM name e.g. canopy_water_balance into a camel case name
        /// like CanopyWaterBalance
        /// </summary>
        public static string CamelCase(string Name)
        {
            int PosUnderScore = -1;
            do
            {
                string UpperChar = Name[PosUnderScore + 1].ToString();
                UpperChar = UpperChar.ToUpper();
                Name = Name.Remove(PosUnderScore + 1, 1);
                Name = Name.Insert(PosUnderScore + 1, UpperChar);

                if (PosUnderScore != -1)
                    Name = Name.Remove(PosUnderScore, 1);
                PosUnderScore = Name.IndexOf('_');
            }
            while (PosUnderScore != -1);
            return Name;
        }

        /// <summary>
        /// A helper function for getting the parent name from the specified
        /// fully qualified name passed in. Assumes delimiter of '.'.
        /// e.g. if Name = .Paddock.ModelB
        ///      then returns .Paddock
        /// </summary>
        public static string ParentName(string Name)
        {
            int PosLastPeriod = Name.LastIndexOf('.');
            return Name.Substring(0, PosLastPeriod);
        }
    }
}
