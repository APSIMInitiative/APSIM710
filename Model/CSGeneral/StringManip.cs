using System;
using System.Collections;
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
                for (int i = 0; (i<= (Values.Length - 1)); i++)
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
                for (int i = 0; (i<= (Values.Count - 1)); i++)
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



		// ------------------------------------------------------------------
		// This method splits values on a comma but also honours double quotes
		// ensuring something in double quotes is never split.
		//     eg: if text = value1, "value 2, 2a", value3
		//     then: words[0] = value1
		//           words[1] = value2, 2a
		//           words[2] = value3
		// ------------------------------------------------------------------
		public static StringCollection SplitStringHonouringQuotes(string Text, string Delimiters)
			{
			StringCollection ReturnStrings = new StringCollection();

			string DelimitersAndQuote = Delimiters + "\"";
			string DelimitersAndSpace = Delimiters + " ";
			int Start = IndexNotOfAny(Text, DelimitersAndSpace.ToCharArray());
			int Stop;
			while (Start != -1)
				{
				if (Text[Start] == '\"')
					{
					Stop = Text.IndexOf('\"', Start+1);
					if (Stop == -1)
						throw new Exception("Mismatched quotes in string: " + Text);
					Stop++;
					}
				else
					Stop = Text.IndexOfAny(DelimitersAndQuote.ToCharArray(), Start);
							
				if (Stop == -1)
					Stop = Text.Length;

				ReturnStrings.Add(Text.Substring(Start, Stop - Start));
				Start = IndexNotOfAny(Text, DelimitersAndSpace.ToCharArray(), Stop+1);
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
				ReturnString = St.Substring(PosOpenBracket+1, PosCloseBracket-PosOpenBracket-1).Trim();
				St = St.Remove(PosOpenBracket, PosCloseBracket-PosOpenBracket+1).Trim();
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
	}
}
