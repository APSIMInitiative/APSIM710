namespace ApsimFile
   {
   using System.IO;
   using System.Text;
   using System.Collections.Specialized;
   using System.Reflection;
   using System.Runtime.InteropServices;
   using System;
   using System.Collections.Generic;

   public class IniFile
      {
      [DllImport("kernel32", EntryPoint = "WritePrivateProfileStringW", CharSet = CharSet.Unicode, SetLastError = true, ExactSpelling = true)]
      private static extern Int32 WritePrivateProfileString(string lpApplicationName, string lpKeyName, string lpString, string lpFileName);
      [DllImport("kernel32", EntryPoint = "GetPrivateProfileStringW", CharSet = CharSet.Unicode)]
      private static extern Int32 GetPrivateProfileString(string lpApplicationName, string lpKeyName, string lpDefault, string lpReturnedString, int nSize, string lpFileName);
      [DllImport("kernel32", EntryPoint = "GetPrivateProfileSectionW", CharSet = CharSet.Unicode, SetLastError = true, ExactSpelling = true)]
      private static extern Int32 GetPrivateProfileSection(string lpApplicationName, string lpReturned, Int32 nSize, string lpFileName);

      // ----------------------------------------
      // Returns a key value from an .ini file.
      // ----------------------------------------
      private static string INIRead(string INIPath, string SectionName, string KeyName, string DefaultValue)
         {
         string functionReturnValue = null;
         // primary version of call gets single value given all parameters
         Int32 n;

         string sData = new string(' ', 32768);
         // allocate some room 
         n = GetPrivateProfileString(SectionName, KeyName, DefaultValue, sData, sData.Length, INIPath);
         if (n > 0)
            {
            // return whatever it gave us
            functionReturnValue = Configuration.RemoveMacros(sData.Substring(0, n));
            }
         else
            {
            functionReturnValue = "";
            }
         return functionReturnValue;
         }

      // ----------------------------------------
      // Returns the contents of a section
      // ----------------------------------------
      public static string INIReadSection(string INIPath, string SectionName)
         {
         Int32 n;
         string sData = new string(' ', 32768);
         // allocate some room 
         n = GetPrivateProfileSection(SectionName, sData, 32768, INIPath);
         string[] Values = sData.ToString().Split('\0');
         // change embedded NULLs to pipe chars
         string ReturnString = "";
         foreach (string Line in Values)
            {
            if (ReturnString != "")
               {
               ReturnString = ReturnString + "\r\n";
               }
            ReturnString = ReturnString + Line;
            }
         return ReturnString;
         }


      // ----------------------------------------
      // Returns a key value from an .ini file.
      // ----------------------------------------
      public static string INIRead(string INIPath, string SectionName, string KeyName)
         {
         return INIRead(INIPath, SectionName, KeyName, "");
         }


      // --------------------------------------------------
      // Returns multiple key values from an .ini file.
      // Assumes the values use a numbering system appended
      // to the key name.
      // --------------------------------------------------
      public static StringCollection INIReadMultiple(string INIPath, string SectionName, string KeyName)
         {
         StringCollection Values = new StringCollection();
         int KeyNumber = 1;
         string Value = INIRead(INIPath, SectionName, KeyName + KeyNumber.ToString());
         while (Value != "")
            {
            Values.Add(Value);
            KeyNumber = KeyNumber + 1;
            Value = INIRead(INIPath, SectionName, KeyName + KeyNumber.ToString());
            }
         return Values;
         }


      // -----------------------------------------------------
      // Returns all key names from a section of an .ini file.
      // -----------------------------------------------------
      public static string[] INIReadAllKeys(string INIPath, string SectionName)
         {
         string Value = INIRead(INIPath, SectionName, null, "");
         char[] Separator = { '\0' };
         return Value.Split(Separator, StringSplitOptions.RemoveEmptyEntries);
         }

      public static string[] INIReadAllSections(string INIPath)
         {
         // returns all section names given just path
         string Value = INIRead(INIPath, null, null, "");
         char[] Separator = { '\0' };
         string[] Values = Value.Split(Separator, StringSplitOptions.RemoveEmptyEntries);
         return Values;
         }

      public static void INIWrite(string INIPath, string SectionName, string KeyName, string TheValue)
         {
         WritePrivateProfileString(SectionName, KeyName, TheValue, INIPath);
         }


      // --------------------------------------------------
      // Writes multiple key values from an .ini file.
      // Assumes the values use a numbering system appended
      // to the key name.
      // --------------------------------------------------
      public static void INIWriteMultiple(string INIPath, string SectionName, string KeyName, string[] Values)
    {
        int KeyNumber = 1;
        string Value = INIRead(INIPath, SectionName, KeyName + KeyNumber.ToString());
        while (Value != "") {
            INIDeleteKey(INIPath, SectionName, KeyName + KeyNumber.ToString());
            KeyNumber = KeyNumber + 1;
            Value = INIRead(INIPath, SectionName, KeyName + KeyNumber.ToString());
        }
        
        KeyNumber = 1;
        foreach ( string ValueString in Values) {
        INIWrite(INIPath, SectionName, KeyName + KeyNumber.ToString(), ValueString);
            KeyNumber = KeyNumber + 1;
        }
    }


      public static void INIDeleteKey(string INIPath, string SectionName, string KeyName)
         {
         // delete single line from section
         WritePrivateProfileString(SectionName, KeyName, null, INIPath);
         }

      public static void INIDeleteSection(string INIPath, string SectionName)
         {
         // delete section from INI file
         WritePrivateProfileString(SectionName, null, null, INIPath);
         }

      }

   }
