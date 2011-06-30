using System;
using System.IO;
//using System.Drawing;
//using System.Drawing.Imaging;
using System.Xml;
using System.Xml.Xsl;
using System.Xml.XPath;
using System.Diagnostics;
using System.Collections.Generic;


namespace CSGeneral
{
    /// <summary>
    /// General utility functions
    /// </summary>
    public class Utility
    {
        public static string EncodeBase64ToString(string base64String)
        {
            // Convert Base64 string back to a simmple string

            // No memos then exit
            if (base64String.Equals("")) return "";


            //Open up MemoryStream object to obtain an array of character bytes
            System.IO.MemoryStream mem = new System.IO.MemoryStream(
                  Convert.FromBase64String(base64String));

            string str = "";
            byte[] bite = mem.ToArray();

            // Loop through array adding each character byte to the end of a string
            foreach (byte abyte in bite)
                str += Convert.ToChar(abyte);

            //return formatted string
            return str;

        }

        public static string EncodeStringToBase64(string str)
        {
            // Converts given string to Base64

            System.IO.MemoryStream mem = new System.IO.MemoryStream(str.Length);
            StreamWriter Out = new StreamWriter(mem);
            Out.Write(str);

            // Loop through each character in the memo, writing each to a MemoryStream buffer
            //foreach (char character in str.ToCharArray())
            //    mem.WriteByte(Convert.ToByte(character));

            // convert byte array characters to Base64 return it.
            return Convert.ToBase64String(mem.GetBuffer());

        }

        public static Process RunProcess(string Executable, string Arguments, string JobFolder)
        {
            if (!File.Exists(Executable))
                throw new System.Exception("Cannot execute file: " + Executable + ". File not found.");
            Process PlugInProcess = new Process();
            PlugInProcess.StartInfo.FileName = Executable;
            PlugInProcess.StartInfo.Arguments = Arguments;
            PlugInProcess.StartInfo.UseShellExecute = Path.GetExtension(Executable) != ".exe";
            PlugInProcess.StartInfo.CreateNoWindow = true;
            if (!PlugInProcess.StartInfo.UseShellExecute)
            {
                PlugInProcess.StartInfo.RedirectStandardOutput = true;
                PlugInProcess.StartInfo.RedirectStandardError = true;
            }
            PlugInProcess.StartInfo.WorkingDirectory = JobFolder;
            PlugInProcess.Start();
            return PlugInProcess;
        }
        public static string CheckProcessExitedProperly(Process PlugInProcess)
        {
            if (!PlugInProcess.StartInfo.UseShellExecute)
            {
                string msg = PlugInProcess.StandardOutput.ReadToEnd();
                PlugInProcess.WaitForExit();
                if (PlugInProcess.ExitCode != 0)
                {
                    msg += PlugInProcess.StandardError.ReadToEnd();
                    if (msg != "")
                        throw new System.Exception("Error from " + Path.GetFileName(PlugInProcess.StartInfo.FileName) + ": "
                                                                 + msg);
                }
                return msg;
            }
            else
                return "";
        }
        public static string ConvertURLToPath(string Url)
        {
            Uri uri = new Uri(Url);
            return uri.LocalPath;
        }

        public static void EnsureFileNameIsUnique(ref string FileName)
        {
            // -------------------------------------------------------------
            // Make sure the filename is unique. i.e. add a number to the end
            // to force uniqueness.
            // -------------------------------------------------------------
            string BaseName = Path.GetFileNameWithoutExtension(FileName);
            int Number = 1;
            while (File.Exists(FileName))
            {
                FileName = Path.Combine(Path.GetDirectoryName(FileName), BaseName + Number.ToString() + Path.GetExtension(FileName));
                Number++;
            }
            if (File.Exists(FileName))
                throw new Exception("Cannot find a unique filename for file: " + BaseName);
        }
        public static void DeleteFiles(string FileSpec, bool Recurse)
        {
            if (Directory.Exists(Path.GetDirectoryName(FileSpec)))
            {
                foreach (string FileName in Directory.GetFiles(Path.GetDirectoryName(FileSpec),
                                                               Path.GetFileName(FileSpec)))
                    File.Delete(FileName);
                if (Recurse)
                {
                    foreach (string SubDirectory in Directory.GetDirectories(Path.GetDirectoryName(FileSpec)))
                        DeleteFiles(Path.Combine(SubDirectory, Path.GetFileName(FileSpec)), true);
                }
            }
        }

        public static void FindFiles(string DirectoryName, string FileSpec, ref List<string> FileNames,
                                     bool SearchHiddenFolders)
        {
            foreach (string FileName in Directory.GetFiles(DirectoryName, FileSpec))
                FileNames.Add(FileName);

            foreach (string ChildDirectoryName in Directory.GetDirectories(DirectoryName))
            {
                if (SearchHiddenFolders || File.GetAttributes(ChildDirectoryName) != FileAttributes.Hidden)
                    FindFiles(ChildDirectoryName, FileSpec, ref FileNames, SearchHiddenFolders);
            }
        }


        public static bool CheckForInvalidChars(string s)
        {

            if ((s.Contains("/")) || (s.Contains("\\")) || (s.Contains("<")) || (s.Contains(">")) || (s.Contains("\"")) || (s.Contains("\'")) || (s.Contains("`")) || (s.Contains(":")) || (s.Contains("?")) || (s.Contains("|")) || (s.Contains("*")) || (s.Contains("&")) || (s.Contains("=")) || (s.Contains("!")))
            {
                return true;
            }
            else { return false; }


        }

        public static string FindFileOnPath(string FileName)
        {
            string PathVariable = Environment.GetEnvironmentVariable("PATH");
            if (PathVariable == null)
                throw new Exception("Cannot find PATH environment variable");

            string[] Paths = PathVariable.Split(";".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            foreach (string DirectoryName in Paths)
            {
                string FullPath = Path.Combine(DirectoryName, FileName);
                if (File.Exists(FullPath))
                    return FullPath;
            }
            return "";
        }

        /// <summary>
        /// Returns true if the specified type T is of type TypeName
        public static bool IsOfType(Type T, String TypeName)
        {
            while (T != null)
            {
                if (T.ToString() == TypeName)
                    return true;

                if (T.GetInterface(TypeName) != null)
                    return true;

                T = T.BaseType;
            }
            return false;
        }
        public enum CompilationMode
        {
            /// <summary>
            /// Unknown
            /// </summary>
            Invalid,
            /// <summary>
            /// Native Win32 code
            /// </summary>
            Native,
            /// <summary>
            /// Common Language Runtime
            /// </summary>
            CLR,
            /// <summary>
            /// Mixed mode
            /// </summary>
            Mixed
        };
        //=========================================================================
        /// <summary>
        /// Determine if the file refered to is a native win32 or a CLR assembly.
        /// Mixed mode assemblies are CLR.
        /// Visual C++ Developer Center. http://msdn2.microsoft.com/en-us/library/c91d4yzb(VS.80).aspx
        /// </summary>
        /// <param name="filename">File name of the Assembly or native dll to probe.</param>
        /// <returns>Compilation mode.</returns>
        //=========================================================================
        static public CompilationMode isManaged(string filename)
        {
            try
            {
                byte[] data = new byte[4096];
                FileInfo file = new FileInfo(filename);
                Stream fin = file.Open(FileMode.Open, FileAccess.Read, FileShare.ReadWrite);
                /*Int32 iRead =*/
                fin.Read(data, 0, 4096);
                fin.Close();

                // If we are running on Linux, the executable/so will start with the string 0x7f + 'ELF'
                // If the 5 byte is 1, it's a 32-bit image (2 indicates 64-bit)
                // If the 16th byte is 3, it's a shared object; 2 means it's an executable
                // If it's a Mono/.Net assembly, we should the the "Windows" header

                // For now, if we're on Linux just see if it has an "ELF" header
                if (Path.VolumeSeparatorChar == '/' && data[0] == 0x7f && data[1] == 'E' && data[2] == 'L' && data[3] == 'F')
                    return CompilationMode.Native;

                // Verify this is a executable/dll
                if (UInt16FromBytes(data, 0) != 0x5a4d)
                    return CompilationMode.Invalid;

                uint headerOffset = UInt32FromBytes(data, 0x3c);  // This will get the address for the WinNT header

                //at the file offset specified at offset 0x3c, is a 4-byte
                //signature that identifies the file as a PE format image file. This signature is “PE\0\0”
                if (UInt32FromBytes(data, headerOffset) != 0x00004550)
                    return CompilationMode.Invalid;

                //uint machineType = UInt16FromBytes(data, headerOffset + 4); //type of machine
                uint optionalHdrBase = headerOffset + 24;
                //uint exportTableAddr = UInt32FromBytes(data, optionalHdrBase + 96);     //.edata
                uint exportTableSize = UInt32FromBytes(data, optionalHdrBase + 96 + 4); //.edata size

                Int32 iLightningAddr = (int)headerOffset + 24 + 208;    //CLR runtime header addr & size
                Int32 iSum = 0;
                Int32 iTop = iLightningAddr + 8;

                for (int i = iLightningAddr; i < iTop; ++i)
                    iSum |= data[i];

                if (iSum == 0)
                    return CompilationMode.Native;
                else
                {
                    if (exportTableSize > 0)
                        return CompilationMode.Mixed;
                    else
                        return CompilationMode.CLR;
                }
            }
            catch (Exception e)
            {
                throw (e);
            }
        }
        static private UInt32 UInt32FromBytes(byte[] p, uint offset)
        {
            return (UInt32)(p[offset + 3] << 24 | p[offset + 2] << 16 | p[offset + 1] << 8 | p[offset]);
        }
        static private UInt16 UInt16FromBytes(byte[] p, uint offset)
        {
            return (UInt16)(p[offset + 1] << 8 | p[offset]);
        }

        public static DateTime FromJulianDate(int julianDate)
        {

            // This implementation is only valid for dates in the Gregorian
            // calender (after 15 October 1582).
            // THIS IS BASED ON THE ALGORITHM BY FLIEGEL AND VAN FLANDERN IN
            // C.ACM VOL.11 (OCT,1968) P.657

            double work = julianDate + 68569.0;
            double work0 = (int)(4.0 * work / 146097.0);
            work = work - (int)((146097.0 * work0 + 3.0) / 4.0);
            double yy = (int)(4000.0 * (work + 1.0) / 1461001.0);

            work = work - (int)(1461.0 * yy / 4.0) + 31.0;
            double mm = (int)(80.0 * work / 2447.0);
            double day = work - (int)(2447.0 * mm / 80.0);

            work = (int)(mm / 11.0);
            double month = mm + 2.0 - 12.0 * work;
            double year = 100.0 * (work0 - 49.0) + yy + work;

            return new DateTime((int)year, (int)month, (int)day);
        }
        public static int ToJulianDate(DateTime d)
        {
            double Quotnt = (int)((d.Month - 14.0) / 12.0);

            return (int)(d.Day - 32075.0 + (int)(1461.0 * (d.Year + 4800.0 + Quotnt) /4.0)
                                    + (int)( 367.0 * (d.Month - 2.0    - Quotnt * 12.0) / 12.0)
                                    - (int)(   3.0 * (int)((d.Year + 4900.0 + Quotnt) / 100.0) /4.0));

        }


    }
}