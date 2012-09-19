using System;
using System.IO;
using System.Net;
using System.Text;
using System.Xml;
using System.Xml.Xsl;
using System.Xml.XPath;
using System.Diagnostics;
using System.Collections.Generic;
using System.Reflection;
using System.Net.Sockets;
using System.Threading;
using System.Collections.Specialized;

namespace CSGeneral
{
    /// <summary>
    /// General utility functions
    /// </summary>
    public class Utility{
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
            // Determine whether or not the file is an executable; execute from the shell if it's not
            PlugInProcess.StartInfo.UseShellExecute = isManaged(Executable) == CompilationMode.Invalid;
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
                string msg = "";
                if (PlugInProcess.StartInfo.RedirectStandardOutput)
                   msg = PlugInProcess.StandardOutput.ReadToEnd();
                PlugInProcess.WaitForExit();
                if (PlugInProcess.ExitCode != 0)
                {
                    if (PlugInProcess.StartInfo.RedirectStandardError)
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
                if (SearchHiddenFolders || (File.GetAttributes(ChildDirectoryName) & FileAttributes.Hidden) != FileAttributes.Hidden)
                    FindFiles(ChildDirectoryName, FileSpec, ref FileNames, SearchHiddenFolders);
            }
        }

        /// <summary>
        /// Check for valid characters allowed in component names
        /// </summary>
        /// <param name="s">Test string</param>
        /// <returns>True if an invalid character is found</returns>
        public static bool CheckForInvalidChars(string s)
        {
            if ((s.Contains(",")) || (s.Contains(".")) || (s.Contains("/")) || (s.Contains("\\")) || (s.Contains("<")) || (s.Contains(">")) || (s.Contains("\"")) || (s.Contains("\'")) || (s.Contains("`")) || (s.Contains(":")) || (s.Contains("?")) || (s.Contains("|")) || (s.Contains("*")) || (s.Contains("&")) || (s.Contains("=")) || (s.Contains("!")))
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
			string[] Paths;
			string PathSeparator;
			
			if (Path.VolumeSeparatorChar == '/') 
				PathSeparator = ":";
			else
				PathSeparator = ";";

			Paths = PathVariable.Split(PathSeparator.ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
			
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

                // If we're on Linux, see if it's a hash bang script. Should really
                // check executable flag via Mono.Unix.Native.Syscall.stat() too
                if (Path.VolumeSeparatorChar == '/' &&
                    Convert.ToChar(data[0]) == '#' &&
                    Convert.ToChar(data[1]) == '!')
                    return CompilationMode.Native;
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

        /// <summary>
        /// Perform an insertion sort (stable sort) on the specified list.
        /// </summary>
        public static void StableSort<T>(IList<T> list, Comparison<T> comparison)
        {
            if (list == null)
                throw new ArgumentNullException("list");
            if (comparison == null)
                throw new ArgumentNullException("comparison");

            int count = list.Count;
            for (int j = 1; j < count; j++)
            {
                T key = list[j];

                int i = j - 1;
                for (; i >= 0 && comparison(list[i], key) > 0; i--)
                {
                    list[i + 1] = list[i];
                }
                list[i + 1] = key;
            }
        }


        /// <summary>
        /// Return all fields. The normal .NET reflection doesn't return private fields in base classes.
        /// This function does.
        /// </summary>
        public static List<FieldInfo> GetAllFields(Type type, BindingFlags flags)
        {
            if (type == typeof(Object)) return new List<FieldInfo>();

            var list = GetAllFields(type.BaseType, flags);
            // in order to avoid duplicates, force BindingFlags.DeclaredOnly
            list.AddRange(type.GetFields(flags | BindingFlags.DeclaredOnly));
            return list;
        }

        public static object GetValueOfFieldOrProperty(string Name, object Obj)
        {
            BindingFlags Flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.IgnoreCase;
            FieldInfo F = Obj.GetType().GetField(Name, Flags);
            if (F != null)
                return F.GetValue(Obj);

            PropertyInfo P = Obj.GetType().GetProperty(Name, Flags);
            if (P != null)
                return P.GetValue(Obj, null);

            return null;
        }

        /// <summary>
        ///  Upload a file via ftp
        /// </summary>
        /// <param name="localFileName">Name of the file to be uploaded</param>
        /// <param name="username">remote username</param>
        /// <param name="password">remote password</param>
        /// <param name="hostname">remote hostname</param>
        /// <param name="remoteFileName">Full path and name of where the file goes</param>
        /// <returns></returns>
        public static bool UploadFtp(string localFileName, string username, string password, string hostname, string remoteFileName)
        {
            // Get the object used to communicate with the server.
            FtpWebRequest request = (FtpWebRequest)WebRequest.Create("ftp://" + hostname + remoteFileName);
            request.Method = WebRequestMethods.Ftp.UploadFile;
            request.Credentials = new NetworkCredential();

            // Copy the contents of the file to the request stream.
            StreamReader sourceStream = new StreamReader(localFileName);
            byte[] fileContents = Encoding.UTF8.GetBytes(sourceStream.ReadToEnd());
            sourceStream.Close();
            request.ContentLength = fileContents.Length;

            Stream requestStream = request.GetRequestStream();
            requestStream.Write(fileContents, 0, fileContents.Length);
            requestStream.Close();

            FtpWebResponse response = (FtpWebResponse)request.GetResponse();
            string retVal = response.StatusDescription;
            response.Close();

            return retVal != "200";
        }


        /// <summary>
        /// Look through the specified string for an environment variable name surrounded by
        /// % characters. Replace them with the environment variable value.
        /// </summary>
        public static string ReplaceEnvironmentVariables(string CommandLine)
        {
            if (CommandLine == null)
                return CommandLine;

            int PosPercent = CommandLine.IndexOf('%');
            while (PosPercent != -1)
            {
                string Value = null;
                int EndVariablePercent = CommandLine.IndexOf('%', PosPercent + 1);
                if (EndVariablePercent != -1)
                {
                    string VariableName = CommandLine.Substring(PosPercent + 1, EndVariablePercent - PosPercent - 1);
                    Value = System.Environment.GetEnvironmentVariable(VariableName);
                    if (Value == null)
                        Value = System.Environment.GetEnvironmentVariable(VariableName, EnvironmentVariableTarget.User);
                }

                if (Value != null)
                {
                    CommandLine = CommandLine.Remove(PosPercent, EndVariablePercent - PosPercent + 1);
                    CommandLine = CommandLine.Insert(PosPercent, Value);
                    PosPercent = PosPercent + 1;
                }

                else
                    PosPercent = PosPercent + 1;

                if (PosPercent >= CommandLine.Length)
                    PosPercent = -1;
                else
                    PosPercent = CommandLine.IndexOf('%', PosPercent);
            }
            return CommandLine;
        }


        /// <summary>
        /// Send a string to the specified socket server. Returns the response string. Will throw
        /// if cannot connect.
        /// </summary>
        public static string SocketSend(string ServerName, int Port, string Data)
        {
            string Response = null;
            TcpClient Server = new TcpClient(ServerName, Convert.ToInt32(Port));
            try
            {
                Byte[] data = System.Text.Encoding.ASCII.GetBytes(Data);
                Server.GetStream().Write(data, 0, data.Length);

                Byte[] bytes = new Byte[8192];

                // Wait for data to become available.
                while (!Server.GetStream().DataAvailable)
                    Thread.Sleep(10);

                // Loop to receive all the data sent by the client.
                while (Server.GetStream().DataAvailable)
                {
                    int NumBytesRead = Server.GetStream().Read(bytes, 0, bytes.Length);
                    Response += System.Text.Encoding.ASCII.GetString(bytes, 0, NumBytesRead);
                }
            }
            finally
            {
                Server.Close();
            }
            return Response;
        }

        /// <summary>
        /// Store all macros found in the command line arguments. Macros are keyword = value
        /// </summary>
        public static Dictionary<string, string> ParseCommandLine(string[] args)
        {
            Dictionary<string, string> Options = new Dictionary<string, string>(StringComparer.CurrentCultureIgnoreCase);
            for (int i = 0; i < args.Length; i++)
            {
                StringCollection MacroBits = CSGeneral.StringManip.SplitStringHonouringQuotes(args[i], "=");
                if (MacroBits.Count == 2)
                    Options.Add(MacroBits[0].Replace("\"", ""), MacroBits[1].Replace("\"", ""));
            }
            return Options;
        }

    }
}
