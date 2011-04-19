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
    }
}