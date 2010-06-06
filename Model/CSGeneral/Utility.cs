using System;
using System.IO;
//using System.Drawing;
//using System.Drawing.Imaging;
using System.Xml;
using System.Xml.Xsl;
using System.Xml.XPath;
using System.Diagnostics;


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

         // Loop through each character in the memo, writing each to a MemoryStream buffer
         foreach (char character in str.ToCharArray())
            mem.WriteByte(Convert.ToByte(character));

         // convert byte array characters to Base64 return it.
         return Convert.ToBase64String(mem.GetBuffer());

         }

      public static string ApplyStyleSheet(string Contents, string StylesheetContents)
         {
         //Create the XmlParserContext and reader.
         StringReader StyleSheetReader = new StringReader(StylesheetContents);
         XmlTextReader StyleSheet = new XmlTextReader(StyleSheetReader);

         //Load the stylesheet.
         XslCompiledTransform xslt = new XslCompiledTransform();
         //xslt.Load(StyleSheet, new XmlUrlResolver(), XmlSecureResolver.CreateEvidenceForUrl(""));
         xslt.Load("D:\\development\\APSIMUI\\ProtocolToVariables.xsl");

         //Load the file to transform.

         StringReader ContentsReader = new StringReader(Contents);
         XPathDocument doc = new XPathDocument(ContentsReader);

         //Create an XmlTextWriter which outputs to the console.
         StringWriter SWriter = new StringWriter();
         XmlTextWriter Writer = new XmlTextWriter(SWriter);

         //Transform the file and send the output to the console.
         xslt.Transform(doc, Writer);
         Writer.Close();
         return SWriter.ToString();
         }

      public static Process RunProcess(string Executable, string Arguments, string JobFolder)
         {
         if (!File.Exists(Executable))
            throw new System.Exception("Cannot execute file: " + Executable + ". File not found.");
         Process PlugInProcess = new Process();
         PlugInProcess.StartInfo.FileName = Executable;
         PlugInProcess.StartInfo.Arguments = Arguments;
         PlugInProcess.StartInfo.UseShellExecute = false;
         PlugInProcess.StartInfo.CreateNoWindow = true;
         PlugInProcess.StartInfo.RedirectStandardOutput = true;
         PlugInProcess.StartInfo.RedirectStandardError = true;
         PlugInProcess.StartInfo.WorkingDirectory = JobFolder;
         PlugInProcess.Start();
         return PlugInProcess;
         }
      public static void CheckProcessExitedProperly(Process PlugInProcess)
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
         }
      public static string ConvertURLToPath(string Url)
         {
         int PosColon = Url.IndexOf(':');
         if (PosColon != -1)
            return Url.Substring(PosColon + 4).Replace('/', '\\');
         return Url;
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
            FileName = Path.GetDirectoryName(FileName) + "\\" + BaseName + Number.ToString() + Path.GetExtension(FileName);
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
                  DeleteFiles(SubDirectory + "\\" + Path.GetFileName(FileSpec), true);
               }
            }
         }

      }
   }
