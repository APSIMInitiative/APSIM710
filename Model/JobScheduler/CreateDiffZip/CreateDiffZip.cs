using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Diagnostics;
using CSGeneral;
using System.Xml;
using System.Net;
using System.Net.Sockets;

class Program
{

    /// <summary>
    /// This program looks in a specified directory for all SVN diffs. It then zip's them up 
    /// and puts the .zip file into C:\\inetpub\\wwwroot\\Files\\%PatchFileName%.Diffs.zip
    /// Assumes that C:\Program Files\7-Zip\7z.exe exists. 
    /// </summary>
    static int Main(string[] args)
    {
        try
        {

            return Go(args);
        }
        catch (Exception err)
        {
            Console.WriteLine(err.ToString());
            return 1;
        }
    }

    private static int Go(string[] args)
    {
        Dictionary<string, string> Macros = Utility.ParseCommandLine(args);
        if (!Macros.ContainsKey("Directory") || !Macros.ContainsKey("PatchFileName"))
            throw new Exception("Usage: CreateDiffZip Directory=C:\\Apsim PatchFileName=xxx");
        

        string DirectoryName = Macros["Directory"].Replace('\\', '/');
        Directory.SetCurrentDirectory(DirectoryName);

        string PatchFileName = Macros["PatchFileName"];

        // Find SVN.exe on the path.
        string SVNFileName;
        if (Path.DirectorySeparatorChar == '/') SVNFileName = Utility.FindFileOnPath("git");
        else SVNFileName = Utility.FindFileOnPath("git.exe");
        if (SVNFileName == "")
            throw new Exception("Cannot find git on PATH");

        // Run an SVN stat command
        Process P = Utility.RunProcess(SVNFileName, "status --porcelain", ".");
        string StdOut = Utility.CheckProcessExitedProperly(P);
        string[] Lines = StdOut.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);

        string TempDirectory = Path.GetTempPath() + PatchFileName;

        TempDirectory = TempDirectory.Replace('\\','/');
        if (Directory.Exists(TempDirectory))
            Directory.Delete(TempDirectory, true);
        Directory.CreateDirectory(TempDirectory);
        // Copy all files reported to be different to our temporary directory.
        List<string> ModifiedFiles = new List<string>();
        foreach (string Line in Lines)
        {
            if (Line.Length >= 9)
            {
                string FileName = Line.Substring(3).Trim('"');
                string Status = Line.Substring(0, 2);
                string extension = Path.GetExtension(FileName);
                if (!Directory.Exists(FileName) && 
                    (extension == ".out" || extension == ".sum") &&
                    Status != " D" && Status != "??")
                   ModifiedFiles.Add(FileName);
            }
        }

        // For each modified file, copy to temp directory
        foreach (string FileName in ModifiedFiles)
        {
            string FullDestFileName =  TempDirectory + "/" + FileName;
            CopyFileToSVNTempDirectory( FileName, FullDestFileName);
        }

        // Now loop through all entries files in our temporary directory and remove entries
        // that don't apply.
        string outZip = "";
        if (Lines.Length > 0)
        {
            // Now zip up the whole temporary directory.
            outZip = Directory.GetCurrentDirectory() + Path.DirectorySeparatorChar;
            if (File.Exists(PatchFileName)) 
               outZip += Path.GetFileNameWithoutExtension(PatchFileName);
            else             
               outZip += PatchFileName;
            outZip += ".diffs.zip";

            string zipExe;

            if (Path.DirectorySeparatorChar == '/')
                zipExe = Utility.FindFileOnPath("7za");
            else
                zipExe = "C:\\Program Files\\7-Zip\\7z.exe";

            Console.WriteLine("Creating diff file: " + outZip);

            P = Utility.RunProcess(zipExe, "a -tzip \"" + outZip + "\" \"" + TempDirectory + "\"", Path.GetTempPath());
            Utility.CheckProcessExitedProperly(P);

            if (Environment.MachineName.ToUpper() == "BOB")
                {
                File.Copy(outZip, "C:\\inetpub\\wwwroot\\Files\\" + Path.GetFileName(outZip), true);
                File.Delete(outZip);
                }
        }
        // Now get rid of our temporary directory.
        Directory.Delete(TempDirectory, true);

        // Now report the number of diffs to the Builds Database.
        return ReportNumDiffs(DirectoryName, ModifiedFiles, PatchFileName, outZip);
    }



    private static void CopyFileToSVNTempDirectory( string FullSourceFileName, string FullDestFileName)
    {
        // Copy the file that's different.
        Directory.CreateDirectory(Path.GetDirectoryName(FullDestFileName));
        if (File.Exists(FullSourceFileName))
        {
            File.Copy(FullSourceFileName, FullDestFileName, true);
            File.SetAttributes(FullDestFileName, FileAttributes.Archive);
        }
        else
        {
            StreamWriter MissingFile = new StreamWriter(FullDestFileName + ".missing");
            MissingFile.Write(" ");
            MissingFile.Close();
        }

    }

    /// <summary>
    /// Report the number of diffs to the database.
    /// </summary>
    private static int ReportNumDiffs(string ApsimDirectoryName, List<string> ModifiedFiles, string PatchFileName, string outZip)
    {
        // Some of the diffs in ModifiedFiles will be from the patch, so to get a count of
        // the number of files that were changed by APSIM running we need to remove those
        // files from the list that were sent in the patch.
        string[] PatchFileNames = new string [0];
        if (File.Exists(PatchFileName)) 
          PatchFileNames = Zip.FileNamesInZip(PatchFileName, "");

        foreach (string FileNameInPatch in PatchFileNames)
        {
            Stream PatchContents = Zip.UnZipFile(PatchFileName, FileNameInPatch, "");
            if (PatchContents == null)
                throw new Exception("missing file " + FileNameInPatch + " in patchfile");

            string localVersionOfPatchedFile = Path.Combine(ApsimDirectoryName,FileNameInPatch).Replace('\\', '/');
            bool AreEqual;
            if (Path.GetFileName(FileNameInPatch) == "DotNetProxies.cs")
                AreEqual = true;
            else
                AreEqual = FilesAreIdentical(PatchContents, localVersionOfPatchedFile);

            // If the files are identical then remove it from the list of ModifiedFiles,
            // otherwise make sure it is in the list.
            int I = StringManip.IndexOfCaseInsensitive(ModifiedFiles, localVersionOfPatchedFile);
            if (AreEqual)
            {
                if (I != -1)
                    ModifiedFiles.RemoveAt(I);
            }
            else
            {
                if (I == -1 && Path.GetFileName(FileNameInPatch) != "patch.revisions")
                    ModifiedFiles.Add(localVersionOfPatchedFile);
            }
        }

        // Now we should have a list of the "real" diffs.
        int JobID = Convert.ToInt32(System.Environment.GetEnvironmentVariable("JobID"));
        string url = "http://www.apsim.info/APSIM.Builds.Service/BuildsClassic.svc/UpdateNumDiffs" +
                             "?JobID=" + JobID +
                             "&NumDiffs=" + ModifiedFiles.Count +
                             "&DbConnectPassword=" + Environment.GetEnvironmentVariable("DB_CONN_PSW");
        Utils.REST.CallService<object>(url);
        if (ModifiedFiles.Count != 0)
        {
            Console.WriteLine("Files that are different:");
            foreach (string FileName in ModifiedFiles)
                Console.WriteLine(FileName);

            Console.WriteLine("Build is not clean");
            return 1;
        }
        return 0;
    }

    /// <summary>
    /// Do a binary comparison of the 2 specified files. Return true if they compare the same.
    /// </summary>
    private static bool FilesAreIdentical(Stream FileFromPatch, string FileName)
    {
        // If user has put a deleted file into the patch then always signal true.
        if (FileFromPatch.Length == 0)
            return true;

        // If Bob doesn't have the file on disk then this is a diff - signal false.
        if (!File.Exists(FileName))
            return false;

        FileStream fs = new FileStream(FileName, FileMode.Open, FileAccess.Read, System.IO.FileShare.Read);

        // If we get this far then do a binary comparison.
        FileFromPatch.Seek(0, SeekOrigin.Begin);
        BinaryReader File1 = new BinaryReader(FileFromPatch);
        BinaryReader File2 = new BinaryReader(fs);

        bool Ok = BinaryComparison(File1, File2);
        File1.Close();
        File2.Close();
        fs.Close();
        return Ok;

    }

    /// <summary>
    /// Do a binary comparison of the 2 specified files. Return true if they compare the same.
    /// </summary>
    private static bool FilesAreIdentical(string FileName1, string FileName2)
    {
        if (!File.Exists(FileName1) && !File.Exists(FileName2))
            return true;
        else if (!File.Exists(FileName1) || !File.Exists(FileName2))
            return false;

        FileStream fs1 = new FileStream(FileName1, FileMode.Open, FileAccess.Read, System.IO.FileShare.Read);
        FileStream fs2 = new FileStream(FileName2, FileMode.Open, FileAccess.Read, System.IO.FileShare.Read);

        BinaryReader File1 = new BinaryReader(fs1);
        BinaryReader File2 = new BinaryReader(fs2);

        bool Ok = BinaryComparison(File1, File2);
        File1.Close();
        File2.Close();
        fs1.Close();
        fs2.Close();
        return Ok;
    }

    private static bool BinaryComparison(BinaryReader File1, BinaryReader File2)
    {
        byte[] Contents1 = new byte[100];
        byte[] Contents2 = new byte[100];

        int NumBytesRead1 = File1.Read(Contents1, 0, 100);
        int NumBytesRead2 = File2.Read(Contents2, 0, 100);

        bool Ok = (NumBytesRead1 == NumBytesRead2);

        while (Ok && NumBytesRead1 > 0)
        {
            // Compare the byte arrays.
            for (int i = 0; i < NumBytesRead1; i++)
            {
                if (Contents1[i] != Contents2[i])
                {
                    Ok = false;
                    break;
                }
            }

            if (Ok)
            {
                // Read next set of bytes.
                NumBytesRead1 = File1.Read(Contents1, 0, 100);
                NumBytesRead2 = File2.Read(Contents2, 0, 100);
                Ok = (NumBytesRead1 == NumBytesRead2);
            }
        }
        return Ok;
    }
}
