using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Diagnostics;
using CSGeneral;
using System.Xml;
using System.Net;
using System.Net.Sockets;
using UIUtility;

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
            if (args.Length != 2)
                throw new Exception("Usage: JobSchedulerCreateDiffZip DirectoryName PatchFileName");

            Go(args[0], args[1]);
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

    private static void Go(string DirectoryName, string PatchFileName)
    {
        // Get the revision number of this directory.
        string SVNFileName = Utility.FindFileOnPath("svn.exe");
        if (SVNFileName == "")
            throw new Exception("Cannot find svn.exe on PATH");

        // Run an SVN stat command
        Process P = Utility.RunProcess(SVNFileName, "-q stat", DirectoryName);
        string StdOut = Utility.CheckProcessExitedProperly(P);
        string[] Lines = StdOut.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);

        string TempDirectory = Path.GetTempPath() + Path.GetFileNameWithoutExtension(PatchFileName);
        if (Directory.Exists(TempDirectory))
            Directory.Delete(TempDirectory, true);

        // Copy all files reported to be different to our temporary directory.
        List<string> ModifiedFiles = new List<string>();
        foreach (string Line in Lines)
        {
            if (Line.Length >= 9)
            {
                string FileName = Line.Substring(8);
                string FullSourceFileName = Path.Combine(DirectoryName, FileName);
                string FullDestFileName = Path.Combine(TempDirectory, FileName);

                if (!Directory.Exists(FullSourceFileName) && Path.GetFileName(FullSourceFileName) != "DotNetProxies.cs")
                    ModifiedFiles.Add(FullSourceFileName);
            }
        }


        // For each modified file, copy to temp directory
        foreach (string FileName in ModifiedFiles)
        {
            string FullDestFileName = FileName.Replace(DirectoryName, TempDirectory);
            CopyFileToSVNTempDirectory(DirectoryName, FileName, FileName, FullDestFileName);
        }

        // Now loop through all entries files in our temporary directory and remove entries
        // that don't apply.
        if (Lines.Length > 0)
        {
            List<string> Entries = new List<string>();
            Utility.FindFiles(TempDirectory, "entries", ref Entries, true);
            foreach (string FileName in Entries)
                ModifyEntriesFile(FileName);

            // Now zip up the whole temporary directory.
            Directory.CreateDirectory("C:\\inetpub\\wwwroot\\Files");
            P = Utility.RunProcess("C:\\Program Files\\7-Zip\\7z.exe",
                                   "a -tzip \"C:\\inetpub\\wwwroot\\Files\\" + Path.GetFileNameWithoutExtension(PatchFileName) + ".diffs.zip\" \"" + Path.GetFileNameWithoutExtension(PatchFileName) + "\"",
                                   Path.GetTempPath());
            Utility.CheckProcessExitedProperly(P);

            // Now get rid of our temporary directory.
            Directory.Delete(TempDirectory, true);
        }

        // Now report the number of diffs to the Builds Database.
        ReportNumDiffs(DirectoryName, ModifiedFiles, PatchFileName);
    }



    private static void CopyFileToSVNTempDirectory(string DirectoryName, string FileName, string FullSourceFileName, string FullDestFileName)
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

        // Also copy the SVN files required.
        string SVNSourceDirectory = Path.GetDirectoryName(FullSourceFileName) + "\\.svn";
        string SVNDestDirectory = Path.GetDirectoryName(FullDestFileName) + "\\.svn";
        string ShortFileName = Path.GetFileName(FileName);

        Directory.CreateDirectory(SVNDestDirectory);
        File.SetAttributes(SVNDestDirectory, FileAttributes.Hidden);

        if (File.Exists(SVNSourceDirectory + "\\prop-base\\" + ShortFileName + ".svn-base"))
        {
            Directory.CreateDirectory(SVNDestDirectory + "\\prop-base");
            File.Copy(SVNSourceDirectory + "\\prop-base\\" + ShortFileName + ".svn-base",
                     SVNDestDirectory + "\\prop-base\\" + ShortFileName + ".svn-base", true);
            File.SetAttributes(SVNDestDirectory + "\\prop-base\\" + ShortFileName + ".svn-base", FileAttributes.Archive);
        }

        if (File.Exists(SVNSourceDirectory + "\\text-base\\" + ShortFileName + ".svn-base"))
        {
            Directory.CreateDirectory(SVNDestDirectory + "\\text-base");
            File.Copy(SVNSourceDirectory + "\\text-base\\" + ShortFileName + ".svn-base",
                      SVNDestDirectory + "\\text-base\\" + ShortFileName + ".svn-base", true);
            File.SetAttributes(SVNDestDirectory + "\\text-base\\" + ShortFileName + ".svn-base", FileAttributes.Archive);
        }
        CopySVNFilesForDirectory(SVNSourceDirectory, SVNDestDirectory, DirectoryName);
    }

    /// <summary>
    /// Copy all the required SVN files for the specified source directory and all of it's parent directories.
    /// </summary>
    private static void CopySVNFilesForDirectory(string SVNSourceDirectory, string SVNDestDirectory, string DirectoryName)
    {
        if (!File.Exists(SVNDestDirectory + "\\all-wcprops"))
        {
            Directory.CreateDirectory(SVNDestDirectory);
            File.SetAttributes(SVNDestDirectory, FileAttributes.Hidden);

            foreach (string SVNName in Directory.GetFiles(SVNSourceDirectory))
            {
                File.Copy(SVNName, SVNDestDirectory + "\\" + Path.GetFileName(SVNName), true);
                File.SetAttributes(SVNDestDirectory + "\\" + Path.GetFileName(SVNName), FileAttributes.Archive);
            }
            // Now see if we need to do parent directory as well.
            SVNSourceDirectory = Path.GetFullPath(SVNSourceDirectory + "\\..\\..");
            if (SVNSourceDirectory.Length >= DirectoryName.Length)
            {
                SVNSourceDirectory = SVNSourceDirectory + "\\.svn";
                SVNDestDirectory = Path.GetFullPath(SVNDestDirectory + "\\..\\..\\.svn");
                CopySVNFilesForDirectory(SVNSourceDirectory, SVNDestDirectory, DirectoryName);
            }
        }
    }

    /// <summary>
    /// Modify the specified SVN "entries" to remove all unwanted directory and file names.
    /// </summary>
    private static void ModifyEntriesFile(string FileName)
    {
        StreamReader In = new StreamReader(FileName);
        string[] Lines = In.ReadToEnd().Split("\n".ToCharArray());
        In.Close();
        File.Delete(FileName);

        StreamWriter Out = new StreamWriter(FileName);
        Out.Write(Lines[0] + "\n");
        int i = CopyEntryBlockToOut(Lines, 1, Out);

        while (i + 1 < Lines.Length)
        {
            if (Lines[i + 1] == "dir")
            {
                string SVNFileName = Lines[i];
                if (Directory.Exists(Path.GetDirectoryName(FileName) + "\\..\\" + SVNFileName))
                    i = CopyEntryBlockToOut(Lines, i, Out);
                else
                    i = SkipEntryBlock(Lines, i);
            }
            else if (Lines[i + 1] == "file")
            {
                string SVNFileName = Path.GetDirectoryName(FileName) + "\\..\\" + Lines[i];
                if (File.Exists(SVNFileName + ".missing"))
                {
                    i = CopyEntryBlockToOut(Lines, i, Out);
                    File.Delete(SVNFileName + ".missing");
                }
                else if (File.Exists(SVNFileName))
                    i = CopyEntryBlockToOut(Lines, i, Out);
                else
                    i = SkipEntryBlock(Lines, i);
            }
            else
                throw new Exception("Unknown SVN entities block: " + Lines[i + 1]);
        }

        Out.Close();
    }

    /// <summary>
    /// Copy an "entries" block to the specified out stream.
    /// </summary>
    private static int CopyEntryBlockToOut(string[] Lines, int i, StreamWriter Out)
    {
        string Delimiter = "\f";
        do
        {
            Out.Write(Lines[i] + "\n");
            i++;
        }
        while (Lines[i] != Delimiter);
        Out.Write(Delimiter + "\n");
        return i + 1;
    }

    /// <summary>
    /// Skip over this SVN entry block.
    /// </summary>
    private static int SkipEntryBlock(string[] Lines, int i)
    {
        string Delimiter = "\f";
        do
        {
            i++;
        }
        while (Lines[i] != Delimiter);
        return i + 1;
    }


    /// <summary>
    /// Report the number of diffs to the database.
    /// </summary>
    private static void ReportNumDiffs(string ApsimDirectoryName, List<string> ModifiedFiles, string PatchFileName)
    {
        // Some of the diffs in ModifiedFiles will be from the patch, so to get a count of
        // the number of files that were changed by APSIM running we need to remove those
        // files from the list that were sent in the patch.
        string SaveDirectory = Path.Combine(Path.GetTempPath(), "SavedPatchFiles");
        string[] PatchFileNames;
        PatchFileNames = Zip.FileNamesInZip(PatchFileName, "");

        for (int i = 0; i < PatchFileNames.Length; i++)
            PatchFileNames[i] = Path.Combine(ApsimDirectoryName, PatchFileNames[i]);

        foreach (string FileNameInPatch in PatchFileNames)
        {
            bool AreEqual;
            string RelativeFileName = FileNameInPatch.Replace(ApsimDirectoryName + "\\", "");
            string WindowsFileName = FileNameInPatch.Replace('/', '\\');
            Stream PatchContents = Zip.UnZipFile(PatchFileName, RelativeFileName, "");

            AreEqual = FilesAreIdentical(PatchContents, WindowsFileName);

            // If the files are identical then remove it from the list of ModifiedFiles,
            // otherwise make sure it is in the list.
            int I = StringManip.IndexOfCaseInsensitive(ModifiedFiles, WindowsFileName);
            if (AreEqual)
            {
                if (I != -1)
                    ModifiedFiles.RemoveAt(I);
            }
            else
            {
                if (I == -1 && Path.GetFileName(WindowsFileName) != "patch.revisions")
                    ModifiedFiles.Add(WindowsFileName);
            }
        }

        bool SomeJobsHaveFailed = JobScheduler.TalkToJobScheduler("GetVariable~SomeJobsHaveFailed") == "Yes";

        int JobID = Convert.ToInt32(JobScheduler.TalkToJobScheduler("GetVariable~JobID"));

        // Now we should have a list of the "real" diffs.
        ApsimBuildsDB Db = new ApsimBuildsDB();
        Db.Open();
        Db.SetNumDiffs(JobID, ModifiedFiles.Count);
        if (ModifiedFiles.Count == 0 && !SomeJobsHaveFailed)
            Db.UpdateStatus(JobID, "Pass");
        else
        {
            Console.WriteLine("Files that are different:");
            foreach (string FileName in ModifiedFiles)
                Console.WriteLine(FileName);
            Db.UpdateStatus(JobID, "Fail");
            string DiffsFileName = "http://bob.apsim.info/files/" + Path.GetFileNameWithoutExtension(PatchFileName) + ".diffs.zip";
            Db.UpdateDiffFileName(JobID, DiffsFileName);
            Db.Close();
            throw new Exception("Build is not clean");
        }

        Db.Close();

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

        // If we get this far then do a binary comparison.
        FileFromPatch.Seek(0, SeekOrigin.Begin);
        BinaryReader File1 = new BinaryReader(FileFromPatch);
        BinaryReader File2 = new BinaryReader(File.Open(FileName, FileMode.Open));

        bool Ok = BinaryComparison(File1, File2);
        File1.Close();
        File2.Close();
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

        BinaryReader File1 = new BinaryReader(File.Open(FileName1, FileMode.Open));
        BinaryReader File2 = new BinaryReader(File.Open(FileName2, FileMode.Open));

        bool Ok = BinaryComparison(File1, File2);
        File1.Close();
        File2.Close();
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

