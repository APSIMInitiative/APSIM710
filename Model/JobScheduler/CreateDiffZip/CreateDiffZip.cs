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

            Go(args);
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

    private static void Go(string[] args)
    {
        Dictionary<string, string> Macros = Utility.ParseCommandLine(args);
        if (!Macros.ContainsKey("Directory") || !Macros.ContainsKey("PatchFileName"))
            throw new Exception("Usage: CreateDiffZip Directory=C:\\Apsim PatchFileName=xxx");
        

        string DirectoryName = Macros["Directory"].Replace('\\', '/');
        if (!Directory.Exists(DirectoryName)) { throw new Exception("Directory " + DirectoryName + " does not exist"); }

        string PatchFileName = Macros["PatchFileName"];
        
        if (!File.Exists(PatchFileName)) { throw new Exception("PatchFileName " + PatchFileName + " does not exist"); }

        // Find SVN.exe on the path.
        string SVNFileName;
        if (Path.DirectorySeparatorChar == '/') SVNFileName = Utility.FindFileOnPath("svn");
        else SVNFileName = Utility.FindFileOnPath("svn.exe");
        if (SVNFileName == "")
            throw new Exception("Cannot find svn on PATH");

        // Run an SVN stat command
        Process P = Utility.RunProcess(SVNFileName, "-q stat", DirectoryName);
        string StdOut = Utility.CheckProcessExitedProperly(P);
        string[] Lines = StdOut.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);

        string TempDirectory = (Path.GetTempPath() + Path.GetFileNameWithoutExtension(PatchFileName)).Replace('\\','/');
        if (Directory.Exists(TempDirectory))
            Directory.Delete(TempDirectory, true);
        Directory.CreateDirectory(TempDirectory);
        // Copy all files reported to be different to our temporary directory.
        List<string> ModifiedFiles = new List<string>();
        foreach (string Line in Lines)
        {
            if (Line.Length >= 9)
            {
                string FileName = Line.Substring(8);
                string FileExt = Path.GetExtension(FileName);
                string Status = Line.Substring(0, 1);
                string FullSourceFileName = Path.Combine(DirectoryName, FileName).Replace('\\', '/'); ;
                if (Path.DirectorySeparatorChar == '/' &&
                    !Directory.Exists(FullSourceFileName) &&
                    Status != "D" &&
                    FileExt == ".sum")
                {
                   Process D = Utility.RunProcess("/bin/sh", "-c \"svn diff --diff-cmd diff -x --ignore-matching-lines=Component\\|INPUT -x -uw \\\"" + FileName +"\\\" | wc -l\"", DirectoryName);
                   int numlines = Convert.ToInt32(Utility.CheckProcessExitedProperly(D));
                   if (numlines > 2)
                     ModifiedFiles.Add(FullSourceFileName);

                } else if (!Directory.Exists(FullSourceFileName) &&
                           Path.GetFileName(FullSourceFileName) != "DotNetProxies.cs" &&
                           Status != "D")
                     ModifiedFiles.Add(FullSourceFileName);
            }
        }


        // For each modified file, copy to temp directory
        foreach (string FileName in ModifiedFiles)
        {
            string FullDestFileName = FileName.Replace(DirectoryName, TempDirectory);
            CopyFileToSVNTempDirectory( FileName, FullDestFileName);
        }

        // Now loop through all entries files in our temporary directory and remove entries
        // that don't apply.
        if (Lines.Length > 0)
        {
            List<string> Entries = new List<string>();
            Utility.FindFiles(TempDirectory, "entries", ref Entries, true, true);
            foreach (string FileName in Entries)
                ModifyEntriesFile(FileName);

            // Now zip up the whole temporary directory.
            string outZip = Path.Combine(Directory.GetCurrentDirectory(), 
                                         Path.GetFileNameWithoutExtension(PatchFileName) + ".diffs.zip");
            string zipExe;

            if (Path.DirectorySeparatorChar == '/')
                zipExe = Utility.FindFileOnPath("7za");
            else
                zipExe = "C:\\Program Files\\7-Zip\\7z.exe";
            P = Utility.RunProcess(zipExe, "a -tzip \"" + outZip + "\" \"" + TempDirectory + "\"", Path.GetTempPath());
            Utility.CheckProcessExitedProperly(P);

            if (Environment.MachineName.ToUpper() == "BOB")
                {
                File.Copy(outZip, "C:\\inetpub\\wwwroot\\Files\\" + Path.GetFileName(outZip), true);
                File.Delete(outZip);
                }
            // Now get rid of our temporary directory.
            Directory.Delete(TempDirectory, true);
        }

        // Now report the number of diffs to the Builds Database.
        ReportNumDiffs(DirectoryName, ModifiedFiles, PatchFileName);
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

        // Also copy the SVN files required.
        string SVNSourceDirectory = Path.Combine(Path.GetDirectoryName(FullSourceFileName), ".svn");
        string SVNDestDirectory = Path.Combine(Path.GetDirectoryName(FullDestFileName), ".svn");
        string ShortFileName = Path.GetFileName(FullDestFileName);

        Directory.CreateDirectory(SVNDestDirectory);
        File.SetAttributes(SVNDestDirectory, FileAttributes.Hidden);

        if (File.Exists(Path.Combine(SVNSourceDirectory, "prop-base", ShortFileName + ".svn-base")))
        {
            Directory.CreateDirectory(Path.Combine(SVNDestDirectory, "prop-base"));
            File.Copy(Path.Combine(SVNSourceDirectory, "prop-base", ShortFileName + ".svn-base"),
                      Path.Combine(SVNDestDirectory, "prop-base", ShortFileName + ".svn-base"), true);
            File.SetAttributes(Path.Combine(SVNDestDirectory, "prop-base", ShortFileName + ".svn-base"), FileAttributes.Archive);
        }

        if (File.Exists(Path.Combine(SVNSourceDirectory, "text-base", ShortFileName + ".svn-base")))
        {
            Directory.CreateDirectory(Path.Combine(SVNDestDirectory, "text-base"));
            File.Copy(Path.Combine(SVNSourceDirectory, "text-base", ShortFileName + ".svn-base"),
                      Path.Combine(SVNDestDirectory, "text-base", ShortFileName + ".svn-base"), true);
            File.SetAttributes(Path.Combine(SVNDestDirectory, "text-base", ShortFileName + ".svn-base"), FileAttributes.Archive);
        }
        CopySVNFilesForDirectory(SVNSourceDirectory, SVNDestDirectory);
    }

    /// <summary>
    /// Copy all the required SVN files for the specified source directory and all of it's parent directories.
    /// </summary>
    private static void CopySVNFilesForDirectory(string SVNSourceDirectory, string SVNDestDirectory)
    {
        if (!File.Exists(Path.Combine(SVNDestDirectory, "all-wcprops")))
        {
            Directory.CreateDirectory(SVNDestDirectory);
            File.SetAttributes(SVNDestDirectory, FileAttributes.Hidden);
            if (Directory.Exists(SVNSourceDirectory))
                foreach (string SVNName in Directory.GetFiles(SVNSourceDirectory))
                {
                    File.Copy(SVNName, Path.Combine(SVNDestDirectory, Path.GetFileName(SVNName)), true);
                    File.SetAttributes(Path.Combine(SVNDestDirectory, Path.GetFileName(SVNName)), FileAttributes.Archive);
                }
            // Now see if we need to do parent directory as well.
            SVNSourceDirectory = Path.GetFullPath(Path.Combine(SVNSourceDirectory, "..", ".."));
            if (Directory.Exists(Path.Combine(SVNSourceDirectory,  ".svn")))
            {
                SVNSourceDirectory = Path.Combine(SVNSourceDirectory, ".svn");
                SVNDestDirectory = Path.GetFullPath(Path.Combine(SVNDestDirectory,  "..", ".svn"));
                CopySVNFilesForDirectory(SVNSourceDirectory, SVNDestDirectory);
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
                if (Directory.Exists(Path.Combine(Path.GetDirectoryName(FileName), "..", SVNFileName)))
                    i = CopyEntryBlockToOut(Lines, i, Out);
                else
                    i = SkipEntryBlock(Lines, i);
            }
            else if (Lines[i + 1] == "file")
            {
                string SVNFileName = Path.Combine(Path.GetDirectoryName(FileName), "..", Lines[i]);
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
        string[] PatchFileNames;
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
        ApsimBuildsDB Db = new ApsimBuildsDB();
        Db.Open();
        int JobID = Convert.ToInt32(System.Environment.GetEnvironmentVariable("JobID"));
        Db.SetNumDiffs(JobID, ModifiedFiles.Count);
        if (ModifiedFiles.Count != 0)
        {
            Console.WriteLine("Files that are different:");
            foreach (string FileName in ModifiedFiles)
                Console.WriteLine(FileName);

            string prefix = "";
            if (Environment.MachineName.ToUpper() != "BOB") prefix = Environment.MachineName;

            string DiffsFileName = "http://bob.apsim.info/files/" + Path.GetFileNameWithoutExtension(PatchFileName) + 
                (prefix != "" ? "." + prefix : "") + ".diffs.zip";
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
