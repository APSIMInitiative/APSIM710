using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using CSGeneral;
using System.IO;
using UIUtility;

class JobSchedulerApplyPatch
{
    /// <summary>
    /// This program applies a patch to an APSIM directory. It also copies all patched files to 
    /// a temporary directory so that they can be later compared with what Bob runs.
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            if (args.Length != 2)
                throw new Exception("Usage: JobSchedulerApplyPatch DirectoryName PatchFileName");

            Go(args[0], args[1]);
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

    private static void Go(string ApsimDirectoryName, string PatchFileName)
    {
        // Create an empty save directory under temp.
        string SaveDirectory = Path.Combine(Path.GetTempPath(), "SavedPatchFiles");
        if (Directory.Exists(SaveDirectory))
            Directory.Delete(SaveDirectory, true);

        string[] PatchedFileNames;
        if (Path.GetExtension(PatchFileName) == ".zip")
        {
            PatchedFileNames = Zip.UnZipFiles(PatchFileName, ApsimDirectoryName, "");
        }
        else
        {
            // Run the patch.
            string PatchExecutable = Path.Combine(ApsimDirectoryName, "..", "BuildLibraries", "Pat-ch.exe");
            Process P = Utility.RunProcess(PatchExecutable,
                                           "-p0 -E -f -s -i " + StringManip.DQuote(PatchFileName), ApsimDirectoryName);
            string StdOut = Utility.CheckProcessExitedProperly(P);

            Directory.CreateDirectory(SaveDirectory);
            PatchedFileNames = Patch.FilesInPatch(PatchFileName, ApsimDirectoryName);

            // Copy each patched file to the save directory.
            foreach (string FileName in PatchedFileNames)
            {
                string DestFileName = FileName.ToLower();
                DestFileName = DestFileName.Replace(ApsimDirectoryName.ToLower(), SaveDirectory);
                if (File.Exists(FileName))
                {
                    Directory.CreateDirectory(Path.GetDirectoryName(DestFileName));
                    File.Copy(FileName, DestFileName);
                }
            }
        }

        // Find SVN.exe on the path.
        string SVNFileName = Utility.FindFileOnPath("svn.exe");
        if (SVNFileName == "")
            throw new Exception("Cannot find svn.exe on PATH");

        // Get a list of files currently known to SVN
        Process SVN = Utility.RunProcess(SVNFileName, "-q stat", ApsimDirectoryName);
        string SVNStdOut = Utility.CheckProcessExitedProperly(SVN);
        string[] Lines = SVNStdOut.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
        List<string> SVNFileNames = new List<string>();
        foreach (string Line in Lines)
        {
            if (Line.Length >= 9)
            {
                string FileName = Line.Substring(8);
                SVNFileNames.Add(Path.Combine(ApsimDirectoryName, FileName));
            }
        }

        // Some of the files in the patch file will be additions or deletions. We need to tell SVN 
        // about these
        //List<string> FilesNotKnownToSVN = (List<string>) ;

        foreach (string FileName in PatchedFileNames)
        {
            string Arguments = null;

            FileInfo info = new FileInfo(FileName);
            if (!info.Exists || info.Length == 0)
                Arguments += "delete --force " + StringManip.DQuote(FileName);
            else if (!SVNFileNames.Contains(FileName))
                Arguments += "add --force " + StringManip.DQuote(FileName);

            if (Arguments != null)
            {
                EnsureDirectoryIsUnderSVN(Path.GetDirectoryName(FileName), SVNFileName);
                Console.WriteLine(SVNFileName + " " + Arguments);
                Process P = Utility.RunProcess(SVNFileName, Arguments, ApsimDirectoryName);
                Console.WriteLine(Utility.CheckProcessExitedProperly(P));
            }
        }
    }

    /// <summary>
    /// Ensure a directory is under SVN control.
    /// </summary>
    private static void EnsureDirectoryIsUnderSVN(string DirectoryName, string SVNFileName)
    {
        if (!Directory.Exists(Path.Combine(DirectoryName, ".svn")))
        {
            int PosSlash = DirectoryName.LastIndexOf('\\');
            if (PosSlash == -1)
                throw new Exception("Invalid directory found: " + DirectoryName);
            string ParentName = DirectoryName.Substring(0, PosSlash);
            string ChildName = DirectoryName.Substring(PosSlash + 1);
            if (!Directory.Exists(Path.Combine(ParentName, ".svn")))
                EnsureDirectoryIsUnderSVN(ParentName, SVNFileName);  // parent dir.
            else
            {
                Console.WriteLine(SVNFileName + " add ", DirectoryName);
                Process P = Utility.RunProcess(SVNFileName, "add " + DirectoryName, DirectoryName);
                Console.WriteLine(Utility.CheckProcessExitedProperly(P));
            }
        }
    }

}

