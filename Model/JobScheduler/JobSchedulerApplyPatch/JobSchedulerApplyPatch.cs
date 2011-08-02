using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using CSGeneral;
using System.IO;

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
        // Run the patch.
        string PatchExecutable = Path.Combine(ApsimDirectoryName, "..", "BuildLibraries", "Pat-ch.exe");
        Process P = Utility.RunProcess(PatchExecutable,
                                       "-p0 -E -f -s -i " + StringManip.DQuote(PatchFileName), ApsimDirectoryName);
        string StdOut = Utility.CheckProcessExitedProperly(P);

        // Create an empty save directory under temp.
        string SaveDirectory = Path.Combine(Path.GetTempPath(), "SavedPatchFiles");
        if (Directory.Exists(SaveDirectory))
            Directory.Delete(SaveDirectory, true);
        Directory.CreateDirectory(SaveDirectory);

        // Copy each patched file to the save directory.
        foreach (KeyValuePair<string, int> PatchFile in Patch.FilesInPatch(PatchFileName, ApsimDirectoryName))
        {
            string FileName = PatchFile.Key;
            string DestFileName = FileName.ToLower();
            DestFileName = DestFileName.Replace(ApsimDirectoryName.ToLower(), SaveDirectory);
            if (File.Exists(FileName))
            {
                Directory.CreateDirectory(Path.GetDirectoryName(DestFileName));
                File.Copy(FileName, DestFileName);
            }
        }
    }
}

