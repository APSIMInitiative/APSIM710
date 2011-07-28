using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CSGeneral;
using System.Diagnostics;
using System.IO;


class JobSchedulerIfCleanDoCommit
{
    /// <summary>
    /// This program creates C:\\inetpub\\wwwroot\\Files\\Summary.html
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            if (args.Length != 2)
                throw new Exception("Usage: JobSchedulerIfCleanDoCommit.cs ApsimDirectoryName PatchFileName");

            Go(args[0], args[1]);
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }

    private static void Go(string ApsimDirectory, string PatchFileName)
    {
        ApsimBuildsDB DB = new ApsimBuildsDB();
        DB.Open();
        int JobID = Convert.ToInt32(JobScheduler.TalkToJobScheduler("GetVariable~JobID"));

        try
        {
            Dictionary<string, object> Details = DB.GetDetails(JobID);

            string Status = Details["Status"].ToString();
            if (Details["DoCommit"].ToString() == "0")
                Console.WriteLine("The commit option was unchecked on the upload patch form");

            else if (Status == "Pass")
            {
                // Find SVN.exe on the path.
                string SVNFileName = Utility.FindFileOnPath("svn.exe");
                if (SVNFileName == "")
                    throw new Exception("Cannot find svn.exe on PATH");

                // Some of the files in the patch file will be additions or deletions. We need to tell SVN 
                // about these before we do a commit.
                Dictionary<string, int> FileNames = Patch.FilesInPatch(PatchFileName, ApsimDirectory);
                foreach (KeyValuePair<string, int> Line in FileNames)
                {
                    string Arguments = null;
                    string FileName = Line.Key;
                    int Revision = Line.Value;
                    if (Revision == 0)
                    {
                        // File has been added in this patch.
                        Arguments += "add --force " + StringManip.DQuote(FileName);
                    }
                    else
                    {
                        FileInfo info = new FileInfo(FileName);
                        if (info.Exists && info.Length == 0)
                        {
                            // File has been deleted in this patch - add to SVN.
                            Arguments += "delete --force " + StringManip.DQuote(FileName);
                        }
                    }

                    if (Arguments != null)
                    {
                        EnsureDirectoryIsUnderSVN(Path.GetDirectoryName(FileName), SVNFileName);
                        Console.WriteLine(SVNFileName + " " + Arguments);
                        Process P = Utility.RunProcess(SVNFileName, Arguments, ApsimDirectory);
                        Console.WriteLine(Utility.CheckProcessExitedProperly(P));
                    }
                }

                string BugID = Details["BugID"].ToString();
                string Description = Details["Description"].ToString() + "\r\nbugid: " + BugID;
                string SVNArguments = "commit --username " + Details["UserName"] + " --password " + Details["Password"] +
                                  " -m " + StringManip.DQuote(Description);
                Process SVNP = Utility.RunProcess(SVNFileName, SVNArguments, ApsimDirectory);
                string StdOut = Utility.CheckProcessExitedProperly(SVNP);
                Console.WriteLine(StdOut);
            }
            else
                Console.WriteLine("Not clean - no commit");
        }
        catch (Exception err)
        {
            DB.UpdateStatus(JobID, "Fail");
            DB.Close();
            throw err;
        }
        DB.Close();

    }

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

