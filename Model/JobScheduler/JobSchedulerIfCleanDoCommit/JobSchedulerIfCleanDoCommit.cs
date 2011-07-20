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
        int NumDiffs = DB.GetNumDiffs();
        Dictionary<string, object> Details = DB.GetDetails();
        if (Details["DoCommit"].ToString() == "0")
            Console.WriteLine("The commit option was unchecked on the upload patch form");

        else if (NumDiffs == 0)
        {
            // Find SVN.exe on the path.
            string SVNFileName = Utility.FindFileOnPath("svn.exe");
            if (SVNFileName == "")
                throw new Exception("Cannot find svn.exe on PATH");

            // Some of the files in the patch file will be additions or deletions. We need to tell SVN 
            // about these before we do a commit.
            string Arguments = " --verbose -t --dry-run -p0 -i \"c:\\Upload\\" + PatchFileName + ".patch\"";
            Process P = Utility.RunProcess(ApsimDirectory + "\\..\\BuildLibraries\\pat-ch.exe",
                                           Arguments, ApsimDirectory);
            string StdOut = P.StandardOutput.ReadToEnd();
            P.WaitForExit();
            string[] StdOutLines = StdOut.Split("\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
            StdOut = "";
            foreach (string Line in StdOutLines)
            {
                if (Line.IndexOf("|--- ") == 0)
                {
                    int PosTab = Line.IndexOf('\t');
                    if (PosTab != -1)
                    {
                        Arguments = null;

                        string FileName = Line.Substring(5, PosTab - 5);
                        FileName = Path.Combine(ApsimDirectory, FileName);
                        FileName = FileName.Replace("/", "\\");
                        if (Line.Contains("(revision 0)"))
                        {
                            // File has been added in this patch.
                            Arguments += "add --force " + FileName;
                        }
                        else
                        {
                            FileInfo info = new FileInfo(FileName);
                            if (info.Length == 0)
                            {
                                // File has been deleted in this patch - add to SVN.
                                Arguments += "delete --force " + FileName;
                            }
                        }

                        if (Arguments != null)
                        {
                            EnsureDirectoryIsUnderSVN(Path.GetDirectoryName(FileName), SVNFileName);
                            Console.WriteLine(SVNFileName + " " + Arguments);
                            P = Utility.RunProcess(SVNFileName, Arguments, ApsimDirectory);
                            Console.WriteLine(Utility.CheckProcessExitedProperly(P));
                        }
                    }

                }
            }


            Arguments = "commit --username " + Details["UserName"] + " --password " + Details["Password"];
            P = Utility.RunProcess(SVNFileName, Arguments, ApsimDirectory);
            StdOut += Utility.CheckProcessExitedProperly(P);
            Console.WriteLine(StdOut);
        }
        else
            Console.WriteLine("Not clean - no commit");
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

