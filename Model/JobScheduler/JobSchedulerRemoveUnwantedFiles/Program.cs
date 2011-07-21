using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Diagnostics;
using CSGeneral;

class Program
{


    /// <summary>
    /// This program removes all SVN unversioned files from a specified directory.
    /// </summary>
    static int Main(string[] args)
    {
        try
        {
            // Make sure we have the right number of arguments.
            if (args.Length != 1)
                throw new Exception("Usage: RemoveUnversionedFiles directory");
            string directory = args[0];
            string SVNFileName = Utility.FindFileOnPath("svn.exe");
            if (SVNFileName == "")
                throw new Exception("Cannot find svn.exe on PATH");

            // Start an SVN process to return a list of unversioned files.
            Process P = Utility.RunProcess(SVNFileName, "status --non-interactive --no-ignore", directory);
            string StdOut = Utility.CheckProcessExitedProperly(P);
            string[] StdOutLines = StdOut.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);

            // Loop through all lines the SVN process produced.
            foreach (string line in StdOutLines)
            {
                if (line.Length > 8)
                {
                    if (line[0] == '?' || line[0] == 'I')
                    {
                        string relativePath = line.Substring(8);

                        string path = Path.Combine(directory, relativePath);
                        if (Directory.Exists(path))
                            Directory.Delete(path, true);
                        else if (File.Exists(path))
                        {
                            if ((File.GetAttributes(path) & FileAttributes.ReadOnly) == FileAttributes.ReadOnly)
                            {
                                // do nothing
                            }
                            else
                            {
                                File.SetAttributes(path, FileAttributes.Normal);
                                File.Delete(path);
                            }
                        }
                    }
                }
            }

        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }

        return 0;
    }
}
