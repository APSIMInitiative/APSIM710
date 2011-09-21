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
            string SVNFileName = FindFileOnPath("svn.exe");
            if (SVNFileName == "")
                throw new Exception("Cannot find svn.exe on PATH");

            // Start an SVN process to return a list of unversioned files.
            Process P = RunProcess(SVNFileName, "status --non-interactive --no-ignore", directory);
            string StdOut = CheckProcessExitedProperly(P);
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
	
	
        public static string FindFileOnPath(string FileName)
        {
            string PathVariable = Environment.GetEnvironmentVariable("PATH");
            if (PathVariable == null)
                throw new Exception("Cannot find PATH environment variable");
			string[] Paths;
			string PathSeparator;
			
			if (Path.VolumeSeparatorChar == '/') 
				PathSeparator = ":";
			else
				PathSeparator = ";";

			Paths = PathVariable.Split(PathSeparator.ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
			
            foreach (string DirectoryName in Paths)
            {
                string FullPath = Path.Combine(DirectoryName, FileName);
                if (File.Exists(FullPath))
                    return FullPath;
            }
            return "";
        }
		
        private static Process RunProcess(string Executable, string Arguments, string JobFolder)
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
        private static string CheckProcessExitedProperly(Process PlugInProcess)
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
}
