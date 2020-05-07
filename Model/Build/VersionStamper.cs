//css_ref System;
//css_ref System.Core;﻿
using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Linq;
using System.Text;
//using CSGeneral;
using System.IO;
using System.Diagnostics;
using System.Reflection;

class VersionStamper
{
    const int Major = 7;
    const int Minor = 10;
    
    static int Main(string[] args)
    {
        Dictionary<string, string> Macros = ParseCommandLine(args);
        try
        {
            if (!Macros.ContainsKey("Directory"))
                throw new Exception("Usage: VersionStamper Directory=c:\\Apsim [Increment=Yes] [RevisionNumber=4191]");

            string RevisionNumber = "";
			if (Macros.ContainsKey("RevisionNumber") && Macros["RevisionNumber"] != "%REVISION_NUMBER%")
				RevisionNumber = Macros["RevisionNumber"];
			else
			{
				// Use hash of last git commit as revision number
				string svnexe = "git.exe";
				if (Path.DirectorySeparatorChar == '/') svnexe = "git";
				try
				{
					// Start an SVN process and get head revision number
					string SVNFileName = FindFileOnPath(svnexe);
					string Arguments = "show-ref --head --tags -s";
					
					Process P = RunProcess(SVNFileName, Arguments, 
						Macros["Directory"] == "" ? 
						Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location) :
						Macros["Directory"]);
					string StdOut = CheckProcessExitedProperly(P);
					string[] StdOutLines = StdOut.Split("\r\n".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
					RevisionNumber = StdOutLines[0];
				}
				catch (Exception e)
				{
					Console.WriteLine("WARNING - while finding git commit hash: " + e.Message);
				}
			}
            

            // Write the VersionInfo.make
            StreamWriter Out = new StreamWriter("VersionInfo.make");
            Out.WriteLine("MAJOR_VERSION=" + Major.ToString());
            Out.WriteLine("MINOR_VERSION=" + Minor.ToString());
            Out.WriteLine("BUILD_NUMBER=" + RevisionNumber);
            Out.Close();

            // Write the VersionInfo.bat
            Out = new StreamWriter("VersionInfo.bat");
            Out.WriteLine("@echo off");
            Out.WriteLine("set MAJOR_VERSION=" + Major.ToString());
            Out.WriteLine("set MINOR_VERSION=" + Minor.ToString());
            Out.WriteLine("set BUILD_NUMBER=" + RevisionNumber);
            Out.Close();

            // Write the VersionInfo.sh
            Out = new StreamWriter("VersionInfo.sh");
            Out.WriteLine("#!/bin/sh");
            Out.WriteLine("export MAJOR_VERSION=" + Major.ToString());
            Out.WriteLine("export MINOR_VERSION=" + Minor.ToString());
            Out.WriteLine("export BUILD_NUMBER=" + RevisionNumber);
            Out.Close();

            // Write the VersionInfo.cs
            Out = new StreamWriter("VersionInfo.cs");
            Out.WriteLine("using System.Reflection;");
            Out.WriteLine("[assembly: AssemblyFileVersion(\"" + Major.ToString() + "." +
                                                                Minor.ToString() + "." +
                                                                RevisionNumber + "." +
																"0\")]");
            Out.Close();

            // Write the VersionInfo.vb
            Out = new StreamWriter("VersionInfo.vb");
            Out.WriteLine("Imports System.Reflection");
            Out.WriteLine("<Assembly: AssemblyFileVersion(\"" + Major.ToString() + "." +
                                                                Minor.ToString() + "." +
                                                                RevisionNumber + "." +
																"0\")>");
            Out.Close();

            // Write the VersionInfo.cpp
            Out = new StreamWriter("VersionInfo.cpp");
            Out.WriteLine("std::string EXPORT getApsimVersion(void) {return \"" + 
                                                                Major.ToString() + "." +
                                                                Minor.ToString() + "\";}");
            Out.WriteLine("std::string EXPORT getApsimBuildNumber(void) {return \"" + RevisionNumber + "\";}");
            Out.Close();
        }
        catch (Exception err)
        {
            Console.WriteLine(err.Message);
            return 1;
        }
        return 0;
    }
    
    /// <summary>
    /// Store all macros found in the command line arguments. Macros are keyword = value
    /// </summary>
    static Dictionary<string, string> ParseCommandLine(string[] args)
    {
        Dictionary<string, string> Options = new Dictionary<string, string>(StringComparer.CurrentCultureIgnoreCase);
        for (int i = 0; i < args.Length; i++)
        {
            StringCollection MacroBits = SplitStringHonouringQuotes(args[i], "=");
            if (MacroBits.Count == 2)
                Options.Add(MacroBits[0].Replace("\"", ""), MacroBits[1].Replace("\"", ""));
        }
        return Options;
    }


    static string FindFileOnPath(string FileName)
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
    
    
        static Process RunProcess(string Executable, string Arguments, string JobFolder)
        {
            if (!File.Exists(Executable))
                throw new System.Exception("Cannot execute file: " + Executable + ". File not found.");
            Process PlugInProcess = new Process();
            PlugInProcess.StartInfo.FileName = Executable;
            PlugInProcess.StartInfo.Arguments = Arguments;
            // Determine whether or not the file is an executable; execute from the shell if it's not
            PlugInProcess.StartInfo.UseShellExecute = false;
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
        static string CheckProcessExitedProperly(Process PlugInProcess)
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
    
        /// ------------------------------------------------------------------
        /// <summary>
        /// This method splits values on a comma but also honours double quotes
        /// ensuring something in double quotes is never split.
        ///     eg: if text = value1, "value 2, 2a", value3
        ///     then: words[0] = value1
        ///           words[1] = value2, 2a
        ///           words[2] = value3
        /// All values returned have been trimmed of spaces and double quotes.
        /// </summary>
        // ------------------------------------------------------------------
        static StringCollection SplitStringHonouringQuotes(string Text, string Delimiters)
        {
            StringCollection ReturnStrings = new StringCollection();
            if (Text.Trim() == "")
                return ReturnStrings;

            bool InsideQuotes = false;
            int Start = IndexNotOfAny(Text, " ".ToCharArray());
            for (int i = Start; i < Text.Length; i++)
            {
                if (Text[i] == '"')
                    InsideQuotes = !InsideQuotes; // toggle

                else if (!InsideQuotes)
                {
                    if (Delimiters.IndexOf(Text[i]) != -1)
                    {
                        // Found a word - store it.
                        if (Start != i)
                            ReturnStrings.Add(Text.Substring(Start, i - Start).Trim(" ".ToCharArray()));
                        Start = i+1;

                    }
                }
            }
            if (Start != Text.Length)
                ReturnStrings.Add(Text.Substring(Start, Text.Length - Start).Trim(" ".ToCharArray()));

            // remove leading and trailing quote if necessary.
            for (int i = 0; i < ReturnStrings.Count; i++)
            {
                if (ReturnStrings[i][0] == '"' && ReturnStrings[i][ReturnStrings[i].Length - 1] == '"')
                {
                    ReturnStrings[i] = ReturnStrings[i].Substring(1, ReturnStrings[i].Length - 2).Trim();
                    if (ReturnStrings[i] == "")
                    {
                        ReturnStrings.RemoveAt(i);
                        i--;
                    }
                }
            }
            return ReturnStrings;
        }
    
        // ------------------------------------------------------------------
        // This method complements the string function IndexOfAny by
        // providing a NOT version. Returns -1 if non of the specified
        // characters are found in specified string.
        // ------------------------------------------------------------------
        static int IndexNotOfAny(string Text, char[] Delimiters)
        {
            return IndexNotOfAny(Text, Delimiters, 0);
        }

        // ------------------------------------------------------------------
        // This method complements the string function IndexOfAny by
        // providing a NOT version. Returns -1 if non of the specified
        // characters are found in specified string.
        // ------------------------------------------------------------------
        static int IndexNotOfAny(string Text, char[] Delimiters, int Pos)
        {
            string DelimitersString = new string(Delimiters);
            for (int i = Pos; i < Text.Length; i++)
            {
                if (DelimitersString.IndexOf(Text[i]) == -1)
                    return i;
            }
            return -1;
        }

    
    
}

