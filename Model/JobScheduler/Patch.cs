using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using CSGeneral;

public class Patch
{


    /// <summary>
    /// A function for reading the filenames in a patch and returning their revision number.
    /// </summary>
    public static string[] FilesInPatch(string PatchFileName, string RootDirectory)
    {
        List<string> FileNames = new List<string>();

        StreamReader Patch = new StreamReader(PatchFileName);

        // Go looking for lines like:
        //    --- Tests/RunAllPlant2Tests.txt	(revision 1790)
        while (!Patch.EndOfStream)
        {
            string Line = Patch.ReadLine();
            if (Line.Contains("--- "))
            {
                Line = Line.Remove(0, 4);
                string FileName = Line;
                string BracketedValue = StringManip.SplitOffBracketedValue(ref FileName, '(', ')');
                string[] RevisionBits = BracketedValue.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                if (RevisionBits.Length == 2)
                {
                    FileName = Path.Combine(RootDirectory, FileName);
                    FileName = FileName.Replace("/", "\\");  // get rid of yucky unix slashes.
                    FileNames.Add(FileName);
                }
            }
        }
        Patch.Close();
        return FileNames.ToArray();
    }
}
