
using System;
using System.Collections;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.IO;

using ApsimFile;


namespace UIUtility
{

    public class Toolboxes
    {



        // ------------------------------------ 
        // folders property 
        // ------------------------------------ 
        public List<string> Folders
        {
            get { return Configuration.Instance.Settings("ToolBoxFolder"); }
            set { Configuration.Instance.SetSettings("ToolBoxFolder", value); }
        }

        // ------------------------------------ 
        // file names property 
        // ------------------------------------ 
        public List<string> Filenames
        {
            get
            {
                List<string> Files = new List<string>();
                List<string> ToolBoxFolders = Configuration.Instance.Settings("ToolBoxFolder");
                foreach (string ToolBoxFolder in ToolBoxFolders)
                {
                    foreach (string FileName in Directory.GetFiles(ToolBoxFolder))
                    {
                        if (Path.GetExtension(FileName) == ".xml" | Path.GetExtension(FileName) == ".soils")
                        {
                            Files.Add(FileName);
                        }
                    }
                }
                return Files;
            }
        }


        // ------------------------------------ 
        // names property 
        // ------------------------------------ 
        public List<string> Names
        {
            get
            {
                List<string> Files = new List<string>();
                List<string> Toolboxes = Filenames;
                for (int i = 0; i <= Toolboxes.Count - 1; i++)
                {
                    if (File.Exists(Toolboxes[i]))
                    {
                        Files.Add(Path.GetFileNameWithoutExtension(Toolboxes[i]));
                    }
                }
                return Files;
            }
        }


        // --------------------------------------------------- 
        // create a new empty toolbox at the specified file 
        // --------------------------------------------------- 
        public void CreateNew(string Filename)
        {
            StreamWriter sr = File.CreateText(Filename);
            sr.WriteLine("<folder name=\"" + Path.GetFileNameWithoutExtension(Filename) + "\">");
            sr.WriteLine("</folder>");
            sr.Close();
        }


        // --------------------------------------------------- 
        // create a new empty toolbox at the specified file 
        // --------------------------------------------------- 
        public string NameToFileName(string Name)
        {
            foreach (string FName in Filenames)
            {
                if (Path.GetFileNameWithoutExtension(FName) == Name)
                {
                    return FName;
                }
            }
            throw new System.Exception("Cannot find toolbox filename for name: " + Name);
        }
    }
}