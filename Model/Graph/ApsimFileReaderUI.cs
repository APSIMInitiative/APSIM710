using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using CSGeneral;
using ApsimFile;
using System.Collections.Specialized;

namespace Graph
   {
   public partial class ApsimFileReaderUI : Graph.DataUserInterface
      {
      public ApsimFileReaderUI()
         {
         InitializeComponent();
         }

      protected override void OnLoad()
         {
         //ParentUI = (ChartPageUI)Parent;
         }
      public override void OnRefresh()
         {
         // -----------------------------------------------
         // Called when it's time to refresh the canvas and
         // everything on it.
         // -----------------------------------------------
         base.OnRefresh();

         FileList.TextChanged -= OnTextChanged;
         List<string> FileNames = XmlHelper.Values(Data, "FileName");

         string[] AllLines = new string[FileNames.Count];
         FileNames.CopyTo(AllLines);
         FileList.Lines = AllLines;
         FileList.TextChanged += OnTextChanged;
         }

      private void OnBrowseButtonClick(object sender, EventArgs e)
         {
         if (OpenFileDialog.ShowDialog() == DialogResult.OK)
            {
            string[] Files = OpenFileDialog.FileNames;
            string FileString = "";
            for (int i = 0; i != Files.Length; i++)
               {
               if (FileString != "")
                  FileString += "\r\n";
               FileString += Controller.ToRelativePath(Files[i]);
               }
            FileList.Text = FileString;
            }
         }

      private void OnTextChanged(object sender, EventArgs e)
         {
         try
            {
            List<string> FileNames = new List<string>();
            FileNames.AddRange(FileList.Lines);
            XmlHelper.SetValues(Data, "FileName", FileNames);
            ApsimFile.Component Comp = Controller.ApsimData.Find(NodePath);
            if (Comp != null)
               Comp.Contents = GetData();

            OnRefresh();
            }
         catch (Exception )
            {
            }
         }


      }
   }

