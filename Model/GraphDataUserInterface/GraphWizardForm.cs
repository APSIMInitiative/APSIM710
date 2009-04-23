using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using Controllers;
using UIUtility;
using System.IO;
using CSGeneral;
using System.Collections.Specialized;

namespace GraphDataUserInterface
   {
   public partial class GraphWizardForm : Form
      {
      private XmlNode Graphs;
      private BaseController Controller;
      private ApsimFile.Component SelectedComponent;
      private StringCollection OutputFileNames = new StringCollection();
      private List<BaseView> Views = new List<BaseView>();
      private List<ApsimFile.Component> Components = new List<ApsimFile.Component>();
      private int CurrentView = -1;

      public GraphWizardForm()
         {
         InitializeComponent();
         }


      public void Go(BaseController Cntr)
         {
         Controller = Cntr;
         PopulateGraphList();
         }
      public void Go(BaseController Cntr, string ResponseFileName)
         {
         StreamReader In = new StreamReader(ResponseFileName);
         string Line = In.ReadLine();
         while (Line != null && Line != "")
            {
            OutputFileNames.Add(Line);
            Line = In.ReadLine();
            }

         Controller = Cntr;
         PopulateGraphList();
         }

      private void PopulateGraphList()
         {
         // Find and load the graph toolbox into a XmlDocument ready for processing.
         XmlDocument Doc = new XmlDocument();
         Toolboxes Tools = new Toolboxes();
         foreach (string FileName in Tools.Filenames)
            {
            if (Path.GetFileNameWithoutExtension(FileName).ToLower() == "graph")
               {
               Doc.Load(FileName);
               break;
               }
            }

         // Find the <graphs> element in the graph toolbox and put all graph names into
         // the GraphList so the user can choose one.
         if (Doc.DocumentElement != null)
            {
            // Go look for a child name called 'graphs'
            Graphs = XmlHelper.Find(Doc.DocumentElement, "Graphs");
            if (Graphs != null)
               {
               GraphList.Items.Clear();
               foreach (string GraphName in XmlHelper.ChildNames(Graphs, ""))
                  GraphList.Items.Add(GraphName);
               if (GraphList.Items.Count > 0)
                  GraphList.SelectedIndex = 0;
               }
            }
         }

      private void OnNextButtonClick(object sender, EventArgs e)
         {
         // ----------------------------------------------------
         // User has clicked NEXT so work out which view to 
         // display next for the user.
         // ----------------------------------------------------

         CloseView();

         CurrentView++;
         if (CurrentView == Views.Count - 1)
            NextButton.Text = "Finish";

         if (CurrentView >= Views.Count)
            {
            Close();
            Controller.SelectedPath = SelectedComponent.FullPath;
            Controller.Explorer.RefreshCurrentView();
            }
         else
            ShowView();
         }

      private void CreateViews(ApsimFile.Component Comp)
         {
         // ----------------------------------------------------
         // Create a view for the specified component and add
         // it to the list of views that we'll display later.
         // ----------------------------------------------------

         foreach (ApsimFile.Component Child in Comp.ChildNodes)
            CreateViews(Child);

         if (Comp.Type == "GDApsimFileReader")
            {
            // Auto populate the ApsimFileReader with the filenames in the response file.
            string FileReaderContents = "<GDApsimFileReader name=\"ApsimFileReader\">";
            foreach (string FileName in OutputFileNames)
               FileReaderContents += "<FileName>" + FileName + "</FileName>";

            FileReaderContents += "</GDApsimFileReader>";
            Comp.Contents = FileReaderContents;
            }
         if (Comp.Type != "Graph")
            {
            if (Comp.Type == "GDApsimFileReader" && OutputFileNames.Count > 0)
               {
               // No need to put the file reader in the wizard as it already has filenames.
               return;
               }
            BaseView View = Controller.CreateUI(Comp.Type);
            if (View != null && View.GetType().ToString() != "VBUserInterface.EmptyUI")
               {
               Views.Add(View);
               Components.Add(Comp);
               }
            }
         }

      private void ShowView()
         {
         if (CurrentView >= 0 && CurrentView < Views.Count)
            {
            Views[CurrentView].OnLoad(Controller, Components[CurrentView].FullPath, Components[CurrentView].Contents);
            Views[CurrentView].Parent = ViewPanel;
            Views[CurrentView].Dock = DockStyle.Fill;
            Views[CurrentView].Show();
            Views[CurrentView].OnRefresh();
            }
         }

      private void CloseView()
         {
         if (CurrentView == -1)
            {
            // OK We are still at the select a graph screen. Find out which
            // graph was selected and go recursively create all the BaseViews
            // ready for showing to the user.
            XmlNode SelectedNode = XmlHelper.Find(Graphs, GraphList.Items[GraphList.SelectedIndex].ToString());
            if (Controller.ApsimData.RootComponent == null)
               {
               Controller.ApsimData.New("<folder name=\"Graphs\"/>");
               SelectedComponent = Controller.ApsimData.RootComponent;
               }
            SelectedComponent = Controller.Selection.Add(SelectedNode.OuterXml);
            CreateViews(SelectedComponent);
            GraphList.Visible = false;
            GraphListLabel.Visible = false;
            }
         else
            {
            BaseView View = Views[CurrentView];
            ApsimFile.Component Comp = Controller.ApsimData.Find(View.NodePath);
            Comp.Contents = View.GetData();
            View.Visible = false;
            }
         }


      }
   }