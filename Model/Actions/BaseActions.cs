using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;

using System.Collections.Specialized;
using System.IO;
using System.Windows.Forms;
using System.Xml;
using System.Drawing;

using ApsimFile;
using Controllers;
using CSGeneral;
using UIBits;
//InputDialog
using System.Drawing.Printing;
namespace Actions
{



	public class BaseActions
	{
		public static void FileOpen(BaseController Controller)
		{
			string LastDir = null;
			if (Controller.FileSaveAfterPrompt()) {
				if ((Configuration.Instance.GetFrequentList().Count > 0)) {
					LastDir = Configuration.Instance.GetFrequentList()[0];
				} else {
					LastDir = Environment.GetFolderPath(Environment.SpecialFolder.Personal);
				}

				OpenFileDialog dialog = new OpenFileDialog();
				if (!string.IsNullOrEmpty(LastDir)) {
					LastDir = LastDir.Substring(0, LastDir.LastIndexOf(Path.DirectorySeparatorChar));
					dialog.InitialDirectory = LastDir;
				}

				dialog.Filter = Configuration.Instance.Setting("DialogFilter");
				dialog.DefaultExt = Configuration.Instance.Setting("DefaultExtension");
				dialog.RestoreDirectory = true;
				if (dialog.ShowDialog() == DialogResult.OK) {
					Controller.Explorer.CloseUI();
					Controller.ApsimData.OpenFile(dialog.FileName);
					Controller.RefreshToolStrips();
				}
			}
		}
		public static bool FileSave(BaseController Controller)
		{
			if (Controller.ApsimData.FileName == "Untitled") {
				return BaseActions.FileSaveAs(Controller);
			} else {
				return Controller.ApsimData.Save();
			}
		}
		public static bool FileSaveAs(BaseController Controller)
		{
			bool result = false;
			SaveFileDialog Dialog = new SaveFileDialog();
			string LastDir = null;
			if ((Configuration.Instance.GetFrequentList().Count > 0)) {
				LastDir = Configuration.Instance.GetFrequentList()[0];
				LastDir = LastDir.Substring(0, LastDir.LastIndexOf(Path.DirectorySeparatorChar));
			} else {
				LastDir = Environment.GetFolderPath(Environment.SpecialFolder.Personal);
			}
			Dialog.InitialDirectory = LastDir;
			Dialog.Filter = Configuration.Instance.Setting("DialogFilter");
			Dialog.DefaultExt = Configuration.Instance.Setting("DefaultExtension");
			Dialog.AddExtension = true;
			Dialog.OverwritePrompt = true;
			if (Dialog.ShowDialog() == DialogResult.OK) {
				Controller.Explorer.SaveCurrentView();
				result = Controller.ApsimData.SaveAs(Dialog.FileName);
				Controller.RefreshToolStrips();
			}
			return result;
		}
		public static void HelpAbout(BaseController Controller)
		{
			if (!string.IsNullOrEmpty(Configuration.Instance.Setting("SplashScreen"))) {
				Form SplashForm = (Form)BaseController.CreateClass(Configuration.Instance.Setting("SplashScreen"));
				SplashForm.ShowDialog();
			}
		}
		public static void AddFolder(BaseController Controller)
		{
			// --------------------------------------------------------
			// Add a folder
			// --------------------------------------------------------
			Controller.Selection.Add("<folder/>");
		}
		public static void Delete(BaseController Controller)
		{
			// --------------------------------------------------------
			// Delete selected nodes
			// --------------------------------------------------------
			System.Collections.Specialized.StringCollection PathsToDelete = Controller.SelectedPaths;

			string ParentSelection = Controller.SelectedPaths[0];
			foreach (string SelectedPath in PathsToDelete) {
				if (ParentSelection.IndexOf(SelectedPath) == 0) {
					ParentSelection = SelectedPath;
				}
			}
			ApsimFile.Component Selection = Controller.ApsimData.Find(ParentSelection);
			Controller.SelectedPath = Selection.Parent.FullPath;

			foreach (string SelectedPath in PathsToDelete) {
				ApsimFile.Component CompToDelete = Controller.ApsimData.Find(SelectedPath);
				CompToDelete.Parent.Delete(CompToDelete);
			}
			Controller.Explorer.RefreshCurrentView();
		}
		public static void Rename(BaseController Controller)
		{
			// --------------------------------------------------------
			// Rename selected nodes
			// --------------------------------------------------------

			foreach (string SelectedPath in Controller.SelectedPaths) {
				//get the new name the user entered
				string NewName = UIBits.InputDialog.InputBox("Enter new name for node:", "Rename the selected node", Controller.Selection.Name, false);

				//set rename the selected node

				if (!String.IsNullOrEmpty(NewName)) {
					if (!CSGeneral.Utility.CheckForInvalidChars(NewName)) {
						ApsimFile.Component Comp = Controller.ApsimData.Find(SelectedPath);
						Comp.Name = NewName;
						// Now tell the base controller about the new selections.
						Controller.SelectedPath = Comp.FullPath;
					} else {
						MessageBox.Show("You can not use characters such as < > / \\ ' \" ` : ? | * & = ! . , or space in the name");
					}

				}

			}
		}
		public static void DeleteFactor(BaseController Controller)
		{
			// --------------------------------------------------------
			// Delete selected nodes
			// --------------------------------------------------------
			ApsimFile.Component CompToDelete = Controller.FactorialSelection;
			//if this is the fatorial node and there is a project loaded, then you cannot delete it
			if ((CompToDelete.Parent == null) || (CompToDelete.Parent.Parent == null)) {
				return;
			}

			// find next sibling, or previous sibling, or parent the set SelectedFactorialPath
			ApsimFile.Component CompToSelect = CompToDelete.Parent;

			Controller.SelectedFactorialPath = CompToSelect.FullPath;
			CompToDelete.Parent.Delete(CompToDelete);

			Controller.Explorer.RefreshCurrentView();
		}
		public static void AddFactorFolder(BaseController Controller)
		{
			// --------------------------------------------------------
			// Add a folder
			// --------------------------------------------------------
			Controller.FactorialSelection.Add("<folder/>");
		}
		public static void AddFactor(BaseController Controller)
		{
			// --------------------------------------------------------
			// Add a folder
			// --------------------------------------------------------
			Controller.FactorialSelection.Add("<factor/>");

		}

		#region "Printing"
		private static List<string> ComponentsToPrint = new List<string>();
		private static int ComponentsToPrintIndex;
		private static BaseController Contr;

		public static void Print(BaseController Controller)
		{
			PrintDocument pd = new PrintDocument();
			pd.PrintPage += OnPrintPage;

			UIBits.PrintPreviewDialog PreviewDialog = new UIBits.PrintPreviewDialog();
			PreviewDialog.Document = pd;

			ComponentsToPrint.Clear();
			GetListOfComponentsToPrint(Controller, Controller.Selection);
			ComponentsToPrintIndex = -1;
			Contr = Controller;
			PreviewDialog.ShowDialog();
		}

		private static void OnPrintPage(System.Object sender, System.Drawing.Printing.PrintPageEventArgs e)
		{
			ComponentsToPrintIndex = ComponentsToPrintIndex + 1;
			if (ComponentsToPrintIndex < ComponentsToPrint.Count) {
				Contr.SelectedPath = ComponentsToPrint[ComponentsToPrintIndex];
				Contr.Explorer.CurrentView.PrintPage(e.MarginBounds, e.Graphics);
			} else {
				e.Cancel = true;
			}
			e.HasMorePages = (ComponentsToPrintIndex + 1 < ComponentsToPrint.Count);
			if (!e.HasMorePages) {
				ComponentsToPrintIndex = -1;
			}
		}


		private static void GetListOfComponentsToPrint(BaseController Controller, ApsimFile.Component Component)
		{
			if ((Component.Type == "Graph") || (Component.Type == "RegressionGraph") || (Component.Type == "GraphReport") || (Component.Type == "memo")) {
				ComponentsToPrint.Add(Component.FullPath);
			} else {
				foreach (ApsimFile.Component Child in Component.ChildNodes) {
					GetListOfComponentsToPrint(Controller, Child);
				}
			}
		}
		#endregion

		#region "Export"

		public static void ExportToBMP(BaseController Controller)
		{
			Export(Controller, ".bmp");
		}
		public static void ExportToGIF(BaseController Controller)
		{
			Export(Controller, ".gif");
		}
		public static void ExportToJPG(BaseController Controller)
		{
			Export(Controller, ".jpg");
		}
		public static void ExportToPNG(BaseController Controller)
		{
			Export(Controller, ".png");
		}
		public static void Export(BaseController Controller, string Extension)
		{
			FolderBrowserDialog FolderDialog = new FolderBrowserDialog();
			if (FolderDialog.ShowDialog() == DialogResult.OK) {
				ComponentsToPrint.Clear();
				Contr = Controller;

				ExportAll(Contr, Contr.Selection, FolderDialog.SelectedPath, Extension);

			}
			MessageBox.Show("All graphs have been exported", "Export completed", MessageBoxButtons.OK, MessageBoxIcon.Information);
		}


		public static void ExportAll(BaseController Controller, Component Selection, string ExportDirectory, string ExportExtension)
		{
			Contr = Controller;

            Application.DoEvents();

			// go export all graphs that we can find.
			ExportAllRecursively(Selection, ExportDirectory, ExportExtension);

		}

		private static void ExportAllRecursively(Component Component, string ExportDirectory, string ExportExtension)
		{
			if ((Component.Type == "Graph") || (Component.Type == "Graph2") || (Component.Type == "RegressionGraph") || (Component.Type == "GraphReport") || (Component.Type == "memo")) {
				ExportComponent(Component.FullPath, ExportDirectory, ExportExtension);

			} else if ((Component.Type == "area") || (Component.Type == "simulation") || (Component.Type == "folder") || (Component.Type == "outputfile")) {
				// Need to recurse.
				if ((Component.Type == "folder") || (Component.Type == "simulation")) {
					ExportDirectory = Path.Combine(ExportDirectory, Component.Name);
				}
				foreach (ApsimFile.Component Child in Component.ChildNodes) {
					ExportAllRecursively(Child, ExportDirectory, ExportExtension);
				}
			}

		}

		private static void ExportComponent(string SelectedPath, string ExportDirectory, string ExportExtension)
		{
			Directory.CreateDirectory(ExportDirectory);

			Contr.SelectedPath = SelectedPath;
            Application.DoEvents();
			Rectangle r = new Rectangle(0, 0, 800, 800);
			Bitmap img = new Bitmap(r.Width, r.Height);

			Graphics g = Graphics.FromImage(img);
			g.FillRectangle(Brushes.White, r);

			Contr.Explorer.CurrentView.PrintPage(r, g);

			g.Dispose();
			string NewFileName = Path.Combine(ExportDirectory, Contr.ApsimData.Find(SelectedPath).Name + ExportExtension);
			if ((Path.GetExtension(NewFileName) == ".bmp")) {
				img.Save(NewFileName, System.Drawing.Imaging.ImageFormat.Bmp);
			} else if ((Path.GetExtension(NewFileName) == ".gif")) {
				img.Save(NewFileName, System.Drawing.Imaging.ImageFormat.Gif);
			} else if ((Path.GetExtension(NewFileName) == ".jpg")) {
				img.Save(NewFileName, System.Drawing.Imaging.ImageFormat.Jpeg);
			} else if ((Path.GetExtension(NewFileName) == ".png")) {
				img.Save(NewFileName, System.Drawing.Imaging.ImageFormat.Png);
			} else {
                img.Dispose();
                throw new Exception("Invalid format for exporting: " + NewFileName);
			}
		}

		#endregion


		public static void Cut(BaseController Controller)
		{
			// --------------------------------------------------------
			// Perform a clipboard cut operation
			// --------------------------------------------------------
			Copy(Controller);
			Delete(Controller);
		}
		public static void Copy(BaseController Controller)
		{
			// --------------------------------------------------------
			// Perform a clipboard copy operation
			// --------------------------------------------------------

			XmlDocument Doc = new XmlDocument();
			XmlNode Root = Doc.AppendChild(Doc.CreateElement("dummy"));
			foreach (string ComponentPath in Controller.SelectedPaths) {
				Component Component = Controller.ApsimData.Find(ComponentPath);
				if ((Component != null)) {
					XmlNode Node = Root.AppendChild(Doc.CreateElement(Component.Type));
					Component.Write(Node);
				}

			}
			Clipboard.SetDataObject(Root.InnerXml, true);
		}
		public static void Paste(BaseController Controller)
		{
			// --------------------------------------------------------
			// Perform a clipboard paste operation
			// --------------------------------------------------------
			IDataObject iData = Clipboard.GetDataObject();
			string xml = Convert.ToString(iData.GetData(DataFormats.Text));
                string[] sims = xml.Split(new string[] { "simulation name=\"" }, StringSplitOptions.RemoveEmptyEntries);
                for (int i = 1; i < sims.Length; i++)
                {
                    sims[i] = sims[i].Substring(0, sims[i].IndexOf('\"'));

                    bool unique = true;
                    byte count = 1;
                    do
                    {
                        foreach (Component sim in Controller.ApsimData.RootComponent.ChildNodes)
                        {
                            unique = IsNameUnique(sim, sims[i]);
                            if (!unique)
                                break;
                        }
                        if (!unique)
                        {
                            if (count > 9)
                                xml = xml.Replace("\"" + sims[i], "\"" + sims[i].Substring(0, sims[i].Length - 2) + count); //can be done faster with StringBuilder, not worth the overhead unless replacing thousands of names.
                            else if (count > 1)
                                xml = xml.Replace("\"" + sims[i], "\"" + sims[i].Substring(0, sims[i].Length - 1) + count); 
                            else
                                xml = xml.Replace("\"" + sims[i], "\"" + sims[i] + count);
                            sims = xml.Split(new string[] { "simulation name=\"" }, StringSplitOptions.RemoveEmptyEntries);
                            sims[i] = sims[i].Substring(0, sims[i].IndexOf('\"'));
                        }
                        count++;
                    } while (!unique);
                }

			Controller.Selection.Add(xml);
		}

        private static bool IsNameUnique(Component parent, string name)
        {
            bool unique = true;

            if (parent.Name.Equals(name))
               unique = false;
            else
                foreach (Component c in parent.ChildNodes)
                {
                    if (!IsNameUnique(c, name))
                        unique=false;
                }

            return unique;    
        }

		public static void MoveUp(BaseController Controller)
		{
			// --------------------------------------------------------        
			// Move all selected items up
			// --------------------------------------------------------
			List<string> PathsToMove = new List<string>();
			foreach (string SelectedPath in Controller.SelectedPaths) {
				PathsToMove.Add(SelectedPath.Substring(SelectedPath.LastIndexOf(XmlHelper.Delimiter) + 1));
			}
			ApsimFile.Component Parent = Controller.ApsimData.Find(Controller.SelectedPaths[0]).Parent;
			Parent.MoveUp(PathsToMove);
		}
		public static void MoveDown(BaseController Controller)
		{
			// --------------------------------------------------------        
			// Move all selected items down
			// --------------------------------------------------------
			List<string> PathsToMove = new List<string>();
			foreach (string SelectedPath in Controller.SelectedPaths) {
				PathsToMove.Add(SelectedPath.Substring(SelectedPath.LastIndexOf(XmlHelper.Delimiter) + 1));
			}
			ApsimFile.Component Parent = Controller.ApsimData.Find(Controller.SelectedPaths[0]).Parent;
			Parent.MoveDown(PathsToMove);
		}

		public static void ExpandAll(BaseController Controller)
		{
			// --------------------------------------------------------
			// Expand all nodes in tree.
			// --------------------------------------------------------
			Controller.Explorer.ExpandAll();
		}
		public static void CollapseAll(BaseController Controller)
		{
			// --------------------------------------------------------
			// Collapse all nodes in tree.
			// --------------------------------------------------------
			Controller.Explorer.CollapseAll();
		}

        public static void ExpandSelNode(BaseController Controller)
        {
            // --------------------------------------------------------
            // Expands the currently selected node in tree.
            // --------------------------------------------------------
            Controller.Explorer.ExpandSelNode();
        }
        public static void CollapseSelNode(BaseController Controller)
        {
            // --------------------------------------------------------
            // Collapse the currently selected node in tree.
            // --------------------------------------------------------
            Controller.Explorer.CollapseSelNode();
        }


		public static void Unlink(BaseController Controller)
		{
			Controller.Selection.MakeConcrete();
		}

		public static void UnlinkRecursively(BaseController Controller)
		{
			Controller.Selection.MakeConcreteRecursively();
		}

	}
}



