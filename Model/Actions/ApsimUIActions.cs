using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;

using System.IO;
using System.Collections.Specialized;
using System.Windows.Forms;

using System.Xml;

using ApsimFile;
using Controllers;
using CSGeneral;
using UIUtility;
using System.Runtime.InteropServices;
namespace Actions
{




	public class ApsimUIActions
	{
		public static void FileNew(BaseController Controller)
		{
			if (Controller.FileSaveAfterPrompt()) {
				System.Windows.Forms.OpenFileDialog dialog = new System.Windows.Forms.OpenFileDialog();
				string NewSimFolder = Configuration.Instance.Setting("NewSimulationFolder");
				dialog.InitialDirectory = NewSimFolder;
				dialog.Title = "New Simulation";
				dialog.Filter = Configuration.Instance.Setting("DialogFilter");
				//only show .apsim files (this changes to .soils file if APSoil not ApsimUI). 
				dialog.DefaultExt = Configuration.Instance.Setting("DefaultExtension");
				//once again changes to .soils when APSoil
				dialog.Multiselect = false;
				//don't let them select multiple files
				if (dialog.ShowDialog() == System.Windows.Forms.DialogResult.OK) {
					Controller.Explorer.CloseUI();
					//close whatever simulation is currently in the ExplorerUI
					Controller.ApsimData.NewFromFile(dialog.FileName);
					//store the xml in the .apsim file into the ApsimData variable in the Controller
				}
			}
		}

		public static void HelpDocumentation(BaseController Controller)
		{
			string HelpURL = Configuration.Instance.Setting("docfile");
			Process.Start(HelpURL);
		}
		public static void ClusterHelpDocumentation(BaseController Controller)
		{
			string HelpURL = Configuration.Instance.Setting("ClusterHelpPage");
			Process.Start(HelpURL);
		}
		public static void ApsimSearchEngine(BaseController Controller)
		{
			string url = Configuration.Instance.Setting("ApsimSearchEngine");
			Process.Start(url);
		}
		public static void ApsimInternetGroup(BaseController Controller)
		{
			string url = Configuration.Instance.Setting("ApsimInternetGroup");
			Process.Start(url);
		}



		#region "Simulation methods"
		public static void Run(BaseController Controller)
		{
			// ------------------------------------------------
			// Go looking for simulations to run. Look at the
			// currently selected nodes first and progressively
			// their parents until some simulations are found.
			// ------------------------------------------------
			if (Configuration.Instance.Setting("ReloadPlugInsBeforeRunningAPSIM") == "Yes") {
				PlugIns.LoadAll();
			}

			if ((bool)BaseActions.FileSave(Controller)) {
				Control[] RunPanels = Controller.MainForm.Controls.Find("RunToolStrip", true);
				if (RunPanels.Length == 1) {
					ApsimRunToolStrip.Instance.RunApsim((ToolStrip)RunPanels[0], Controller);
					//_
					//Controller.ApsimData, _
					//Controller.SelectedPaths)
				}
			}
		}
		public static void CreateSIM(BaseController Controller)
		{
			// ------------------------------------------------
			// Create a .sim file.
			// ------------------------------------------------
			if ((bool)BaseActions.FileSave(Controller)) {
				Control[] RunPanels = Controller.MainForm.Controls.Find("RunToolStrip", true);
				if (RunPanels.Length == 1) {
					ApsimRunToolStrip.Instance.CreateSIM((ToolStrip)RunPanels[0], Controller);
				}
			}
		}

		public static void Enable(BaseController Controller)
		{
			foreach (string NodePath in Controller.SelectedPaths) {
				Controller.ApsimData.Find(NodePath).Enabled = true;
			}
		}
		public static void Disable(BaseController Controller)
		{
			foreach (string NodePath in Controller.SelectedPaths) {
				Controller.ApsimData.Find(NodePath).Enabled = false;
			}
		}
		public static void ToggleFactorialMode(BaseController Controller)
		{
			if (((Controller.ApsimData.RootComponent != null))) {
				Controller.FactorialMode = !Controller.FactorialMode;
			}

		}

		private static ToolStripProgressBar ProgressBar;
		private static ToolStripStatusLabel ProgressLabel;
		public static void RunOnCluster(BaseController Controller)
		{
			if ((!(BaseActions.FileSave(Controller))))
				return;

			StatusStrip StatusBar = (StatusStrip)Controller.MainForm.Controls.Find("StatusStrip1", true)[0];
			ProgressBar = (ToolStripProgressBar)StatusBar.Items[0];
			ProgressLabel = (ToolStripStatusLabel)StatusBar.Items[1];
			StatusBar.Visible = true;

			try {
				UIBits.ClusterForm F = new UIBits.ClusterForm();
				if (F.ShowDialog() == DialogResult.OK) {
					Cursor.Current = Cursors.WaitCursor;
					Configuration.Instance.SetSetting("dropboxFolder", F.DropFolder);
					Configuration.Instance.SetSetting("dropboxApsimVersion", F.Version);
					if (F.archIsUnix) {
						Configuration.Instance.SetSetting("dropboxIsUnix", "true");
					} else {
						Configuration.Instance.SetSetting("dropboxIsUnix", "false");
					}
					Configuration.Instance.SetSetting("dropboxSimsPerJob", F.simsPerJobNumber.ToString());

					List<string> FilesToRun = new List<string>();
					if (string.IsNullOrEmpty(F.FolderOfFiles)) {
						if ((!string.IsNullOrEmpty(Controller.ApsimData.FileName))) {
							FilesToRun.Add(Controller.ApsimData.FileName);
						}
					} else {
						Utility.FindFiles(F.FolderOfFiles, "*.apsim", ref FilesToRun, false);
					}
					if ((FilesToRun.Count > 0)) {
						ToowoombaCluster.RunOnCluster(FilesToRun, F.DropFolder, F.Version, F.archIsUnix, F.simsPerJobNumber, F.NiceUser, UpdateProgress);
						MessageBox.Show("Your job has been placed in your dropbox folder. Your outputs will appear adjacent.", "For your information", MessageBoxButtons.OK, MessageBoxIcon.Information);
					}
				}
			} catch (Exception ex) {
				MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
			}
			Cursor.Current = Cursors.Default;
			StatusBar.Visible = false;
		}

		private static void UpdateProgress(int Percent, string Msg)
		{
			ProgressBar.Value = Math.Min(100, Math.Max(0, Percent));
			ProgressLabel.Text = Msg;
			Application.DoEvents();
		}
		[DllImport("ShellExtensions.dll", EntryPoint = "excelFiles", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true)]

		#endregion

		#region "Output file methods"
		private static extern void excelFiles(string outFileList);
		[DllImport("ShellExtensions.dll", EntryPoint = "apsvisFiles", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true)]
		private static extern void apsvisFiles(string outFileList);
		[DllImport("ShellExtensions.dll", EntryPoint = "apsimoutlookFiles", CharSet = CharSet.Ansi, SetLastError = true, ExactSpelling = true)]
		private static extern void apsimoutlookFiles(string outFileList);

		public static void Graph(BaseController Controller)
		{
			string FileNames = UIUtility.OutputFileUtility.GetCSVListOfOutputFiles(Controller);
			if (string.IsNullOrEmpty(FileNames)) {
				MessageBox.Show("No output files found");
			} else {
				apsvisFiles(FileNames);
			}
		}

		public static void ApsimOutlook(BaseController Controller)
		{
			string FileNames = UIUtility.OutputFileUtility.GetCSVListOfOutputFiles(Controller);
			if (string.IsNullOrEmpty(FileNames)) {
				MessageBox.Show("No output files found");
			} else {
				apsimoutlookFiles(FileNames);
			}
		}

		public static void Excel(BaseController Controller)
		{
			string FileNames = UIUtility.OutputFileUtility.GetCSVListOfOutputFiles(Controller);
			ExcelExport exportChooser = new ExcelExport();
			if (string.IsNullOrEmpty(FileNames)) {
				return;
			}

			string[] fileList = FileNames.Split(new char[] {','});
			//break up the names into a list
			// load the list into the dialog
			foreach (string s in fileList) {
				exportChooser.fileListBox.Items.Add(s, true);
			}
            if (exportChooser.ShowDialog() == DialogResult.OK)
            {
                FileNames = exportChooser.fileList;
                excelFiles(FileNames);
            }
		}
		#endregion

		#region "Checkpoint"
		public static void CheckPoint(BaseController Controller)
		{
			if (MessageBox.Show("Are you sure you want to save and checkpoint your simulation and outputfiles, overwriting any previous checkpoints?", "Are you sure?", MessageBoxButtons.YesNo, MessageBoxIcon.Exclamation) == DialogResult.Yes) {
				// Save first
				if ((!(BaseActions.FileSave(Controller))))
					return;

				// empty the checkpoint sub folder.
				string CheckPointFolder = Path.Combine(Path.GetDirectoryName(Controller.ApsimData.FileName), "CheckPoint");

				if (Directory.Exists(CheckPointFolder)) {
					Directory.Delete(CheckPointFolder, true);
				}
				Directory.CreateDirectory(CheckPointFolder);

				// Get a complete list of files names (.out, .sum & .apsim) to copy to checkpoint folder.
				List<string> FileNames = new List<string>();
				FileNames.Add(Controller.ApsimData.FileName);
				UIUtility.OutputFileUtility.GetOutputFiles(Controller, Controller.ApsimData.RootComponent, FileNames);
				UIUtility.OutputFileUtility.GetSummaryFiles(Controller, Controller.ApsimData.RootComponent, FileNames);

				// Copy all files to checkpoint folder. If any files don't exist then 
				// create zero byte files.
				foreach (string FileName in FileNames) {
					string DestFileName = CheckPointFolder + Path.DirectorySeparatorChar + Path.GetFileName(FileName);
					if (File.Exists(FileName)) {
						File.Copy(FileName, DestFileName, true);
					} else {
						StreamWriter Out = new StreamWriter(DestFileName);
						Out.Close();
					}
				}
				MessageBox.Show("All simulation, output and summary files have been checkpointed", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
			}
		}

		public static void RevertFromCheckPoint(BaseController Controller)
		{
			string CheckPointFolder = Path.Combine(Path.GetDirectoryName(Controller.ApsimData.FileName), "CheckPoint");
			if (!Directory.Exists(CheckPointFolder)) {
				MessageBox.Show("No checkpoint found.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
			}

			if (MessageBox.Show("Are you sure you want to overwrite your current simulation, output and summary files with an earlier checkpoint?", "Are you sure?", MessageBoxButtons.YesNo, MessageBoxIcon.Exclamation) == DialogResult.Yes) {
				// Restore all files.
				foreach (string FileName in Directory.GetFiles(CheckPointFolder)) {
					string DestFileName = Path.Combine(Path.GetDirectoryName(Controller.ApsimData.FileName), Path.GetFileName(FileName));
					File.Copy(FileName, DestFileName, true);
				}
				Controller.SelectedPath = Controller.ApsimData.RootComponent.FullPath;
				Controller.ApsimData.ReloadFromFile();
				Directory.Delete(CheckPointFolder, true);
			}
		}

		public static void RemoveCheckpoint(BaseController Controller)
		{

			if (MessageBox.Show("Are you sure you want to remove the current checkpoint? This will delete all files in the checkpoint folder.", "Are you sure?", MessageBoxButtons.YesNo, MessageBoxIcon.Exclamation) == DialogResult.Yes) {
				// empty the checkpoint sub folder.
				string CheckPointFolder = Path.Combine(Path.GetDirectoryName(Controller.ApsimData.FileName), "CheckPoint");
				if (Directory.Exists(CheckPointFolder)) {
					Directory.Delete(CheckPointFolder, true);
				}
			}
		}

		#endregion

		public static void ImportConFile(BaseController Controller)
		{
			if ((!(BaseActions.FileSave(Controller))))
				return;
			OpenFileDialog F = new OpenFileDialog();
			F.Filter = "Con files (*.con)|*.con|All files (*.*)|*.*";
			F.Title = "Select a .con file to import";
			if (F.ShowDialog() == DialogResult.OK) {
				XmlNode NewXmlNode = ConToApsim.Converter.Go(F.FileName);
				if ((NewXmlNode != null)) {
					Controller.ApsimData = new ApsimFile.ApsimFile(NewXmlNode.OuterXml);
				}
			}
		}

		public static void Plant2Documentation(BaseController Controller)
		{
			if ((!(BaseActions.FileSave(Controller))))
				return;
			string XmlFileName = Controller.ApsimData.FileName;
			//Dim HtmlFileName As String = Path.GetTempPath() + Path.GetFileNameWithoutExtension(XmlFileName) + ".html" //removed from temp dir as Firefox can't handle abs dirs. JF
			string HtmlFileName = Path.Combine("..", "Documentation", "Plant2Docs", Path.GetFileNameWithoutExtension(XmlFileName) + ".html");
			string Arguments = StringManip.DQuote(XmlFileName) + " " + StringManip.DQuote(HtmlFileName);

			//Dim P As Process = Process.Start(Configuration.ApsimBinDirectory + "\Plant2Documentation", Arguments)
			Process P = Utility.RunProcess(Path.Combine(Configuration.ApsimBinDirectory(), "Plant2Documentation.exe"), Arguments, Path.GetDirectoryName(XmlFileName));
			Utility.CheckProcessExitedProperly(P);
			Process.Start(HtmlFileName);
		}

		public static void ProbeDLL(BaseController Controller)
		{
			if ((!(BaseActions.FileSave(Controller))))
				return;
			string XmlFileName = Controller.ApsimData.FileName;

			//Dim P As Process = Process.Start(Configuration.ApsimBinDirectory + "\Plant2Documentation", Arguments)
			Process P = Utility.RunProcess(Path.Combine(Configuration.ApsimBinDirectory(), "ProbeDLL.exe"), XmlFileName, Path.GetDirectoryName(XmlFileName));
			Utility.CheckProcessExitedProperly(P);

			MessageBox.Show("Finished writing to <info> section.", "Done", MessageBoxButtons.OK, MessageBoxIcon.Information);
		}


		public static void CreateDuplicates(BaseController Controller)
		{
			UIBits.DuplicateForm f = new UIBits.DuplicateForm();
			if (f.ShowDialog() == DialogResult.OK) {
				ApsimFile.Component Comp = Controller.Selection;

				for (int i = 1; i <= f.NumDuplicates; i++) {
					Comp.Parent.Duplicate(Comp, f.DoLinkDuplicates);
				}

			}
		}


		public static void LinkWherePossible(BaseController Controller)
		{
			Cursor.Current = Cursors.WaitCursor;
			ApsimFile.Component Base = Controller.Selection;
			List<ApsimFile.Component> Siblings = new List<ApsimFile.Component>();
			Base.Parent.FindRecursively(Base.Type, ref Siblings);

			foreach (ApsimFile.Component Sibling in Siblings) {
				if (Sibling.FullPath != Base.FullPath) {
					Sibling.ConvertToShortcutsUsingBase(Base);
				}
			}
			Controller.ApsimData.PublishComponentChanged(Controller.ApsimData.RootComponent);

			Cursor.Current = Cursors.Default;
		}
	}
}





