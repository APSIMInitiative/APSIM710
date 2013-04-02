using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Diagnostics;

using System.Drawing;
using System.IO;
using System.Net;
using System.Reflection;
using System.Text;
using System.Windows.Forms;
using System.Xml;

using ApsimFile;
using Controllers;
using CSGeneral;
using CSUserInterface;
//SoilUI
using UIBits;
//ErrorMessageForm
using UIUtility;
using ExcelUtility;
namespace Actions
{


	public class SoilActions
	{

		public static void FileNew(BaseController Controller)
		{
			if (Controller.FileSaveAfterPrompt()) {
				SaveFileDialog Dialog = new SaveFileDialog();
				Dialog.Filter = "Soils files (*.soils)|*.soils|" + "All files (*.*)|*.*";
				Dialog.DefaultExt = "soils";
				Dialog.Title = "Provide a filename to save the new soils file to";
				if (Dialog.ShowDialog() == DialogResult.OK) {
					StreamWriter Out = new StreamWriter(Dialog.FileName);
					Out.WriteLine("<folder name=\"Soils\" version=\"" + ApsimFile.APSIMChangeTool.CurrentVersion.ToString() + "\"/>");
					Out.Close();
					Controller.ApsimData.OpenFile(Dialog.FileName);
				}
			}
		}
		public static void AddSoil(BaseController Controller)
		{
            throw new NotImplementedException();
            //XmlNode NewSoil = Soil.Create("Soil");
            //Controller.Selection.Add(NewSoil.OuterXml);
		}




		#region "Import methods"
		public static void ImportFromSoils(BaseController Controller)
		{
			OpenFileDialog Dialog = new OpenFileDialog();
			Dialog.Filter = "Soils files (*.soils)|*.soils|All files (*.*)|*.*";
			Dialog.Title = "Select 1 or more .soils file(s) to import from";
			Dialog.Multiselect = true;
			Dialog.RestoreDirectory = true;
			if (Dialog.ShowDialog() == DialogResult.OK) {
				foreach (string FileName in Dialog.FileNames) {
					XmlDocument Doc = new XmlDocument();
					Doc.Load(FileName);
					APSIMChangeTool.Upgrade(Doc.DocumentElement);
					Controller.Selection.Add(Doc.DocumentElement.InnerXml);
				}
			}
		}
		public static void ImportFromPar(BaseController Controller)
		{
			OpenFileDialog Dialog = new OpenFileDialog();
			Dialog.Filter = "Par files (*.par)|*.par|All files (*.*)|*.*";
			Dialog.Title = "Select 1 or more .par file(s) to import from";
			Dialog.Multiselect = true;
			Dialog.RestoreDirectory = true;
			if (Dialog.ShowDialog() == DialogResult.OK) {
				foreach (string FileName in Dialog.FileNames) {
					Cursor.Current = Cursors.WaitCursor;
					Controller.Selection.Add(SoilParFile.Import(FileName));
					Cursor.Current = Cursors.Default;
				}
			}
		}


		private static ToolStripProgressBar ProgressBar;
		public static void FileOpenXLS(BaseController Controller)
		{
			StatusStrip StatusBar = (StatusStrip)Controller.MainForm.Controls.Find("StatusStrip1", true)[0];
			ProgressBar = (ToolStripProgressBar)StatusBar.Items[0];
			StatusBar.Visible = true;

			OpenFileDialog Dialog = new OpenFileDialog();
			Dialog.Filter = "Spreadsheet files (*.xls;*.xlsx)|*.xls;*.xlsx|All files (*.*)|*.*";
			Dialog.Title = "Select a spreadsheet to open";
			Dialog.RestoreDirectory = true;
			if (Dialog.ShowDialog() == DialogResult.OK) {
				Cursor.Current = Cursors.WaitCursor;
				DataTable Table = ExcelUtility.ExcelHelper.GetDataFromSheet(Dialog.FileName, "SoilData");
                XmlNode Data = SoilDataTable.TableToSoilXML(Table, UpdateProgress);
				Controller.ApsimData.New(Data.OuterXml);
				Cursor.Current = Cursors.Default;
			}
			StatusBar.Visible = false;
		}

		private static void UpdateProgress(int Percent)
		{
			ProgressBar.Value = Percent;
			Application.DoEvents();
		}

		#endregion



		#region "Export methods"

		public static void ExportToSoils(BaseController Controller)
		{
			SaveFileDialog Dialog = new SaveFileDialog();
			Dialog.Filter = "Soils files (*.soils)|*.soils|All files (*.*)|*.*";
			Dialog.Title = "Enter a .soils file to export to";
			Dialog.DefaultExt = "soils";
			if (Dialog.ShowDialog() == DialogResult.OK) {
				XmlDocument Doc = new XmlDocument();
				if (!File.Exists(Dialog.FileName)) {
					Doc.AppendChild(XmlHelper.CreateNode(Doc, "soils", ""));
				} else {
					Doc.Load(Dialog.FileName);
				}

				foreach (string SelectedPath in Controller.SelectedPaths) {
					ApsimFile.Component Comp = Controller.ApsimData.Find(SelectedPath);
					XmlDocument NodeDoc = new XmlDocument();
					NodeDoc.LoadXml(Comp.FullXML());
					Doc.DocumentElement.AppendChild(Doc.ImportNode(NodeDoc.DocumentElement, true));
				}
				XmlHelper.SetAttribute(Doc.DocumentElement, "version", ApsimFile.APSIMChangeTool.CurrentVersion.ToString());
				Doc.Save(Dialog.FileName);
				MessageBox.Show("Soils have been successfully exported to '" + Dialog.FileName + "'. It is suggested that you rename soils within the new file to avoid confusion.", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
			}
		}
		public static void ExportToSpreadsheet(BaseController Controller)
		{
			SaveFileDialog Dialog = new SaveFileDialog();
			Dialog.Filter = "Spreadsheet files (*.xlsx)|*.xlsx|All files (*.*)|*.*";
			Dialog.Title = "Enter a spreadsheet file to export to";
			Dialog.DefaultExt = "xlsx";
            Dialog.OverwritePrompt = true;
			if (Dialog.ShowDialog() == DialogResult.OK) {
                Cursor.Current = Cursors.WaitCursor;
				XmlDocument Doc = new XmlDocument();
				Doc.LoadXml(Controller.ApsimData.RootComponent.FullXML());
				List<string> Paths = new List<string>();
				foreach (string Path in Controller.SelectedPaths) {
					Paths.Add(Path);
				}
                DataTable Table = SoilDataTable.SoilXMLToTable(Doc.DocumentElement, Paths);
				ExcelHelper.SendDataToSheet(Dialog.FileName, "SoilData", Table);
                Cursor.Current = Cursors.Default;
				MessageBox.Show("Soils have been successfully exported to '" + Dialog.FileName + "'. It is suggested that you rename soils within the new file to avoid confusion.", "Success", MessageBoxButtons.OK, MessageBoxIcon.Information);
			}
		}

		#endregion



		public static void CheckSoils(BaseController Controller)
		{
			// User wants to check all soils for consistency 
			Cursor.Current = Cursors.WaitCursor;
			string ErrorMessage = "";
			foreach (string SelectedPath in Controller.SelectedPaths) {
				CheckSoils(Controller.ApsimData.Find(SelectedPath), ref ErrorMessage);
			}
			if (string.IsNullOrEmpty(ErrorMessage)) {
				MessageBox.Show("All soils checked out ok. No problems were encountered", "No problems encountered", MessageBoxButtons.OK, MessageBoxIcon.Information);
			} else {
				UIBits.ErrorMessageForm ErrorForm = new UIBits.ErrorMessageForm();
				ErrorForm.SetText(ErrorMessage);
				ErrorForm.Show();
			}
			Cursor.Current = Cursors.Default;
		}
		private static void CheckSoils(ApsimFile.Component Data, ref string ErrorMessage)
		{
			if (Data.Type.ToLower() == "soil") {
                throw new NotImplementedException();
                //XmlNode ThisSoil = Soil.CreateFromXML(Data.FullXML());
				//string Errors = Soil.CheckForErrors(ThisSoil, true);
                //if (!string.IsNullOrEmpty(Errors)) {
                //    ErrorMessage += Environment.NewLine + XmlHelper.Name(ThisSoil) + Environment.NewLine + StringManip.IndentText(Errors, 6);
				//}
			} else if (Data.Type.ToLower() == "folder") {
				foreach (ApsimFile.Component Child in Data.ChildNodes) {
					CheckSoils(Child, ref ErrorMessage);
				}
			}
		}

		public static void SortSoils(BaseController Controller)
		{
			Cursor.Current = Cursors.WaitCursor;
			Controller.Selection.Sort();
			Cursor.Current = Cursors.Default;
		}


		public static void Version(BaseController Controller)
		{
			MessageBox.Show(VersionString(), "Apsoil version", MessageBoxButtons.OK, MessageBoxIcon.Information);
		}
		public static string VersionString()
		{
			return "Version " + Configuration.Instance.Setting("Version");
		}
		public static void OpenLatestVersionOfSoilsDatabase(BaseController Controller)
		{

			if (Controller.FileSaveAfterPrompt()) {
				Cursor.Current = Cursors.WaitCursor;
				WebRequest request = WebRequest.Create("http://www.apsim.info/Wiki/public/Upload/ApSoil/APSRU-Australia-Soils.soils");
				HttpWebResponse response = (HttpWebResponse)request.GetResponse();
				if (response.StatusDescription == "OK") {
					Stream dataStream = response.GetResponseStream();
					StreamReader reader = new StreamReader(dataStream);
					string responseFromServer = reader.ReadToEnd();

					string SoilsFileName = Path.GetTempFileName();
					StreamWriter SoilsFile = new StreamWriter(SoilsFileName);
					SoilsFile.Write(responseFromServer);
					SoilsFile.Close();
					reader.Close();
					dataStream.Close();
					Controller.ApsimData.NewFromFile(SoilsFileName);
					File.Delete(SoilsFileName);
				} else {
					MessageBox.Show("Cannot connect to www.apsim.info", "Failure", MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
				response.Close();
				Cursor.Current = Cursors.Default;
			}
		}
		public static void ReleaseNotes(BaseController Controller)
		{
			string URL = Configuration.Instance.Setting("ReleaseNotes");
			System.Diagnostics.Process.Start(URL);
		}

		public static void GoogleEarthSoils(BaseController Controller)
		{
			if (Controller.FileSaveAfterPrompt()) {
				Cursor.Current = Cursors.WaitCursor;
				WebRequest request = WebRequest.Create("http://www.apsim.info/ApsoilWeb/ApsoilKML.aspx");
				HttpWebResponse response = (HttpWebResponse)request.GetResponse();
				if (response.StatusDescription == "OK") {
					Stream dataStream = response.GetResponseStream();
					StreamReader reader = new StreamReader(dataStream);
					string responseFromServer = reader.ReadToEnd();

					string KMLFileName = Path.GetTempPath() + "Soils.kmz";
					StreamWriter KMLFile = new StreamWriter(KMLFileName);
					KMLFile.Write(responseFromServer);
					KMLFile.Close();
					reader.Close();
					dataStream.Close();
					Process P = CSGeneral.Utility.RunProcess(KMLFileName, "", Path.GetTempPath());
					string Errors = CSGeneral.Utility.CheckProcessExitedProperly(P);
					if (!string.IsNullOrEmpty(Errors)) {
						MessageBox.Show(Errors, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
					}

				} else {
					MessageBox.Show("Cannot connect to www.apsim.info", "Failure", MessageBoxButtons.OK, MessageBoxIcon.Error);
				}
				response.Close();
				Cursor.Current = Cursors.Default;
			}
		}



		#region "PrintSoil"
		public static void PrintSoil(BaseController Controller)
		{
			//Dim SoilUI As CSUserInterface.SoilUI = DirectCast(Controller.Explorer.CurrentView, CSUserInterface.SoilUI)

			//Dim PrintDocument As New System.Drawing.Printing.PrintDocument()
			//AddHandler PrintDocument.BeginPrint, AddressOf SoilUI.OnBeginPrint
			//AddHandler PrintDocument.PrintPage, AddressOf SoilUI.OnPrintPage
			//AddHandler PrintDocument.QueryPageSettings, AddressOf OnQueryPageSettings

			//Dim PrintDialog As New PrintDialog()
			//PrintDialog.Document = PrintDocument

			//Dim PreviewDialog As New System.Windows.Forms.PrintPreviewDialog()
			//PreviewDialog.Document = PrintDocument
			//PreviewDialog.ShowDialog()
		}
		private static void OnQueryPageSettings(object sender, System.Drawing.Printing.QueryPageSettingsEventArgs e)
		{
			e.PageSettings.Margins.Left = 50;
			e.PageSettings.Margins.Top = 50;
			e.PageSettings.Margins.Right = 50;
			e.PageSettings.Margins.Bottom = 50;
		}
		#endregion


	}
}




