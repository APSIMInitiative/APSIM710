using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Diagnostics;
using System.Windows.Forms;

using System.IO;

using ApsimFile;
using Controllers;
using CSGeneral;
namespace CSUserInterface
{


	public partial class FileUI : BaseView
	{

		private DateTime FileDateTime;
        string FullFileName;

        public FileUI() : base()
		{

			//This call is required by the Windows Form Designer.
			InitializeComponent();

			//Add any initialization after the InitializeComponent() call

		}

        [System.Diagnostics.DebuggerStepThrough()]
		public override void OnRefresh()
		{
			// ------------------------------
			// Refresh this window.
			// ------------------------------

			// Get a filename
			string FileName = null;
			if ((XmlHelper.Type(Data) == "outputfile") || (XmlHelper.Type(Data) == "summaryfile")) {
				FileName = ApsimFile.ComponentUtility.CalcFileName(Controller.ApsimData.Find(NodePath));
				FileContentsBox.ReadOnly = true;
				if (XmlHelper.Type(Data) == "outputfile") {
					XmlHelper.SetValue(Data, "filename", FileName);
					System.Xml.XmlNode fileNameNode = XmlHelper.Find(Data, "filename");
					XmlHelper.SetAttribute(fileNameNode, "output", "yes");

				}
			} else {
				System.Xml.XmlNode fileNameNode = XmlHelper.Find(Data, "filename");
				FileName = fileNameNode.InnerText;
				XmlHelper.SetAttribute(fileNameNode, "input", "yes");
				FileContentsBox.ReadOnly = false;
			}

			HelpText = FileName;

			FullFileName = Configuration.RemoveMacros(FileName);
			// Add a path to filename if necessary.
			if (!string.IsNullOrEmpty(Controller.ApsimData.FileName)) {
				FullFileName = Path.Combine(Path.GetDirectoryName(Controller.ApsimData.FileName), FullFileName);
			}

			bool ErrorsFound = false;
			bool WarningsFound = false;
			if (File.Exists(FullFileName)) {
				string text = null;
				StreamReader sr = null;

				try {
					sr = new StreamReader(FullFileName);
					text = sr.ReadToEnd();
					sr.Close();
					sr = null;
					FileContentsBox.Text = text;
					FileDateTime = File.GetLastWriteTime(FullFileName);

					ErrorsFound = text.Contains("APSIM  Fatal  Error");
					WarningsFound = text.Contains("APSIM Warning Error");
				} catch (System.Exception) {
				}

			} else {
				FileContentsBox.Text = "<File doesn't exist>";
			}

			BrowseButton.Visible = ((XmlHelper.Type(Data) != "outputfile") && (XmlHelper.Type(Data) != "summaryfile"));

			GotoErrorButton.Visible = ErrorsFound;
			GotoWarningButton.Visible = WarningsFound;
			if (ErrorsFound) {
				SearchString("APSIM  Fatal  Error");
			}
			Timer.Enabled = true;
		}

		public override void OnClose()
		{
			base.OnClose();
			Timer.Enabled = false;
		}
		private void OnBrowseClick(System.Object sender, System.EventArgs e)
		{
			// ----------------------------------------------
			// User has clicked on browse button
			// ----------------------------------------------
			if (OpenFileDialog.ShowDialog() == DialogResult.OK) {
				HelpText = OpenFileDialog.FileName;
				string FileName = OpenFileDialog.FileName;
				//If the File is in the same directory as the .apsim file then just use a relative path not an absolute path. Get rid of any directories in the path, just have the filename.
				//there will be no path for the .apsim file if the user has not saved yet.
				if ((!string.IsNullOrEmpty(Path.GetDirectoryName(Controller.ApsimData.FileName)))) {
					FileName = FileName.Replace(Path.GetDirectoryName(Controller.ApsimData.FileName) + Path.DirectorySeparatorChar, "");
					//replace the directories in the file path with "" IF they match the directory path of the .apsim file.
				}
				//If the file is in the same directory as the install location for this version of Apsim. Then just use the "%apsuite" macro instead of the installation path.
				//This way you won't need to change it when you upgrade to a new version of Apsim. (eg. APSIMSettings.ApsimDirectory for Apsim version 6.0 would be "C:\Program Files\Apsim6")
				FileName = Configuration.AddMacros(FileName);
				//replace the directories in the file path with "%apsuite" IF they match the directory path of the install directory of this version of Apsim.
				XmlHelper.SetValue(Data, "filename", FileName);
				//change the chunk of xml (from the .apsim file) for this node to the new file name.
				this.OnRefresh();
				//refresh the FileUI gui.
			}
		}


		public override void OnSave()
		{
			if ((!FileContentsBox.ReadOnly) && FileContentsBox.Modified && System.IO.File.Exists(FullFileName)) {
				FileContentsBox.SaveFile(FullFileName, RichTextBoxStreamType.PlainText);
			}
		}

		private void SearchButton_Click(System.Object sender, System.EventArgs e)
		{
			SearchString(SearchTextBox.Text);
		}
		private void SearchString(string text)
		{
			if (text.Length > 0) {
				FileContentsBox.SelectionBackColor = Color.FromKnownColor(KnownColor.Control);
				FileContentsBox.SelectionStart = FileContentsBox.SelectionStart + 1;
				int indexToText = FileContentsBox.Find(text, FileContentsBox.SelectionStart, RichTextBoxFinds.None);
				if (indexToText == -1) {
					FileContentsBox.SelectionStart = 0;
				}
				if (indexToText >= 0) {
					FileContentsBox.SelectionBackColor = Color.LightBlue;
					FileContentsBox.ScrollToCaret();
				}
			}
		}

		private void FileContentsBox_KeyDown(System.Object sender, System.Windows.Forms.KeyEventArgs e)
		{
			if (e.KeyCode.Equals(Keys.F3)) {
				SearchString(SearchTextBox.Text);
			} else if (e.KeyCode.Equals(Keys.F) && (Control.ModifierKeys == Keys.Control)) {
				SearchTextBox.Focus();
			}
		}


		private void OnSearchBoxKeyDown(System.Object sender, System.Windows.Forms.KeyEventArgs e)
		{
			if (e.KeyCode.Equals(Keys.F3) || e.KeyCode.Equals(Keys.Return)) {
				SearchString(SearchTextBox.Text);
			}

		}

		private void OnGotoErrorClick(System.Object sender, System.EventArgs e)
		{
			SearchString("APSIM  Fatal  Error");
		}

		private void OnGotoWarningClick(System.Object sender, System.EventArgs e)
		{
			SearchString("APSIM Warning Error");
		}

		private void OnTimerTick(System.Object sender, System.EventArgs e)
		{
			if (!string.IsNullOrEmpty(FullFileName) && File.Exists(FullFileName) && FileDateTime != File.GetLastWriteTime(FullFileName)) {
				OnRefresh();
			}
		}
	}
}
