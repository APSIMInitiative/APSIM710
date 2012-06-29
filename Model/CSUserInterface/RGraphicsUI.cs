using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Diagnostics;
using System.Windows.Forms;

using System.Collections.Specialized;
using System.IO;
using System.Xml;
using Microsoft.Win32;

using Controllers;
using CSGeneral;
//using CSGeneral.Utility;

using UIBits;
namespace CSUserInterface
{
	//InputDialog

	public partial class RGraphicsUI : BaseView
	{

		public RGraphicsUI() : base()
		{

			//This call is required by the Windows Form Designer.
			InitializeComponent();

			//Add any initialization after the InitializeComponent() call
			ScriptBox = new QWhale.Editor.SyntaxEdit();

			ScriptBox.WordWrap = false;
			ScriptBox.Gutter.Options = (QWhale.Editor.GutterOptions)(((QWhale.Editor.GutterOptions.PaintLineNumbers | QWhale.Editor.GutterOptions.PaintLinesOnGutter) | QWhale.Editor.GutterOptions.PaintBookMarks) | QWhale.Editor.GutterOptions.PaintLineModificators);
			ScriptBox.Dock = DockStyle.Fill;
			ScriptBox.BringToFront();
			int[] TabStops = { 3 };
			ScriptBox.Lines.TabStops = TabStops;
			ScriptBox.Lines.UseSpaces = true;

			ScriptPage.Controls.Add(ScriptBox);

            ConsoleBox.BringToFront();
		}

        protected override void OnLoad()
		{
		}
		// -----------------------------------
		// Refresh the UI
		// -----------------------------------

		public override void OnRefresh()
		{
			string Script = "";
			foreach (XmlNode ScriptNode in XmlHelper.ChildNodes(Data, "script")) {
				Script = Script + XmlHelper.Value(ScriptNode, "text");
			}
			ScriptBox.Text = Script;
			ConsoleBox.Text = "";

			doIt(Script);

		}
		public override void OnSave()
		{
			// --------------------------------------
			// Save the script box if it has changed.
			// --------------------------------------
			Data.RemoveAll();

			XmlNode ScriptNode = Data.AppendChild(Data.OwnerDocument.CreateElement("script"));
			XmlHelper.SetName(ScriptNode, "script");
			XmlHelper.SetValue(ScriptNode, "text", ScriptBox.Text);

		}

		private void doIt(string script)
		{
			int desiredImageWidth = this.TabControl.Size.Width - 5;
			int desiredImageHeight = this.TabControl.Size.Height - 5;
			string[] fullpath = NodePath.Split(new char[] {'/'});
			string nodeName = fullpath[fullpath.Length - 1];
			string imageFileName = Directory.GetCurrentDirectory() + Path.DirectorySeparatorChar + nodeName + ".png";
			string scriptFileName = Directory.GetCurrentDirectory() + Path.DirectorySeparatorChar + nodeName + ".R";

			List<string> OutputFileNames = new List<string>();
			UIUtility.OutputFileUtility.GetOutputFiles(Controller, Controller.Selection, OutputFileNames);

			// Build the R script from our XML value
			StringWriter newScript = new StringWriter();
			newScript.WriteLine("# Automatically generated - do not edit");
			newScript.WriteLine("width<- " + desiredImageWidth.ToString());
			newScript.WriteLine("height<- " + desiredImageHeight.ToString());
			newScript.WriteLine("imageFileName <- \"" + imageFileName.Replace("\\", "/") + "\"");
			newScript.Write("inputFiles <- c(");

			bool first = true;
			foreach (string outputfile in OutputFileNames) {
				if ((!(first))) {
					newScript.Write(",");
				}
				newScript.Write("\"" + outputfile.Replace("\\", "/") + "\"");
				first = false;
			}
			newScript.WriteLine(")");
			newScript.Write(script);

			bool needsRerun = false;

			// See if the script has changed since its last run
			if ((File.Exists(scriptFileName))) {
				StreamReader sfp = new StreamReader(scriptFileName, false);
				string oldScript = sfp.ReadToEnd();
				needsRerun = !(string.Equals(oldScript, newScript.ToString()));
				sfp.Close();
			} else {
				needsRerun = true;
			}

			// See if the input files have changed
			if ((!(File.Exists(imageFileName)))) {
				needsRerun = true;
			} else {
				// See if a simulation has been run that invalidates this image
				System.DateTime myDate = File.GetCreationTime(imageFileName);
				foreach (string outputfile in OutputFileNames) {
					if ((File.Exists(outputfile) && (File.GetCreationTime(outputfile) > myDate))) {
						needsRerun = true;
					}
				}
			}

			// See if the window size has changed
			if ((!(needsRerun) && File.Exists(imageFileName))) {
				Image diskImage = Image.FromFile(imageFileName);
				if (((desiredImageWidth != diskImage.Width) || (desiredImageHeight != diskImage.Height))) {
					needsRerun = true;
				}
				diskImage.Dispose();
			} else {
				needsRerun = true;
			}

			bool canRun = true;
			if ((needsRerun)) {
				foreach (string outputfile in OutputFileNames) {
					if ((!(File.Exists(outputfile)))) {
						canRun = false;
					}
				}
			}

			if ((!(canRun))) {
				this.ConsoleBox.Text = "Output files are missing. Can't run R. Run APSIM first.";
			} else if ((needsRerun)) {
				StreamWriter fp = new StreamWriter(scriptFileName, false);
				fp.Write(newScript.ToString());
				fp.Close();

				// scrub the old image so that we can be sure it's regenerated
				try {
					File.Delete(imageFileName);
				} catch (System.IO.IOException) {
				} finally {
				}

				// try and run R with this script
				RegistryKey regKey = Registry.LocalMachine.OpenSubKey("SOFTWARE\\R-core\\R", false);
				if (((regKey != null))) {
					string rpath = (string)regKey.GetValue("InstallPath", "");

					// Should test somehow for pre 2.12.x that doesnt have rscript installed
					string rcmd = rpath + "\\bin\\Rscript.exe";
					string args = "--slave --vanilla \"" + scriptFileName + "\"";

					string consoleMsg = "Command:   " + rcmd + Environment.NewLine + Environment.NewLine;
					consoleMsg += "Arguments: " + args + Environment.NewLine + Environment.NewLine;
					this.ConsoleBox.Text = consoleMsg;

					System.Diagnostics.Process p = Utility.RunProcess(rcmd, args, Directory.GetCurrentDirectory());
					p.WaitForExit();

					consoleMsg += "stdout: " + Environment.NewLine + p.StandardOutput.ReadToEnd() + Environment.NewLine + Environment.NewLine;
					consoleMsg += "stderr: " + Environment.NewLine + p.StandardError.ReadToEnd() + Environment.NewLine + Environment.NewLine;
					consoleMsg += "script: " + Environment.NewLine + newScript.ToString();
					this.ConsoleBox.Text = consoleMsg;
				}

			}

			// update displayed image
			if ((File.Exists(imageFileName))) {
				FileStream newImageStream = new FileStream(imageFileName, FileMode.Open, FileAccess.Read);
				this.PictureBox.Image = Image.FromStream(newImageStream);
				newImageStream.Dispose();
			}


		}

		public void onTabSelected(object sender, TabControlEventArgs e)
		{
			doIt(ScriptBox.Text.ToString());
		}

	}
}
