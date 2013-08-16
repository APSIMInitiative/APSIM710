using Microsoft.VisualBasic;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Data;
using System.Drawing;
using System.Diagnostics;
using System.Windows.Forms;
using System.Xml;
using CSGeneral;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
namespace CSUserInterface
{
    public partial class ScriptUI : Controllers.BaseView
	{

        public ScriptUI()
        {
            InitializeComponent();
        }

        // -----------------------------------
		// Refresh the UI
		// -----------------------------------
		public override void OnRefresh()
		{
			// Fill the property grid.
			XmlNode UINode = XmlHelper.Find(Data, "ui");
			string UIXml = "<ui/>";
			if ((UINode == null) || UINode.InnerXml == "") {
				TabControl.TabPages.Remove(Properties);

			} else {
				if (TabControl.TabPages.Count == 1) {
					TabControl.TabPages.Insert(0, Properties);
				}
				UIXml = UINode.OuterXml;
			}
			GenericUI.OnLoad(Controller, NodePath, UIXml);
			GenericUI.OnRefresh();

			PropertiesMenuItem.Checked = TabControl.TabPages.Count == 2;

            string scriptText = XmlHelper.Value(Data, "text");
			if (scriptText.Contains("Imports ")) {
				TextBox.Lexer = VbParser;
			} else {
				TextBox.Lexer = CsParser;
			}
            TextBox.Text = scriptText;
            Assembly.LoadFile(Path.Combine(Path.GetDirectoryName(Application.ExecutablePath), "CSDotNetComponentInterface.dll"));
			Assembly.LoadFile(Types.GetProbeInfoDLLFileName());

			foreach (string @ref in XmlHelper.ValuesRecursive(Data, "reference")) {
				if (File.Exists(@ref)) {
					Assembly.LoadFile(@ref);
				} else if (File.Exists(RuntimeEnvironment.GetRuntimeDirectory() + @ref)) {
					Assembly.LoadFile(RuntimeEnvironment.GetRuntimeDirectory() + @ref);
				} else if (File.Exists(Path.Combine(Path.GetDirectoryName(Application.ExecutablePath), @ref))) {
					Assembly.LoadFile(Path.Combine(Path.GetDirectoryName(Application.ExecutablePath), @ref));
				} else {
					MessageBox.Show("Error loading reference '" + @ref + "' - file does not exist" + Environment.NewLine + 
                        "Tried:" + Environment.NewLine + @ref + Environment.NewLine + 
                        RuntimeEnvironment.GetRuntimeDirectory() + @ref + Environment.NewLine + 
                        Path.Combine(Path.GetDirectoryName(Application.ExecutablePath), @ref));
				}
			}


			CsParser.RegisterAllAssemblies();
			VbParser.RegisterAllAssemblies();
			int[] TabStops = { 3 };
			TextBox.Lines.TabStops = TabStops;
			TextBox.Lines.UseSpaces = true;
		}



		public override void OnSave()
		{
			// --------------------------------------
			// Save the script box if it has changd.
			// --------------------------------------
			string Contents = "";
			if ((GenericUI != null)) {
				GenericUI.OnSave();
				Contents = GenericUI.GetData();
			}

			List<XmlNode> refs = new List<XmlNode>();
			refs = XmlHelper.ChildNodes(Data, "reference");

			Data.RemoveAll();

			if (!string.IsNullOrEmpty(Contents)) {
				XmlDocument Doc = new XmlDocument();
				Doc.LoadXml(Contents);
				Data.AppendChild(Data.OwnerDocument.ImportNode(Doc.DocumentElement, true));
			}

			foreach (XmlNode xn in refs) {
				Data.AppendChild(Data.OwnerDocument.ImportNode(xn, true));
			}

			XmlHelper.SetValue(Data, "text", TextBox.Text);

		}


		private void OnButtonClicked(System.Object sender, System.Windows.Forms.ToolStripItemClickedEventArgs e)
		{
			TextBox.DisplaySearchDialog();
		}

		private void OnPropertiesMenuItemClick(System.Object sender, System.EventArgs e)
		{
			if (TabControl.TabPages.Count == 1) {
				TabControl.TabPages.Insert(0, Properties);
                
                // Fill the property grid.
                string UIXml = "<ui/>";
                XmlNode UINode = XmlHelper.Find(Data, "ui");
                if ((UINode != null))
                {
                    UIXml = UINode.OuterXml;
                }
                GenericUI.OnLoad(Controller, NodePath, UIXml);
                GenericUI.OnRefresh();

                PropertiesMenuItem.Checked = TabControl.TabPages.Count == 2;
                TabControl.SelectedIndex = 0;
            }
            else
            {
                // Save the current contents of the grid
                GenericUI.OnSave();
                string Contents = GenericUI.GetData();
                if (!string.IsNullOrEmpty(Contents) && PropertiesMenuItem.Checked)
                {
                    XmlDocument Doc = new XmlDocument();
                    Doc.LoadXml(Contents);
                    XmlNode UINode = XmlHelper.Find(Data, "ui");
                    UINode.InnerXml = Doc.FirstChild.InnerXml;
                }
                TabControl.TabPages.Remove(Properties);
			}
			PropertiesMenuItem.Checked = TabControl.TabPages.Count == 2;
		}

        private void VariablesButton_Click(object sender, EventArgs e)
        {
            VariablesEventsForm F = new VariablesEventsForm(Controller);
            F.Show();
        }
	}
}


