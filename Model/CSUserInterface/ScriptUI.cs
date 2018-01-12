using System;
using System.Collections.Generic;
using System.Drawing;
using System.IO;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Windows.Forms;
using System.Xml;
using CSGeneral;

namespace CSUserInterface
{
    public partial class ScriptUI : Controllers.BaseView
	{
        /// <summary>
        /// Used for storing the editor position
        /// </summary>
        struct CursorPos
        {
            public int TopLine;
            public int CharIndex;
        }

        /// <summary>
        /// The list of script editor position information
        /// </summary>
        private Dictionary<string, CursorPos> positions = new Dictionary<string, CursorPos>();

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

            // restore the scrolling and cursor position for the textbox
            CursorPos pos;
            if (positions.TryGetValue(this.NodePath, out pos))
            {
                TextBox.MoveToLine(pos.TopLine, 0);
                // ensure that the cursor is only set if it was in view
                if (pos.TopLine < TextBox.Source.GetPositionFromCharIndex(pos.CharIndex).Y)
                    TextBox.Selection.SelectionStart = pos.CharIndex;
                TextBox.Selection.SelectionLength = 0;
            }
            TextBox.Focus();    // this doesn't really work because the system tinkers with a few other controls
                                // after this procedure and then the textbox loses focus anyway. It would
                                // be good to find a solution for this. 
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

            // Store the cursor and text scroll position for the TextBox
            // Firstly find the top visible line number
            int lineVisible = 0;
            while ((lineVisible < TextBox.DisplayLines.Count) && !TextBox.DisplayLines.IsPointVisible(new Point(0, lineVisible)))
            {
                lineVisible++;
            }

            if (TextBox.Selection != null)
            {
                int charIndex = TextBox.Selection.SelectionStart;
                CursorPos position = new CursorPos();
                position.TopLine = lineVisible;
                position.CharIndex = charIndex;
                if (!positions.ContainsKey(this.NodePath))
                {
                    positions.Add(this.NodePath, position);
                }
                else
                {
                    positions[this.NodePath] = position;
                }
            }
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


