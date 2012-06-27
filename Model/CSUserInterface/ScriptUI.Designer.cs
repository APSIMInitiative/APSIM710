namespace CSUserInterface
{
    partial class ScriptUI
    {
        /// <summary>
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ScriptUI));
            this.TabControl = new System.Windows.Forms.TabControl();
            this.PopupMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.PropertiesMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.Properties = new System.Windows.Forms.TabPage();
            this.GenericUI = new CSUserInterface.GenericUI();
            this.Script = new System.Windows.Forms.TabPage();
            this.TextBox = new QWhale.Editor.SyntaxEdit(this.components);
            this.ToolStrip1 = new System.Windows.Forms.ToolStrip();
            this.FindReplaceButton = new System.Windows.Forms.ToolStripButton();
            this.CsParser = new QWhale.Syntax.Parsers.CsParser();
            this.VbParser = new QWhale.Syntax.Parsers.VbParser();
            this.TabControl.SuspendLayout();
            this.PopupMenu.SuspendLayout();
            this.Properties.SuspendLayout();
            this.Script.SuspendLayout();
            this.ToolStrip1.SuspendLayout();
            this.SuspendLayout();
            // 
            // TabControl
            // 
            this.TabControl.ContextMenuStrip = this.PopupMenu;
            this.TabControl.Controls.Add(this.Properties);
            this.TabControl.Controls.Add(this.Script);
            this.TabControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.TabControl.Location = new System.Drawing.Point(0, 16);
            this.TabControl.Name = "TabControl";
            this.TabControl.SelectedIndex = 0;
            this.TabControl.Size = new System.Drawing.Size(655, 525);
            this.TabControl.TabIndex = 2;
            // 
            // PopupMenu
            // 
            this.PopupMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.PropertiesMenuItem});
            this.PopupMenu.Name = "ContextMenuStrip";
            this.PopupMenu.Size = new System.Drawing.Size(169, 48);
            // 
            // PropertiesMenuItem
            // 
            this.PropertiesMenuItem.Checked = true;
            this.PropertiesMenuItem.CheckState = System.Windows.Forms.CheckState.Checked;
            this.PropertiesMenuItem.Name = "PropertiesMenuItem";
            this.PropertiesMenuItem.Size = new System.Drawing.Size(168, 22);
            this.PropertiesMenuItem.Text = "Show &properties?";
            this.PropertiesMenuItem.Click += new System.EventHandler(this.OnPropertiesMenuItemClick);
            // 
            // Properties
            // 
            this.Properties.Controls.Add(this.GenericUI);
            this.Properties.Location = new System.Drawing.Point(4, 22);
            this.Properties.Name = "Properties";
            this.Properties.Padding = new System.Windows.Forms.Padding(3);
            this.Properties.Size = new System.Drawing.Size(647, 499);
            this.Properties.TabIndex = 0;
            this.Properties.Text = "Properties";
            this.Properties.UseVisualStyleBackColor = true;
            // 
            // GenericUI
            // 
            this.GenericUI.AutoScroll = true;
            this.GenericUI.BackColor = System.Drawing.SystemColors.Window;
            this.GenericUI.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GenericUI.HelpText = "";
            this.GenericUI.Location = new System.Drawing.Point(3, 3);
            this.GenericUI.Name = "GenericUI";
            this.GenericUI.Size = new System.Drawing.Size(641, 493);
            this.GenericUI.TabIndex = 0;
            // 
            // Script
            // 
            this.Script.Controls.Add(this.TextBox);
            this.Script.Controls.Add(this.ToolStrip1);
            this.Script.Location = new System.Drawing.Point(4, 22);
            this.Script.Name = "Script";
            this.Script.Padding = new System.Windows.Forms.Padding(3);
            this.Script.Size = new System.Drawing.Size(647, 499);
            this.Script.TabIndex = 1;
            this.Script.Text = "Script";
            this.Script.UseVisualStyleBackColor = true;
            // 
            // TextBox
            // 
            this.TextBox.BackColor = System.Drawing.SystemColors.Window;
            this.TextBox.Cursor = System.Windows.Forms.Cursors.IBeam;
            this.TextBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.TextBox.Font = new System.Drawing.Font("Courier New", 10F);
            this.TextBox.Gutter.Options = ((QWhale.Editor.GutterOptions)((((QWhale.Editor.GutterOptions.PaintLineNumbers | QWhale.Editor.GutterOptions.PaintLinesOnGutter) 
            | QWhale.Editor.GutterOptions.PaintBookMarks) 
            | QWhale.Editor.GutterOptions.PaintLineModificators)));
            this.TextBox.Location = new System.Drawing.Point(3, 28);
            this.TextBox.Name = "TextBox";
            this.TextBox.Outlining.AllowOutlining = true;
            this.TextBox.Size = new System.Drawing.Size(641, 468);
            this.TextBox.TabIndex = 0;
            this.TextBox.Text = "";
            // 
            // ToolStrip1
            // 
            this.ToolStrip1.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.FindReplaceButton});
            this.ToolStrip1.Location = new System.Drawing.Point(3, 3);
            this.ToolStrip1.Name = "ToolStrip1";
            this.ToolStrip1.Size = new System.Drawing.Size(641, 25);
            this.ToolStrip1.TabIndex = 1;
            this.ToolStrip1.Text = "ToolStrip1";
            this.ToolStrip1.ItemClicked += new System.Windows.Forms.ToolStripItemClickedEventHandler(this.OnButtonClicked);
            // 
            // FindReplaceButton
            // 
            this.FindReplaceButton.Image = ((System.Drawing.Image)(resources.GetObject("FindReplaceButton.Image")));
            this.FindReplaceButton.ImageTransparentColor = System.Drawing.Color.Magenta;
            this.FindReplaceButton.Name = "FindReplaceButton";
            this.FindReplaceButton.Size = new System.Drawing.Size(89, 22);
            this.FindReplaceButton.Text = "Find/Replace";
            // 
            // CsParser
            // 
            this.CsParser.DefaultState = 0;
            this.CsParser.Options = ((QWhale.Syntax.SyntaxOptions)(((((((QWhale.Syntax.SyntaxOptions.Outline | QWhale.Syntax.SyntaxOptions.SmartIndent) 
            | QWhale.Syntax.SyntaxOptions.CodeCompletion) 
            | QWhale.Syntax.SyntaxOptions.SyntaxErrors) 
            | QWhale.Syntax.SyntaxOptions.AutoComplete) 
            | QWhale.Syntax.SyntaxOptions.FormatCase) 
            | QWhale.Syntax.SyntaxOptions.FormatSpaces)));
            this.CsParser.XmlScheme = resources.GetString("CsParser.XmlScheme");
            // 
            // VbParser
            // 
            this.VbParser.DefaultState = 0;
            this.VbParser.Options = ((QWhale.Syntax.SyntaxOptions)(((((((((QWhale.Syntax.SyntaxOptions.Outline | QWhale.Syntax.SyntaxOptions.SmartIndent) 
            | QWhale.Syntax.SyntaxOptions.CodeCompletion) 
            | QWhale.Syntax.SyntaxOptions.SyntaxErrors) 
            | QWhale.Syntax.SyntaxOptions.ReparseOnLineChange) 
            | QWhale.Syntax.SyntaxOptions.AutoComplete) 
            | QWhale.Syntax.SyntaxOptions.FormatCase) 
            | QWhale.Syntax.SyntaxOptions.FormatSpaces) 
            | QWhale.Syntax.SyntaxOptions.CodeCompletionTabs)));
            this.VbParser.XmlScheme = resources.GetString("VbParser.XmlScheme");
            // 
            // ScriptUI
            // 
            this.Controls.Add(this.TabControl);
            this.Name = "ScriptUI";
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.TabControl, 0);
            this.TabControl.ResumeLayout(false);
            this.PopupMenu.ResumeLayout(false);
            this.Properties.ResumeLayout(false);
            this.Script.ResumeLayout(false);
            this.Script.PerformLayout();
            this.ToolStrip1.ResumeLayout(false);
            this.ToolStrip1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TabControl TabControl;
        private System.Windows.Forms.TabPage Properties;
        private System.Windows.Forms.TabPage Script;
        private CSUserInterface.GenericUI GenericUI;
        private QWhale.Editor.SyntaxEdit TextBox;
        private QWhale.Syntax.Parsers.CsParser CsParser;
        private QWhale.Syntax.Parsers.VbParser VbParser;
        private System.Windows.Forms.ToolStrip ToolStrip1;
        private System.Windows.Forms.ToolStripButton FindReplaceButton;
        private System.Windows.Forms.ContextMenuStrip PopupMenu;
        private System.Windows.Forms.ToolStripMenuItem PropertiesMenuItem;
    }
}
