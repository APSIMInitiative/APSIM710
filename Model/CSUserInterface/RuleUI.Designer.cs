namespace CSUserInterface
{
    partial class RuleUI
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(RuleUI));
            this.TabControl = new System.Windows.Forms.TabControl();
            this.PopupMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.AddMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.DeleteMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.EditMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.ToolStripSeparator1 = new System.Windows.Forms.ToolStripSeparator();
            this.PropertiesMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.PropertiesTabPage = new System.Windows.Forms.TabPage();
            this.ImageList = new System.Windows.Forms.ImageList(this.components);
            this.GenericUI = new CSUserInterface.GenericUI();
            this.TabControl.SuspendLayout();
            this.PopupMenu.SuspendLayout();
            this.PropertiesTabPage.SuspendLayout();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Size = new System.Drawing.Size(1022, 16);
            // 
            // TabControl
            // 
            this.TabControl.ContextMenuStrip = this.PopupMenu;
            this.TabControl.Controls.Add(this.PropertiesTabPage);
            this.TabControl.Dock = System.Windows.Forms.DockStyle.Fill;
            this.TabControl.Location = new System.Drawing.Point(0, 16);
            this.TabControl.Name = "TabControl";
            this.TabControl.SelectedIndex = 0;
            this.TabControl.Size = new System.Drawing.Size(1022, 800);
            this.TabControl.TabIndex = 3;
            // 
            // PopupMenu
            // 
            this.PopupMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.AddMenuItem,
            this.DeleteMenuItem,
            this.EditMenuItem,
            this.ToolStripSeparator1,
            this.PropertiesMenuItem});
            this.PopupMenu.Name = "ContextMenuStrip";
            this.PopupMenu.Size = new System.Drawing.Size(236, 98);
            this.PopupMenu.Opening += new System.ComponentModel.CancelEventHandler(this.OnPopupOpening);
            // 
            // AddMenuItem
            // 
            this.AddMenuItem.Name = "AddMenuItem";
            this.AddMenuItem.Size = new System.Drawing.Size(235, 22);
            this.AddMenuItem.Text = "&Add another script item";
            this.AddMenuItem.Click += new System.EventHandler(this.OnAddMenuClick);
            // 
            // DeleteMenuItem
            // 
            this.DeleteMenuItem.Name = "DeleteMenuItem";
            this.DeleteMenuItem.Size = new System.Drawing.Size(235, 22);
            this.DeleteMenuItem.Text = "&Delete this script item";
            this.DeleteMenuItem.Click += new System.EventHandler(this.OnDeleteMenuClick);
            // 
            // EditMenuItem
            // 
            this.EditMenuItem.Name = "EditMenuItem";
            this.EditMenuItem.Size = new System.Drawing.Size(235, 22);
            this.EditMenuItem.Text = "&Edit this script item";
            this.EditMenuItem.Click += new System.EventHandler(this.OnEditMenuClick);
            // 
            // ToolStripSeparator1
            // 
            this.ToolStripSeparator1.Name = "ToolStripSeparator1";
            this.ToolStripSeparator1.Size = new System.Drawing.Size(232, 6);
            // 
            // PropertiesMenuItem
            // 
            this.PropertiesMenuItem.Name = "PropertiesMenuItem";
            this.PropertiesMenuItem.Size = new System.Drawing.Size(235, 22);
            this.PropertiesMenuItem.Text = "Add a &properties user interface";
            this.PropertiesMenuItem.Click += new System.EventHandler(this.OnPropertiesMenuClick);
            // 
            // PropertiesTabPage
            // 
            this.PropertiesTabPage.Controls.Add(this.GenericUI);
            this.PropertiesTabPage.Location = new System.Drawing.Point(4, 22);
            this.PropertiesTabPage.Name = "PropertiesTabPage";
            this.PropertiesTabPage.Size = new System.Drawing.Size(1014, 774);
            this.PropertiesTabPage.TabIndex = 0;
            this.PropertiesTabPage.Text = "Properties";
            this.PropertiesTabPage.UseVisualStyleBackColor = true;
            // 
            // ImageList
            // 
            this.ImageList.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("ImageList.ImageStream")));
            this.ImageList.TransparentColor = System.Drawing.Color.Transparent;
            this.ImageList.Images.SetKeyName(0, "find.png");
            // 
            // GenericUI
            // 
            this.GenericUI.AutoScroll = true;
            this.GenericUI.BackColor = System.Drawing.SystemColors.Control;
            this.GenericUI.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GenericUI.HelpText = "";
            this.GenericUI.Location = new System.Drawing.Point(0, 0);
            this.GenericUI.Name = "GenericUI";
            this.GenericUI.Size = new System.Drawing.Size(1014, 774);
            this.GenericUI.TabIndex = 0;
            // 
            // RuleUI
            // 
            this.Controls.Add(this.TabControl);
            this.Name = "RuleUI";
            this.Size = new System.Drawing.Size(1022, 816);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.TabControl, 0);
            this.TabControl.ResumeLayout(false);
            this.PopupMenu.ResumeLayout(false);
            this.PropertiesTabPage.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.ContextMenuStrip PopupMenu;
        private System.Windows.Forms.ToolStripMenuItem AddMenuItem;
        private System.Windows.Forms.ToolStripMenuItem DeleteMenuItem;
        private System.Windows.Forms.ToolStripSeparator ToolStripSeparator1;
        private System.Windows.Forms.ToolStripMenuItem EditMenuItem;
        private System.Windows.Forms.ToolStripMenuItem PropertiesMenuItem;
        private System.Windows.Forms.ImageList ImageList;
        private System.Windows.Forms.TabControl TabControl;
        private System.Windows.Forms.TabPage PropertiesTabPage;
    }
}
