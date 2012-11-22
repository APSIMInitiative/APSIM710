namespace CSUserInterface
{
    partial class ProfileUI
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
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle1 = new System.Windows.Forms.DataGridViewCellStyle();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ProfileUI));
            this.TopPanel = new System.Windows.Forms.Panel();
            this.Grid = new UIBits.EnhancedGrid();
            this.panel1 = new System.Windows.Forms.Panel();
            this.ManageCropsButton = new System.Windows.Forms.Button();
            this.Label = new System.Windows.Forms.Label();
            this.Splitter = new System.Windows.Forms.Splitter();
            this.Properties = new SoilPropertyUI();
            this.splitter1 = new System.Windows.Forms.Splitter();
            this.OCUnitsMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.walkleyBlackToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.totalToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.PHUnitsMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.toolStripMenuItem1 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem2 = new System.Windows.Forms.ToolStripMenuItem();
            this.BoronUnitsMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.toolStripMenuItem3 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem4 = new System.Windows.Forms.ToolStripMenuItem();
            this.NO3UnitsMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.NO3ppm = new System.Windows.Forms.ToolStripMenuItem();
            this.NO3kgha = new System.Windows.Forms.ToolStripMenuItem();
            this.SWUnitsMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.toolStripMenuItem7 = new System.Windows.Forms.ToolStripMenuItem();
            this.toolStripMenuItem8 = new System.Windows.Forms.ToolStripMenuItem();
            this.mmToolStripMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.NH4UnitsMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.NH4ppm = new System.Windows.Forms.ToolStripMenuItem();
            this.NH4kgha = new System.Windows.Forms.ToolStripMenuItem();
            this.CheckButton = new System.Windows.Forms.Button();
            this.TopPanel.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
            this.panel1.SuspendLayout();
            this.OCUnitsMenu.SuspendLayout();
            this.PHUnitsMenu.SuspendLayout();
            this.BoronUnitsMenu.SuspendLayout();
            this.NO3UnitsMenu.SuspendLayout();
            this.SWUnitsMenu.SuspendLayout();
            this.NH4UnitsMenu.SuspendLayout();
            this.SuspendLayout();
            // 
            // TopPanel
            // 
            this.TopPanel.Controls.Add(this.Grid);
            this.TopPanel.Controls.Add(this.panel1);
            this.TopPanel.Controls.Add(this.Label);
            this.TopPanel.Controls.Add(this.Splitter);
            this.TopPanel.Controls.Add(this.Properties);
            this.TopPanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.TopPanel.Location = new System.Drawing.Point(0, 16);
            this.TopPanel.Name = "TopPanel";
            this.TopPanel.Size = new System.Drawing.Size(655, 298);
            this.TopPanel.TabIndex = 16;
            // 
            // Grid
            // 
            this.Grid.AllowUserToAddRows = false;
            this.Grid.AllowUserToDeleteRows = false;
            this.Grid.BackgroundColor = System.Drawing.SystemColors.Window;
            this.Grid.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.Grid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.Grid.DataSourceTable = null;
            dataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight;
            dataGridViewCellStyle1.BackColor = System.Drawing.SystemColors.Window;
            dataGridViewCellStyle1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            dataGridViewCellStyle1.ForeColor = System.Drawing.SystemColors.ControlText;
            dataGridViewCellStyle1.SelectionBackColor = System.Drawing.SystemColors.Highlight;
            dataGridViewCellStyle1.SelectionForeColor = System.Drawing.SystemColors.HighlightText;
            dataGridViewCellStyle1.WrapMode = System.Windows.Forms.DataGridViewTriState.False;
            this.Grid.DefaultCellStyle = dataGridViewCellStyle1;
            this.Grid.Dock = System.Windows.Forms.DockStyle.Fill;
            this.Grid.Location = new System.Drawing.Point(275, 71);
            this.Grid.Name = "Grid";
            this.Grid.RowHeadersVisible = false;
            this.Grid.Size = new System.Drawing.Size(380, 227);
            this.Grid.TabIndex = 25;
            this.Grid.CellEndEdit += new System.Windows.Forms.DataGridViewCellEventHandler(this.OnEndEdit);
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.CheckButton);
            this.panel1.Controls.Add(this.ManageCropsButton);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Top;
            this.panel1.Location = new System.Drawing.Point(275, 40);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(380, 31);
            this.panel1.TabIndex = 28;
            // 
            // ManageCropsButton
            // 
            this.ManageCropsButton.Image = ((System.Drawing.Image)(resources.GetObject("ManageCropsButton.Image")));
            this.ManageCropsButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.ManageCropsButton.Location = new System.Drawing.Point(6, 3);
            this.ManageCropsButton.Name = "ManageCropsButton";
            this.ManageCropsButton.Size = new System.Drawing.Size(119, 23);
            this.ManageCropsButton.TabIndex = 28;
            this.ManageCropsButton.Text = "Manage crops";
            this.ManageCropsButton.UseVisualStyleBackColor = true;
            this.ManageCropsButton.Click += new System.EventHandler(this.ManageCropsButtonClick);
            // 
            // Label
            // 
            this.Label.BackColor = System.Drawing.SystemColors.Highlight;
            this.Label.Dock = System.Windows.Forms.DockStyle.Top;
            this.Label.Font = new System.Drawing.Font("Microsoft Sans Serif", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Label.ForeColor = System.Drawing.SystemColors.HighlightText;
            this.Label.Location = new System.Drawing.Point(275, 0);
            this.Label.Name = "Label";
            this.Label.Size = new System.Drawing.Size(380, 40);
            this.Label.TabIndex = 24;
            this.Label.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // Splitter
            // 
            this.Splitter.Location = new System.Drawing.Point(272, 0);
            this.Splitter.Name = "Splitter";
            this.Splitter.Size = new System.Drawing.Size(3, 298);
            this.Splitter.TabIndex = 18;
            this.Splitter.TabStop = false;
            // 
            // Properties
            // 
            this.Properties.AutoScroll = true;
            this.Properties.BackColor = System.Drawing.SystemColors.Window;
            this.Properties.Dock = System.Windows.Forms.DockStyle.Left;
            this.Properties.HelpText = "";
            this.Properties.Location = new System.Drawing.Point(0, 0);
            this.Properties.Name = "Properties";
            this.Properties.Size = new System.Drawing.Size(272, 298);
            this.Properties.TabIndex = 29;
            // 
            // splitter1
            // 
            this.splitter1.BackColor = System.Drawing.SystemColors.ScrollBar;
            this.splitter1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D;
            this.splitter1.Dock = System.Windows.Forms.DockStyle.Top;
            this.splitter1.Location = new System.Drawing.Point(0, 314);
            this.splitter1.Name = "splitter1";
            this.splitter1.Size = new System.Drawing.Size(655, 3);
            this.splitter1.TabIndex = 17;
            this.splitter1.TabStop = false;
            this.splitter1.SplitterMoved += new System.Windows.Forms.SplitterEventHandler(this.OnSplitterMoved);
            // 
            // OCUnitsMenu
            // 
            this.OCUnitsMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.walkleyBlackToolStripMenuItem,
            this.totalToolStripMenuItem});
            this.OCUnitsMenu.Name = "contextMenuStrip1";
            this.OCUnitsMenu.Size = new System.Drawing.Size(160, 48);
            this.OCUnitsMenu.ItemClicked += new System.Windows.Forms.ToolStripItemClickedEventHandler(this.UnitsMenuItemClicked);
            // 
            // walkleyBlackToolStripMenuItem
            // 
            this.walkleyBlackToolStripMenuItem.Name = "walkleyBlackToolStripMenuItem";
            this.walkleyBlackToolStripMenuItem.Size = new System.Drawing.Size(159, 22);
            this.walkleyBlackToolStripMenuItem.Text = "Walkley Black %";
            // 
            // totalToolStripMenuItem
            // 
            this.totalToolStripMenuItem.Name = "totalToolStripMenuItem";
            this.totalToolStripMenuItem.Size = new System.Drawing.Size(159, 22);
            this.totalToolStripMenuItem.Text = "Total %";
            // 
            // PHUnitsMenu
            // 
            this.PHUnitsMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripMenuItem1,
            this.toolStripMenuItem2});
            this.PHUnitsMenu.Name = "contextMenuStrip1";
            this.PHUnitsMenu.Size = new System.Drawing.Size(122, 48);
            this.PHUnitsMenu.ItemClicked += new System.Windows.Forms.ToolStripItemClickedEventHandler(this.UnitsMenuItemClicked);
            // 
            // toolStripMenuItem1
            // 
            this.toolStripMenuItem1.Name = "toolStripMenuItem1";
            this.toolStripMenuItem1.Size = new System.Drawing.Size(121, 22);
            this.toolStripMenuItem1.Text = "1:5 water";
            // 
            // toolStripMenuItem2
            // 
            this.toolStripMenuItem2.Name = "toolStripMenuItem2";
            this.toolStripMenuItem2.Size = new System.Drawing.Size(121, 22);
            this.toolStripMenuItem2.Text = "CaCl2";
            // 
            // BoronUnitsMenu
            // 
            this.BoronUnitsMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripMenuItem3,
            this.toolStripMenuItem4});
            this.BoronUnitsMenu.Name = "contextMenuStrip1";
            this.BoronUnitsMenu.Size = new System.Drawing.Size(145, 48);
            this.BoronUnitsMenu.ItemClicked += new System.Windows.Forms.ToolStripItemClickedEventHandler(this.UnitsMenuItemClicked);
            // 
            // toolStripMenuItem3
            // 
            this.toolStripMenuItem3.Name = "toolStripMenuItem3";
            this.toolStripMenuItem3.Size = new System.Drawing.Size(144, 22);
            this.toolStripMenuItem3.Text = "Hot 1:5 water";
            // 
            // toolStripMenuItem4
            // 
            this.toolStripMenuItem4.Name = "toolStripMenuItem4";
            this.toolStripMenuItem4.Size = new System.Drawing.Size(144, 22);
            this.toolStripMenuItem4.Text = "Hot CaCl2";
            // 
            // NO3UnitsMenu
            // 
            this.NO3UnitsMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.NO3ppm,
            this.NO3kgha});
            this.NO3UnitsMenu.Name = "contextMenuStrip1";
            this.NO3UnitsMenu.Size = new System.Drawing.Size(106, 48);
            this.NO3UnitsMenu.ItemClicked += new System.Windows.Forms.ToolStripItemClickedEventHandler(this.UnitsMenuItemClicked);
            // 
            // NO3ppm
            // 
            this.NO3ppm.Name = "NO3ppm";
            this.NO3ppm.Size = new System.Drawing.Size(105, 22);
            this.NO3ppm.Text = "ppm";
            // 
            // NO3kgha
            // 
            this.NO3kgha.Name = "NO3kgha";
            this.NO3kgha.Size = new System.Drawing.Size(105, 22);
            this.NO3kgha.Text = "kg/ha";
            // 
            // SWUnitsMenu
            // 
            this.SWUnitsMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.toolStripMenuItem7,
            this.toolStripMenuItem8,
            this.mmToolStripMenuItem});
            this.SWUnitsMenu.Name = "contextMenuStrip1";
            this.SWUnitsMenu.Size = new System.Drawing.Size(153, 70);
            this.SWUnitsMenu.ItemClicked += new System.Windows.Forms.ToolStripItemClickedEventHandler(this.UnitsMenuItemClicked);
            // 
            // toolStripMenuItem7
            // 
            this.toolStripMenuItem7.Name = "toolStripMenuItem7";
            this.toolStripMenuItem7.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem7.Text = "mm/mm";
            // 
            // toolStripMenuItem8
            // 
            this.toolStripMenuItem8.Name = "toolStripMenuItem8";
            this.toolStripMenuItem8.Size = new System.Drawing.Size(152, 22);
            this.toolStripMenuItem8.Text = "grav. mm/mm";
            // 
            // mmToolStripMenuItem
            // 
            this.mmToolStripMenuItem.Name = "mmToolStripMenuItem";
            this.mmToolStripMenuItem.Size = new System.Drawing.Size(152, 22);
            this.mmToolStripMenuItem.Text = "mm";
            // 
            // NH4UnitsMenu
            // 
            this.NH4UnitsMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.NH4ppm,
            this.NH4kgha});
            this.NH4UnitsMenu.Name = "contextMenuStrip1";
            this.NH4UnitsMenu.Size = new System.Drawing.Size(106, 48);
            this.NH4UnitsMenu.ItemClicked += new System.Windows.Forms.ToolStripItemClickedEventHandler(this.UnitsMenuItemClicked);
            // 
            // NH4ppm
            // 
            this.NH4ppm.Name = "NH4ppm";
            this.NH4ppm.Size = new System.Drawing.Size(105, 22);
            this.NH4ppm.Text = "ppm";
            // 
            // NH4kgha
            // 
            this.NH4kgha.Name = "NH4kgha";
            this.NH4kgha.Size = new System.Drawing.Size(105, 22);
            this.NH4kgha.Text = "kg/ha";
            // 
            // CheckButton
            // 
            this.CheckButton.Image = ((System.Drawing.Image)(resources.GetObject("CheckButton.Image")));
            this.CheckButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
            this.CheckButton.Location = new System.Drawing.Point(131, 4);
            this.CheckButton.Name = "CheckButton";
            this.CheckButton.Size = new System.Drawing.Size(119, 23);
            this.CheckButton.TabIndex = 29;
            this.CheckButton.Text = "Check soil";
            this.CheckButton.UseVisualStyleBackColor = true;
            this.CheckButton.Click += new System.EventHandler(this.CheckButtonClick);
            // 
            // ProfileUI
            // 
            this.Controls.Add(this.splitter1);
            this.Controls.Add(this.TopPanel);
            this.Name = "ProfileUI";
            this.Size = new System.Drawing.Size(655, 677);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.TopPanel, 0);
            this.Controls.SetChildIndex(this.splitter1, 0);
            this.TopPanel.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
            this.panel1.ResumeLayout(false);
            this.OCUnitsMenu.ResumeLayout(false);
            this.PHUnitsMenu.ResumeLayout(false);
            this.BoronUnitsMenu.ResumeLayout(false);
            this.NO3UnitsMenu.ResumeLayout(false);
            this.SWUnitsMenu.ResumeLayout(false);
            this.NH4UnitsMenu.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.Panel TopPanel;
        private System.Windows.Forms.Splitter Splitter;
        private System.Windows.Forms.Splitter splitter1;
        private System.Windows.Forms.Label Label;
        private UIBits.EnhancedGrid Grid;
        private System.Windows.Forms.ContextMenuStrip OCUnitsMenu;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Button ManageCropsButton;
        private System.Windows.Forms.ToolStripMenuItem walkleyBlackToolStripMenuItem;
        private System.Windows.Forms.ToolStripMenuItem totalToolStripMenuItem;
        private System.Windows.Forms.ContextMenuStrip PHUnitsMenu;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem1;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem2;
        private System.Windows.Forms.ContextMenuStrip BoronUnitsMenu;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem3;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem4;
        private System.Windows.Forms.ContextMenuStrip NO3UnitsMenu;
        private System.Windows.Forms.ToolStripMenuItem NO3ppm;
        private System.Windows.Forms.ToolStripMenuItem NO3kgha;
        private System.Windows.Forms.ContextMenuStrip SWUnitsMenu;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem7;
        private System.Windows.Forms.ToolStripMenuItem toolStripMenuItem8;
        private System.Windows.Forms.ToolStripMenuItem mmToolStripMenuItem;
        private System.Windows.Forms.ContextMenuStrip NH4UnitsMenu;
        private System.Windows.Forms.ToolStripMenuItem NH4ppm;
        private System.Windows.Forms.ToolStripMenuItem NH4kgha;
        private SoilPropertyUI Properties;
        private System.Windows.Forms.Button CheckButton;

    }

}