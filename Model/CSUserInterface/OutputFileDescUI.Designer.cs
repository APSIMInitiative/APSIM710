namespace CSUserInterface
{
    partial class OutputFileDescUI
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
        [System.Diagnostics.DebuggerStepThrough()]
        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(OutputFileDescUI));
            this.OpenFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.GridPanel = new System.Windows.Forms.Panel();
            this.Grid = new UIBits.EnhancedGrid();
            this.HelpPanel = new System.Windows.Forms.Panel();
            this.GridLabel = new System.Windows.Forms.Label();
            this.HelpButton = new System.Windows.Forms.Button();
            this.Splitter1 = new System.Windows.Forms.Splitter();
            this.Splitter2 = new System.Windows.Forms.Splitter();
            this.FilterPanel = new System.Windows.Forms.Panel();
            this.textBoxSearch = new System.Windows.Forms.TextBox();
            this.DictionaryLabel = new System.Windows.Forms.Label();
            this.VariableListView = new System.Windows.Forms.ListView();
            this.ColumnHeader1 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.ColumnHeader4 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.ColumnHeader2 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.ColumnHeader3 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.Label1 = new System.Windows.Forms.Label();
            this.ComponentFilter = new System.Windows.Forms.ComboBox();
            this.ConstantsPanel = new System.Windows.Forms.Panel();
            this.ConstantsBox = new System.Windows.Forms.TextBox();
            this.TitleLabel = new System.Windows.Forms.Label();
            this.ConstantsLabel = new System.Windows.Forms.Label();
            this.GridContextMenu = new System.Windows.Forms.ContextMenuStrip(this.components);
            this.MoveUpMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.MoveDownMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.DeleteVariablesMenuItem = new System.Windows.Forms.ToolStripMenuItem();
            this.BottomPanel = new System.Windows.Forms.Panel();
            this.GridPanel.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
            this.HelpPanel.SuspendLayout();
            this.FilterPanel.SuspendLayout();
            this.ConstantsPanel.SuspendLayout();
            this.GridContextMenu.SuspendLayout();
            this.BottomPanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Size = new System.Drawing.Size(753, 16);
            // 
            // OpenFileDialog
            // 
            this.OpenFileDialog.CheckFileExists = false;
            this.OpenFileDialog.DefaultExt = "out";
            this.OpenFileDialog.Filter = "APSIM output files(*.out)|*.out|All Files (*.*)|*.*";
            this.OpenFileDialog.RestoreDirectory = true;
            this.OpenFileDialog.Title = "Enter output file name";
            // 
            // GridPanel
            // 
            this.GridPanel.Controls.Add(this.Grid);
            this.GridPanel.Controls.Add(this.HelpPanel);
            this.GridPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GridPanel.Location = new System.Drawing.Point(0, 0);
            this.GridPanel.Name = "GridPanel";
            this.GridPanel.Size = new System.Drawing.Size(353, 419);
            this.GridPanel.TabIndex = 11;
            // 
            // Grid
            // 
            this.Grid.AllowDrop = true;
            this.Grid.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.Grid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.Grid.ColumnHeadersVisible = false;
            this.Grid.DataSourceTable = null;
            this.Grid.Dock = System.Windows.Forms.DockStyle.Fill;
            this.Grid.Location = new System.Drawing.Point(0, 30);
            this.Grid.Name = "Grid";
            this.Grid.RowHeadersVisible = false;
            this.Grid.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect;
            this.Grid.Size = new System.Drawing.Size(353, 389);
            this.Grid.TabIndex = 27;
            this.Grid.DragDrop += new System.Windows.Forms.DragEventHandler(this.VariablesGridDragDrop);
            this.Grid.DragEnter += new System.Windows.Forms.DragEventHandler(this.VariablesGridDragEnter);
            this.Grid.DragOver += new System.Windows.Forms.DragEventHandler(this.VariablesGridDragOver);
            // 
            // HelpPanel
            // 
            this.HelpPanel.Controls.Add(this.GridLabel);
            this.HelpPanel.Controls.Add(this.HelpButton);
            this.HelpPanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.HelpPanel.Location = new System.Drawing.Point(0, 0);
            this.HelpPanel.Name = "HelpPanel";
            this.HelpPanel.Size = new System.Drawing.Size(353, 30);
            this.HelpPanel.TabIndex = 27;
            // 
            // GridLabel
            // 
            this.GridLabel.BackColor = System.Drawing.SystemColors.ActiveCaption;
            this.GridLabel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GridLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.GridLabel.ForeColor = System.Drawing.SystemColors.ActiveCaptionText;
            this.GridLabel.Location = new System.Drawing.Point(30, 0);
            this.GridLabel.Name = "GridLabel";
            this.GridLabel.Size = new System.Drawing.Size(323, 30);
            this.GridLabel.TabIndex = 25;
            this.GridLabel.Text = "Output file columns:";
            this.GridLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // HelpButton
            // 
            this.HelpButton.AutoSize = true;
            this.HelpButton.Dock = System.Windows.Forms.DockStyle.Left;
            this.HelpButton.Image = ((System.Drawing.Image)(resources.GetObject("HelpButton.Image")));
            this.HelpButton.Location = new System.Drawing.Point(0, 0);
            this.HelpButton.Name = "HelpButton";
            this.HelpButton.Size = new System.Drawing.Size(30, 30);
            this.HelpButton.TabIndex = 26;
            this.HelpButton.UseVisualStyleBackColor = true;
            this.HelpButton.Click += new System.EventHandler(this.OnHelpClick);
            // 
            // Splitter1
            // 
            this.Splitter1.Dock = System.Windows.Forms.DockStyle.Top;
            this.Splitter1.Location = new System.Drawing.Point(0, 102);
            this.Splitter1.Name = "Splitter1";
            this.Splitter1.Size = new System.Drawing.Size(753, 3);
            this.Splitter1.TabIndex = 22;
            this.Splitter1.TabStop = false;
            // 
            // Splitter2
            // 
            this.Splitter2.Dock = System.Windows.Forms.DockStyle.Right;
            this.Splitter2.Location = new System.Drawing.Point(353, 0);
            this.Splitter2.Name = "Splitter2";
            this.Splitter2.Size = new System.Drawing.Size(3, 419);
            this.Splitter2.TabIndex = 23;
            this.Splitter2.TabStop = false;
            // 
            // FilterPanel
            // 
            this.FilterPanel.Controls.Add(this.textBoxSearch);
            this.FilterPanel.Controls.Add(this.DictionaryLabel);
            this.FilterPanel.Controls.Add(this.VariableListView);
            this.FilterPanel.Controls.Add(this.Label1);
            this.FilterPanel.Controls.Add(this.ComponentFilter);
            this.FilterPanel.Dock = System.Windows.Forms.DockStyle.Right;
            this.FilterPanel.Location = new System.Drawing.Point(356, 0);
            this.FilterPanel.Name = "FilterPanel";
            this.FilterPanel.Size = new System.Drawing.Size(397, 419);
            this.FilterPanel.TabIndex = 18;
            // 
            // textBoxSearch
            // 
            this.textBoxSearch.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.textBoxSearch.BackColor = System.Drawing.SystemColors.Info;
            this.textBoxSearch.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Italic, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.textBoxSearch.ForeColor = System.Drawing.SystemColors.GrayText;
            this.textBoxSearch.Location = new System.Drawing.Point(263, 33);
            this.textBoxSearch.Name = "textBoxSearch";
            this.textBoxSearch.Size = new System.Drawing.Size(122, 20);
            this.textBoxSearch.TabIndex = 23;
            this.textBoxSearch.Text = "Search";
            this.textBoxSearch.TextChanged += new System.EventHandler(this.textBoxSearch_TextChanged);
            this.textBoxSearch.Enter += new System.EventHandler(this.textBoxSearch_Enter);
            this.textBoxSearch.Leave += new System.EventHandler(this.textBoxSearch_Leave);
            // 
            // DictionaryLabel
            // 
            this.DictionaryLabel.BackColor = System.Drawing.SystemColors.ActiveCaption;
            this.DictionaryLabel.Dock = System.Windows.Forms.DockStyle.Top;
            this.DictionaryLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.DictionaryLabel.ForeColor = System.Drawing.SystemColors.ActiveCaptionText;
            this.DictionaryLabel.Location = new System.Drawing.Point(0, 0);
            this.DictionaryLabel.Name = "DictionaryLabel";
            this.DictionaryLabel.Size = new System.Drawing.Size(397, 30);
            this.DictionaryLabel.TabIndex = 22;
            this.DictionaryLabel.Text = "Variables to drag onto grid:";
            this.DictionaryLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // VariableListView
            // 
            this.VariableListView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.VariableListView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.ColumnHeader1,
            this.ColumnHeader4,
            this.ColumnHeader2,
            this.ColumnHeader3});
            this.VariableListView.FullRowSelect = true;
            this.VariableListView.Location = new System.Drawing.Point(11, 59);
            this.VariableListView.Name = "VariableListView";
            this.VariableListView.Size = new System.Drawing.Size(374, 346);
            this.VariableListView.Sorting = System.Windows.Forms.SortOrder.Ascending;
            this.VariableListView.TabIndex = 20;
            this.VariableListView.UseCompatibleStateImageBehavior = false;
            this.VariableListView.View = System.Windows.Forms.View.Details;
            this.VariableListView.ItemDrag += new System.Windows.Forms.ItemDragEventHandler(this.ListViewItemDrag);
            this.VariableListView.DoubleClick += new System.EventHandler(this.VariableListView_DoubleClick);
            // 
            // ColumnHeader1
            // 
            this.ColumnHeader1.Text = "Variable name";
            this.ColumnHeader1.Width = 201;
            // 
            // ColumnHeader4
            // 
            this.ColumnHeader4.Text = "Array?";
            this.ColumnHeader4.Width = 45;
            // 
            // ColumnHeader2
            // 
            this.ColumnHeader2.Text = "Units";
            // 
            // ColumnHeader3
            // 
            this.ColumnHeader3.Text = "Description";
            this.ColumnHeader3.Width = 437;
            // 
            // Label1
            // 
            this.Label1.AutoSize = true;
            this.Label1.Location = new System.Drawing.Point(10, 35);
            this.Label1.Name = "Label1";
            this.Label1.Size = new System.Drawing.Size(86, 13);
            this.Label1.TabIndex = 19;
            this.Label1.Text = "Component filter:";
            // 
            // ComponentFilter
            // 
            this.ComponentFilter.FormattingEnabled = true;
            this.ComponentFilter.Location = new System.Drawing.Point(102, 32);
            this.ComponentFilter.Name = "ComponentFilter";
            this.ComponentFilter.Size = new System.Drawing.Size(155, 21);
            this.ComponentFilter.TabIndex = 18;
            this.ComponentFilter.TextChanged += new System.EventHandler(this.ComponentFilter_TextChanged);
            // 
            // ConstantsPanel
            // 
            this.ConstantsPanel.Controls.Add(this.ConstantsBox);
            this.ConstantsPanel.Controls.Add(this.TitleLabel);
            this.ConstantsPanel.Controls.Add(this.ConstantsLabel);
            this.ConstantsPanel.Dock = System.Windows.Forms.DockStyle.Top;
            this.ConstantsPanel.Location = new System.Drawing.Point(0, 16);
            this.ConstantsPanel.Name = "ConstantsPanel";
            this.ConstantsPanel.Size = new System.Drawing.Size(753, 86);
            this.ConstantsPanel.TabIndex = 24;
            // 
            // ConstantsBox
            // 
            this.ConstantsBox.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.ConstantsBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.ConstantsBox.Location = new System.Drawing.Point(0, 43);
            this.ConstantsBox.Multiline = true;
            this.ConstantsBox.Name = "ConstantsBox";
            this.ConstantsBox.Size = new System.Drawing.Size(753, 43);
            this.ConstantsBox.TabIndex = 19;
            // 
            // TitleLabel
            // 
            this.TitleLabel.AutoSize = true;
            this.TitleLabel.BackColor = System.Drawing.SystemColors.ActiveCaption;
            this.TitleLabel.Dock = System.Windows.Forms.DockStyle.Top;
            this.TitleLabel.Location = new System.Drawing.Point(0, 30);
            this.TitleLabel.Name = "TitleLabel";
            this.TitleLabel.Size = new System.Drawing.Size(39, 13);
            this.TitleLabel.TabIndex = 21;
            this.TitleLabel.Text = "Label2";
            // 
            // ConstantsLabel
            // 
            this.ConstantsLabel.BackColor = System.Drawing.SystemColors.ActiveCaption;
            this.ConstantsLabel.Dock = System.Windows.Forms.DockStyle.Top;
            this.ConstantsLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.ConstantsLabel.ForeColor = System.Drawing.SystemColors.ActiveCaptionText;
            this.ConstantsLabel.Location = new System.Drawing.Point(0, 0);
            this.ConstantsLabel.Name = "ConstantsLabel";
            this.ConstantsLabel.Size = new System.Drawing.Size(753, 30);
            this.ConstantsLabel.TabIndex = 20;
            this.ConstantsLabel.Text = "Constants to put in top of output file:";
            this.ConstantsLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // GridContextMenu
            // 
            this.GridContextMenu.Items.AddRange(new System.Windows.Forms.ToolStripItem[] {
            this.MoveUpMenuItem,
            this.MoveDownMenuItem,
            this.DeleteVariablesMenuItem});
            this.GridContextMenu.Name = "ContextMenu";
            this.GridContextMenu.Size = new System.Drawing.Size(252, 70);
            // 
            // MoveUpMenuItem
            // 
            this.MoveUpMenuItem.Name = "MoveUpMenuItem";
            this.MoveUpMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.Up)));
            this.MoveUpMenuItem.Size = new System.Drawing.Size(251, 22);
            this.MoveUpMenuItem.Text = "Move variables &up";
            // 
            // MoveDownMenuItem
            // 
            this.MoveDownMenuItem.Name = "MoveDownMenuItem";
            this.MoveDownMenuItem.ShortcutKeys = ((System.Windows.Forms.Keys)((System.Windows.Forms.Keys.Control | System.Windows.Forms.Keys.Down)));
            this.MoveDownMenuItem.Size = new System.Drawing.Size(251, 22);
            this.MoveDownMenuItem.Text = "Move variables &down";
            // 
            // DeleteVariablesMenuItem
            // 
            this.DeleteVariablesMenuItem.Name = "DeleteVariablesMenuItem";
            this.DeleteVariablesMenuItem.Size = new System.Drawing.Size(251, 22);
            this.DeleteVariablesMenuItem.Text = "Delete variables";
            // 
            // BottomPanel
            // 
            this.BottomPanel.Controls.Add(this.GridPanel);
            this.BottomPanel.Controls.Add(this.Splitter2);
            this.BottomPanel.Controls.Add(this.FilterPanel);
            this.BottomPanel.Dock = System.Windows.Forms.DockStyle.Fill;
            this.BottomPanel.Location = new System.Drawing.Point(0, 105);
            this.BottomPanel.Name = "BottomPanel";
            this.BottomPanel.Size = new System.Drawing.Size(753, 419);
            this.BottomPanel.TabIndex = 28;
            // 
            // OutputFileDescUI
            // 
            this.Controls.Add(this.BottomPanel);
            this.Controls.Add(this.Splitter1);
            this.Controls.Add(this.ConstantsPanel);
            this.Name = "OutputFileDescUI";
            this.Size = new System.Drawing.Size(753, 524);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.ConstantsPanel, 0);
            this.Controls.SetChildIndex(this.Splitter1, 0);
            this.Controls.SetChildIndex(this.BottomPanel, 0);
            this.GridPanel.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
            this.HelpPanel.ResumeLayout(false);
            this.HelpPanel.PerformLayout();
            this.FilterPanel.ResumeLayout(false);
            this.FilterPanel.PerformLayout();
            this.ConstantsPanel.ResumeLayout(false);
            this.ConstantsPanel.PerformLayout();
            this.GridContextMenu.ResumeLayout(false);
            this.BottomPanel.ResumeLayout(false);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TextBox ConstantsBox;
        private System.Windows.Forms.Splitter Splitter1;
        private System.Windows.Forms.Splitter Splitter2;
        private System.Windows.Forms.Panel ConstantsPanel;
        private System.Windows.Forms.Label GridLabel;
        private System.Windows.Forms.Label ConstantsLabel;
        private System.Windows.Forms.Label DictionaryLabel;
        private System.Windows.Forms.Button HelpButton;
        private System.Windows.Forms.ToolStripMenuItem DeleteVariablesMenuItem;
        private UIBits.EnhancedGrid Grid;
        private System.Windows.Forms.Panel HelpPanel;
        private System.Windows.Forms.Panel BottomPanel;
        private System.Windows.Forms.Label TitleLabel;
        private System.Windows.Forms.ColumnHeader ColumnHeader2;
		private System.Windows.Forms.OpenFileDialog OpenFileDialog;
		private System.Windows.Forms.Panel GridPanel;
		private System.Windows.Forms.ContextMenuStrip GridContextMenu;
		private System.Windows.Forms.ToolStripMenuItem MoveUpMenuItem;
		private System.Windows.Forms.ToolStripMenuItem MoveDownMenuItem;
		private System.Windows.Forms.Panel FilterPanel;
		private System.Windows.Forms.ListView VariableListView;
		private System.Windows.Forms.ColumnHeader ColumnHeader1;
		private System.Windows.Forms.ColumnHeader ColumnHeader4;
		private System.Windows.Forms.ColumnHeader ColumnHeader3;
		private System.Windows.Forms.Label Label1;
		private System.Windows.Forms.ComboBox ComponentFilter;
        private System.Windows.Forms.TextBox textBoxSearch;
    }
}
