namespace CPIUserInterface
{
    partial class TextOutputUI
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

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.splitContainer1 = new System.Windows.Forms.SplitContainer();
            this.panel2 = new System.Windows.Forms.Panel();
            this.comboBox1 = new System.Windows.Forms.ComboBox();
            this.textBox1 = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.Grid = new System.Windows.Forms.DataGridView();
            this.Variable = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Column1 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.afTreeViewColumns1 = new CPIUserInterface.AFTreeViewColumns();
            this.DictionaryLabel = new System.Windows.Forms.Label();
            this.Label1 = new System.Windows.Forms.Label();
            this.ComponentFilter = new System.Windows.Forms.ComboBox();
            this.tabPage3 = new System.Windows.Forms.TabPage();
            this.splitContainer2 = new System.Windows.Forms.SplitContainer();
            this.EventGrid = new System.Windows.Forms.DataGridView();
            this.dataGridViewTextBoxColumn1 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.dataGridViewTextBoxColumn2 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.label5 = new System.Windows.Forms.Label();
            this.label6 = new System.Windows.Forms.Label();
            this.EventsListView = new System.Windows.Forms.ListView();
            this.columnHeader5 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.columnHeader8 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.ComponentEventsFilter = new System.Windows.Forms.ComboBox();
            this.tabPage2 = new System.Windows.Forms.TabPage();
            this.FileContentsBox = new System.Windows.Forms.RichTextBox();
            this.panel1 = new System.Windows.Forms.Panel();
            this.labelLines = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.tabControl1.SuspendLayout();
            this.tabPage1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).BeginInit();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            this.panel2.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
            this.tabPage3.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer2)).BeginInit();
            this.splitContainer2.Panel1.SuspendLayout();
            this.splitContainer2.Panel2.SuspendLayout();
            this.splitContainer2.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.EventGrid)).BeginInit();
            this.tabPage2.SuspendLayout();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Size = new System.Drawing.Size(780, 16);
            // 
            // tabControl1
            // 
            this.tabControl1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.tabControl1.Controls.Add(this.tabPage1);
            this.tabControl1.Controls.Add(this.tabPage3);
            this.tabControl1.Controls.Add(this.tabPage2);
            this.tabControl1.Location = new System.Drawing.Point(0, 19);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(780, 358);
            this.tabControl1.TabIndex = 21;
            this.tabControl1.Selected += new System.Windows.Forms.TabControlEventHandler(this.tabControl1_Selected);
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.splitContainer1);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(772, 332);
            this.tabPage1.TabIndex = 0;
            this.tabPage1.Text = "Variables";
            this.tabPage1.UseVisualStyleBackColor = true;
            // 
            // splitContainer1
            // 
            this.splitContainer1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer1.Location = new System.Drawing.Point(3, 3);
            this.splitContainer1.Name = "splitContainer1";
            // 
            // splitContainer1.Panel1
            // 
            this.splitContainer1.Panel1.Controls.Add(this.panel2);
            this.splitContainer1.Panel1.Controls.Add(this.Grid);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.afTreeViewColumns1);
            this.splitContainer1.Panel2.Controls.Add(this.DictionaryLabel);
            this.splitContainer1.Panel2.Controls.Add(this.Label1);
            this.splitContainer1.Panel2.Controls.Add(this.ComponentFilter);
            this.splitContainer1.Size = new System.Drawing.Size(766, 326);
            this.splitContainer1.SplitterDistance = 451;
            this.splitContainer1.TabIndex = 21;
            // 
            // panel2
            // 
            this.panel2.Controls.Add(this.comboBox1);
            this.panel2.Controls.Add(this.textBox1);
            this.panel2.Controls.Add(this.label3);
            this.panel2.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.panel2.Location = new System.Drawing.Point(0, 293);
            this.panel2.Name = "panel2";
            this.panel2.Size = new System.Drawing.Size(451, 33);
            this.panel2.TabIndex = 1;
            // 
            // comboBox1
            // 
            this.comboBox1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBox1.FormattingEnabled = true;
            this.comboBox1.Items.AddRange(new object[] {
            "second",
            "minute",
            "hour",
            "day",
            "month",
            "year"});
            this.comboBox1.Location = new System.Drawing.Point(154, 9);
            this.comboBox1.Name = "comboBox1";
            this.comboBox1.Size = new System.Drawing.Size(67, 21);
            this.comboBox1.TabIndex = 2;
            // 
            // textBox1
            // 
            this.textBox1.Location = new System.Drawing.Point(114, 9);
            this.textBox1.Name = "textBox1";
            this.textBox1.Size = new System.Drawing.Size(29, 20);
            this.textBox1.TabIndex = 1;
            this.textBox1.Text = "1";
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(3, 12);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(104, 13);
            this.label3.TabIndex = 0;
            this.label3.Text = "Record results every";
            // 
            // Grid
            // 
            this.Grid.AllowDrop = true;
            this.Grid.AllowUserToResizeRows = false;
            this.Grid.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.Grid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.DisableResizing;
            this.Grid.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.Variable,
            this.Column1});
            this.Grid.Location = new System.Drawing.Point(0, 0);
            this.Grid.MultiSelect = false;
            this.Grid.Name = "Grid";
            this.Grid.RowHeadersVisible = false;
            this.Grid.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect;
            this.Grid.Size = new System.Drawing.Size(449, 292);
            this.Grid.TabIndex = 0;
            this.Grid.DragDrop += new System.Windows.Forms.DragEventHandler(this.Grid_DragDrop);
            this.Grid.DragEnter += new System.Windows.Forms.DragEventHandler(this.Grid_DragEnter);
            this.Grid.DragOver += new System.Windows.Forms.DragEventHandler(this.Grid_DragOver);
            this.Grid.KeyDown += new System.Windows.Forms.KeyEventHandler(this.Grid_KeyDown);
            // 
            // Variable
            // 
            this.Variable.HeaderText = "Name";
            this.Variable.Name = "Variable";
            this.Variable.Width = 60;
            // 
            // Column1
            // 
            this.Column1.HeaderText = "Column1";
            this.Column1.Name = "Column1";
            this.Column1.Width = 73;
            // 
            // afTreeViewColumns1
            // 
            this.afTreeViewColumns1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.afTreeViewColumns1.AutoScroll = true;
            this.afTreeViewColumns1.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(171)))), ((int)(((byte)(173)))), ((int)(((byte)(179)))));
            this.afTreeViewColumns1.Location = new System.Drawing.Point(0, 80);
            this.afTreeViewColumns1.Name = "afTreeViewColumns1";
            this.afTreeViewColumns1.Padding = new System.Windows.Forms.Padding(1);
            this.afTreeViewColumns1.Size = new System.Drawing.Size(308, 246);
            this.afTreeViewColumns1.TabIndex = 27;
            this.afTreeViewColumns1.saveChangesEvent += new CPIUserInterface.AFTreeViewColumns.onDataChange(this.afTreeViewColumns1_saveChangesEvent);
            this.afTreeViewColumns1.reloadTreeEvent += new CPIUserInterface.AFTreeViewColumns.reloadTree(this.afTreeViewColumns1_reloadTreeEvent);
            // 
            // DictionaryLabel
            // 
            this.DictionaryLabel.BackColor = System.Drawing.SystemColors.ActiveCaption;
            this.DictionaryLabel.Dock = System.Windows.Forms.DockStyle.Top;
            this.DictionaryLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.DictionaryLabel.ForeColor = System.Drawing.SystemColors.ActiveCaptionText;
            this.DictionaryLabel.Location = new System.Drawing.Point(0, 0);
            this.DictionaryLabel.Name = "DictionaryLabel";
            this.DictionaryLabel.Size = new System.Drawing.Size(311, 36);
            this.DictionaryLabel.TabIndex = 25;
            this.DictionaryLabel.Text = "Variables to drag onto grid:";
            this.DictionaryLabel.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // Label1
            // 
            this.Label1.AutoSize = true;
            this.Label1.Location = new System.Drawing.Point(3, 47);
            this.Label1.Name = "Label1";
            this.Label1.Size = new System.Drawing.Size(86, 13);
            this.Label1.TabIndex = 26;
            this.Label1.Text = "Component filter:";
            // 
            // ComponentFilter
            // 
            this.ComponentFilter.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.ComponentFilter.FormattingEnabled = true;
            this.ComponentFilter.Location = new System.Drawing.Point(95, 44);
            this.ComponentFilter.Name = "ComponentFilter";
            this.ComponentFilter.Size = new System.Drawing.Size(155, 21);
            this.ComponentFilter.TabIndex = 23;
            this.ComponentFilter.TextChanged += new System.EventHandler(this.ComponentFilter_TextChanged);
            // 
            // tabPage3
            // 
            this.tabPage3.Controls.Add(this.splitContainer2);
            this.tabPage3.Location = new System.Drawing.Point(4, 22);
            this.tabPage3.Name = "tabPage3";
            this.tabPage3.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage3.Size = new System.Drawing.Size(772, 332);
            this.tabPage3.TabIndex = 2;
            this.tabPage3.Text = "Reporting";
            this.tabPage3.UseVisualStyleBackColor = true;
            // 
            // splitContainer2
            // 
            this.splitContainer2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.splitContainer2.Location = new System.Drawing.Point(3, 3);
            this.splitContainer2.Name = "splitContainer2";
            // 
            // splitContainer2.Panel1
            // 
            this.splitContainer2.Panel1.Controls.Add(this.EventGrid);
            // 
            // splitContainer2.Panel2
            // 
            this.splitContainer2.Panel2.Controls.Add(this.label5);
            this.splitContainer2.Panel2.Controls.Add(this.label6);
            this.splitContainer2.Panel2.Controls.Add(this.EventsListView);
            this.splitContainer2.Panel2.Controls.Add(this.ComponentEventsFilter);
            this.splitContainer2.Size = new System.Drawing.Size(766, 326);
            this.splitContainer2.SplitterDistance = 451;
            this.splitContainer2.TabIndex = 22;
            // 
            // EventGrid
            // 
            this.EventGrid.AllowDrop = true;
            this.EventGrid.AllowUserToResizeRows = false;
            this.EventGrid.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill;
            this.EventGrid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.EventGrid.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.dataGridViewTextBoxColumn1,
            this.dataGridViewTextBoxColumn2});
            this.EventGrid.Dock = System.Windows.Forms.DockStyle.Fill;
            this.EventGrid.Location = new System.Drawing.Point(0, 0);
            this.EventGrid.MultiSelect = false;
            this.EventGrid.Name = "EventGrid";
            this.EventGrid.RowHeadersVisible = false;
            this.EventGrid.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect;
            this.EventGrid.Size = new System.Drawing.Size(451, 326);
            this.EventGrid.TabIndex = 1;
            this.EventGrid.DragDrop += new System.Windows.Forms.DragEventHandler(this.EventGrid_DragDrop);
            this.EventGrid.DragEnter += new System.Windows.Forms.DragEventHandler(this.EventGrid_DragEnter);
            this.EventGrid.DragOver += new System.Windows.Forms.DragEventHandler(this.EventGrid_DragOver);
            this.EventGrid.KeyDown += new System.Windows.Forms.KeyEventHandler(this.EventGrid_KeyDown);
            // 
            // dataGridViewTextBoxColumn1
            // 
            this.dataGridViewTextBoxColumn1.HeaderText = "Name";
            this.dataGridViewTextBoxColumn1.Name = "dataGridViewTextBoxColumn1";
            // 
            // dataGridViewTextBoxColumn2
            // 
            this.dataGridViewTextBoxColumn2.HeaderText = "Column1";
            this.dataGridViewTextBoxColumn2.Name = "dataGridViewTextBoxColumn2";
            // 
            // label5
            // 
            this.label5.BackColor = System.Drawing.SystemColors.ActiveCaption;
            this.label5.Dock = System.Windows.Forms.DockStyle.Top;
            this.label5.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label5.ForeColor = System.Drawing.SystemColors.ActiveCaptionText;
            this.label5.Location = new System.Drawing.Point(0, 0);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(311, 36);
            this.label5.TabIndex = 25;
            this.label5.Text = "Events to drag onto list:";
            this.label5.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(3, 47);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(86, 13);
            this.label6.TabIndex = 26;
            this.label6.Text = "Component filter:";
            // 
            // EventsListView
            // 
            this.EventsListView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.EventsListView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeader5,
            this.columnHeader8});
            this.EventsListView.FullRowSelect = true;
            this.EventsListView.Location = new System.Drawing.Point(0, 71);
            this.EventsListView.Name = "EventsListView";
            this.EventsListView.Size = new System.Drawing.Size(311, 253);
            this.EventsListView.Sorting = System.Windows.Forms.SortOrder.Ascending;
            this.EventsListView.TabIndex = 24;
            this.EventsListView.UseCompatibleStateImageBehavior = false;
            this.EventsListView.View = System.Windows.Forms.View.Details;
            this.EventsListView.ItemDrag += new System.Windows.Forms.ItemDragEventHandler(this.EventsListView_ItemDrag);
            // 
            // columnHeader5
            // 
            this.columnHeader5.Text = "Event name";
            this.columnHeader5.Width = 133;
            // 
            // columnHeader8
            // 
            this.columnHeader8.Text = "Description";
            this.columnHeader8.Width = 300;
            // 
            // ComponentEventsFilter
            // 
            this.ComponentEventsFilter.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.ComponentEventsFilter.FormattingEnabled = true;
            this.ComponentEventsFilter.Location = new System.Drawing.Point(95, 44);
            this.ComponentEventsFilter.Name = "ComponentEventsFilter";
            this.ComponentEventsFilter.Size = new System.Drawing.Size(155, 21);
            this.ComponentEventsFilter.TabIndex = 23;
            this.ComponentEventsFilter.SelectedIndexChanged += new System.EventHandler(this.ComponentEventsFilter_SelectedIndexChanged);
            // 
            // tabPage2
            // 
            this.tabPage2.Controls.Add(this.FileContentsBox);
            this.tabPage2.Controls.Add(this.panel1);
            this.tabPage2.Location = new System.Drawing.Point(4, 22);
            this.tabPage2.Name = "tabPage2";
            this.tabPage2.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage2.Size = new System.Drawing.Size(772, 332);
            this.tabPage2.TabIndex = 1;
            this.tabPage2.Text = "Output";
            this.tabPage2.UseVisualStyleBackColor = true;
            // 
            // FileContentsBox
            // 
            this.FileContentsBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.FileContentsBox.Font = new System.Drawing.Font("Courier New", 9F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.FileContentsBox.Location = new System.Drawing.Point(3, 26);
            this.FileContentsBox.Name = "FileContentsBox";
            this.FileContentsBox.ReadOnly = true;
            this.FileContentsBox.Size = new System.Drawing.Size(766, 303);
            this.FileContentsBox.TabIndex = 4;
            this.FileContentsBox.Text = "";
            this.FileContentsBox.WordWrap = false;
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.labelLines);
            this.panel1.Controls.Add(this.label2);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Top;
            this.panel1.Location = new System.Drawing.Point(3, 3);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(766, 23);
            this.panel1.TabIndex = 0;
            // 
            // labelLines
            // 
            this.labelLines.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.labelLines.AutoSize = true;
            this.labelLines.Location = new System.Drawing.Point(705, 5);
            this.labelLines.Name = "labelLines";
            this.labelLines.Size = new System.Drawing.Size(55, 13);
            this.labelLines.TabIndex = 1;
            this.labelLines.Text = "1234 lines";
            this.labelLines.TextAlign = System.Drawing.ContentAlignment.TopRight;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(3, 5);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(35, 13);
            this.label2.TabIndex = 0;
            this.label2.Text = "label2";
            // 
            // TextOutputUI
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.tabControl1);
            this.Name = "TextOutputUI";
            this.Size = new System.Drawing.Size(780, 377);
            this.Load += new System.EventHandler(this.TextOutUI_Load);
            this.Controls.SetChildIndex(this.tabControl1, 0);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.tabControl1.ResumeLayout(false);
            this.tabPage1.ResumeLayout(false);
            this.splitContainer1.Panel1.ResumeLayout(false);
            this.splitContainer1.Panel2.ResumeLayout(false);
            this.splitContainer1.Panel2.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).EndInit();
            this.splitContainer1.ResumeLayout(false);
            this.panel2.ResumeLayout(false);
            this.panel2.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
            this.tabPage3.ResumeLayout(false);
            this.splitContainer2.Panel1.ResumeLayout(false);
            this.splitContainer2.Panel2.ResumeLayout(false);
            this.splitContainer2.Panel2.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer2)).EndInit();
            this.splitContainer2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.EventGrid)).EndInit();
            this.tabPage2.ResumeLayout(false);
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tabPage1;
        private System.Windows.Forms.SplitContainer splitContainer1;
        internal System.Windows.Forms.Label DictionaryLabel;
        internal System.Windows.Forms.Label Label1;
        internal System.Windows.Forms.ComboBox ComponentFilter;
        private System.Windows.Forms.TabPage tabPage2;
        private System.Windows.Forms.DataGridView Grid;
        private System.Windows.Forms.DataGridViewTextBoxColumn Variable;
        private System.Windows.Forms.DataGridViewTextBoxColumn Column1;
        internal System.Windows.Forms.RichTextBox FileContentsBox;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.Panel panel2;
        private System.Windows.Forms.ComboBox comboBox1;
        private System.Windows.Forms.TextBox textBox1;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.TabPage tabPage3;
        private System.Windows.Forms.SplitContainer splitContainer2;
        internal System.Windows.Forms.Label label5;
        internal System.Windows.Forms.Label label6;
        internal System.Windows.Forms.ListView EventsListView;
        internal System.Windows.Forms.ColumnHeader columnHeader5;
        internal System.Windows.Forms.ColumnHeader columnHeader8;
        internal System.Windows.Forms.ComboBox ComponentEventsFilter;
        private System.Windows.Forms.DataGridView EventGrid;
        private System.Windows.Forms.DataGridViewTextBoxColumn dataGridViewTextBoxColumn1;
        private System.Windows.Forms.DataGridViewTextBoxColumn dataGridViewTextBoxColumn2;
        private System.Windows.Forms.Label labelLines;
        private AFTreeViewColumns afTreeViewColumns1;


    }
}
