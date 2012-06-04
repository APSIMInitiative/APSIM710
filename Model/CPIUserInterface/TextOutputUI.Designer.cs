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
            this.Grid = new System.Windows.Forms.DataGridView();
            this.Variable = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Column1 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.DictionaryLabel = new System.Windows.Forms.Label();
            this.Label1 = new System.Windows.Forms.Label();
            this.VariableListView = new System.Windows.Forms.ListView();
            this.ColumnHeader1 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.ColumnHeader4 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.ColumnHeader3 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
            this.ComponentFilter = new System.Windows.Forms.ComboBox();
            this.tabPage2 = new System.Windows.Forms.TabPage();
            this.FileContentsBox = new System.Windows.Forms.RichTextBox();
            this.panel1 = new System.Windows.Forms.Panel();
            this.label2 = new System.Windows.Forms.Label();
            this.tabControl1.SuspendLayout();
            this.tabPage1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.splitContainer1)).BeginInit();
            this.splitContainer1.Panel1.SuspendLayout();
            this.splitContainer1.Panel2.SuspendLayout();
            this.splitContainer1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
            this.tabPage2.SuspendLayout();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Size = new System.Drawing.Size(843, 16);
            // 
            // tabControl1
            // 
            this.tabControl1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.tabControl1.Controls.Add(this.tabPage1);
            this.tabControl1.Controls.Add(this.tabPage2);
            this.tabControl1.Location = new System.Drawing.Point(0, 19);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(843, 332);
            this.tabControl1.TabIndex = 21;
            this.tabControl1.Selected += new System.Windows.Forms.TabControlEventHandler(this.tabControl1_Selected);
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.splitContainer1);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(835, 306);
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
            this.splitContainer1.Panel1.Controls.Add(this.Grid);
            // 
            // splitContainer1.Panel2
            // 
            this.splitContainer1.Panel2.Controls.Add(this.DictionaryLabel);
            this.splitContainer1.Panel2.Controls.Add(this.Label1);
            this.splitContainer1.Panel2.Controls.Add(this.VariableListView);
            this.splitContainer1.Panel2.Controls.Add(this.ComponentFilter);
            this.splitContainer1.Size = new System.Drawing.Size(829, 300);
            this.splitContainer1.SplitterDistance = 481;
            this.splitContainer1.TabIndex = 21;
            // 
            // Grid
            // 
            this.Grid.AllowDrop = true;
            this.Grid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.Grid.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.Variable,
            this.Column1});
            this.Grid.Dock = System.Windows.Forms.DockStyle.Fill;
            this.Grid.Location = new System.Drawing.Point(0, 0);
            this.Grid.MultiSelect = false;
            this.Grid.Name = "Grid";
            this.Grid.RowHeadersVisible = false;
            this.Grid.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect;
            this.Grid.Size = new System.Drawing.Size(481, 300);
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
            // 
            // Column1
            // 
            this.Column1.HeaderText = "Column1";
            this.Column1.Name = "Column1";
            // 
            // DictionaryLabel
            // 
            this.DictionaryLabel.BackColor = System.Drawing.SystemColors.ActiveCaption;
            this.DictionaryLabel.Dock = System.Windows.Forms.DockStyle.Top;
            this.DictionaryLabel.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.DictionaryLabel.ForeColor = System.Drawing.SystemColors.ActiveCaptionText;
            this.DictionaryLabel.Location = new System.Drawing.Point(0, 0);
            this.DictionaryLabel.Name = "DictionaryLabel";
            this.DictionaryLabel.Size = new System.Drawing.Size(344, 36);
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
            // VariableListView
            // 
            this.VariableListView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
            | System.Windows.Forms.AnchorStyles.Left) 
            | System.Windows.Forms.AnchorStyles.Right)));
            this.VariableListView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.ColumnHeader1,
            this.ColumnHeader4,
            this.ColumnHeader3});
            this.VariableListView.FullRowSelect = true;
            this.VariableListView.Location = new System.Drawing.Point(0, 82);
            this.VariableListView.Name = "VariableListView";
            this.VariableListView.Size = new System.Drawing.Size(348, 210);
            this.VariableListView.Sorting = System.Windows.Forms.SortOrder.Ascending;
            this.VariableListView.TabIndex = 24;
            this.VariableListView.UseCompatibleStateImageBehavior = false;
            this.VariableListView.View = System.Windows.Forms.View.Details;
            this.VariableListView.ItemDrag += new System.Windows.Forms.ItemDragEventHandler(this.VariableListView_ItemDrag);
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
            // ColumnHeader3
            // 
            this.ColumnHeader3.Text = "Description";
            this.ColumnHeader3.Width = 437;
            // 
            // ComponentFilter
            // 
            this.ComponentFilter.FormattingEnabled = true;
            this.ComponentFilter.Location = new System.Drawing.Point(95, 44);
            this.ComponentFilter.Name = "ComponentFilter";
            this.ComponentFilter.Size = new System.Drawing.Size(155, 21);
            this.ComponentFilter.TabIndex = 23;
            this.ComponentFilter.TextChanged += new System.EventHandler(this.ComponentFilter_TextChanged);
            // 
            // tabPage2
            // 
            this.tabPage2.Controls.Add(this.FileContentsBox);
            this.tabPage2.Controls.Add(this.panel1);
            this.tabPage2.Location = new System.Drawing.Point(4, 22);
            this.tabPage2.Name = "tabPage2";
            this.tabPage2.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage2.Size = new System.Drawing.Size(835, 306);
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
            this.FileContentsBox.Size = new System.Drawing.Size(829, 277);
            this.FileContentsBox.TabIndex = 4;
            this.FileContentsBox.Text = "";
            this.FileContentsBox.WordWrap = false;
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.label2);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Top;
            this.panel1.Location = new System.Drawing.Point(3, 3);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(829, 23);
            this.panel1.TabIndex = 0;
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
            // TextOutUI
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.tabControl1);
            this.Name = "TextOutUI";
            this.Size = new System.Drawing.Size(843, 351);
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
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
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
        internal System.Windows.Forms.ListView VariableListView;
        internal System.Windows.Forms.ColumnHeader ColumnHeader1;
        internal System.Windows.Forms.ColumnHeader ColumnHeader4;
        internal System.Windows.Forms.ColumnHeader ColumnHeader3;
        internal System.Windows.Forms.ComboBox ComponentFilter;
        private System.Windows.Forms.TabPage tabPage2;
        private System.Windows.Forms.DataGridView Grid;
        private System.Windows.Forms.DataGridViewTextBoxColumn Variable;
        private System.Windows.Forms.DataGridViewTextBoxColumn Column1;
        internal System.Windows.Forms.RichTextBox FileContentsBox;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Label label2;


    }
}
