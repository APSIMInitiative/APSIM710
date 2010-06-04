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
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle4 = new System.Windows.Forms.DataGridViewCellStyle();
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle5 = new System.Windows.Forms.DataGridViewCellStyle();
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle6 = new System.Windows.Forms.DataGridViewCellStyle();
            this.TopPanel = new System.Windows.Forms.Panel();
            this.Grid = new System.Windows.Forms.DataGridView();
            this.TotalPanel = new System.Windows.Forms.Panel();
            this.TotalGrid = new System.Windows.Forms.DataGridView();
            this.DummyScrollBar = new System.Windows.Forms.VScrollBar();
            this.Splitter = new System.Windows.Forms.Splitter();
            this.Properties = new VBUserInterface.GenericUI();
            this.splitter1 = new System.Windows.Forms.Splitter();
            this.Label = new System.Windows.Forms.Label();
            this.TopPanel.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
            this.TotalPanel.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.TotalGrid)).BeginInit();
            this.SuspendLayout();
            // 
            // TopPanel
            // 
            this.TopPanel.Controls.Add(this.Grid);
            this.TopPanel.Controls.Add(this.Label);
            this.TopPanel.Controls.Add(this.TotalPanel);
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
            this.Grid.AllowUserToResizeRows = false;
            this.Grid.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill;
            this.Grid.BackgroundColor = System.Drawing.SystemColors.Window;
            this.Grid.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.Grid.ClipboardCopyMode = System.Windows.Forms.DataGridViewClipboardCopyMode.EnableWithoutHeaderText;
            dataGridViewCellStyle4.Alignment = System.Windows.Forms.DataGridViewContentAlignment.TopRight;
            dataGridViewCellStyle4.BackColor = System.Drawing.SystemColors.Control;
            dataGridViewCellStyle4.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            dataGridViewCellStyle4.ForeColor = System.Drawing.SystemColors.WindowText;
            dataGridViewCellStyle4.SelectionBackColor = System.Drawing.SystemColors.Highlight;
            dataGridViewCellStyle4.SelectionForeColor = System.Drawing.SystemColors.HighlightText;
            dataGridViewCellStyle4.WrapMode = System.Windows.Forms.DataGridViewTriState.True;
            this.Grid.ColumnHeadersDefaultCellStyle = dataGridViewCellStyle4;
            this.Grid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            dataGridViewCellStyle5.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight;
            dataGridViewCellStyle5.BackColor = System.Drawing.SystemColors.Window;
            dataGridViewCellStyle5.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            dataGridViewCellStyle5.ForeColor = System.Drawing.SystemColors.ControlText;
            dataGridViewCellStyle5.Format = "N3";
            dataGridViewCellStyle5.NullValue = null;
            dataGridViewCellStyle5.SelectionBackColor = System.Drawing.SystemColors.Highlight;
            dataGridViewCellStyle5.SelectionForeColor = System.Drawing.SystemColors.HighlightText;
            dataGridViewCellStyle5.WrapMode = System.Windows.Forms.DataGridViewTriState.False;
            this.Grid.DefaultCellStyle = dataGridViewCellStyle5;
            this.Grid.Dock = System.Windows.Forms.DockStyle.Fill;
            this.Grid.Location = new System.Drawing.Point(270, 23);
            this.Grid.Name = "Grid";
            this.Grid.RowHeadersVisible = false;
            this.Grid.Size = new System.Drawing.Size(385, 251);
            this.Grid.TabIndex = 20;
            this.Grid.Scroll += new System.Windows.Forms.ScrollEventHandler(this.OnGridScroll);
            this.Grid.KeyDown += new System.Windows.Forms.KeyEventHandler(this.OnKeyDown);
            // 
            // TotalPanel
            // 
            this.TotalPanel.Controls.Add(this.TotalGrid);
            this.TotalPanel.Controls.Add(this.DummyScrollBar);
            this.TotalPanel.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.TotalPanel.Location = new System.Drawing.Point(270, 274);
            this.TotalPanel.Name = "TotalPanel";
            this.TotalPanel.Size = new System.Drawing.Size(385, 24);
            this.TotalPanel.TabIndex = 23;
            // 
            // TotalGrid
            // 
            this.TotalGrid.AllowUserToResizeColumns = false;
            this.TotalGrid.AllowUserToResizeRows = false;
            this.TotalGrid.BackgroundColor = System.Drawing.SystemColors.Window;
            this.TotalGrid.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.TotalGrid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
            this.TotalGrid.ColumnHeadersVisible = false;
            dataGridViewCellStyle6.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight;
            dataGridViewCellStyle6.BackColor = System.Drawing.SystemColors.Highlight;
            dataGridViewCellStyle6.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            dataGridViewCellStyle6.ForeColor = System.Drawing.SystemColors.HighlightText;
            dataGridViewCellStyle6.Format = "N3";
            dataGridViewCellStyle6.NullValue = null;
            dataGridViewCellStyle6.SelectionBackColor = System.Drawing.SystemColors.Highlight;
            dataGridViewCellStyle6.SelectionForeColor = System.Drawing.SystemColors.HighlightText;
            dataGridViewCellStyle6.WrapMode = System.Windows.Forms.DataGridViewTriState.False;
            this.TotalGrid.DefaultCellStyle = dataGridViewCellStyle6;
            this.TotalGrid.Location = new System.Drawing.Point(0, 0);
            this.TotalGrid.Name = "TotalGrid";
            this.TotalGrid.RowHeadersVisible = false;
            this.TotalGrid.ScrollBars = System.Windows.Forms.ScrollBars.None;
            this.TotalGrid.Size = new System.Drawing.Size(368, 24);
            this.TotalGrid.TabIndex = 23;
            // 
            // DummyScrollBar
            // 
            this.DummyScrollBar.Dock = System.Windows.Forms.DockStyle.Right;
            this.DummyScrollBar.Location = new System.Drawing.Point(368, 0);
            this.DummyScrollBar.Name = "DummyScrollBar";
            this.DummyScrollBar.Size = new System.Drawing.Size(17, 24);
            this.DummyScrollBar.TabIndex = 24;
            this.DummyScrollBar.Visible = false;
            // 
            // Splitter
            // 
            this.Splitter.Location = new System.Drawing.Point(267, 0);
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
            this.Properties.Size = new System.Drawing.Size(267, 298);
            this.Properties.TabIndex = 19;
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
            // Label
            // 
            this.Label.BackColor = System.Drawing.SystemColors.Highlight;
            this.Label.Dock = System.Windows.Forms.DockStyle.Top;
            this.Label.ForeColor = System.Drawing.SystemColors.HighlightText;
            this.Label.Location = new System.Drawing.Point(270, 0);
            this.Label.Name = "Label";
            this.Label.Size = new System.Drawing.Size(385, 23);
            this.Label.TabIndex = 24;
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
            this.TotalPanel.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.TotalGrid)).EndInit();
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.Panel TopPanel;
        private System.Windows.Forms.Splitter Splitter;
        private System.Windows.Forms.Splitter splitter1;
        private VBUserInterface.GenericUI Properties;
        private System.Windows.Forms.DataGridView Grid;
        private System.Windows.Forms.Panel TotalPanel;
        private System.Windows.Forms.DataGridView TotalGrid;
        private System.Windows.Forms.VScrollBar DummyScrollBar;
        private System.Windows.Forms.Label Label;

        }
    }
