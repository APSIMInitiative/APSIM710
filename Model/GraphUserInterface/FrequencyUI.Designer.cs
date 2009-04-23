namespace GraphUserInterface
    {
    partial class FrequencyUI
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
            FarPoint.Win.Spread.TipAppearance tipAppearance1 = new FarPoint.Win.Spread.TipAppearance();
            this.GroupBox = new System.Windows.Forms.GroupBox();
            this.Spread = new FarPoint.Win.Spread.FpSpread();
            this.Grid = new FarPoint.Win.Spread.SheetView();
            this.GroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.Spread)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
            this.SuspendLayout();
            // 
            // GroupBox
            // 
            this.GroupBox.Controls.Add(this.Spread);
            this.GroupBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GroupBox.Location = new System.Drawing.Point(0, 18);
            this.GroupBox.Name = "GroupBox";
            this.GroupBox.Size = new System.Drawing.Size(242, 132);
            this.GroupBox.TabIndex = 2;
            this.GroupBox.TabStop = false;
            this.GroupBox.Text = "GroupBox";
            // 
            // Spread
            // 
            this.Spread.AccessibleDescription = "Spread, Sheet1, Row 0, Column 0, ";
            this.Spread.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.Spread.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.Spread.ColumnSplitBoxPolicy = FarPoint.Win.Spread.SplitBoxPolicy.AsNeeded;
            this.Spread.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
            this.Spread.Location = new System.Drawing.Point(6, 19);
            this.Spread.Name = "Spread";
            this.Spread.RowSplitBoxPolicy = FarPoint.Win.Spread.SplitBoxPolicy.AsNeeded;
            this.Spread.Sheets.AddRange(new FarPoint.Win.Spread.SheetView[] {
            this.Grid});
            this.Spread.Size = new System.Drawing.Size(230, 107);
            this.Spread.TabIndex = 0;
            tipAppearance1.BackColor = System.Drawing.SystemColors.Info;
            tipAppearance1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            tipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText;
            this.Spread.TextTipAppearance = tipAppearance1;
            this.Spread.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
            // 
            // Grid
            // 
            this.Grid.Reset();
            // Formulas and custom names must be loaded with R1C1 reference style
            this.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
            this.Grid.ColumnCount = 2;
            this.Grid.RowCount = 10;
            this.Grid.RowHeader.ColumnCount = 0;
            this.Grid.AutoUpdateNotes = true;
            this.Grid.ColumnHeader.Cells.Get(0, 0).Value = "Label";
            this.Grid.ColumnHeader.Cells.Get(0, 1).Value = "Filter";
            this.Grid.Columns.Get(0).Label = "Label";
            this.Grid.Columns.Get(0).Width = 97F;
            this.Grid.Columns.Get(1).Label = "Filter";
            this.Grid.Columns.Get(1).Width = 110F;
            this.Grid.RowHeader.Columns.Default.Resizable = false;
            this.Grid.SheetName = "Sheet1";
            this.Grid.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
            // 
            // FrequencyUI
            // 
            this.Controls.Add(this.GroupBox);
            this.Name = "FrequencyUI";
            this.Size = new System.Drawing.Size(242, 150);
            this.Controls.SetChildIndex(this.GroupBox, 0);
            this.GroupBox.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.Spread)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.GroupBox GroupBox;
        private FarPoint.Win.Spread.FpSpread Spread;
        private FarPoint.Win.Spread.SheetView Grid;
        }
    }
