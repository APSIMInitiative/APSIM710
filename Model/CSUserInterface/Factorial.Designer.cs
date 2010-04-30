namespace CSUserInterface
{
    partial class Factorial
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
            FarPoint.Win.Spread.CellType.CheckBoxCellType checkBoxCellType1 = new FarPoint.Win.Spread.CellType.CheckBoxCellType();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType1 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType2 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            FarPoint.Win.Spread.CellType.CheckBoxCellType checkBoxCellType2 = new FarPoint.Win.Spread.CellType.CheckBoxCellType();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType3 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType4 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            FarPoint.Win.Spread.CellType.CheckBoxCellType checkBoxCellType3 = new FarPoint.Win.Spread.CellType.CheckBoxCellType();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType5 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType6 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            FarPoint.Win.Spread.CellType.CheckBoxCellType checkBoxCellType4 = new FarPoint.Win.Spread.CellType.CheckBoxCellType();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType7 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType8 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            FarPoint.Win.Spread.CellType.CheckBoxCellType checkBoxCellType5 = new FarPoint.Win.Spread.CellType.CheckBoxCellType();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType9 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType10 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            FarPoint.Win.Spread.CellType.CheckBoxCellType checkBoxCellType6 = new FarPoint.Win.Spread.CellType.CheckBoxCellType();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType11 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            FarPoint.Win.Spread.CellType.ComboBoxCellType comboBoxCellType12 = new FarPoint.Win.Spread.CellType.ComboBoxCellType();
            this.fpSpread = new FarPoint.Win.Spread.FpSpread();
            this.fpSpread_Sheet1 = new FarPoint.Win.Spread.SheetView();
            this.fpSpread_Sheet2 = new FarPoint.Win.Spread.SheetView();
            this.button1 = new System.Windows.Forms.Button();
            this.button2 = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            ((System.ComponentModel.ISupportInitialize)(this.fpSpread)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.fpSpread_Sheet1)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.fpSpread_Sheet2)).BeginInit();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Text = "Factorial";
            // 
            // fpSpread
            // 
            this.fpSpread.AccessibleDescription = "fpSpread, Sheet2, Row 0, Column 0, ";
            this.fpSpread.AllowCellOverflow = true;
            this.fpSpread.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.fpSpread.BackColor = System.Drawing.SystemColors.Window;
            this.fpSpread.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.fpSpread.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
            this.fpSpread.Location = new System.Drawing.Point(3, 70);
            this.fpSpread.Name = "fpSpread";
            this.fpSpread.Sheets.AddRange(new FarPoint.Win.Spread.SheetView[] {
            this.fpSpread_Sheet1,
            this.fpSpread_Sheet2});
            this.fpSpread.Size = new System.Drawing.Size(649, 468);
            this.fpSpread.TabIndex = 2;
            tipAppearance1.BackColor = System.Drawing.SystemColors.Info;
            tipAppearance1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            tipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText;
            this.fpSpread.TextTipAppearance = tipAppearance1;
            this.fpSpread.VerticalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
            this.fpSpread.EditChange += new FarPoint.Win.Spread.EditorNotifyEventHandler(this.fpSpread_EditChange);
            this.fpSpread.ActiveSheetIndex = 1;
            // 
            // fpSpread_Sheet1
            // 
            this.fpSpread_Sheet1.Reset();
            // Formulas and custom names must be loaded with R1C1 reference style
            this.fpSpread_Sheet1.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
            this.fpSpread_Sheet1.ColumnCount = 5;
            this.fpSpread_Sheet1.RowCount = 20;
            this.fpSpread_Sheet1.RowHeader.ColumnCount = 0;
            this.fpSpread_Sheet1.AutoUpdateNotes = true;
            this.fpSpread_Sheet1.Cells.Get(0, 0).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(0, 0).ColumnSpan = 5;
            this.fpSpread_Sheet1.Cells.Get(0, 0).Value = "Common";
            this.fpSpread_Sheet1.Cells.Get(0, 1).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(0, 2).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(0, 3).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(0, 4).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(1, 0).TabStop = true;
            this.fpSpread_Sheet1.Cells.Get(1, 1).CellType = checkBoxCellType1;
            this.fpSpread_Sheet1.Cells.Get(1, 1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Center;
            this.fpSpread_Sheet1.Cells.Get(1, 1).Value = true;
            comboBoxCellType1.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType1.Items = new string[] {
        "Sorghum Sowing Rule",
        "Soil"};
            this.fpSpread_Sheet1.Cells.Get(1, 2).CellType = comboBoxCellType1;
            this.fpSpread_Sheet1.Cells.Get(1, 2).Value = "Sorghum Sowing Rule";
            comboBoxCellType2.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType2.Items = new string[] {
        "skiprow",
        "density"};
            this.fpSpread_Sheet1.Cells.Get(1, 3).CellType = comboBoxCellType2;
            this.fpSpread_Sheet1.Cells.Get(1, 3).Value = "skiprow";
            this.fpSpread_Sheet1.Cells.Get(1, 4).Value = "solid, single, double";
            this.fpSpread_Sheet1.Cells.Get(2, 1).CellType = checkBoxCellType2;
            this.fpSpread_Sheet1.Cells.Get(2, 1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Center;
            this.fpSpread_Sheet1.Cells.Get(2, 1).Value = true;
            comboBoxCellType3.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType3.Items = new string[] {
        "Sorghum Sowing Rule",
        "Soil"};
            this.fpSpread_Sheet1.Cells.Get(2, 2).CellType = comboBoxCellType3;
            this.fpSpread_Sheet1.Cells.Get(2, 2).Value = "Sorghum Sowing Rule";
            comboBoxCellType4.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType4.Items = new string[] {
        "skiprow",
        "density"};
            this.fpSpread_Sheet1.Cells.Get(2, 3).CellType = comboBoxCellType4;
            this.fpSpread_Sheet1.Cells.Get(2, 3).Value = "density";
            this.fpSpread_Sheet1.Cells.Get(2, 4).Value = "3.5, 5, 7.5";
            this.fpSpread_Sheet1.Cells.Get(4, 0).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(4, 0).ColumnSpan = 5;
            this.fpSpread_Sheet1.Cells.Get(4, 0).Value = "Group 1";
            this.fpSpread_Sheet1.Cells.Get(4, 1).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(4, 2).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(4, 3).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(4, 4).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(5, 1).BackColor = System.Drawing.Color.AliceBlue;
            this.fpSpread_Sheet1.Cells.Get(5, 1).ColumnSpan = 4;
            this.fpSpread_Sheet1.Cells.Get(5, 1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Left;
            this.fpSpread_Sheet1.Cells.Get(5, 1).Value = "SubGroup";
            this.fpSpread_Sheet1.Cells.Get(5, 2).BackColor = System.Drawing.Color.AliceBlue;
            this.fpSpread_Sheet1.Cells.Get(5, 2).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Left;
            this.fpSpread_Sheet1.Cells.Get(5, 3).BackColor = System.Drawing.Color.AliceBlue;
            this.fpSpread_Sheet1.Cells.Get(5, 3).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Left;
            this.fpSpread_Sheet1.Cells.Get(5, 4).BackColor = System.Drawing.Color.AliceBlue;
            this.fpSpread_Sheet1.Cells.Get(5, 4).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Left;
            this.fpSpread_Sheet1.Cells.Get(6, 1).CellType = checkBoxCellType3;
            this.fpSpread_Sheet1.Cells.Get(6, 1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Center;
            this.fpSpread_Sheet1.Cells.Get(6, 1).Value = true;
            comboBoxCellType5.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType5.Items = new string[] {
        "Sorghum Sowing Rule",
        "Soil"};
            this.fpSpread_Sheet1.Cells.Get(6, 2).CellType = comboBoxCellType5;
            this.fpSpread_Sheet1.Cells.Get(6, 2).Value = "Sorghum Sowing Rule";
            comboBoxCellType6.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType6.Items = new string[] {
        "sowdate"};
            this.fpSpread_Sheet1.Cells.Get(6, 3).CellType = comboBoxCellType6;
            this.fpSpread_Sheet1.Cells.Get(6, 3).Value = "sowdate";
            this.fpSpread_Sheet1.Cells.Get(6, 4).Value = "15-Oct, 15-Nov, 15-Dec, 15-Jan, 15-Feb";
            this.fpSpread_Sheet1.Cells.Get(8, 1).BackColor = System.Drawing.Color.AliceBlue;
            this.fpSpread_Sheet1.Cells.Get(8, 1).ColumnSpan = 4;
            this.fpSpread_Sheet1.Cells.Get(8, 1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Left;
            this.fpSpread_Sheet1.Cells.Get(8, 1).Value = "SubGroup";
            this.fpSpread_Sheet1.Cells.Get(8, 2).BackColor = System.Drawing.Color.AliceBlue;
            this.fpSpread_Sheet1.Cells.Get(8, 3).BackColor = System.Drawing.Color.AliceBlue;
            this.fpSpread_Sheet1.Cells.Get(8, 4).BackColor = System.Drawing.Color.AliceBlue;
            this.fpSpread_Sheet1.Cells.Get(9, 1).CellType = checkBoxCellType4;
            this.fpSpread_Sheet1.Cells.Get(9, 1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Center;
            this.fpSpread_Sheet1.Cells.Get(9, 1).Value = true;
            comboBoxCellType7.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType7.Items = new string[] {
        "Sorghum Sowing Rule",
        "Soil"};
            this.fpSpread_Sheet1.Cells.Get(9, 2).CellType = comboBoxCellType7;
            this.fpSpread_Sheet1.Cells.Get(9, 2).Value = "Sorghum Sowing Rule";
            comboBoxCellType8.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType8.Items = new string[] {
        "sowdate"};
            this.fpSpread_Sheet1.Cells.Get(9, 3).CellType = comboBoxCellType8;
            this.fpSpread_Sheet1.Cells.Get(9, 3).Value = "sowdate";
            this.fpSpread_Sheet1.Cells.Get(9, 4).Value = "15-Sep, 15-Oct, 15-Nov, 15-Dec, 15-Jan";
            this.fpSpread_Sheet1.Cells.Get(11, 0).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(11, 0).ColumnSpan = 5;
            this.fpSpread_Sheet1.Cells.Get(11, 0).Value = "Group 2";
            this.fpSpread_Sheet1.Cells.Get(11, 2).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(11, 3).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(11, 4).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(12, 1).CellType = checkBoxCellType5;
            this.fpSpread_Sheet1.Cells.Get(12, 1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Center;
            this.fpSpread_Sheet1.Cells.Get(12, 1).Value = true;
            comboBoxCellType9.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType9.Items = new string[] {
        "Sorghum Sowing Rule",
        "Soil"};
            this.fpSpread_Sheet1.Cells.Get(12, 2).CellType = comboBoxCellType9;
            this.fpSpread_Sheet1.Cells.Get(12, 2).Value = "Sorghum Sowing Rule";
            comboBoxCellType10.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType10.Items = new string[] {
        "sowdate"};
            this.fpSpread_Sheet1.Cells.Get(12, 3).CellType = comboBoxCellType10;
            this.fpSpread_Sheet1.Cells.Get(12, 3).Value = "sowdate";
            this.fpSpread_Sheet1.Cells.Get(12, 4).Value = "15-Sep, 15-Oct, 15-Nov, 15-Dec, 15-Jan";
            this.fpSpread_Sheet1.Cells.Get(14, 0).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(14, 0).ColumnSpan = 5;
            this.fpSpread_Sheet1.Cells.Get(14, 0).Value = "Group 3";
            this.fpSpread_Sheet1.Cells.Get(14, 2).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(14, 3).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(14, 4).BackColor = System.Drawing.Color.LightSteelBlue;
            this.fpSpread_Sheet1.Cells.Get(15, 1).CellType = checkBoxCellType6;
            this.fpSpread_Sheet1.Cells.Get(15, 1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Center;
            this.fpSpread_Sheet1.Cells.Get(15, 1).Value = true;
            comboBoxCellType11.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType11.Items = new string[] {
        "Sorghum Sowing Rule",
        "Soil"};
            this.fpSpread_Sheet1.Cells.Get(15, 2).CellType = comboBoxCellType11;
            this.fpSpread_Sheet1.Cells.Get(15, 2).Value = "Sorghum Sowing Rule";
            comboBoxCellType12.ButtonAlign = FarPoint.Win.ButtonAlign.Right;
            comboBoxCellType12.Items = new string[] {
        "sowdate"};
            this.fpSpread_Sheet1.Cells.Get(15, 3).CellType = comboBoxCellType12;
            this.fpSpread_Sheet1.Cells.Get(15, 3).Value = "sowdate";
            this.fpSpread_Sheet1.Cells.Get(15, 4).Value = "15-Sep, 15-Oct, 15-Nov, 15-Dec, 15-Jan";
            this.fpSpread_Sheet1.ColumnHeader.AutoText = FarPoint.Win.Spread.HeaderAutoText.Blank;
            this.fpSpread_Sheet1.ColumnHeader.Cells.Get(0, 1).Value = "Active";
            this.fpSpread_Sheet1.ColumnHeader.Cells.Get(0, 2).Value = "Target";
            this.fpSpread_Sheet1.ColumnHeader.Cells.Get(0, 3).Value = "Variable";
            this.fpSpread_Sheet1.ColumnHeader.Cells.Get(0, 4).Value = "Parameters";
            this.fpSpread_Sheet1.Columns.Get(0).Width = 23F;
            this.fpSpread_Sheet1.Columns.Get(1).HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Center;
            this.fpSpread_Sheet1.Columns.Get(1).Label = "Active";
            this.fpSpread_Sheet1.Columns.Get(1).Width = 37F;
            this.fpSpread_Sheet1.Columns.Get(2).Label = "Target";
            this.fpSpread_Sheet1.Columns.Get(2).Width = 142F;
            this.fpSpread_Sheet1.Columns.Get(3).Label = "Variable";
            this.fpSpread_Sheet1.Columns.Get(3).Width = 73F;
            this.fpSpread_Sheet1.Columns.Get(4).Label = "Parameters";
            this.fpSpread_Sheet1.Columns.Get(4).Width = 196F;
            this.fpSpread_Sheet1.DefaultStyle.Locked = false;
            this.fpSpread_Sheet1.DefaultStyle.Parent = "DataAreaDefault";
            this.fpSpread_Sheet1.DefaultStyle.VerticalAlignment = FarPoint.Win.Spread.CellVerticalAlignment.Center;
            this.fpSpread_Sheet1.RowHeader.Columns.Default.Resizable = false;
            this.fpSpread_Sheet1.Rows.Get(0).BackColor = System.Drawing.Color.White;
            this.fpSpread_Sheet1.Rows.Get(2).BackColor = System.Drawing.Color.White;
            this.fpSpread_Sheet1.Rows.Get(7).BackColor = System.Drawing.Color.White;
            this.fpSpread_Sheet1.SheetName = "Sheet1";
            this.fpSpread_Sheet1.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
            // 
            // fpSpread_Sheet2
            // 
            this.fpSpread_Sheet2.Reset();
            // Formulas and custom names must be loaded with R1C1 reference style
            this.fpSpread_Sheet2.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
            this.fpSpread_Sheet2.AutoUpdateNotes = true;
            this.fpSpread_Sheet2.DefaultStyle.HorizontalAlignment = FarPoint.Win.Spread.CellHorizontalAlignment.Left;
            this.fpSpread_Sheet2.DefaultStyle.Locked = false;
            this.fpSpread_Sheet2.DefaultStyle.Parent = "DataAreaDefault";
            this.fpSpread_Sheet2.DefaultStyle.VerticalAlignment = FarPoint.Win.Spread.CellVerticalAlignment.Center;
            this.fpSpread_Sheet2.GrayAreaBackColor = System.Drawing.SystemColors.Window;
            this.fpSpread_Sheet2.RowHeader.Columns.Default.Resizable = false;
            this.fpSpread_Sheet2.SheetName = "Sheet2";
            this.fpSpread_Sheet2.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(468, 27);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(86, 23);
            this.button1.TabIndex = 3;
            this.button1.Text = "Add";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Visible = false;
            // 
            // button2
            // 
            this.button2.Location = new System.Drawing.Point(3, 33);
            this.button2.Name = "button2";
            this.button2.Size = new System.Drawing.Size(75, 23);
            this.button2.TabIndex = 4;
            this.button2.Text = "Remove";
            this.button2.UseVisualStyleBackColor = true;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(85, 28);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(318, 13);
            this.label1.TabIndex = 5;
            this.label1.Text = "Add a variable by clicking on the drop down in the Target Column.";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(85, 47);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(370, 13);
            this.label2.TabIndex = 6;
            this.label2.Text = "Remove a variable by selecting the variable and clicking the Remove button.";
            // 
            // Factorial
            // 
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.button2);
            this.Controls.Add(this.fpSpread);
            this.Controls.Add(this.button1);
            this.HelpText = "Factorial";
            this.Name = "Factorial";
            this.Controls.SetChildIndex(this.button1, 0);
            this.Controls.SetChildIndex(this.fpSpread, 0);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.button2, 0);
            this.Controls.SetChildIndex(this.label1, 0);
            this.Controls.SetChildIndex(this.label2, 0);
            ((System.ComponentModel.ISupportInitialize)(this.fpSpread)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.fpSpread_Sheet1)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.fpSpread_Sheet2)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private FarPoint.Win.Spread.FpSpread fpSpread1;
        private FarPoint.Win.Spread.SheetView fpSpread_Sheet1;
        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.Button button2;
        private FarPoint.Win.Spread.FpSpread fpSpread;
        private FarPoint.Win.Spread.SheetView fpSpread_Sheet2;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
    }
}
