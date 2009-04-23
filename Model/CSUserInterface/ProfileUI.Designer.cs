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
            FarPoint.Win.Spread.TipAppearance tipAppearance1 = new FarPoint.Win.Spread.TipAppearance();
            this.Grid = new FarPoint.Win.Spread.FpSpread();
            this.SoilProfile = new FarPoint.Win.Spread.SheetView();
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.SoilProfile)).BeginInit();
            this.SuspendLayout();
            // 
            // Grid
            // 
            this.Grid.AccessibleDescription = "Grid, Soil profile, Row 0, Column 0, ";
            this.Grid.AllowDragDrop = true;
            this.Grid.Dock = System.Windows.Forms.DockStyle.Fill;
            this.Grid.EditModeReplace = true;
            this.Grid.HorizontalScrollBarPolicy = FarPoint.Win.Spread.ScrollBarPolicy.AsNeeded;
            this.Grid.Location = new System.Drawing.Point(0, 40);
            this.Grid.Name = "Grid";
            this.Grid.SelectionBlockOptions = ((FarPoint.Win.Spread.SelectionBlockOptions)(((FarPoint.Win.Spread.SelectionBlockOptions.Cells | FarPoint.Win.Spread.SelectionBlockOptions.Rows)
                        | FarPoint.Win.Spread.SelectionBlockOptions.Sheet)));
            this.Grid.Sheets.AddRange(new FarPoint.Win.Spread.SheetView[] {
            this.SoilProfile});
            this.Grid.Size = new System.Drawing.Size(655, 501);
            this.Grid.TabIndex = 13;
            this.Grid.TabStrip.ButtonPolicy = FarPoint.Win.Spread.TabStripButtonPolicy.AsNeeded;
            this.Grid.TabStripPolicy = FarPoint.Win.Spread.TabStripPolicy.Never;
            this.Grid.TabStripRatio = 0.512295081967213;
            tipAppearance1.BackColor = System.Drawing.SystemColors.Info;
            tipAppearance1.Font = new System.Drawing.Font("Microsoft Sans Serif", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            tipAppearance1.ForeColor = System.Drawing.SystemColors.InfoText;
            this.Grid.TextTipAppearance = tipAppearance1;
            this.Grid.TextTipPolicy = FarPoint.Win.Spread.TextTipPolicy.Floating;
            // 
            // SoilProfile
            // 
            this.SoilProfile.Reset();
            // Formulas and custom names must be loaded with R1C1 reference style
            this.SoilProfile.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.R1C1;
            this.SoilProfile.ColumnCount = 21;
            this.SoilProfile.ColumnHeader.RowCount = 2;
            this.SoilProfile.RowCount = 100;
            this.SoilProfile.AutoUpdateNotes = true;
            this.SoilProfile.ColumnHeader.AutoText = FarPoint.Win.Spread.HeaderAutoText.Blank;
            this.SoilProfile.ColumnHeader.Cells.Get(0, 0).Value = "Depth";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 1).Value = "Texture";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 2).Value = "SWCon";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 3).Value = "MWCon";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 4).Value = "FBiom";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 5).Value = "Finert";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 6).Value = "KS";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 7).Value = "OC";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 8).Value = "EC";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 9).Value = "pH";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 10).Value = "Cl";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 11).Value = "Boron";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 12).Value = "CEC";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 13).Value = "Ca";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 14).Value = "Mg";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 15).Value = "Na";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 16).Value = "K";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 17).Value = "ESP";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 18).Value = "Particle size";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 19).Value = "Particle size";
            this.SoilProfile.ColumnHeader.Cells.Get(0, 20).Value = "Particle size";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 0).Value = "(cm)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 2).Value = "(0-1)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 3).Value = "(0-1)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 4).Value = "(0-1)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 5).Value = "(0-1)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 6).Value = "(mm/day)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 7).Value = "(%)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 8).Value = "(mS/cm)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 9).Locked = false;
            this.SoilProfile.ColumnHeader.Cells.Get(1, 9).Value = "(water)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 10).Value = "(mg/kg)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 11).Value = "(mg/kg)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 12).Value = "(cmol+/kg)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 13).Value = "(cmol+/kg)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 14).Value = "(cmol+/kg)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 15).Value = "(cmol+/kg)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 16).Value = "(cmol+/kg)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 17).Value = "(%)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 18).Value = "sand (%)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 19).Value = "silt (%)";
            this.SoilProfile.ColumnHeader.Cells.Get(1, 20).Value = "clay (%)";
            this.SoilProfile.Columns.Get(0).Label = "(cm)";
            this.SoilProfile.Columns.Get(0).Locked = true;
            this.SoilProfile.Columns.Get(18).Label = "sand (%)";
            this.SoilProfile.Columns.Get(18).Width = 79F;
            this.SoilProfile.Columns.Get(19).Label = "silt (%)";
            this.SoilProfile.Columns.Get(19).Width = 79F;
            this.SoilProfile.Columns.Get(20).Label = "clay (%)";
            this.SoilProfile.Columns.Get(20).Width = 79F;
            this.SoilProfile.Protect = false;
            this.SoilProfile.RowHeader.Columns.Default.Resizable = false;
            this.SoilProfile.SheetName = "Soil profile";
            this.SoilProfile.ReferenceStyle = FarPoint.Win.Spread.Model.ReferenceStyle.A1;
            // 
            // ProfileUI
            // 
            this.Controls.Add(this.Grid);
            this.Name = "ProfileUI";
            this.Controls.SetChildIndex(this.Grid, 0);
            ((System.ComponentModel.ISupportInitialize)(this.Grid)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.SoilProfile)).EndInit();
            this.ResumeLayout(false);

            }

        #endregion

        private FarPoint.Win.Spread.FpSpread Grid;
        private FarPoint.Win.Spread.SheetView SoilProfile;
        }
    }
