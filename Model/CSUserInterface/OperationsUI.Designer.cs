namespace CSUserInterface
{
	partial class OperationsUI
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
			if (disposing && components != null) {
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
			this.TabControl = new System.Windows.Forms.TabControl();
			this.TabPage1 = new System.Windows.Forms.TabPage();
			this.TabPage2 = new System.Windows.Forms.TabPage();
			this.StartOfDayGrid = new UIBits.EnhancedGrid();
			this.EndOfDayGrid = new UIBits.EnhancedGrid();
			this.TabControl.SuspendLayout();
			this.TabPage1.SuspendLayout();
			this.TabPage2.SuspendLayout();
			((System.ComponentModel.ISupportInitialize)this.StartOfDayGrid).BeginInit();
			((System.ComponentModel.ISupportInitialize)this.EndOfDayGrid).BeginInit();
			this.SuspendLayout();
			//
			//MyHelpLabel
			//
			this.MyHelpLabel.Size = new System.Drawing.Size(843, 16);
			//
			//TabControl
			//
			this.TabControl.Controls.Add(this.TabPage1);
			this.TabControl.Controls.Add(this.TabPage2);
			this.TabControl.Dock = System.Windows.Forms.DockStyle.Fill;
			this.TabControl.Location = new System.Drawing.Point(0, 16);
			this.TabControl.Name = "TabControl";
			this.TabControl.SelectedIndex = 0;
			this.TabControl.Size = new System.Drawing.Size(843, 573);
			this.TabControl.TabIndex = 3;
			//
			//TabPage1
			//
			this.TabPage1.Controls.Add(this.StartOfDayGrid);
			this.TabPage1.Location = new System.Drawing.Point(4, 22);
			this.TabPage1.Name = "TabPage1";
			this.TabPage1.Padding = new System.Windows.Forms.Padding(3);
			this.TabPage1.Size = new System.Drawing.Size(835, 547);
			this.TabPage1.TabIndex = 0;
			this.TabPage1.Text = "Start of day";
			this.TabPage1.UseVisualStyleBackColor = true;
			//
			//TabPage2
			//
			this.TabPage2.Controls.Add(this.EndOfDayGrid);
			this.TabPage2.Location = new System.Drawing.Point(4, 22);
			this.TabPage2.Name = "TabPage2";
			this.TabPage2.Padding = new System.Windows.Forms.Padding(3);
			this.TabPage2.Size = new System.Drawing.Size(835, 547);
			this.TabPage2.TabIndex = 1;
			this.TabPage2.Text = "End of day";
			this.TabPage2.UseVisualStyleBackColor = true;
			//
			//StartOfDayGrid
			//
			this.StartOfDayGrid.AllowUserToAddRows = false;
			this.StartOfDayGrid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
			this.StartOfDayGrid.DataSourceTable = null;
			this.StartOfDayGrid.Dock = System.Windows.Forms.DockStyle.Fill;
			this.StartOfDayGrid.Location = new System.Drawing.Point(3, 3);
			this.StartOfDayGrid.Name = "StartOfDayGrid";
			this.StartOfDayGrid.RowHeadersVisible = false;
			this.StartOfDayGrid.Size = new System.Drawing.Size(829, 541);
			this.StartOfDayGrid.TabIndex = 0;
			//
			//EndOfDayGrid
			//
			this.EndOfDayGrid.AllowUserToAddRows = false;
			this.EndOfDayGrid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize;
			this.EndOfDayGrid.DataSourceTable = null;
			this.EndOfDayGrid.Dock = System.Windows.Forms.DockStyle.Fill;
			this.EndOfDayGrid.Location = new System.Drawing.Point(3, 3);
			this.EndOfDayGrid.Name = "EndOfDayGrid";
			this.EndOfDayGrid.RowHeadersVisible = false;
			this.EndOfDayGrid.Size = new System.Drawing.Size(829, 541);
			this.EndOfDayGrid.TabIndex = 0;
			//
			//OperationsUI
			//
			this.Controls.Add(this.TabControl);
			this.Name = "OperationsUI";
			this.Size = new System.Drawing.Size(843, 589);
			this.Controls.SetChildIndex(this.MyHelpLabel, 0);
			this.Controls.SetChildIndex(this.TabControl, 0);
			this.TabControl.ResumeLayout(false);
			this.TabPage1.ResumeLayout(false);
			this.TabPage2.ResumeLayout(false);
			((System.ComponentModel.ISupportInitialize)this.StartOfDayGrid).EndInit();
			((System.ComponentModel.ISupportInitialize)this.EndOfDayGrid).EndInit();
			this.ResumeLayout(false);

		}

        #endregion

        internal System.Windows.Forms.TabControl TabControl;
		internal System.Windows.Forms.TabPage TabPage1;
		internal System.Windows.Forms.TabPage TabPage2;
		internal UIBits.EnhancedGrid StartOfDayGrid;
		internal UIBits.EnhancedGrid EndOfDayGrid;
	}
}
