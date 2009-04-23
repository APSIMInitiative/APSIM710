namespace GraphUserInterface
    {
    partial class REMSUI
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(REMSUI));
            this.GroupBox = new System.Windows.Forms.GroupBox();
            this.BrowseButton = new System.Windows.Forms.Button();
            this.DataSourceDropDown = new System.Windows.Forms.ComboBox();
            this.label4 = new System.Windows.Forms.Label();
            this.TreatmentDropDown = new System.Windows.Forms.ComboBox();
            this.label3 = new System.Windows.Forms.Label();
            this.ExperimentDropDown = new System.Windows.Forms.ComboBox();
            this.label2 = new System.Windows.Forms.Label();
            this.FileNameEdit = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.OpenFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.GroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // GroupBox
            // 
            this.GroupBox.Controls.Add(this.BrowseButton);
            this.GroupBox.Controls.Add(this.DataSourceDropDown);
            this.GroupBox.Controls.Add(this.label4);
            this.GroupBox.Controls.Add(this.TreatmentDropDown);
            this.GroupBox.Controls.Add(this.label3);
            this.GroupBox.Controls.Add(this.ExperimentDropDown);
            this.GroupBox.Controls.Add(this.label2);
            this.GroupBox.Controls.Add(this.FileNameEdit);
            this.GroupBox.Controls.Add(this.label1);
            this.GroupBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GroupBox.Location = new System.Drawing.Point(0, 18);
            this.GroupBox.Name = "GroupBox";
            this.GroupBox.Size = new System.Drawing.Size(148, 216);
            this.GroupBox.TabIndex = 2;
            this.GroupBox.TabStop = false;
            this.GroupBox.Text = "GroupBox";
            // 
            // BrowseButton
            // 
            this.BrowseButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.BrowseButton.Image = ((System.Drawing.Image)(resources.GetObject("BrowseButton.Image")));
            this.BrowseButton.Location = new System.Drawing.Point(113, 15);
            this.BrowseButton.Name = "BrowseButton";
            this.BrowseButton.Size = new System.Drawing.Size(26, 26);
            this.BrowseButton.TabIndex = 10;
            this.BrowseButton.UseVisualStyleBackColor = true;
            this.BrowseButton.Click += new System.EventHandler(this.OnBrowseClick);
            // 
            // DataSourceDropDown
            // 
            this.DataSourceDropDown.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.DataSourceDropDown.FormattingEnabled = true;
            this.DataSourceDropDown.Items.AddRange(new object[] {
            "Statistics",
            "Plot",
            "Crop",
            "Soil Layered"});
            this.DataSourceDropDown.Location = new System.Drawing.Point(9, 186);
            this.DataSourceDropDown.Name = "DataSourceDropDown";
            this.DataSourceDropDown.Size = new System.Drawing.Size(130, 21);
            this.DataSourceDropDown.TabIndex = 7;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(6, 170);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(68, 13);
            this.label4.TabIndex = 6;
            this.label4.Text = "Data source:";
            // 
            // TreatmentDropDown
            // 
            this.TreatmentDropDown.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.TreatmentDropDown.FormattingEnabled = true;
            this.TreatmentDropDown.Location = new System.Drawing.Point(9, 135);
            this.TreatmentDropDown.Name = "TreatmentDropDown";
            this.TreatmentDropDown.Size = new System.Drawing.Size(130, 21);
            this.TreatmentDropDown.TabIndex = 5;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(6, 119);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(58, 13);
            this.label3.TabIndex = 4;
            this.label3.Text = "Treatment:";
            // 
            // ExperimentDropDown
            // 
            this.ExperimentDropDown.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.ExperimentDropDown.FormattingEnabled = true;
            this.ExperimentDropDown.Location = new System.Drawing.Point(9, 90);
            this.ExperimentDropDown.Name = "ExperimentDropDown";
            this.ExperimentDropDown.Size = new System.Drawing.Size(130, 21);
            this.ExperimentDropDown.TabIndex = 3;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(6, 74);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(62, 13);
            this.label2.TabIndex = 2;
            this.label2.Text = "Experiment:";
            // 
            // FileNameEdit
            // 
            this.FileNameEdit.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.FileNameEdit.Location = new System.Drawing.Point(9, 44);
            this.FileNameEdit.Name = "FileNameEdit";
            this.FileNameEdit.Size = new System.Drawing.Size(130, 20);
            this.FileNameEdit.TabIndex = 1;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(6, 28);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(55, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "File name:";
            // 
            // OpenFileDialog
            // 
            this.OpenFileDialog.DefaultExt = "mdb";
            this.OpenFileDialog.Filter = "MDB files (*.MDB)|*.mdb|(All files (*.*)|*.*";
            this.OpenFileDialog.Multiselect = true;
            this.OpenFileDialog.RestoreDirectory = true;
            this.OpenFileDialog.Title = "Select a REMS database file";
            // 
            // REMSUI
            // 
            this.Controls.Add(this.GroupBox);
            this.Name = "REMSUI";
            this.Size = new System.Drawing.Size(148, 234);
            this.Controls.SetChildIndex(this.GroupBox, 0);
            this.GroupBox.ResumeLayout(false);
            this.GroupBox.PerformLayout();
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.GroupBox GroupBox;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.ComboBox ExperimentDropDown;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox FileNameEdit;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.ComboBox DataSourceDropDown;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.ComboBox TreatmentDropDown;
        private System.Windows.Forms.Button BrowseButton;
        private System.Windows.Forms.OpenFileDialog OpenFileDialog;
        }
    }
