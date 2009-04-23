namespace GraphUserInterface
    {
    partial class ApsimFileReaderUI
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ApsimFileReaderUI));
            this.OpenFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.GroupBox = new System.Windows.Forms.GroupBox();
            this.FileList = new System.Windows.Forms.TextBox();
            this.BrowseButton = new System.Windows.Forms.Button();
            this.GroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // OpenFileDialog
            // 
            this.OpenFileDialog.DefaultExt = "out";
            this.OpenFileDialog.Filter = "Output files (*.out)|*.out|CSV files (*.csv)|*.csv|(All files (*.*)|*.*";
            this.OpenFileDialog.Multiselect = true;
            this.OpenFileDialog.RestoreDirectory = true;
            this.OpenFileDialog.Title = "Select 1 or more output files";
            // 
            // GroupBox
            // 
            this.GroupBox.Controls.Add(this.FileList);
            this.GroupBox.Controls.Add(this.BrowseButton);
            this.GroupBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GroupBox.Location = new System.Drawing.Point(0, 18);
            this.GroupBox.Name = "GroupBox";
            this.GroupBox.Size = new System.Drawing.Size(178, 119);
            this.GroupBox.TabIndex = 8;
            this.GroupBox.TabStop = false;
            this.GroupBox.Text = "Apsim File Reader";
            // 
            // FileList
            // 
            this.FileList.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.FileList.Location = new System.Drawing.Point(6, 43);
            this.FileList.Multiline = true;
            this.FileList.Name = "FileList";
            this.FileList.Size = new System.Drawing.Size(167, 70);
            this.FileList.TabIndex = 8;
            this.FileList.WordWrap = false;
            // 
            // BrowseButton
            // 
            this.BrowseButton.Image = ((System.Drawing.Image)(resources.GetObject("BrowseButton.Image")));
            this.BrowseButton.Location = new System.Drawing.Point(6, 17);
            this.BrowseButton.Name = "BrowseButton";
            this.BrowseButton.Size = new System.Drawing.Size(26, 26);
            this.BrowseButton.TabIndex = 9;
            this.BrowseButton.UseVisualStyleBackColor = true;
            this.BrowseButton.Click += new System.EventHandler(this.OnBrowseButtonClick);
            // 
            // ApsimFileReaderUI
            // 
            this.BackColor = System.Drawing.SystemColors.Window;
            this.Controls.Add(this.GroupBox);
            this.Name = "ApsimFileReaderUI";
            this.Size = new System.Drawing.Size(178, 137);
            this.Controls.SetChildIndex(this.GroupBox, 0);
            this.GroupBox.ResumeLayout(false);
            this.GroupBox.PerformLayout();
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.OpenFileDialog OpenFileDialog;
        private System.Windows.Forms.GroupBox GroupBox;
        private System.Windows.Forms.TextBox FileList;
       private System.Windows.Forms.Button BrowseButton;
        }
    }
