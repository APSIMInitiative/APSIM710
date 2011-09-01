namespace UIBits
   {
   partial class ClusterForm
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
             System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(ClusterForm));
             this.label4 = new System.Windows.Forms.Label();
             this.DropBoxFolder = new System.Windows.Forms.TextBox();
             this.OkButton = new System.Windows.Forms.Button();
             this.CancelButton = new System.Windows.Forms.Button();
             this.folderBrowserDialog1 = new System.Windows.Forms.FolderBrowserDialog();
             this.groupBox1 = new System.Windows.Forms.GroupBox();
             this.arch_win32 = new System.Windows.Forms.CheckBox();
             this.arch_unix = new System.Windows.Forms.CheckBox();
             this.custom_select = new System.Windows.Forms.CheckBox();
             this.release_select = new System.Windows.Forms.CheckBox();
             this.AllSimsCheckBox = new System.Windows.Forms.CheckBox();
             this.AllFilesCheckBox = new System.Windows.Forms.CheckBox();
             this.FolderTextBox = new System.Windows.Forms.TextBox();
             this.BrowseButton = new System.Windows.Forms.Button();
             this.VersionBox = new System.Windows.Forms.TextBox();
             this.folderBrowserDialog2 = new System.Windows.Forms.FolderBrowserDialog();
             this.BrowseButton2 = new System.Windows.Forms.Button();
             this.openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
             this.BootlegSelector = new System.Windows.Forms.Button();
             this.label1 = new System.Windows.Forms.Label();
             this.simsPerJob = new System.Windows.Forms.NumericUpDown();
             this.HelpButton = new System.Windows.Forms.Button();
             this.groupBox1.SuspendLayout();
             ((System.ComponentModel.ISupportInitialize)(this.simsPerJob)).BeginInit();
             this.SuspendLayout();
             // 
             // label4
             // 
             this.label4.AutoSize = true;
             this.label4.Location = new System.Drawing.Point(12, 286);
             this.label4.Name = "label4";
             this.label4.Size = new System.Drawing.Size(150, 13);
             this.label4.TabIndex = 4;
             this.label4.Text = "Destination directory (dropbox)";
             // 
             // DropBoxFolder
             // 
             this.DropBoxFolder.Location = new System.Drawing.Point(15, 321);
             this.DropBoxFolder.Name = "DropBoxFolder";
             this.DropBoxFolder.Size = new System.Drawing.Size(385, 20);
             this.DropBoxFolder.TabIndex = 5;
             // 
             // OkButton
             // 
             this.OkButton.DialogResult = System.Windows.Forms.DialogResult.OK;
             this.OkButton.Location = new System.Drawing.Point(510, 295);
             this.OkButton.Name = "OkButton";
             this.OkButton.Size = new System.Drawing.Size(75, 23);
             this.OkButton.TabIndex = 9;
             this.OkButton.Text = "Ok";
             this.OkButton.UseVisualStyleBackColor = true;
             // 
             // CancelButton
             // 
             this.CancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
             this.CancelButton.Location = new System.Drawing.Point(510, 334);
             this.CancelButton.Name = "CancelButton";
             this.CancelButton.Size = new System.Drawing.Size(75, 23);
             this.CancelButton.TabIndex = 10;
             this.CancelButton.Text = "Cancel";
             this.CancelButton.UseVisualStyleBackColor = true;
             // 
             // groupBox1
             // 
             this.groupBox1.Controls.Add(this.HelpButton);
             this.groupBox1.Controls.Add(this.simsPerJob);
             this.groupBox1.Controls.Add(this.label1);
             this.groupBox1.Controls.Add(this.BootlegSelector);
             this.groupBox1.Controls.Add(this.arch_win32);
             this.groupBox1.Controls.Add(this.arch_unix);
             this.groupBox1.Controls.Add(this.custom_select);
             this.groupBox1.Controls.Add(this.release_select);
             this.groupBox1.Controls.Add(this.AllSimsCheckBox);
             this.groupBox1.Controls.Add(this.AllFilesCheckBox);
             this.groupBox1.Controls.Add(this.FolderTextBox);
             this.groupBox1.Controls.Add(this.BrowseButton);
             this.groupBox1.Controls.Add(this.VersionBox);
             this.groupBox1.Location = new System.Drawing.Point(12, 13);
             this.groupBox1.Name = "groupBox1";
             this.groupBox1.Size = new System.Drawing.Size(590, 258);
             this.groupBox1.TabIndex = 3;
             this.groupBox1.TabStop = false;
             this.groupBox1.Text = "What do you want to run?";
             // 
             // arch_win32
             // 
             this.arch_win32.AutoSize = true;
             this.arch_win32.Location = new System.Drawing.Point(6, 189);
             this.arch_win32.Name = "arch_win32";
             this.arch_win32.Size = new System.Drawing.Size(54, 17);
             this.arch_win32.TabIndex = 26;
             this.arch_win32.Text = "win32";
             this.arch_win32.UseVisualStyleBackColor = true;
             this.arch_win32.Click += new System.EventHandler(this.arch_win32_Click);
             // 
             // arch_unix
             // 
             this.arch_unix.AutoSize = true;
             this.arch_unix.Location = new System.Drawing.Point(87, 189);
             this.arch_unix.Name = "arch_unix";
             this.arch_unix.Size = new System.Drawing.Size(45, 17);
             this.arch_unix.TabIndex = 25;
             this.arch_unix.Text = "unix";
             this.arch_unix.UseVisualStyleBackColor = true;
             this.arch_unix.Click += new System.EventHandler(this.arch_unix_Click);
             // 
             // custom_select
             // 
             this.custom_select.AutoSize = true;
             this.custom_select.Location = new System.Drawing.Point(6, 149);
             this.custom_select.Name = "custom_select";
             this.custom_select.Size = new System.Drawing.Size(99, 17);
             this.custom_select.TabIndex = 23;
             this.custom_select.Text = "Custom Version";
             this.custom_select.UseVisualStyleBackColor = true;
             this.custom_select.Click += new System.EventHandler(this.custom_select_Click);
             // 
             // release_select
             // 
             this.release_select.AutoSize = true;
             this.release_select.Checked = true;
             this.release_select.CheckState = System.Windows.Forms.CheckState.Checked;
             this.release_select.Location = new System.Drawing.Point(6, 113);
             this.release_select.Name = "release_select";
             this.release_select.Size = new System.Drawing.Size(103, 17);
             this.release_select.TabIndex = 22;
             this.release_select.Text = "Release Version";
             this.release_select.UseVisualStyleBackColor = true;
             this.release_select.Click += new System.EventHandler(this.release_select_Click);
             // 
             // AllSimsCheckBox
             // 
             this.AllSimsCheckBox.AutoSize = true;
             this.AllSimsCheckBox.Checked = true;
             this.AllSimsCheckBox.CheckState = System.Windows.Forms.CheckState.Checked;
             this.AllSimsCheckBox.Location = new System.Drawing.Point(6, 34);
             this.AllSimsCheckBox.Name = "AllSimsCheckBox";
             this.AllSimsCheckBox.Size = new System.Drawing.Size(154, 17);
             this.AllSimsCheckBox.TabIndex = 16;
             this.AllSimsCheckBox.Text = "All simulations in current file";
             this.AllSimsCheckBox.UseVisualStyleBackColor = true;
             this.AllSimsCheckBox.CheckedChanged += new System.EventHandler(this.AllSimsCheckBox_CheckedChanged);
             this.AllSimsCheckBox.Click += new System.EventHandler(this.AllSimsCheckBox_Click);
             // 
             // AllFilesCheckBox
             // 
             this.AllFilesCheckBox.AutoSize = true;
             this.AllFilesCheckBox.Location = new System.Drawing.Point(6, 76);
             this.AllFilesCheckBox.Name = "AllFilesCheckBox";
             this.AllFilesCheckBox.Size = new System.Drawing.Size(183, 17);
             this.AllFilesCheckBox.TabIndex = 17;
             this.AllFilesCheckBox.Text = "All .apsim files found under folder:";
             this.AllFilesCheckBox.UseVisualStyleBackColor = true;
             this.AllFilesCheckBox.CheckedChanged += new System.EventHandler(this.AllFilesCheckBox_CheckedChanged);
             this.AllFilesCheckBox.Click += new System.EventHandler(this.AllFilesCheckBox_Click);
             // 
             // FolderTextBox
             // 
             this.FolderTextBox.Location = new System.Drawing.Point(214, 74);
             this.FolderTextBox.Name = "FolderTextBox";
             this.FolderTextBox.Size = new System.Drawing.Size(321, 20);
             this.FolderTextBox.TabIndex = 18;
             // 
             // BrowseButton
             // 
             this.BrowseButton.AutoSize = true;
             this.BrowseButton.Image = ((System.Drawing.Image)(resources.GetObject("BrowseButton.Image")));
             this.BrowseButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
             this.BrowseButton.Location = new System.Drawing.Point(545, 72);
             this.BrowseButton.Name = "BrowseButton";
             this.BrowseButton.Size = new System.Drawing.Size(28, 23);
             this.BrowseButton.TabIndex = 19;
             this.BrowseButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText;
             this.BrowseButton.UseVisualStyleBackColor = true;
             this.BrowseButton.Click += new System.EventHandler(this.BrowseButton_Click);
             // 
             // VersionBox
             // 
             this.VersionBox.Location = new System.Drawing.Point(214, 126);
             this.VersionBox.Name = "VersionBox";
             this.VersionBox.Size = new System.Drawing.Size(321, 20);
             this.VersionBox.TabIndex = 21;
             // 
             // BrowseButton2
             // 
             this.BrowseButton2.AutoSize = true;
             this.BrowseButton2.Image = ((System.Drawing.Image)(resources.GetObject("BrowseButton2.Image")));
             this.BrowseButton2.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
             this.BrowseButton2.Location = new System.Drawing.Point(406, 319);
             this.BrowseButton2.Name = "BrowseButton2";
             this.BrowseButton2.Size = new System.Drawing.Size(28, 23);
             this.BrowseButton2.TabIndex = 22;
             this.BrowseButton2.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText;
             this.BrowseButton2.UseVisualStyleBackColor = true;
             this.BrowseButton2.Click += new System.EventHandler(this.OnBrowseButton2Click);
             // 
             // openFileDialog1
             // 
             this.openFileDialog1.FileName = "openFileDialog1";
             // 
             // BootlegSelector
             // 
             this.BootlegSelector.AutoSize = true;
             this.BootlegSelector.Image = ((System.Drawing.Image)(resources.GetObject("BootlegSelector.Image")));
             this.BootlegSelector.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
             this.BootlegSelector.Location = new System.Drawing.Point(545, 123);
             this.BootlegSelector.Name = "BootlegSelector";
             this.BootlegSelector.Size = new System.Drawing.Size(28, 23);
             this.BootlegSelector.TabIndex = 27;
             this.BootlegSelector.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText;
             this.BootlegSelector.UseVisualStyleBackColor = true;
             this.BootlegSelector.Click += new System.EventHandler(this.OnBootlegSelectorClick);
             // 
             // label1
             // 
             this.label1.AutoSize = true;
             this.label1.Location = new System.Drawing.Point(66, 226);
             this.label1.Name = "label1";
             this.label1.Size = new System.Drawing.Size(95, 13);
             this.label1.TabIndex = 28;
             this.label1.Text = "Simulations per job";
             // 
             // simsPerJob
             // 
             this.simsPerJob.Location = new System.Drawing.Point(6, 224);
             this.simsPerJob.Name = "simsPerJob";
             this.simsPerJob.Size = new System.Drawing.Size(54, 20);
             this.simsPerJob.TabIndex = 30;
             // 
             // HelpButton
             // 
             this.HelpButton.Image = ((System.Drawing.Image)(resources.GetObject("HelpButton.Image")));
             this.HelpButton.Location = new System.Drawing.Point(534, 217);
             this.HelpButton.Name = "HelpButton";
             this.HelpButton.Size = new System.Drawing.Size(39, 30);
             this.HelpButton.TabIndex = 31;
             this.HelpButton.UseVisualStyleBackColor = true;
             this.HelpButton.Click += new System.EventHandler(this.ClusterHelpDocumentation);
             // 
             // ClusterForm
             // 
             this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
             this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
             this.ClientSize = new System.Drawing.Size(614, 369);
             this.Controls.Add(this.BrowseButton2);
             this.Controls.Add(this.groupBox1);
             this.Controls.Add(this.CancelButton);
             this.Controls.Add(this.OkButton);
             this.Controls.Add(this.label4);
             this.Controls.Add(this.DropBoxFolder);
             this.Name = "ClusterForm";
             this.Text = "Run APSIM on the Toowoomba cluster";
             this.Load += new System.EventHandler(this.OnLoad);
             this.groupBox1.ResumeLayout(false);
             this.groupBox1.PerformLayout();
             ((System.ComponentModel.ISupportInitialize)(this.simsPerJob)).EndInit();
             this.ResumeLayout(false);
             this.PerformLayout();

         }

      #endregion

      private System.Windows.Forms.Label label4;
      private System.Windows.Forms.TextBox DropBoxFolder;
      private System.Windows.Forms.Button OkButton;
      private System.Windows.Forms.Button CancelButton;
      private System.Windows.Forms.FolderBrowserDialog folderBrowserDialog1;
      private System.Windows.Forms.GroupBox groupBox1;
      private System.Windows.Forms.TextBox VersionBox;
      private System.Windows.Forms.Button BrowseButton;
      private System.Windows.Forms.TextBox FolderTextBox;
      private System.Windows.Forms.CheckBox AllFilesCheckBox;
      private System.Windows.Forms.CheckBox AllSimsCheckBox;
      private System.Windows.Forms.FolderBrowserDialog folderBrowserDialog2;
      private System.Windows.Forms.Button BrowseButton2;
      private System.Windows.Forms.CheckBox release_select;
      private System.Windows.Forms.CheckBox arch_win32;
      private System.Windows.Forms.CheckBox arch_unix;
      private System.Windows.Forms.CheckBox custom_select;
      private System.Windows.Forms.OpenFileDialog openFileDialog1;
      private System.Windows.Forms.Button BootlegSelector;
      private System.Windows.Forms.Label label1;
      private System.Windows.Forms.NumericUpDown simsPerJob;
      public System.Windows.Forms.Button HelpButton;
      }
   }