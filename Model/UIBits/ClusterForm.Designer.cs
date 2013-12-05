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
             this.ClusterSimulationFolder = new System.Windows.Forms.TextBox();
             this.OkButton = new System.Windows.Forms.Button();
             this.CanclButton = new System.Windows.Forms.Button();
             this.folderBrowserDialog1 = new System.Windows.Forms.FolderBrowserDialog();
             this.groupBox1 = new System.Windows.Forms.GroupBox();
             this.label4 = new System.Windows.Forms.Label();
             this.NiceUserCheckBox = new System.Windows.Forms.CheckBox();
             this.HlpButton = new System.Windows.Forms.Button();
             this.simsPerJob = new System.Windows.Forms.NumericUpDown();
             this.label1 = new System.Windows.Forms.Label();
             this.BootlegSelector = new System.Windows.Forms.Button();
             this.arch_win32 = new System.Windows.Forms.CheckBox();
             this.arch_unix = new System.Windows.Forms.CheckBox();
             this.AllSimsCheckBox = new System.Windows.Forms.CheckBox();
             this.AllFilesCheckBox = new System.Windows.Forms.CheckBox();
             this.FolderTextBox = new System.Windows.Forms.TextBox();
             this.BrowseButton = new System.Windows.Forms.Button();
             this.sfxBox = new System.Windows.Forms.TextBox();
             this.folderBrowserDialog2 = new System.Windows.Forms.FolderBrowserDialog();
             this.BrowseButton2 = new System.Windows.Forms.Button();
             this.openFileDialog1 = new System.Windows.Forms.OpenFileDialog();
             this.writeToZipfile = new System.Windows.Forms.CheckBox();
             this.uploadSelected = new System.Windows.Forms.CheckBox();
             this.username = new System.Windows.Forms.TextBox();
             this.label2 = new System.Windows.Forms.Label();
             this.password = new System.Windows.Forms.TextBox();
             this.label3 = new System.Windows.Forms.Label();
             this.groupBox1.SuspendLayout();
             ((System.ComponentModel.ISupportInitialize)(this.simsPerJob)).BeginInit();
             this.SuspendLayout();
             // 
             // ClusterSimulationFolder
             // 
             this.ClusterSimulationFolder.Location = new System.Drawing.Point(38, 293);
             this.ClusterSimulationFolder.Name = "ClusterSimulationFolder";
             this.ClusterSimulationFolder.Size = new System.Drawing.Size(406, 20);
             this.ClusterSimulationFolder.TabIndex = 5;
             // 
             // OkButton
             // 
             this.OkButton.DialogResult = System.Windows.Forms.DialogResult.OK;
             this.OkButton.Location = new System.Drawing.Point(510, 307);
             this.OkButton.Name = "OkButton";
             this.OkButton.Size = new System.Drawing.Size(75, 23);
             this.OkButton.TabIndex = 9;
             this.OkButton.Text = "Ok";
             this.OkButton.UseVisualStyleBackColor = true;
             // 
             // CanclButton
             // 
             this.CanclButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
             this.CanclButton.Location = new System.Drawing.Point(510, 352);
             this.CanclButton.Name = "CanclButton";
             this.CanclButton.Size = new System.Drawing.Size(75, 23);
             this.CanclButton.TabIndex = 10;
             this.CanclButton.Text = "Cancel";
             this.CanclButton.UseVisualStyleBackColor = true;
             // 
             // groupBox1
             // 
             this.groupBox1.Controls.Add(this.label4);
             this.groupBox1.Controls.Add(this.NiceUserCheckBox);
             this.groupBox1.Controls.Add(this.HlpButton);
             this.groupBox1.Controls.Add(this.simsPerJob);
             this.groupBox1.Controls.Add(this.label1);
             this.groupBox1.Controls.Add(this.BootlegSelector);
             this.groupBox1.Controls.Add(this.arch_win32);
             this.groupBox1.Controls.Add(this.arch_unix);
             this.groupBox1.Controls.Add(this.AllSimsCheckBox);
             this.groupBox1.Controls.Add(this.AllFilesCheckBox);
             this.groupBox1.Controls.Add(this.FolderTextBox);
             this.groupBox1.Controls.Add(this.BrowseButton);
             this.groupBox1.Controls.Add(this.sfxBox);
             this.groupBox1.Location = new System.Drawing.Point(12, 13);
             this.groupBox1.Name = "groupBox1";
             this.groupBox1.Size = new System.Drawing.Size(590, 230);
             this.groupBox1.TabIndex = 3;
             this.groupBox1.TabStop = false;
             this.groupBox1.Text = "What do you want to run?";
             // 
             // label4
             // 
             this.label4.AutoSize = true;
             this.label4.Location = new System.Drawing.Point(6, 112);
             this.label4.Name = "label4";
             this.label4.Size = new System.Drawing.Size(164, 13);
             this.label4.TabIndex = 36;
             this.label4.Text = "Location of self extracting APSIM";
             // 
             // NiceUserCheckBox
             // 
             this.NiceUserCheckBox.AutoSize = true;
             this.NiceUserCheckBox.Checked = true;
             this.NiceUserCheckBox.CheckState = System.Windows.Forms.CheckState.Checked;
             this.NiceUserCheckBox.Location = new System.Drawing.Point(160, 153);
             this.NiceUserCheckBox.Name = "NiceUserCheckBox";
             this.NiceUserCheckBox.Size = new System.Drawing.Size(71, 17);
             this.NiceUserCheckBox.TabIndex = 32;
             this.NiceUserCheckBox.Text = "Nice user";
             this.NiceUserCheckBox.UseVisualStyleBackColor = true;
             // 
             // HlpButton
             // 
             this.HlpButton.Image = ((System.Drawing.Image)(resources.GetObject("HlpButton.Image")));
             this.HlpButton.Location = new System.Drawing.Point(534, 183);
             this.HlpButton.Name = "HlpButton";
             this.HlpButton.Size = new System.Drawing.Size(39, 30);
             this.HlpButton.TabIndex = 31;
             this.HlpButton.UseVisualStyleBackColor = true;
             this.HlpButton.Click += new System.EventHandler(this.ClusterHelpDocumentation);
             // 
             // simsPerJob
             // 
             this.simsPerJob.Location = new System.Drawing.Point(6, 190);
             this.simsPerJob.Name = "simsPerJob";
             this.simsPerJob.Size = new System.Drawing.Size(54, 20);
             this.simsPerJob.TabIndex = 30;
             // 
             // label1
             // 
             this.label1.AutoSize = true;
             this.label1.Location = new System.Drawing.Point(66, 192);
             this.label1.Name = "label1";
             this.label1.Size = new System.Drawing.Size(95, 13);
             this.label1.TabIndex = 28;
             this.label1.Text = "Simulations per job";
             // 
             // BootlegSelector
             // 
             this.BootlegSelector.AutoSize = true;
             this.BootlegSelector.Image = ((System.Drawing.Image)(resources.GetObject("BootlegSelector.Image")));
             this.BootlegSelector.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
             this.BootlegSelector.Location = new System.Drawing.Point(545, 107);
             this.BootlegSelector.Name = "BootlegSelector";
             this.BootlegSelector.Size = new System.Drawing.Size(28, 23);
             this.BootlegSelector.TabIndex = 27;
             this.BootlegSelector.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText;
             this.BootlegSelector.UseVisualStyleBackColor = true;
             this.BootlegSelector.Click += new System.EventHandler(this.OnBootlegSelectorClick);
             // 
             // arch_win32
             // 
             this.arch_win32.AutoSize = true;
             this.arch_win32.Location = new System.Drawing.Point(6, 153);
             this.arch_win32.Name = "arch_win32";
             this.arch_win32.Size = new System.Drawing.Size(70, 17);
             this.arch_win32.TabIndex = 26;
             this.arch_win32.Text = "Windows";
             this.arch_win32.UseVisualStyleBackColor = true;
             // 
             // arch_unix
             // 
             this.arch_unix.AutoSize = true;
             this.arch_unix.Location = new System.Drawing.Point(87, 153);
             this.arch_unix.Name = "arch_unix";
             this.arch_unix.Size = new System.Drawing.Size(51, 17);
             this.arch_unix.TabIndex = 25;
             this.arch_unix.Text = "Linux";
             this.arch_unix.UseVisualStyleBackColor = true;
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
             this.AllSimsCheckBox.Click += new System.EventHandler(this.AllSimsCheckBox_Click);
             // 
             // AllFilesCheckBox
             // 
             this.AllFilesCheckBox.AutoSize = true;
             this.AllFilesCheckBox.Location = new System.Drawing.Point(6, 57);
             this.AllFilesCheckBox.Name = "AllFilesCheckBox";
             this.AllFilesCheckBox.Size = new System.Drawing.Size(183, 17);
             this.AllFilesCheckBox.TabIndex = 17;
             this.AllFilesCheckBox.Text = "All .apsim files found under folder:";
             this.AllFilesCheckBox.UseVisualStyleBackColor = true;
             this.AllFilesCheckBox.Click += new System.EventHandler(this.AllFilesCheckBox_Click);
             // 
             // FolderTextBox
             // 
             this.FolderTextBox.Location = new System.Drawing.Point(209, 57);
             this.FolderTextBox.Name = "FolderTextBox";
             this.FolderTextBox.Size = new System.Drawing.Size(330, 20);
             this.FolderTextBox.TabIndex = 18;
             // 
             // BrowseButton
             // 
             this.BrowseButton.AutoSize = true;
             this.BrowseButton.Image = ((System.Drawing.Image)(resources.GetObject("BrowseButton.Image")));
             this.BrowseButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
             this.BrowseButton.Location = new System.Drawing.Point(545, 55);
             this.BrowseButton.Name = "BrowseButton";
             this.BrowseButton.Size = new System.Drawing.Size(28, 23);
             this.BrowseButton.TabIndex = 19;
             this.BrowseButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText;
             this.BrowseButton.UseVisualStyleBackColor = true;
             this.BrowseButton.Click += new System.EventHandler(this.BrowseButton_Click);
             // 
             // sfxBox
             // 
             this.sfxBox.Location = new System.Drawing.Point(209, 107);
             this.sfxBox.Name = "sfxBox";
             this.sfxBox.Size = new System.Drawing.Size(330, 20);
             this.sfxBox.TabIndex = 21;
             // 
             // BrowseButton2
             // 
             this.BrowseButton2.AutoSize = true;
             this.BrowseButton2.Image = ((System.Drawing.Image)(resources.GetObject("BrowseButton2.Image")));
             this.BrowseButton2.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
             this.BrowseButton2.Location = new System.Drawing.Point(450, 291);
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
             // writeToZipfile
             // 
             this.writeToZipfile.AutoSize = true;
             this.writeToZipfile.Checked = true;
             this.writeToZipfile.CheckState = System.Windows.Forms.CheckState.Checked;
             this.writeToZipfile.Location = new System.Drawing.Point(12, 258);
             this.writeToZipfile.Name = "writeToZipfile";
             this.writeToZipfile.Size = new System.Drawing.Size(139, 17);
             this.writeToZipfile.TabIndex = 23;
             this.writeToZipfile.Text = "Write to zipfile (dropbox)";
             this.writeToZipfile.UseVisualStyleBackColor = true;
             this.writeToZipfile.Click += new System.EventHandler(this.writeToZipfile_Click);
             // 
             // uploadSelected
             // 
             this.uploadSelected.AutoSize = true;
             this.uploadSelected.Location = new System.Drawing.Point(12, 332);
             this.uploadSelected.Name = "uploadSelected";
             this.uploadSelected.Size = new System.Drawing.Size(106, 17);
             this.uploadSelected.TabIndex = 24;
             this.uploadSelected.Text = "Upload to cluster";
             this.uploadSelected.UseVisualStyleBackColor = true;
             this.uploadSelected.CheckedChanged += new System.EventHandler(this.uploadSelected_CheckedChanged);
             this.uploadSelected.Click += new System.EventHandler(this.uploadSelected_Click);
             // 
             // username
             // 
             this.username.Enabled = false;
             this.username.Location = new System.Drawing.Point(136, 370);
             this.username.Name = "username";
             this.username.Size = new System.Drawing.Size(83, 20);
             this.username.TabIndex = 25;
             // 
             // label2
             // 
             this.label2.AutoSize = true;
             this.label2.Location = new System.Drawing.Point(75, 373);
             this.label2.Name = "label2";
             this.label2.Size = new System.Drawing.Size(55, 13);
             this.label2.TabIndex = 33;
             this.label2.Text = "Username";
             // 
             // password
             // 
             this.password.Enabled = false;
             this.password.Location = new System.Drawing.Point(361, 370);
             this.password.Name = "password";
             this.password.PasswordChar = '*';
             this.password.Size = new System.Drawing.Size(83, 20);
             this.password.TabIndex = 34;
             // 
             // label3
             // 
             this.label3.AutoSize = true;
             this.label3.Location = new System.Drawing.Point(302, 373);
             this.label3.Name = "label3";
             this.label3.Size = new System.Drawing.Size(53, 13);
             this.label3.TabIndex = 35;
             this.label3.Text = "Password";
             // 
             // ClusterForm
             // 
             this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
             this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
             this.ClientSize = new System.Drawing.Size(614, 415);
             this.Controls.Add(this.label3);
             this.Controls.Add(this.password);
             this.Controls.Add(this.label2);
             this.Controls.Add(this.username);
             this.Controls.Add(this.uploadSelected);
             this.Controls.Add(this.writeToZipfile);
             this.Controls.Add(this.BrowseButton2);
             this.Controls.Add(this.groupBox1);
             this.Controls.Add(this.CanclButton);
             this.Controls.Add(this.OkButton);
             this.Controls.Add(this.ClusterSimulationFolder);
             this.Name = "ClusterForm";
             this.Text = "Run APSIM on the Toowoomba cluster";
             this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.ClusterForm_FormClosing);
             this.Load += new System.EventHandler(this.OnLoad);
             this.groupBox1.ResumeLayout(false);
             this.groupBox1.PerformLayout();
             ((System.ComponentModel.ISupportInitialize)(this.simsPerJob)).EndInit();
             this.ResumeLayout(false);
             this.PerformLayout();

         }

      #endregion

      private System.Windows.Forms.TextBox ClusterSimulationFolder;
      private System.Windows.Forms.Button OkButton;
      private System.Windows.Forms.Button CanclButton;
      private System.Windows.Forms.FolderBrowserDialog folderBrowserDialog1;
      private System.Windows.Forms.GroupBox groupBox1;
      private System.Windows.Forms.TextBox sfxBox;
      private System.Windows.Forms.Button BrowseButton;
      private System.Windows.Forms.TextBox FolderTextBox;
      private System.Windows.Forms.CheckBox AllFilesCheckBox;
      private System.Windows.Forms.CheckBox AllSimsCheckBox;
      private System.Windows.Forms.FolderBrowserDialog folderBrowserDialog2;
      private System.Windows.Forms.Button BrowseButton2;
      private System.Windows.Forms.CheckBox arch_win32;
      private System.Windows.Forms.CheckBox arch_unix;
      private System.Windows.Forms.OpenFileDialog openFileDialog1;
      private System.Windows.Forms.Button BootlegSelector;
      private System.Windows.Forms.Label label1;
      private System.Windows.Forms.NumericUpDown simsPerJob;
      public System.Windows.Forms.Button HlpButton;
      private System.Windows.Forms.CheckBox NiceUserCheckBox;
      private System.Windows.Forms.CheckBox writeToZipfile;
      private System.Windows.Forms.CheckBox uploadSelected;
      private System.Windows.Forms.TextBox username;
      private System.Windows.Forms.Label label2;
      private System.Windows.Forms.TextBox password;
      private System.Windows.Forms.Label label3;
      private System.Windows.Forms.Label label4;
      }
   }