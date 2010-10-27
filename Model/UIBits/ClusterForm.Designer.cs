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
         this.label2 = new System.Windows.Forms.Label();
         this.PasswordBox = new System.Windows.Forms.TextBox();
         this.label3 = new System.Windows.Forms.Label();
         this.label4 = new System.Windows.Forms.Label();
         this.EmailBox = new System.Windows.Forms.TextBox();
         this.OkButton = new System.Windows.Forms.Button();
         this.CancelButton = new System.Windows.Forms.Button();
         this.folderBrowserDialog1 = new System.Windows.Forms.FolderBrowserDialog();
         this.groupBox1 = new System.Windows.Forms.GroupBox();
         this.VersionBox = new System.Windows.Forms.TextBox();
         this.label6 = new System.Windows.Forms.Label();
         this.BrowseButton = new System.Windows.Forms.Button();
         this.FolderTextBox = new System.Windows.Forms.TextBox();
         this.AllFilesCheckBox = new System.Windows.Forms.CheckBox();
         this.AllSimsCheckBox = new System.Windows.Forms.CheckBox();
         this.groupBox1.SuspendLayout();
         this.SuspendLayout();
         // 
         // label2
         // 
         this.label2.AutoSize = true;
         this.label2.Location = new System.Drawing.Point(16, 16);
         this.label2.Name = "label2";
         this.label2.Size = new System.Drawing.Size(56, 13);
         this.label2.TabIndex = 1;
         this.label2.Text = "Password:";
         // 
         // PasswordBox
         // 
         this.PasswordBox.Location = new System.Drawing.Point(81, 15);
         this.PasswordBox.Name = "PasswordBox";
         this.PasswordBox.Size = new System.Drawing.Size(100, 20);
         this.PasswordBox.TabIndex = 2;
         this.PasswordBox.UseSystemPasswordChar = true;
         // 
         // label3
         // 
         this.label3.AutoSize = true;
         this.label3.Location = new System.Drawing.Point(19, 263);
         this.label3.Name = "label3";
         this.label3.Size = new System.Drawing.Size(75, 13);
         this.label3.TabIndex = 3;
         this.label3.Text = "Email address:";
         // 
         // label4
         // 
         this.label4.AutoSize = true;
         this.label4.Location = new System.Drawing.Point(16, 235);
         this.label4.Name = "label4";
         this.label4.Size = new System.Drawing.Size(464, 13);
         this.label4.TabIndex = 4;
         this.label4.Text = "You will be informed by email when your job is completed and where you can retrie" +
             "ve the outputs.";
         // 
         // EmailBox
         // 
         this.EmailBox.Location = new System.Drawing.Point(100, 260);
         this.EmailBox.Name = "EmailBox";
         this.EmailBox.Size = new System.Drawing.Size(270, 20);
         this.EmailBox.TabIndex = 5;
         // 
         // OkButton
         // 
         this.OkButton.DialogResult = System.Windows.Forms.DialogResult.OK;
         this.OkButton.Location = new System.Drawing.Point(510, 13);
         this.OkButton.Name = "OkButton";
         this.OkButton.Size = new System.Drawing.Size(75, 23);
         this.OkButton.TabIndex = 9;
         this.OkButton.Text = "Ok";
         this.OkButton.UseVisualStyleBackColor = true;
         // 
         // CancelButton
         // 
         this.CancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
         this.CancelButton.Location = new System.Drawing.Point(510, 43);
         this.CancelButton.Name = "CancelButton";
         this.CancelButton.Size = new System.Drawing.Size(75, 23);
         this.CancelButton.TabIndex = 10;
         this.CancelButton.Text = "Cancel";
         this.CancelButton.UseVisualStyleBackColor = true;
         // 
         // groupBox1
         // 
         this.groupBox1.Controls.Add(this.AllSimsCheckBox);
         this.groupBox1.Controls.Add(this.AllFilesCheckBox);
         this.groupBox1.Controls.Add(this.FolderTextBox);
         this.groupBox1.Controls.Add(this.BrowseButton);
         this.groupBox1.Controls.Add(this.label6);
         this.groupBox1.Controls.Add(this.VersionBox);
         this.groupBox1.Location = new System.Drawing.Point(19, 42);
         this.groupBox1.Name = "groupBox1";
         this.groupBox1.Size = new System.Drawing.Size(468, 166);
         this.groupBox1.TabIndex = 3;
         this.groupBox1.TabStop = false;
         this.groupBox1.Text = "What do you want to run?";
         // 
         // VersionBox
         // 
         this.VersionBox.Location = new System.Drawing.Point(112, 124);
         this.VersionBox.Name = "VersionBox";
         this.VersionBox.Size = new System.Drawing.Size(60, 20);
         this.VersionBox.TabIndex = 21;
         // 
         // label6
         // 
         this.label6.AutoSize = true;
         this.label6.Location = new System.Drawing.Point(25, 127);
         this.label6.Name = "label6";
         this.label6.Size = new System.Drawing.Size(80, 13);
         this.label6.TabIndex = 20;
         this.label6.Text = "APSIM version:";
         // 
         // BrowseButton
         // 
         this.BrowseButton.AutoSize = true;
         this.BrowseButton.Image = ((System.Drawing.Image)(resources.GetObject("BrowseButton.Image")));
         this.BrowseButton.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft;
         this.BrowseButton.Location = new System.Drawing.Point(428, 76);
         this.BrowseButton.Name = "BrowseButton";
         this.BrowseButton.Size = new System.Drawing.Size(28, 23);
         this.BrowseButton.TabIndex = 19;
         this.BrowseButton.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageBeforeText;
         this.BrowseButton.UseVisualStyleBackColor = true;
         this.BrowseButton.Click += new System.EventHandler(this.BrowseButton_Click);
         // 
         // FolderTextBox
         // 
         this.FolderTextBox.Location = new System.Drawing.Point(214, 76);
         this.FolderTextBox.Name = "FolderTextBox";
         this.FolderTextBox.Size = new System.Drawing.Size(208, 20);
         this.FolderTextBox.TabIndex = 18;
         // 
         // AllFilesCheckBox
         // 
         this.AllFilesCheckBox.AutoSize = true;
         this.AllFilesCheckBox.Location = new System.Drawing.Point(24, 76);
         this.AllFilesCheckBox.Name = "AllFilesCheckBox";
         this.AllFilesCheckBox.Size = new System.Drawing.Size(183, 17);
         this.AllFilesCheckBox.TabIndex = 17;
         this.AllFilesCheckBox.Text = "All .apsim files found under folder:";
         this.AllFilesCheckBox.UseVisualStyleBackColor = true;
         this.AllFilesCheckBox.Click += new System.EventHandler(this.AllFilesCheckBox_Click);
         this.AllFilesCheckBox.CheckedChanged += new System.EventHandler(this.AllFilesCheckBox_CheckedChanged);
         // 
         // AllSimsCheckBox
         // 
         this.AllSimsCheckBox.AutoSize = true;
         this.AllSimsCheckBox.Checked = true;
         this.AllSimsCheckBox.CheckState = System.Windows.Forms.CheckState.Checked;
         this.AllSimsCheckBox.Location = new System.Drawing.Point(24, 34);
         this.AllSimsCheckBox.Name = "AllSimsCheckBox";
         this.AllSimsCheckBox.Size = new System.Drawing.Size(252, 17);
         this.AllSimsCheckBox.TabIndex = 16;
         this.AllSimsCheckBox.Text = "All simulations under the currently selected node";
         this.AllSimsCheckBox.UseVisualStyleBackColor = true;
         this.AllSimsCheckBox.Click += new System.EventHandler(this.AllSimsCheckBox_Click);
         this.AllSimsCheckBox.CheckedChanged += new System.EventHandler(this.AllSimsCheckBox_CheckedChanged);
         // 
         // ClusterForm
         // 
         this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
         this.ClientSize = new System.Drawing.Size(603, 310);
         this.Controls.Add(this.PasswordBox);
         this.Controls.Add(this.groupBox1);
         this.Controls.Add(this.CancelButton);
         this.Controls.Add(this.OkButton);
         this.Controls.Add(this.label4);
         this.Controls.Add(this.label3);
         this.Controls.Add(this.EmailBox);
         this.Controls.Add(this.label2);
         this.Name = "ClusterForm";
         this.Text = "Run APSIM on the Toowoomba cluster";
         this.Load += new System.EventHandler(this.OnLoad);
         this.groupBox1.ResumeLayout(false);
         this.groupBox1.PerformLayout();
         this.ResumeLayout(false);
         this.PerformLayout();

         }

      #endregion

      private System.Windows.Forms.Label label2;
      private System.Windows.Forms.TextBox PasswordBox;
      private System.Windows.Forms.Label label3;
      private System.Windows.Forms.Label label4;
      private System.Windows.Forms.TextBox EmailBox;
      private System.Windows.Forms.Button OkButton;
      private System.Windows.Forms.Button CancelButton;
      private System.Windows.Forms.FolderBrowserDialog folderBrowserDialog1;
      private System.Windows.Forms.GroupBox groupBox1;
      private System.Windows.Forms.TextBox VersionBox;
      private System.Windows.Forms.Label label6;
      private System.Windows.Forms.Button BrowseButton;
      private System.Windows.Forms.TextBox FolderTextBox;
      private System.Windows.Forms.CheckBox AllFilesCheckBox;
      private System.Windows.Forms.CheckBox AllSimsCheckBox;
      }
   }