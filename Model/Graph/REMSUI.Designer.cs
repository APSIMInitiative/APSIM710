namespace Graph
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
            this.panel1 = new System.Windows.Forms.Panel();
            this.BrowseButton = new System.Windows.Forms.Button();
            this.DataSourceComboBox = new System.Windows.Forms.ComboBox();
            this.label4 = new System.Windows.Forms.Label();
            this.TreatmentComboBox = new System.Windows.Forms.ComboBox();
            this.label3 = new System.Windows.Forms.Label();
            this.ExperimentCmboBox = new System.Windows.Forms.ComboBox();
            this.label2 = new System.Windows.Forms.Label();
            this.FileNameBox = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.OpenFileDialog = new System.Windows.Forms.OpenFileDialog();
            this.panel1.SuspendLayout();
            this.SuspendLayout();
            // 
            // MainPanel
            // 
            this.MainPanel.Location = new System.Drawing.Point(0, 16);
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.BrowseButton);
            this.panel1.Controls.Add(this.DataSourceComboBox);
            this.panel1.Controls.Add(this.label4);
            this.panel1.Controls.Add(this.TreatmentComboBox);
            this.panel1.Controls.Add(this.label3);
            this.panel1.Controls.Add(this.ExperimentCmboBox);
            this.panel1.Controls.Add(this.label2);
            this.panel1.Controls.Add(this.FileNameBox);
            this.panel1.Controls.Add(this.label1);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Top;
            this.panel1.Location = new System.Drawing.Point(0, 16);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(655, 171);
            this.panel1.TabIndex = 7;
            // 
            // BrowseButton
            // 
            this.BrowseButton.Location = new System.Drawing.Point(540, 22);
            this.BrowseButton.Name = "BrowseButton";
            this.BrowseButton.Size = new System.Drawing.Size(75, 23);
            this.BrowseButton.TabIndex = 8;
            this.BrowseButton.Text = "Browse";
            this.BrowseButton.UseVisualStyleBackColor = true;
            this.BrowseButton.Click += new System.EventHandler(this.BrowseButton_Click);
            // 
            // DataSourceComboBox
            // 
            this.DataSourceComboBox.FormattingEnabled = true;
            this.DataSourceComboBox.Location = new System.Drawing.Point(129, 105);
            this.DataSourceComboBox.Name = "DataSourceComboBox";
            this.DataSourceComboBox.Size = new System.Drawing.Size(405, 21);
            this.DataSourceComboBox.TabIndex = 7;
            this.DataSourceComboBox.SelectedIndexChanged += new System.EventHandler(this.DataSourceComboBox_SelectedIndexChanged);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(13, 105);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(68, 13);
            this.label4.TabIndex = 6;
            this.label4.Text = "Data source:";
            // 
            // TreatmentComboBox
            // 
            this.TreatmentComboBox.FormattingEnabled = true;
            this.TreatmentComboBox.Location = new System.Drawing.Point(129, 78);
            this.TreatmentComboBox.Name = "TreatmentComboBox";
            this.TreatmentComboBox.Size = new System.Drawing.Size(405, 21);
            this.TreatmentComboBox.TabIndex = 5;
            this.TreatmentComboBox.SelectedIndexChanged += new System.EventHandler(this.TreatmentComboBox_SelectedIndexChanged);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(13, 78);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(58, 13);
            this.label3.TabIndex = 4;
            this.label3.Text = "Treatment:";
            // 
            // ExperimentCmboBox
            // 
            this.ExperimentCmboBox.FormattingEnabled = true;
            this.ExperimentCmboBox.Location = new System.Drawing.Point(129, 51);
            this.ExperimentCmboBox.Name = "ExperimentCmboBox";
            this.ExperimentCmboBox.Size = new System.Drawing.Size(405, 21);
            this.ExperimentCmboBox.TabIndex = 3;
            this.ExperimentCmboBox.SelectedIndexChanged += new System.EventHandler(this.ExperimentCmboBoxChanged);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(13, 51);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(62, 13);
            this.label2.TabIndex = 2;
            this.label2.Text = "Experiment:";
            // 
            // FileNameBox
            // 
            this.FileNameBox.Location = new System.Drawing.Point(129, 22);
            this.FileNameBox.Name = "FileNameBox";
            this.FileNameBox.Size = new System.Drawing.Size(405, 20);
            this.FileNameBox.TabIndex = 1;
            this.FileNameBox.TextChanged += new System.EventHandler(this.FileNameBox_TextChanged);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(13, 25);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(55, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "File name:";
            // 
            // OpenFileDialog
            // 
            this.OpenFileDialog.DefaultExt = "mdb";
            this.OpenFileDialog.FileName = "openFileDialog1";
            this.OpenFileDialog.Filter = "*.mdb|*.mdb";
            // 
            // REMSUI
            // 
            this.Controls.Add(this.panel1);
            this.Name = "REMSUI";
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.MainPanel, 0);
            this.Controls.SetChildIndex(this.panel1, 0);
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

         }

      #endregion

      private System.Windows.Forms.Panel panel1;
      private System.Windows.Forms.TextBox FileNameBox;
      private System.Windows.Forms.Label label1;
      private System.Windows.Forms.ComboBox DataSourceComboBox;
      private System.Windows.Forms.Label label4;
      private System.Windows.Forms.ComboBox TreatmentComboBox;
      private System.Windows.Forms.Label label3;
      private System.Windows.Forms.ComboBox ExperimentCmboBox;
      private System.Windows.Forms.Label label2;
      private System.Windows.Forms.Button BrowseButton;
      private System.Windows.Forms.OpenFileDialog OpenFileDialog;
      }
   }
