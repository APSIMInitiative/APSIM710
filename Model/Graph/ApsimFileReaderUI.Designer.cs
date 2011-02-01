namespace Graph
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
         this.FileList = new System.Windows.Forms.TextBox();
         this.OpenFileDialog = new System.Windows.Forms.OpenFileDialog();
         this.panel1 = new System.Windows.Forms.Panel();
         this.BrowseButton = new System.Windows.Forms.Button();
         this.MainPanel.SuspendLayout();
         this.panel1.SuspendLayout();
         this.SuspendLayout();
         // 
         // MainPanel
         // 
         this.MainPanel.Controls.Add(this.FileList);
         this.MainPanel.Controls.Add(this.panel1);
         this.MainPanel.Location = new System.Drawing.Point(0, 16);
         this.MainPanel.Size = new System.Drawing.Size(638, 189);
         // 
         // MyHelpLabel
         // 
         this.MyHelpLabel.Size = new System.Drawing.Size(638, 16);
         this.MyHelpLabel.Text = "Enter one or more APSIM file names into the box below. Filespecs (e.g. *.out) are" +
             " permitted.";
         // 
         // FileList
         // 
         this.FileList.Dock = System.Windows.Forms.DockStyle.Top;
         this.FileList.Location = new System.Drawing.Point(0, 30);
         this.FileList.Multiline = true;
         this.FileList.Name = "FileList";
         this.FileList.Size = new System.Drawing.Size(638, 159);
         this.FileList.TabIndex = 0;
         this.FileList.TextChanged += new System.EventHandler(this.OnTextChanged);
         // 
         // OpenFileDialog
         // 
         this.OpenFileDialog.DefaultExt = "*.out";
         this.OpenFileDialog.FileName = "openFileDialog1";
         this.OpenFileDialog.Filter = "Out files|*.out|All files|*.*";
         this.OpenFileDialog.Multiselect = true;
         this.OpenFileDialog.RestoreDirectory = true;
         this.OpenFileDialog.Title = "Select one or more APSIM output files";
         // 
         // panel1
         // 
         this.panel1.Controls.Add(this.BrowseButton);
         this.panel1.Dock = System.Windows.Forms.DockStyle.Top;
         this.panel1.Location = new System.Drawing.Point(0, 0);
         this.panel1.Name = "panel1";
         this.panel1.Size = new System.Drawing.Size(638, 30);
         this.panel1.TabIndex = 2;
         // 
         // BrowseButton
         // 
         this.BrowseButton.Location = new System.Drawing.Point(3, 3);
         this.BrowseButton.Name = "BrowseButton";
         this.BrowseButton.Size = new System.Drawing.Size(75, 23);
         this.BrowseButton.TabIndex = 2;
         this.BrowseButton.Text = "Browse";
         this.BrowseButton.UseVisualStyleBackColor = true;
         this.BrowseButton.Click += new System.EventHandler(this.OnBrowseButtonClick);
         // 
         // ApsimFileReaderUI
         // 
         this.HelpText = "Enter one or more APSIM file names into the box below. Filespecs (e.g. *.out) are" +
             " permitted.";
         this.Name = "ApsimFileReaderUI";
         this.Size = new System.Drawing.Size(638, 488);
         this.MainPanel.ResumeLayout(false);
         this.MainPanel.PerformLayout();
         this.panel1.ResumeLayout(false);
         this.ResumeLayout(false);
         this.PerformLayout();

         }

      #endregion

      private System.Windows.Forms.TextBox FileList;
      private System.Windows.Forms.OpenFileDialog OpenFileDialog;
      private System.Windows.Forms.Panel panel1;
      private System.Windows.Forms.Button BrowseButton;
      }
   }
