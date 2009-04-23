namespace GraphDataUserInterface
   {
   partial class PredictedObservedUI
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
         this.TopPanel = new System.Windows.Forms.Panel();
         this.FieldNames = new System.Windows.Forms.ListBox();
         this.XLabel = new System.Windows.Forms.Label();
         this.TopPanel.SuspendLayout();
         this.SuspendLayout();
         // 
         // MainPanel
         // 
         this.MainPanel.Location = new System.Drawing.Point(0, 16);
         // 
         // TopPanel
         // 
         this.TopPanel.Controls.Add(this.FieldNames);
         this.TopPanel.Controls.Add(this.XLabel);
         this.TopPanel.Dock = System.Windows.Forms.DockStyle.Top;
         this.TopPanel.Location = new System.Drawing.Point(0, 16);
         this.TopPanel.Name = "TopPanel";
         this.TopPanel.Size = new System.Drawing.Size(655, 199);
         this.TopPanel.TabIndex = 7;
         // 
         // FieldNames
         // 
         this.FieldNames.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                     | System.Windows.Forms.AnchorStyles.Right)));
         this.FieldNames.FormattingEnabled = true;
         this.FieldNames.Location = new System.Drawing.Point(12, 44);
         this.FieldNames.Name = "FieldNames";
         this.FieldNames.Size = new System.Drawing.Size(632, 134);
         this.FieldNames.TabIndex = 44;
         this.FieldNames.KeyDown += new System.Windows.Forms.KeyEventHandler(this.OnKeyDown);
         // 
         // XLabel
         // 
         this.XLabel.AutoSize = true;
         this.XLabel.Location = new System.Drawing.Point(9, 26);
         this.XLabel.Name = "XLabel";
         this.XLabel.Size = new System.Drawing.Size(311, 13);
         this.XLabel.TabIndex = 43;
         this.XLabel.Text = "FieldNames to use when matching predicted and observed data.";
         // 
         // PredictedObservedUI
         // 
         this.Controls.Add(this.TopPanel);
         this.Name = "PredictedObservedUI";
         this.Controls.SetChildIndex(this.MyHelpLabel, 0);
         this.Controls.SetChildIndex(this.MainPanel, 0);
         this.Controls.SetChildIndex(this.TopPanel, 0);
         this.TopPanel.ResumeLayout(false);
         this.TopPanel.PerformLayout();
         this.ResumeLayout(false);
         this.PerformLayout();

         }

      #endregion

      private System.Windows.Forms.Panel TopPanel;
      private System.Windows.Forms.ListBox FieldNames;
      private System.Windows.Forms.Label XLabel;
      }
   }
