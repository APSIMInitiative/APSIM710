namespace ApsimRun
   {
   partial class SimulationRunnerReportForm
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
         this.components = new System.ComponentModel.Container();
         System.Windows.Forms.Label label1;
         System.Windows.Forms.ListViewGroup listViewGroup1 = new System.Windows.Forms.ListViewGroup("File Name", System.Windows.Forms.HorizontalAlignment.Left);
         System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(SimulationRunnerReportForm));
         this.SimulationList = new System.Windows.Forms.ListView();
         this.SimulationName = new System.Windows.Forms.ColumnHeader();
         this.HasErrors = new System.Windows.Forms.ColumnHeader();
         this.HasWarnings = new System.Windows.Forms.ColumnHeader();
         this.ImageList = new System.Windows.Forms.ImageList(this.components);
         this.OkButton = new System.Windows.Forms.Button();
         label1 = new System.Windows.Forms.Label();
         this.SuspendLayout();
         // 
         // label1
         // 
         label1.AutoSize = true;
         label1.Location = new System.Drawing.Point(12, 9);
         label1.Name = "label1";
         label1.Size = new System.Drawing.Size(323, 13);
         label1.TabIndex = 2;
         label1.Text = "Double click the simulations below to view the APSIM summary file.";
         // 
         // SimulationList
         // 
         this.SimulationList.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                     | System.Windows.Forms.AnchorStyles.Left)
                     | System.Windows.Forms.AnchorStyles.Right)));
         this.SimulationList.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
         this.SimulationList.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.SimulationName,
            this.HasErrors,
            this.HasWarnings});
         this.SimulationList.GridLines = true;
         listViewGroup1.Header = "File Name";
         listViewGroup1.Name = "FileNameGroup";
         this.SimulationList.Groups.AddRange(new System.Windows.Forms.ListViewGroup[] {
            listViewGroup1});
         this.SimulationList.HideSelection = false;
         this.SimulationList.Location = new System.Drawing.Point(12, 40);
         this.SimulationList.MultiSelect = false;
         this.SimulationList.Name = "SimulationList";
         this.SimulationList.Size = new System.Drawing.Size(498, 403);
         this.SimulationList.SmallImageList = this.ImageList;
         this.SimulationList.TabIndex = 0;
         this.SimulationList.UseCompatibleStateImageBehavior = false;
         this.SimulationList.View = System.Windows.Forms.View.Details;
         this.SimulationList.MouseDoubleClick += new System.Windows.Forms.MouseEventHandler(this.OnDoubleClick);
         // 
         // SimulationName
         // 
         this.SimulationName.Text = "Simulation Name";
         this.SimulationName.Width = 241;
         // 
         // HasErrors
         // 
         this.HasErrors.Text = "HasErrors?";
         this.HasErrors.Width = 101;
         // 
         // HasWarnings
         // 
         this.HasWarnings.Text = "Has Warnings?";
         this.HasWarnings.Width = 116;
         // 
         // ImageList
         // 
         this.ImageList.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("ImageList.ImageStream")));
         this.ImageList.TransparentColor = System.Drawing.Color.Transparent;
         this.ImageList.Images.SetKeyName(0, "check2.png");
         this.ImageList.Images.SetKeyName(1, "error.png");
         this.ImageList.Images.SetKeyName(2, "warning.png");
         // 
         // OkButton
         // 
         this.OkButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
         this.OkButton.DialogResult = System.Windows.Forms.DialogResult.OK;
         this.OkButton.Location = new System.Drawing.Point(435, 11);
         this.OkButton.Name = "OkButton";
         this.OkButton.Size = new System.Drawing.Size(75, 23);
         this.OkButton.TabIndex = 1;
         this.OkButton.Text = "Ok";
         this.OkButton.UseVisualStyleBackColor = true;
         // 
         // SimulationRunnerReportForm
         // 
         this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
         this.ClientSize = new System.Drawing.Size(522, 455);
         this.Controls.Add(label1);
         this.Controls.Add(this.OkButton);
         this.Controls.Add(this.SimulationList);
         this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow;
         this.Name = "SimulationRunnerReportForm";
         this.ShowIcon = false;
         this.ShowInTaskbar = false;
         this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
         this.Text = "Simulation Report";
         this.Shown += new System.EventHandler(this.OnShown);
         this.ResumeLayout(false);
         this.PerformLayout();

         }

      #endregion

      private System.Windows.Forms.ListView SimulationList;
      private System.Windows.Forms.ColumnHeader SimulationName;
      private System.Windows.Forms.ColumnHeader HasErrors;
      private System.Windows.Forms.ColumnHeader HasWarnings;
      private System.Windows.Forms.Button OkButton;
      private System.Windows.Forms.ImageList ImageList;
      }
   }