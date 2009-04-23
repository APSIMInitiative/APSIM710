namespace UIBits
   {
   partial class PrintPreviewDialog
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
         this.PreviewControl = new System.Windows.Forms.PrintPreviewControl();
         this.PrintButton = new System.Windows.Forms.Button();
         this.PageSetupButton = new System.Windows.Forms.Button();
         this.PageNumberUpDown = new System.Windows.Forms.NumericUpDown();
         this.label1 = new System.Windows.Forms.Label();
         this.PrintDialog = new System.Windows.Forms.PrintDialog();
         this.PageSetupDialog = new System.Windows.Forms.PageSetupDialog();
         ((System.ComponentModel.ISupportInitialize)(this.PageNumberUpDown)).BeginInit();
         this.SuspendLayout();
         // 
         // PreviewControl
         // 
         this.PreviewControl.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                     | System.Windows.Forms.AnchorStyles.Left)
                     | System.Windows.Forms.AnchorStyles.Right)));
         this.PreviewControl.Location = new System.Drawing.Point(5, 32);
         this.PreviewControl.Name = "PreviewControl";
         this.PreviewControl.Size = new System.Drawing.Size(614, 616);
         this.PreviewControl.TabIndex = 0;
         // 
         // PrintButton
         // 
         this.PrintButton.Location = new System.Drawing.Point(5, 3);
         this.PrintButton.Name = "PrintButton";
         this.PrintButton.Size = new System.Drawing.Size(75, 23);
         this.PrintButton.TabIndex = 1;
         this.PrintButton.Text = "Print...";
         this.PrintButton.UseVisualStyleBackColor = true;
         this.PrintButton.Click += new System.EventHandler(this.OnPrint);
         // 
         // PageSetupButton
         // 
         this.PageSetupButton.Location = new System.Drawing.Point(87, 3);
         this.PageSetupButton.Name = "PageSetupButton";
         this.PageSetupButton.Size = new System.Drawing.Size(91, 23);
         this.PageSetupButton.TabIndex = 2;
         this.PageSetupButton.Text = "Page Setup...";
         this.PageSetupButton.UseVisualStyleBackColor = true;
         this.PageSetupButton.Click += new System.EventHandler(this.OnPageSetup);
         // 
         // PageNumberUpDown
         // 
         this.PageNumberUpDown.Location = new System.Drawing.Point(225, 6);
         this.PageNumberUpDown.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
         this.PageNumberUpDown.Name = "PageNumberUpDown";
         this.PageNumberUpDown.Size = new System.Drawing.Size(51, 20);
         this.PageNumberUpDown.TabIndex = 3;
         this.PageNumberUpDown.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
         this.PageNumberUpDown.ValueChanged += new System.EventHandler(this.OnPageChanged);
         // 
         // label1
         // 
         this.label1.AutoSize = true;
         this.label1.Location = new System.Drawing.Point(184, 8);
         this.label1.Name = "label1";
         this.label1.Size = new System.Drawing.Size(35, 13);
         this.label1.TabIndex = 4;
         this.label1.Text = "Page:";
         // 
         // PrintDialog
         // 
         this.PrintDialog.AllowCurrentPage = true;
         this.PrintDialog.AllowSelection = true;
         this.PrintDialog.AllowSomePages = true;
         this.PrintDialog.UseEXDialog = true;
         // 
         // PrintPreviewDialog
         // 
         this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
         this.ClientSize = new System.Drawing.Size(623, 652);
         this.Controls.Add(this.label1);
         this.Controls.Add(this.PageNumberUpDown);
         this.Controls.Add(this.PageSetupButton);
         this.Controls.Add(this.PrintButton);
         this.Controls.Add(this.PreviewControl);
         this.Name = "PrintPreviewDialog";
         this.Text = "PrintPreview";
         this.Load += new System.EventHandler(this.OnLoad);
         ((System.ComponentModel.ISupportInitialize)(this.PageNumberUpDown)).EndInit();
         this.ResumeLayout(false);
         this.PerformLayout();

         }

      #endregion

      private System.Windows.Forms.PrintPreviewControl PreviewControl;
      private System.Windows.Forms.Button PrintButton;
      private System.Windows.Forms.Button PageSetupButton;
      private System.Windows.Forms.NumericUpDown PageNumberUpDown;
      private System.Windows.Forms.Label label1;
      private System.Windows.Forms.PrintDialog PrintDialog;
      private System.Windows.Forms.PageSetupDialog PageSetupDialog;
      }
   }