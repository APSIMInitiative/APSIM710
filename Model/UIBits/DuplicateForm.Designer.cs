namespace UIBits
   {
   partial class DuplicateForm
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
         this.label1 = new System.Windows.Forms.Label();
         this.NumberControl = new System.Windows.Forms.NumericUpDown();
         this.LinkedCheckBox = new System.Windows.Forms.CheckBox();
         this.OkButton = new System.Windows.Forms.Button();
         this.CanclButton = new System.Windows.Forms.Button();
         ((System.ComponentModel.ISupportInitialize)(this.NumberControl)).BeginInit();
         this.SuspendLayout();
         // 
         // label1
         // 
         this.label1.AutoSize = true;
         this.label1.Location = new System.Drawing.Point(13, 13);
         this.label1.Name = "label1";
         this.label1.Size = new System.Drawing.Size(181, 13);
         this.label1.TabIndex = 0;
         this.label1.Text = "Enter number of duplicates to create:";
         // 
         // NumberControl
         // 
         this.NumberControl.Location = new System.Drawing.Point(200, 12);
         this.NumberControl.Minimum = new decimal(new int[] {
            1,
            0,
            0,
            0});
         this.NumberControl.Name = "NumberControl";
         this.NumberControl.Size = new System.Drawing.Size(73, 20);
         this.NumberControl.TabIndex = 1;
         this.NumberControl.Value = new decimal(new int[] {
            1,
            0,
            0,
            0});
         // 
         // LinkedCheckBox
         // 
         this.LinkedCheckBox.AutoSize = true;
         this.LinkedCheckBox.CheckAlign = System.Drawing.ContentAlignment.MiddleRight;
         this.LinkedCheckBox.Location = new System.Drawing.Point(14, 48);
         this.LinkedCheckBox.Name = "LinkedCheckBox";
         this.LinkedCheckBox.Size = new System.Drawing.Size(139, 17);
         this.LinkedCheckBox.TabIndex = 2;
         this.LinkedCheckBox.Text = "Create linked duplicates";
         this.LinkedCheckBox.UseVisualStyleBackColor = true;
         // 
         // OkButton
         // 
         this.OkButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
         this.OkButton.DialogResult = System.Windows.Forms.DialogResult.OK;
         this.OkButton.Location = new System.Drawing.Point(308, 12);
         this.OkButton.Name = "OkButton";
         this.OkButton.Size = new System.Drawing.Size(75, 23);
         this.OkButton.TabIndex = 3;
         this.OkButton.Text = "Ok";
         this.OkButton.UseVisualStyleBackColor = true;
         this.OkButton.Click += new System.EventHandler(this.OkButtonClick);
         // 
         // CancelButton
         // 
         this.CanclButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
         this.CanclButton.Location = new System.Drawing.Point(308, 42);
         this.CanclButton.Name = "CancelButton";
         this.CanclButton.Size = new System.Drawing.Size(75, 23);
         this.CanclButton.TabIndex = 4;
         this.CanclButton.Text = "Cancel";
         this.CanclButton.UseVisualStyleBackColor = true;
         this.CanclButton.Click += new System.EventHandler(this.CancelButtonClick);
         // 
         // DuplicateForm
         // 
         this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
         this.ClientSize = new System.Drawing.Size(395, 91);
         this.Controls.Add(this.CanclButton);
         this.Controls.Add(this.OkButton);
         this.Controls.Add(this.LinkedCheckBox);
         this.Controls.Add(this.NumberControl);
         this.Controls.Add(this.label1);
         this.Name = "DuplicateForm";
         this.Text = "DuplicateForm";
         ((System.ComponentModel.ISupportInitialize)(this.NumberControl)).EndInit();
         this.ResumeLayout(false);
         this.PerformLayout();

         }

      #endregion

      private System.Windows.Forms.Label label1;
      private System.Windows.Forms.NumericUpDown NumberControl;
      private System.Windows.Forms.CheckBox LinkedCheckBox;
      private System.Windows.Forms.Button OkButton;
      private System.Windows.Forms.Button CanclButton;
      }
   }