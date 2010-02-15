namespace ConToApsim
   {
   partial class Form1
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
         this.OpenDialog = new System.Windows.Forms.OpenFileDialog();
         this.ListBox = new System.Windows.Forms.ListBox();
         this.SuspendLayout();
         // 
         // OpenDialog
         // 
         this.OpenDialog.DefaultExt = "con";
         this.OpenDialog.Filter = "CON files|*.con|SIM files|*.sim|All files|*.*";
         this.OpenDialog.Title = "Select a .con or .sim file";
         // 
         // ListBox
         // 
         this.ListBox.Dock = System.Windows.Forms.DockStyle.Fill;
         this.ListBox.FormattingEnabled = true;
         this.ListBox.Location = new System.Drawing.Point(0, 0);
         this.ListBox.Name = "ListBox";
         this.ListBox.Size = new System.Drawing.Size(444, 121);
         this.ListBox.TabIndex = 0;
         // 
         // Form1
         // 
         this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
         this.ClientSize = new System.Drawing.Size(444, 124);
         this.Controls.Add(this.ListBox);
         this.Name = "Form1";
         this.Text = "CON to APSIM converter";
         this.ResumeLayout(false);

         }

      #endregion

      private System.Windows.Forms.OpenFileDialog OpenDialog;
      private System.Windows.Forms.ListBox ListBox;
      }
   }

