namespace GraphDataUserInterface
   {
   partial class FilterUI
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
         this.FilterBox = new System.Windows.Forms.TextBox();
         this.label1 = new System.Windows.Forms.Label();
         this.panel1.SuspendLayout();
         this.SuspendLayout();
         // 
         // panel1
         // 
         this.panel1.Controls.Add(this.FilterBox);
         this.panel1.Controls.Add(this.label1);
         this.panel1.Dock = System.Windows.Forms.DockStyle.Top;
         this.panel1.Location = new System.Drawing.Point(0, 16);
         this.panel1.Name = "panel1";
         this.panel1.Size = new System.Drawing.Size(655, 127);
         this.panel1.TabIndex = 7;
         // 
         // FilterBox
         // 
         this.FilterBox.Location = new System.Drawing.Point(129, 22);
         this.FilterBox.Name = "FilterBox";
         this.FilterBox.Size = new System.Drawing.Size(405, 20);
         this.FilterBox.TabIndex = 1;
         this.FilterBox.Leave += new System.EventHandler(this.OnFilterChanged);
         // 
         // label1
         // 
         this.label1.AutoSize = true;
         this.label1.Location = new System.Drawing.Point(13, 25);
         this.label1.Name = "label1";
         this.label1.Size = new System.Drawing.Size(110, 13);
         this.label1.TabIndex = 0;
         this.label1.Text = "Enter filter expression:";
         // 
         // FilterUI
         // 
         this.Controls.Add(this.panel1);
         this.Name = "FilterUI";
         this.Controls.SetChildIndex(this.MainPanel, 0);
         this.Controls.SetChildIndex(this.MyHelpLabel, 0);
         this.Controls.SetChildIndex(this.panel1, 0);
         this.panel1.ResumeLayout(false);
         this.panel1.PerformLayout();
         this.ResumeLayout(false);
         this.PerformLayout();

         }

      #endregion

      private System.Windows.Forms.Panel panel1;
      private System.Windows.Forms.TextBox FilterBox;
      private System.Windows.Forms.Label label1;
      }
   }
