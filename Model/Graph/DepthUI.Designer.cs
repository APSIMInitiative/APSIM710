namespace Graph
   {
   partial class DepthUI
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
         this.DateList = new System.Windows.Forms.CheckedListBox();
         this.label1 = new System.Windows.Forms.Label();
         this.SuspendLayout();
         // 
         // DateList
         // 
         this.DateList.Dock = System.Windows.Forms.DockStyle.Top;
         this.DateList.FormattingEnabled = true;
         this.DateList.Location = new System.Drawing.Point(0, 29);
         this.DateList.Name = "DateList";
         this.DateList.Size = new System.Drawing.Size(655, 184);
         this.DateList.TabIndex = 2;
         this.DateList.ItemCheck += new System.Windows.Forms.ItemCheckEventHandler(this.OnItemChecked);
         // 
         // label1
         // 
         this.label1.AutoSize = true;
         this.label1.Dock = System.Windows.Forms.DockStyle.Top;
         this.label1.Location = new System.Drawing.Point(0, 16);
         this.label1.Name = "label1";
         this.label1.Size = new System.Drawing.Size(220, 13);
         this.label1.TabIndex = 3;
         this.label1.Text = "Tick the dates to include on the depth graph.";
         // 
         // DepthUI
         // 
         this.Controls.Add(this.DateList);
         this.Controls.Add(this.label1);
         this.Name = "DepthUI";
         this.Controls.SetChildIndex(this.MyHelpLabel, 0);
         this.Controls.SetChildIndex(this.label1, 0);
         this.Controls.SetChildIndex(this.DateList, 0);
         this.ResumeLayout(false);
         this.PerformLayout();

         }

      #endregion

      private System.Windows.Forms.CheckedListBox DateList;
      private System.Windows.Forms.Label label1;
      }
   }
