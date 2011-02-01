namespace Graph
   {
   partial class GraphReportUI
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
         this.PrintForm = new TMGDevelopment.Windows.Forms.PrintForm(this.components);
         this.Panel = new System.Windows.Forms.Panel();
         this.SuspendLayout();
         // 
         // Panel
         // 
         this.Panel.Dock = System.Windows.Forms.DockStyle.Fill;
         this.Panel.Location = new System.Drawing.Point(0, 16);
         this.Panel.Name = "Panel";
         this.Panel.Size = new System.Drawing.Size(655, 525);
         this.Panel.TabIndex = 2;
         // 
         // GraphReportUI
         // 
         this.Controls.Add(this.Panel);
         this.Name = "GraphReportUI";
         this.Controls.SetChildIndex(this.MyHelpLabel, 0);
         this.Controls.SetChildIndex(this.Panel, 0);
         this.ResumeLayout(false);

         }

      #endregion

      private TMGDevelopment.Windows.Forms.PrintForm PrintForm;
      protected System.Windows.Forms.Panel Panel;

      }
   }
