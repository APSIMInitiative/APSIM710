namespace CSUserInterface
   {
   partial class PlugInsUI
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
         this.PlugInListBox = new System.Windows.Forms.ListView();
         this.NameColumn = new System.Windows.Forms.ColumnHeader();
         this.LocationColumn = new System.Windows.Forms.ColumnHeader();
         this.RemovePlugInLink = new System.Windows.Forms.LinkLabel();
         this.AddPlugInLink = new System.Windows.Forms.LinkLabel();
         this.label1 = new System.Windows.Forms.Label();
         this.OpenPlugInDialog = new System.Windows.Forms.OpenFileDialog();
         this.SuspendLayout();
         // 
         // PlugInListBox
         // 
         this.PlugInListBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                     | System.Windows.Forms.AnchorStyles.Left)
                     | System.Windows.Forms.AnchorStyles.Right)));
         this.PlugInListBox.CheckBoxes = true;
         this.PlugInListBox.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.NameColumn,
            this.LocationColumn});
         this.PlugInListBox.Location = new System.Drawing.Point(0, 54);
         this.PlugInListBox.MultiSelect = false;
         this.PlugInListBox.Name = "PlugInListBox";
         this.PlugInListBox.Size = new System.Drawing.Size(582, 484);
         this.PlugInListBox.TabIndex = 19;
         this.PlugInListBox.UseCompatibleStateImageBehavior = false;
         this.PlugInListBox.View = System.Windows.Forms.View.Details;
         // 
         // NameColumn
         // 
         this.NameColumn.Text = "Name";
         this.NameColumn.Width = 156;
         // 
         // LocationColumn
         // 
         this.LocationColumn.Text = "Location";
         this.LocationColumn.Width = 249;
         // 
         // RemovePlugInLink
         // 
         this.RemovePlugInLink.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
         this.RemovePlugInLink.AutoSize = true;
         this.RemovePlugInLink.Location = new System.Drawing.Point(588, 70);
         this.RemovePlugInLink.Name = "RemovePlugInLink";
         this.RemovePlugInLink.Size = new System.Drawing.Size(47, 13);
         this.RemovePlugInLink.TabIndex = 21;
         this.RemovePlugInLink.TabStop = true;
         this.RemovePlugInLink.Text = "Remove";
         this.RemovePlugInLink.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.OnRemovePlugInClick);
         // 
         // AddPlugInLink
         // 
         this.AddPlugInLink.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
         this.AddPlugInLink.AutoSize = true;
         this.AddPlugInLink.Location = new System.Drawing.Point(587, 54);
         this.AddPlugInLink.Name = "AddPlugInLink";
         this.AddPlugInLink.Size = new System.Drawing.Size(26, 13);
         this.AddPlugInLink.TabIndex = 20;
         this.AddPlugInLink.TabStop = true;
         this.AddPlugInLink.Text = "Add";
         this.AddPlugInLink.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.OnAddPlugInClick);
         // 
         // label1
         // 
         this.label1.AutoSize = true;
         this.label1.Location = new System.Drawing.Point(4, 35);
         this.label1.Name = "label1";
         this.label1.Size = new System.Drawing.Size(299, 13);
         this.label1.TabIndex = 22;
         this.label1.Text = "These plugins override the default ones in the Options screen.";
         // 
         // OpenPlugInDialog
         // 
         this.OpenPlugInDialog.DefaultExt = "*.xml";
         this.OpenPlugInDialog.Filter = "PlugIn files (*.xml)|*.xml|All Files (*.*)|*.*";
         this.OpenPlugInDialog.RestoreDirectory = true;
         this.OpenPlugInDialog.Title = "Select a plugin to load";
         // 
         // PlugInsUI
         // 
         this.Controls.Add(this.label1);
         this.Controls.Add(this.RemovePlugInLink);
         this.Controls.Add(this.AddPlugInLink);
         this.Controls.Add(this.PlugInListBox);
         this.Name = "PlugInsUI";
         this.Controls.SetChildIndex(this.MyHelpLabel, 0);
         this.Controls.SetChildIndex(this.PlugInListBox, 0);
         this.Controls.SetChildIndex(this.AddPlugInLink, 0);
         this.Controls.SetChildIndex(this.RemovePlugInLink, 0);
         this.Controls.SetChildIndex(this.label1, 0);
         this.ResumeLayout(false);
         this.PerformLayout();

         }

      #endregion

      private System.Windows.Forms.ListView PlugInListBox;
      private System.Windows.Forms.ColumnHeader NameColumn;
      private System.Windows.Forms.ColumnHeader LocationColumn;
      internal System.Windows.Forms.LinkLabel RemovePlugInLink;
      internal System.Windows.Forms.LinkLabel AddPlugInLink;
      private System.Windows.Forms.Label label1;
      internal System.Windows.Forms.OpenFileDialog OpenPlugInDialog;
      }
   }
