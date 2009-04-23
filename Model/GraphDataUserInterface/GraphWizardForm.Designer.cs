namespace GraphDataUserInterface
   {
   partial class GraphWizardForm
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
         System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(GraphWizardForm));
         this.NextButton = new System.Windows.Forms.Button();
         this.ViewPanel = new System.Windows.Forms.Panel();
         this.GraphList = new System.Windows.Forms.ListBox();
         this.GraphListLabel = new System.Windows.Forms.Label();
         this.pictureBox1 = new System.Windows.Forms.PictureBox();
         this.ViewPanel.SuspendLayout();
         ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
         this.SuspendLayout();
         // 
         // NextButton
         // 
         this.NextButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
         this.NextButton.Location = new System.Drawing.Point(736, 499);
         this.NextButton.Name = "NextButton";
         this.NextButton.Size = new System.Drawing.Size(75, 23);
         this.NextButton.TabIndex = 2;
         this.NextButton.Text = "Next";
         this.NextButton.UseVisualStyleBackColor = true;
         this.NextButton.Click += new System.EventHandler(this.OnNextButtonClick);
         // 
         // ViewPanel
         // 
         this.ViewPanel.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                     | System.Windows.Forms.AnchorStyles.Left)
                     | System.Windows.Forms.AnchorStyles.Right)));
         this.ViewPanel.Controls.Add(this.GraphList);
         this.ViewPanel.Controls.Add(this.GraphListLabel);
         this.ViewPanel.Location = new System.Drawing.Point(141, 2);
         this.ViewPanel.Name = "ViewPanel";
         this.ViewPanel.Size = new System.Drawing.Size(670, 490);
         this.ViewPanel.TabIndex = 5;
         // 
         // GraphList
         // 
         this.GraphList.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                     | System.Windows.Forms.AnchorStyles.Left)
                     | System.Windows.Forms.AnchorStyles.Right)));
         this.GraphList.FormattingEnabled = true;
         this.GraphList.Location = new System.Drawing.Point(17, 23);
         this.GraphList.Name = "GraphList";
         this.GraphList.Size = new System.Drawing.Size(645, 459);
         this.GraphList.TabIndex = 6;
         // 
         // GraphListLabel
         // 
         this.GraphListLabel.AutoSize = true;
         this.GraphListLabel.Location = new System.Drawing.Point(14, 7);
         this.GraphListLabel.Name = "GraphListLabel";
         this.GraphListLabel.Size = new System.Drawing.Size(168, 13);
         this.GraphListLabel.TabIndex = 5;
         this.GraphListLabel.Text = "Select the type of graph to create.";
         // 
         // pictureBox1
         // 
         this.pictureBox1.Image = ((System.Drawing.Image)(resources.GetObject("pictureBox1.Image")));
         this.pictureBox1.Location = new System.Drawing.Point(2, 2);
         this.pictureBox1.Name = "pictureBox1";
         this.pictureBox1.Size = new System.Drawing.Size(133, 464);
         this.pictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
         this.pictureBox1.TabIndex = 7;
         this.pictureBox1.TabStop = false;
         // 
         // GraphWizardForm
         // 
         this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
         this.BackColor = System.Drawing.SystemColors.Window;
         this.ClientSize = new System.Drawing.Size(815, 534);
         this.Controls.Add(this.pictureBox1);
         this.Controls.Add(this.ViewPanel);
         this.Controls.Add(this.NextButton);
         this.Name = "GraphWizardForm";
         this.Text = "Create a graph wizard";
         this.ViewPanel.ResumeLayout(false);
         this.ViewPanel.PerformLayout();
         ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
         this.ResumeLayout(false);

         }

      #endregion

      private System.Windows.Forms.Button NextButton;
      private System.Windows.Forms.Panel ViewPanel;
      private System.Windows.Forms.ListBox GraphList;
      private System.Windows.Forms.Label GraphListLabel;
      private System.Windows.Forms.PictureBox pictureBox1;
      }
   }