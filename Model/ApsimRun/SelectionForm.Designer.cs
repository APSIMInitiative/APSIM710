namespace ApsimRun
   {
   partial class SelectionForm
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
         System.Windows.Forms.Label label1;
         this.SimulationList = new System.Windows.Forms.ListBox();
         this.OkButton = new System.Windows.Forms.Button();
         label1 = new System.Windows.Forms.Label();
         this.SuspendLayout();
         // 
         // label1
         // 
         label1.Location = new System.Drawing.Point(12, 10);
         label1.Name = "label1";
         label1.Size = new System.Drawing.Size(306, 43);
         label1.TabIndex = 1;
         label1.Text = "Select the simulations you want to run. Use Ctrl and Shift to select multiple sim" +
             "ulations.";
         // 
         // SimulationList
         // 
         this.SimulationList.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                     | System.Windows.Forms.AnchorStyles.Left)
                     | System.Windows.Forms.AnchorStyles.Right)));
         this.SimulationList.FormattingEnabled = true;
         this.SimulationList.Location = new System.Drawing.Point(15, 56);
         this.SimulationList.Name = "SimulationList";
         this.SimulationList.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended;
         this.SimulationList.Size = new System.Drawing.Size(385, 290);
         this.SimulationList.TabIndex = 0;
         this.SimulationList.KeyDown += new System.Windows.Forms.KeyEventHandler(this.OnKeyDown);
         // 
         // OkButton
         // 
         this.OkButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
         this.OkButton.DialogResult = System.Windows.Forms.DialogResult.OK;
         this.OkButton.Location = new System.Drawing.Point(325, 12);
         this.OkButton.Name = "OkButton";
         this.OkButton.Size = new System.Drawing.Size(75, 23);
         this.OkButton.TabIndex = 2;
         this.OkButton.Text = "Ok";
         this.OkButton.UseVisualStyleBackColor = true;
         // 
         // SelectionForm
         // 
         this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
         this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
         this.ClientSize = new System.Drawing.Size(412, 365);
         this.ControlBox = false;
         this.Controls.Add(this.OkButton);
         this.Controls.Add(label1);
         this.Controls.Add(this.SimulationList);
         this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow;
         this.Name = "SelectionForm";
         this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
         this.Text = "SelectionForm";
         this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.OnClosing);
         this.ResumeLayout(false);

         }

      #endregion

      private System.Windows.Forms.ListBox SimulationList;
      private System.Windows.Forms.Button OkButton;
      }
   }