namespace GraphUserInterface
    {
    partial class StatsUI
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
            this.GroupBox = new System.Windows.Forms.GroupBox();
            this.label3 = new System.Windows.Forms.Label();
            this.RollingMean = new System.Windows.Forms.NumericUpDown();
            this.label2 = new System.Windows.Forms.Label();
            this.StatsList = new System.Windows.Forms.CheckedListBox();
            this.label1 = new System.Windows.Forms.Label();
            this.FieldList = new System.Windows.Forms.CheckedListBox();
            this.IncludeZeros = new System.Windows.Forms.CheckBox();
            this.GroupBox.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.RollingMean)).BeginInit();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Size = new System.Drawing.Size(207, 16);
            // 
            // GroupBox
            // 
            this.GroupBox.Controls.Add(this.IncludeZeros);
            this.GroupBox.Controls.Add(this.label3);
            this.GroupBox.Controls.Add(this.RollingMean);
            this.GroupBox.Controls.Add(this.label2);
            this.GroupBox.Controls.Add(this.StatsList);
            this.GroupBox.Controls.Add(this.label1);
            this.GroupBox.Controls.Add(this.FieldList);
            this.GroupBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GroupBox.Location = new System.Drawing.Point(0, 16);
            this.GroupBox.Name = "GroupBox";
            this.GroupBox.Size = new System.Drawing.Size(207, 181);
            this.GroupBox.TabIndex = 2;
            this.GroupBox.TabStop = false;
            this.GroupBox.Text = "GroupBox";
            // 
            // label3
            // 
            this.label3.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(108, 133);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(72, 13);
            this.label3.TabIndex = 7;
            this.label3.Text = "Rolling Mean:";
            // 
            // RollingMean
            // 
            this.RollingMean.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.RollingMean.Location = new System.Drawing.Point(111, 149);
            this.RollingMean.Name = "RollingMean";
            this.RollingMean.Size = new System.Drawing.Size(90, 20);
            this.RollingMean.TabIndex = 6;
            this.RollingMean.ValueChanged += new System.EventHandler(this.OnRollingMeanChanged);
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(108, 38);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(34, 13);
            this.label2.TabIndex = 3;
            this.label2.Text = "Stats:";
            // 
            // StatsList
            // 
            this.StatsList.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)));
            this.StatsList.FormattingEnabled = true;
            this.StatsList.Items.AddRange(new object[] {
            "Mean",
            "Count",
            "Minimum",
            "Maximum",
            "Sum",
            "10",
            "20",
            "30",
            "40",
            "50",
            "60",
            "70",
            "80",
            "90"});
            this.StatsList.Location = new System.Drawing.Point(110, 54);
            this.StatsList.Name = "StatsList";
            this.StatsList.Size = new System.Drawing.Size(90, 64);
            this.StatsList.TabIndex = 2;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(6, 37);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(66, 13);
            this.label1.TabIndex = 1;
            this.label1.Text = "Field names:";
            // 
            // FieldList
            // 
            this.FieldList.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)));
            this.FieldList.FormattingEnabled = true;
            this.FieldList.Location = new System.Drawing.Point(6, 53);
            this.FieldList.Name = "FieldList";
            this.FieldList.Size = new System.Drawing.Size(98, 109);
            this.FieldList.TabIndex = 0;
            // 
            // IncludeZeros
            // 
            this.IncludeZeros.AutoSize = true;
            this.IncludeZeros.Location = new System.Drawing.Point(105, 19);
            this.IncludeZeros.Name = "IncludeZeros";
            this.IncludeZeros.Size = new System.Drawing.Size(95, 17);
            this.IncludeZeros.TabIndex = 8;
            this.IncludeZeros.Text = "Include zeros?";
            this.IncludeZeros.UseVisualStyleBackColor = true;
            this.IncludeZeros.CheckedChanged += new System.EventHandler(this.OnIncludeZeros);
            // 
            // StatsUI
            // 
            this.Controls.Add(this.GroupBox);
            this.Name = "StatsUI";
            this.Size = new System.Drawing.Size(207, 197);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.GroupBox, 0);
            this.GroupBox.ResumeLayout(false);
            this.GroupBox.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.RollingMean)).EndInit();
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.GroupBox GroupBox;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.CheckedListBox StatsList;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.CheckedListBox FieldList;
       private System.Windows.Forms.Label label3;
       private System.Windows.Forms.NumericUpDown RollingMean;
       private System.Windows.Forms.CheckBox IncludeZeros;
        }
    }
