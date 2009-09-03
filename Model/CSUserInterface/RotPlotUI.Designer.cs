namespace CSUserInterface
{
    partial class RotPlotUI
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
            this.txtFile = new System.Windows.Forms.TextBox();
            this.button1 = new System.Windows.Forms.Button();
            this.label11 = new System.Windows.Forms.Label();
            this.dlgOpen = new System.Windows.Forms.OpenFileDialog();
            this.panel5 = new System.Windows.Forms.Panel();
            this.chkFit = new System.Windows.Forms.CheckBox();
            this.label8 = new System.Windows.Forms.Label();
            this.calEnd = new System.Windows.Forms.DateTimePicker();
            this.label6 = new System.Windows.Forms.Label();
            this.calStart = new System.Windows.Forms.DateTimePicker();
            this.split1 = new System.Windows.Forms.SplitContainer();
            this.panel1 = new System.Windows.Forms.Panel();
            this.panel2 = new System.Windows.Forms.Panel();
            this.split2 = new System.Windows.Forms.SplitContainer();
            this.pnlLegend = new System.Windows.Forms.Panel();
            this.treeView1 = new System.Windows.Forms.TreeView();
            this.panel3 = new System.Windows.Forms.Panel();
            this.spnDate = new System.Windows.Forms.VScrollBar();
            this.label10 = new System.Windows.Forms.Label();
            this.calSelected = new System.Windows.Forms.DateTimePicker();
            this.panel5.SuspendLayout();
            this.split1.Panel1.SuspendLayout();
            this.split1.Panel2.SuspendLayout();
            this.split1.SuspendLayout();
            this.panel1.SuspendLayout();
            this.split2.Panel1.SuspendLayout();
            this.split2.Panel2.SuspendLayout();
            this.split2.SuspendLayout();
            this.panel3.SuspendLayout();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Text = "Rotation Plot";
            // 
            // txtFile
            // 
            this.txtFile.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.txtFile.Location = new System.Drawing.Point(61, 26);
            this.txtFile.Name = "txtFile";
            this.txtFile.Size = new System.Drawing.Size(534, 20);
            this.txtFile.TabIndex = 2;
            // 
            // button1
            // 
            this.button1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.button1.Location = new System.Drawing.Point(604, 24);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(50, 23);
            this.button1.TabIndex = 29;
            this.button1.Text = "Browse";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // label11
            // 
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(3, 29);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(52, 13);
            this.label11.TabIndex = 28;
            this.label11.Text = "Filename:";
            // 
            // dlgOpen
            // 
            this.dlgOpen.Filter = "xml Files|*.xml|log Files|*.log";
            // 
            // panel5
            // 
            this.panel5.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.panel5.Controls.Add(this.chkFit);
            this.panel5.Controls.Add(this.label8);
            this.panel5.Controls.Add(this.calEnd);
            this.panel5.Controls.Add(this.label6);
            this.panel5.Controls.Add(this.calStart);
            this.panel5.Location = new System.Drawing.Point(2, 508);
            this.panel5.Name = "panel5";
            this.panel5.Size = new System.Drawing.Size(425, 32);
            this.panel5.TabIndex = 38;
            this.panel5.Visible = false;
            // 
            // chkFit
            // 
            this.chkFit.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.chkFit.AutoSize = true;
            this.chkFit.Checked = true;
            this.chkFit.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chkFit.Location = new System.Drawing.Point(328, 9);
            this.chkFit.Name = "chkFit";
            this.chkFit.Size = new System.Drawing.Size(75, 17);
            this.chkFit.TabIndex = 42;
            this.chkFit.Text = "Fit to Form";
            this.chkFit.UseVisualStyleBackColor = true;
            this.chkFit.CheckedChanged += new System.EventHandler(this.panel1_Resize);
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(163, 10);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(52, 13);
            this.label8.TabIndex = 41;
            this.label8.Text = "End Date";
            // 
            // calEnd
            // 
            this.calEnd.Enabled = false;
            this.calEnd.Format = System.Windows.Forms.DateTimePickerFormat.Short;
            this.calEnd.Location = new System.Drawing.Point(221, 6);
            this.calEnd.MaxDate = new System.DateTime(3000, 12, 31, 0, 0, 0, 0);
            this.calEnd.MinDate = new System.DateTime(1800, 1, 1, 0, 0, 0, 0);
            this.calEnd.Name = "calEnd";
            this.calEnd.Size = new System.Drawing.Size(92, 20);
            this.calEnd.TabIndex = 40;
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(4, 10);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(55, 13);
            this.label6.TabIndex = 39;
            this.label6.Text = "Start Date";
            // 
            // calStart
            // 
            this.calStart.Enabled = false;
            this.calStart.Format = System.Windows.Forms.DateTimePickerFormat.Short;
            this.calStart.Location = new System.Drawing.Point(65, 6);
            this.calStart.MaxDate = new System.DateTime(3000, 12, 31, 0, 0, 0, 0);
            this.calStart.MinDate = new System.DateTime(1800, 1, 1, 0, 0, 0, 0);
            this.calStart.Name = "calStart";
            this.calStart.Size = new System.Drawing.Size(92, 20);
            this.calStart.TabIndex = 38;
            // 
            // split1
            // 
            this.split1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.split1.FixedPanel = System.Windows.Forms.FixedPanel.Panel2;
            this.split1.Location = new System.Drawing.Point(0, 52);
            this.split1.Name = "split1";
            // 
            // split1.Panel1
            // 
            this.split1.Panel1.AutoScroll = true;
            this.split1.Panel1.BackColor = System.Drawing.SystemColors.Window;
            this.split1.Panel1.Controls.Add(this.panel1);
            // 
            // split1.Panel2
            // 
            this.split1.Panel2.AutoScroll = true;
            this.split1.Panel2.Controls.Add(this.split2);
            this.split1.Size = new System.Drawing.Size(655, 450);
            this.split1.SplitterDistance = 449;
            this.split1.TabIndex = 40;
            this.split1.Visible = false;
            // 
            // panel1
            // 
            this.panel1.AutoScroll = true;
            this.panel1.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.panel1.Controls.Add(this.panel2);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.panel1.Location = new System.Drawing.Point(0, 0);
            this.panel1.Name = "panel1";
            this.panel1.Padding = new System.Windows.Forms.Padding(8);
            this.panel1.Size = new System.Drawing.Size(449, 450);
            this.panel1.TabIndex = 31;
            this.panel1.Resize += new System.EventHandler(this.panel1_Resize);
            // 
            // panel2
            // 
            this.panel2.BackColor = System.Drawing.SystemColors.Window;
            this.panel2.Location = new System.Drawing.Point(2, -1);
            this.panel2.Name = "panel2";
            this.panel2.Size = new System.Drawing.Size(253, 300);
            this.panel2.TabIndex = 12;
            this.panel2.Paint += new System.Windows.Forms.PaintEventHandler(this.panel2_Paint);
            this.panel2.MouseDown += new System.Windows.Forms.MouseEventHandler(this.panel2_MouseDown);
            // 
            // split2
            // 
            this.split2.BackColor = System.Drawing.SystemColors.Window;
            this.split2.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.split2.Dock = System.Windows.Forms.DockStyle.Fill;
            this.split2.Location = new System.Drawing.Point(0, 0);
            this.split2.Name = "split2";
            this.split2.Orientation = System.Windows.Forms.Orientation.Horizontal;
            // 
            // split2.Panel1
            // 
            this.split2.Panel1.Controls.Add(this.pnlLegend);
            // 
            // split2.Panel2
            // 
            this.split2.Panel2.Controls.Add(this.treeView1);
            this.split2.Panel2.Controls.Add(this.panel3);
            this.split2.Size = new System.Drawing.Size(202, 450);
            this.split2.SplitterDistance = 100;
            this.split2.TabIndex = 1;
            // 
            // pnlLegend
            // 
            this.pnlLegend.BackColor = System.Drawing.SystemColors.Window;
            this.pnlLegend.Dock = System.Windows.Forms.DockStyle.Top;
            this.pnlLegend.Location = new System.Drawing.Point(0, 0);
            this.pnlLegend.Name = "pnlLegend";
            this.pnlLegend.Size = new System.Drawing.Size(200, 59);
            this.pnlLegend.TabIndex = 40;
            this.pnlLegend.Paint += new System.Windows.Forms.PaintEventHandler(this.pnlLegend_Paint);
            // 
            // treeView1
            // 
            this.treeView1.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.treeView1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.treeView1.Location = new System.Drawing.Point(0, 35);
            this.treeView1.Name = "treeView1";
            this.treeView1.Size = new System.Drawing.Size(200, 309);
            this.treeView1.TabIndex = 45;
            // 
            // panel3
            // 
            this.panel3.BackColor = System.Drawing.SystemColors.Window;
            this.panel3.Controls.Add(this.spnDate);
            this.panel3.Controls.Add(this.label10);
            this.panel3.Controls.Add(this.calSelected);
            this.panel3.Dock = System.Windows.Forms.DockStyle.Top;
            this.panel3.ForeColor = System.Drawing.SystemColors.ControlText;
            this.panel3.Location = new System.Drawing.Point(0, 0);
            this.panel3.Name = "panel3";
            this.panel3.Size = new System.Drawing.Size(200, 35);
            this.panel3.TabIndex = 43;
            // 
            // spnDate
            // 
            this.spnDate.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.spnDate.Location = new System.Drawing.Point(174, 0);
            this.spnDate.Name = "spnDate";
            this.spnDate.Size = new System.Drawing.Size(17, 34);
            this.spnDate.TabIndex = 31;
            this.spnDate.ValueChanged += new System.EventHandler(this.spnDate_ValueChanged);
            // 
            // label10
            // 
            this.label10.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(4, 10);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(75, 13);
            this.label10.TabIndex = 30;
            this.label10.Text = "Selected Date";
            // 
            // calSelected
            // 
            this.calSelected.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
            this.calSelected.Format = System.Windows.Forms.DateTimePickerFormat.Short;
            this.calSelected.Location = new System.Drawing.Point(82, 6);
            this.calSelected.MaxDate = new System.DateTime(3000, 12, 31, 0, 0, 0, 0);
            this.calSelected.MinDate = new System.DateTime(1800, 1, 1, 0, 0, 0, 0);
            this.calSelected.Name = "calSelected";
            this.calSelected.Size = new System.Drawing.Size(91, 20);
            this.calSelected.TabIndex = 29;
            this.calSelected.ValueChanged += new System.EventHandler(this.calSelected_ValueChanged);
            // 
            // RotPlotUI
            // 
            this.Controls.Add(this.panel5);
            this.Controls.Add(this.button1);
            this.Controls.Add(this.label11);
            this.Controls.Add(this.txtFile);
            this.Controls.Add(this.split1);
            this.HelpText = "Rotation Plot";
            this.Name = "RotPlotUI";
            this.Controls.SetChildIndex(this.split1, 0);
            this.Controls.SetChildIndex(this.txtFile, 0);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.label11, 0);
            this.Controls.SetChildIndex(this.button1, 0);
            this.Controls.SetChildIndex(this.panel5, 0);
            this.panel5.ResumeLayout(false);
            this.panel5.PerformLayout();
            this.split1.Panel1.ResumeLayout(false);
            this.split1.Panel2.ResumeLayout(false);
            this.split1.ResumeLayout(false);
            this.panel1.ResumeLayout(false);
            this.split2.Panel1.ResumeLayout(false);
            this.split2.Panel2.ResumeLayout(false);
            this.split2.ResumeLayout(false);
            this.panel3.ResumeLayout(false);
            this.panel3.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.TextBox txtFile;
        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.OpenFileDialog dlgOpen;
        private System.Windows.Forms.Panel panel5;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.DateTimePicker calEnd;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.DateTimePicker calStart;
        private System.Windows.Forms.CheckBox chkFit;
        private System.Windows.Forms.SplitContainer split1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Panel panel2;
        private System.Windows.Forms.SplitContainer split2;
        private System.Windows.Forms.Panel pnlLegend;
        private System.Windows.Forms.Panel panel3;
        private System.Windows.Forms.VScrollBar spnDate;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.DateTimePicker calSelected;
        private System.Windows.Forms.TreeView treeView1;
    }
}
