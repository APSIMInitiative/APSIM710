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
            this.components = new System.ComponentModel.Container();
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(RotPlotUI));
            this.dlgOpen = new System.Windows.Forms.OpenFileDialog();
            this.imageList1 = new System.Windows.Forms.ImageList(this.components);
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.tclUI1 = new VBUserInterface.TclUI();
            this.tabPage3 = new System.Windows.Forms.TabPage();
            this.split1 = new System.Windows.Forms.SplitContainer();
            this.panel1 = new System.Windows.Forms.Panel();
            this.panel2 = new System.Windows.Forms.Panel();
            this.split2 = new System.Windows.Forms.SplitContainer();
            this.pnlLegend = new System.Windows.Forms.Panel();
            this.treeView1 = new System.Windows.Forms.TreeView();
            this.panel3 = new System.Windows.Forms.Panel();
            this.button1 = new System.Windows.Forms.Button();
            this.label11 = new System.Windows.Forms.Label();
            this.txtFile = new System.Windows.Forms.TextBox();
            this.panel5 = new System.Windows.Forms.Panel();
            this.spnDate = new System.Windows.Forms.VScrollBar();
            this.lblDayOfYear = new System.Windows.Forms.Label();
            this.label10 = new System.Windows.Forms.Label();
            this.calSelected = new System.Windows.Forms.DateTimePicker();
            this.label1 = new System.Windows.Forms.Label();
            this.chkFit = new System.Windows.Forms.CheckBox();
            this.label8 = new System.Windows.Forms.Label();
            this.calEnd = new System.Windows.Forms.DateTimePicker();
            this.label6 = new System.Windows.Forms.Label();
            this.calStart = new System.Windows.Forms.DateTimePicker();
            this.lblFileName = new System.Windows.Forms.Label();
            this.tabControl1.SuspendLayout();
            this.tabPage1.SuspendLayout();
            this.tabPage3.SuspendLayout();
            this.split1.Panel1.SuspendLayout();
            this.split1.Panel2.SuspendLayout();
            this.split1.SuspendLayout();
            this.panel1.SuspendLayout();
            this.split2.Panel1.SuspendLayout();
            this.split2.Panel2.SuspendLayout();
            this.split2.SuspendLayout();
            this.panel5.SuspendLayout();
            this.SuspendLayout();
            // 
            // dlgOpen
            // 
            this.dlgOpen.Filter = "xml Files|*.xml|log Files|*.log";
            // 
            // imageList1
            // 
            this.imageList1.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("imageList1.ImageStream")));
            this.imageList1.TransparentColor = System.Drawing.Color.Transparent;
            this.imageList1.Images.SetKeyName(0, "check216.png");
            this.imageList1.Images.SetKeyName(1, "delete216.png");
            this.imageList1.Images.SetKeyName(2, "layout_center16.png");
            this.imageList1.Images.SetKeyName(3, "scroll16.png");
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tabPage1);
            this.tabControl1.Controls.Add(this.tabPage3);
            this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabControl1.Location = new System.Drawing.Point(0, 16);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(655, 525);
            this.tabControl1.TabIndex = 0;
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.tclUI1);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(647, 499);
            this.tabPage1.TabIndex = 0;
            this.tabPage1.Text = "Rotations";
            this.tabPage1.UseVisualStyleBackColor = true;
            // 
            // tclUI1
            // 
            this.tclUI1.AutoScroll = true;
            this.tclUI1.AutoSize = true;
            this.tclUI1.AutoSizeMode = System.Windows.Forms.AutoSizeMode.GrowAndShrink;
            this.tclUI1.BackColor = System.Drawing.SystemColors.Window;
            this.tclUI1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tclUI1.HelpText = "";
            this.tclUI1.Location = new System.Drawing.Point(3, 3);
            this.tclUI1.Name = "tclUI1";
            this.tclUI1.Size = new System.Drawing.Size(641, 493);
            this.tclUI1.TabIndex = 1;
            // 
            // tabPage3
            // 
            this.tabPage3.Controls.Add(this.lblFileName);
            this.tabPage3.Controls.Add(this.split1);
            this.tabPage3.Controls.Add(this.txtFile);
            this.tabPage3.Controls.Add(this.label11);
            this.tabPage3.Controls.Add(this.button1);
            this.tabPage3.Controls.Add(this.panel5);
            this.tabPage3.Location = new System.Drawing.Point(4, 22);
            this.tabPage3.Name = "tabPage3";
            this.tabPage3.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage3.Size = new System.Drawing.Size(647, 499);
            this.tabPage3.TabIndex = 2;
            this.tabPage3.Text = "Results";
            this.tabPage3.UseVisualStyleBackColor = true;
            // 
            // split1
            // 
            this.split1.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                        | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.split1.FixedPanel = System.Windows.Forms.FixedPanel.Panel2;
            this.split1.Location = new System.Drawing.Point(3, 39);
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
            this.split1.Size = new System.Drawing.Size(641, 439);
            this.split1.SplitterDistance = 435;
            this.split1.TabIndex = 43;
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
            this.panel1.Size = new System.Drawing.Size(435, 439);
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
            this.split2.Panel1.AutoScroll = true;
            this.split2.Panel1.Controls.Add(this.pnlLegend);
            // 
            // split2.Panel2
            // 
            this.split2.Panel2.Controls.Add(this.treeView1);
            this.split2.Panel2.Controls.Add(this.panel3);
            this.split2.Size = new System.Drawing.Size(202, 439);
            this.split2.SplitterDistance = 103;
            this.split2.TabIndex = 1;
            // 
            // pnlLegend
            // 
            this.pnlLegend.BackColor = System.Drawing.SystemColors.Window;
            this.pnlLegend.Dock = System.Windows.Forms.DockStyle.Top;
            this.pnlLegend.Location = new System.Drawing.Point(0, 0);
            this.pnlLegend.Name = "pnlLegend";
            this.pnlLegend.Size = new System.Drawing.Size(183, 250);
            this.pnlLegend.TabIndex = 40;
            this.pnlLegend.Paint += new System.Windows.Forms.PaintEventHandler(this.pnlLegend_Paint);
            // 
            // treeView1
            // 
            this.treeView1.BorderStyle = System.Windows.Forms.BorderStyle.None;
            this.treeView1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.treeView1.ImageIndex = 2;
            this.treeView1.ImageList = this.imageList1;
            this.treeView1.Location = new System.Drawing.Point(0, 226);
            this.treeView1.Name = "treeView1";
            this.treeView1.SelectedImageIndex = 0;
            this.treeView1.Size = new System.Drawing.Size(200, 104);
            this.treeView1.TabIndex = 45;
            // 
            // panel3
            // 
            this.panel3.BackColor = System.Drawing.SystemColors.Window;
            this.panel3.Dock = System.Windows.Forms.DockStyle.Top;
            this.panel3.ForeColor = System.Drawing.SystemColors.ControlText;
            this.panel3.Location = new System.Drawing.Point(0, 0);
            this.panel3.Name = "panel3";
            this.panel3.Size = new System.Drawing.Size(200, 226);
            this.panel3.TabIndex = 43;
            this.panel3.Visible = false;
            // 
            // button1
            // 
            this.button1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.button1.Location = new System.Drawing.Point(591, 473);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(50, 23);
            this.button1.TabIndex = 42;
            this.button1.Text = "Browse";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Visible = false;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // label11
            // 
            this.label11.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.label11.AutoSize = true;
            this.label11.Location = new System.Drawing.Point(4, 481);
            this.label11.Name = "label11";
            this.label11.Size = new System.Drawing.Size(44, 13);
            this.label11.TabIndex = 41;
            this.label11.Text = "Source:";
            this.label11.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // txtFile
            // 
            this.txtFile.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.txtFile.Location = new System.Drawing.Point(534, 475);
            this.txtFile.Name = "txtFile";
            this.txtFile.Size = new System.Drawing.Size(51, 20);
            this.txtFile.TabIndex = 40;
            this.txtFile.Visible = false;
            // 
            // panel5
            // 
            this.panel5.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.panel5.AutoScroll = true;
            this.panel5.Controls.Add(this.spnDate);
            this.panel5.Controls.Add(this.lblDayOfYear);
            this.panel5.Controls.Add(this.label10);
            this.panel5.Controls.Add(this.calSelected);
            this.panel5.Controls.Add(this.label1);
            this.panel5.Controls.Add(this.chkFit);
            this.panel5.Controls.Add(this.label8);
            this.panel5.Controls.Add(this.calEnd);
            this.panel5.Controls.Add(this.label6);
            this.panel5.Controls.Add(this.calStart);
            this.panel5.Location = new System.Drawing.Point(0, 1);
            this.panel5.Name = "panel5";
            this.panel5.Size = new System.Drawing.Size(647, 34);
            this.panel5.TabIndex = 39;
            this.panel5.Visible = false;
            // 
            // spnDate
            // 
            this.spnDate.Location = new System.Drawing.Point(457, 0);
            this.spnDate.Name = "spnDate";
            this.spnDate.Size = new System.Drawing.Size(17, 34);
            this.spnDate.TabIndex = 31;
            this.spnDate.ValueChanged += new System.EventHandler(this.spnDate_ValueChanged);
            // 
            // lblDayOfYear
            // 
            this.lblDayOfYear.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
            this.lblDayOfYear.Location = new System.Drawing.Point(508, 9);
            this.lblDayOfYear.Name = "lblDayOfYear";
            this.lblDayOfYear.Size = new System.Drawing.Size(34, 15);
            this.lblDayOfYear.TabIndex = 44;
            this.lblDayOfYear.Text = "365";
            this.lblDayOfYear.TextAlign = System.Drawing.ContentAlignment.MiddleCenter;
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(310, 9);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(49, 13);
            this.label10.TabIndex = 30;
            this.label10.Text = "Selected";
            // 
            // calSelected
            // 
            this.calSelected.Format = System.Windows.Forms.DateTimePickerFormat.Short;
            this.calSelected.Location = new System.Drawing.Point(365, 6);
            this.calSelected.MaxDate = new System.DateTime(3000, 12, 31, 0, 0, 0, 0);
            this.calSelected.MinDate = new System.DateTime(1800, 1, 1, 0, 0, 0, 0);
            this.calSelected.Name = "calSelected";
            this.calSelected.Size = new System.Drawing.Size(91, 20);
            this.calSelected.TabIndex = 29;
            this.calSelected.ValueChanged += new System.EventHandler(this.calSelected_ValueChanged);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(481, 9);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(26, 13);
            this.label1.TabIndex = 43;
            this.label1.Text = "Day";
            // 
            // chkFit
            // 
            this.chkFit.AutoSize = true;
            this.chkFit.Checked = true;
            this.chkFit.CheckState = System.Windows.Forms.CheckState.Checked;
            this.chkFit.Location = new System.Drawing.Point(558, 10);
            this.chkFit.Name = "chkFit";
            this.chkFit.Size = new System.Drawing.Size(75, 17);
            this.chkFit.TabIndex = 42;
            this.chkFit.Text = "Fit to Form";
            this.chkFit.UseVisualStyleBackColor = true;
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(157, 10);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(52, 13);
            this.label8.TabIndex = 41;
            this.label8.Text = "End Date";
            // 
            // calEnd
            // 
            this.calEnd.Enabled = false;
            this.calEnd.Format = System.Windows.Forms.DateTimePickerFormat.Short;
            this.calEnd.Location = new System.Drawing.Point(213, 6);
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
            this.calStart.Location = new System.Drawing.Point(61, 6);
            this.calStart.MaxDate = new System.DateTime(3000, 12, 31, 0, 0, 0, 0);
            this.calStart.MinDate = new System.DateTime(1800, 1, 1, 0, 0, 0, 0);
            this.calStart.Name = "calStart";
            this.calStart.Size = new System.Drawing.Size(86, 20);
            this.calStart.TabIndex = 38;
            // 
            // lblFileName
            // 
            this.lblFileName.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
            this.lblFileName.AutoSize = true;
            this.lblFileName.Location = new System.Drawing.Point(47, 481);
            this.lblFileName.Name = "lblFileName";
            this.lblFileName.Size = new System.Drawing.Size(52, 13);
            this.lblFileName.TabIndex = 44;
            this.lblFileName.Text = "Filename:";
            this.lblFileName.TextAlign = System.Drawing.ContentAlignment.MiddleLeft;
            // 
            // RotPlotUI
            // 
            this.Controls.Add(this.tabControl1);
            this.HelpText = "Rotation Plot";
            this.Name = "RotPlotUI";
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.tabControl1, 0);
            this.tabControl1.ResumeLayout(false);
            this.tabPage1.ResumeLayout(false);
            this.tabPage1.PerformLayout();
            this.tabPage3.ResumeLayout(false);
            this.tabPage3.PerformLayout();
            this.split1.Panel1.ResumeLayout(false);
            this.split1.Panel2.ResumeLayout(false);
            this.split1.ResumeLayout(false);
            this.panel1.ResumeLayout(false);
            this.split2.Panel1.ResumeLayout(false);
            this.split2.Panel2.ResumeLayout(false);
            this.split2.ResumeLayout(false);
            this.panel5.ResumeLayout(false);
            this.panel5.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.OpenFileDialog dlgOpen;
        private System.Windows.Forms.ImageList imageList1;
        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tabPage1;
        private System.Windows.Forms.TabPage tabPage3;
        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.Label label11;
        private System.Windows.Forms.TextBox txtFile;
        private System.Windows.Forms.Panel panel5;
        private System.Windows.Forms.VScrollBar spnDate;
        private System.Windows.Forms.Label lblDayOfYear;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.DateTimePicker calSelected;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.CheckBox chkFit;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.DateTimePicker calEnd;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.DateTimePicker calStart;
        private System.Windows.Forms.SplitContainer split1;
        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Panel panel2;
        private System.Windows.Forms.SplitContainer split2;
        private System.Windows.Forms.Panel pnlLegend;
        private System.Windows.Forms.TreeView treeView1;
        private System.Windows.Forms.Panel panel3;
        private VBUserInterface.TclUI tclUI1;
        private System.Windows.Forms.Label lblFileName;
    }
}
