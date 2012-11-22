namespace CSUserInterface
{
    partial class InitWaterUI
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
            this.label7 = new System.Windows.Forms.Label();
            this.groupBox3 = new System.Windows.Forms.GroupBox();
            this.RelativeToCombo = new System.Windows.Forms.ComboBox();
            this.label4 = new System.Windows.Forms.Label();
            this.Label2 = new System.Windows.Forms.Label();
            this.PAWEdit = new System.Windows.Forms.TextBox();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.Label3 = new System.Windows.Forms.Label();
            this.DepthEdit = new System.Windows.Forms.TextBox();
            this.label6 = new System.Windows.Forms.Label();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.EvenlyDistributedRadio = new System.Windows.Forms.RadioButton();
            this.FilledFromTopRadio = new System.Windows.Forms.RadioButton();
            this.Label1 = new System.Windows.Forms.Label();
            this.PercentEdit = new System.Windows.Forms.NumericUpDown();
            this.label5 = new System.Windows.Forms.Label();
            this.splitter1 = new System.Windows.Forms.Splitter();
            this.SoilGraph = new Graph.SoilGraphUI();
            this.panel1.SuspendLayout();
            this.groupBox3.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.groupBox1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.PercentEdit)).BeginInit();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Size = new System.Drawing.Size(743, 16);
            // 
            // panel1
            // 
            this.panel1.Controls.Add(this.label7);
            this.panel1.Controls.Add(this.groupBox3);
            this.panel1.Controls.Add(this.groupBox2);
            this.panel1.Controls.Add(this.label6);
            this.panel1.Controls.Add(this.groupBox1);
            this.panel1.Controls.Add(this.label5);
            this.panel1.Dock = System.Windows.Forms.DockStyle.Left;
            this.panel1.Location = new System.Drawing.Point(0, 16);
            this.panel1.Name = "panel1";
            this.panel1.Size = new System.Drawing.Size(296, 701);
            this.panel1.TabIndex = 2;
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(8, 292);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(23, 13);
            this.label7.TabIndex = 33;
            this.label7.Text = "OR";
            // 
            // groupBox3
            // 
            this.groupBox3.Controls.Add(this.RelativeToCombo);
            this.groupBox3.Controls.Add(this.label4);
            this.groupBox3.Controls.Add(this.Label2);
            this.groupBox3.Controls.Add(this.PAWEdit);
            this.groupBox3.Location = new System.Drawing.Point(24, 323);
            this.groupBox3.Name = "groupBox3";
            this.groupBox3.Size = new System.Drawing.Size(266, 100);
            this.groupBox3.TabIndex = 32;
            this.groupBox3.TabStop = false;
            this.groupBox3.Text = "Specifying a plant available water (PAW) directly";
            // 
            // RelativeToCombo
            // 
            this.RelativeToCombo.FormattingEnabled = true;
            this.RelativeToCombo.Location = new System.Drawing.Point(73, 73);
            this.RelativeToCombo.Name = "RelativeToCombo";
            this.RelativeToCombo.Size = new System.Drawing.Size(137, 21);
            this.RelativeToCombo.TabIndex = 22;
            this.RelativeToCombo.TextChanged += new System.EventHandler(this.OnPAWChanged);
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(8, 76);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(61, 13);
            this.label4.TabIndex = 21;
            this.label4.Text = "Relative to:";
            // 
            // Label2
            // 
            this.Label2.AutoSize = true;
            this.Label2.Location = new System.Drawing.Point(70, 37);
            this.Label2.Name = "Label2";
            this.Label2.Size = new System.Drawing.Size(52, 13);
            this.Label2.TabIndex = 20;
            this.Label2.Text = "mm water";
            // 
            // PAWEdit
            // 
            this.PAWEdit.Location = new System.Drawing.Point(9, 33);
            this.PAWEdit.Name = "PAWEdit";
            this.PAWEdit.Size = new System.Drawing.Size(56, 20);
            this.PAWEdit.TabIndex = 19;
            this.PAWEdit.TextChanged += new System.EventHandler(this.OnPAWChanged);
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.Label3);
            this.groupBox2.Controls.Add(this.DepthEdit);
            this.groupBox2.Location = new System.Drawing.Point(24, 195);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(266, 78);
            this.groupBox2.TabIndex = 31;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Specifying a depth of wet soil";
            // 
            // Label3
            // 
            this.Label3.AutoSize = true;
            this.Label3.Location = new System.Drawing.Point(70, 33);
            this.Label3.Name = "Label3";
            this.Label3.Size = new System.Drawing.Size(59, 13);
            this.Label3.TabIndex = 16;
            this.Label3.Text = "cm wet soil";
            // 
            // DepthEdit
            // 
            this.DepthEdit.Location = new System.Drawing.Point(9, 30);
            this.DepthEdit.Name = "DepthEdit";
            this.DepthEdit.Size = new System.Drawing.Size(53, 20);
            this.DepthEdit.TabIndex = 15;
            this.DepthEdit.TextChanged += new System.EventHandler(this.OnDepthWetSoilChanged);
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Location = new System.Drawing.Point(8, 170);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(23, 13);
            this.label6.TabIndex = 30;
            this.label6.Text = "OR";
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.EvenlyDistributedRadio);
            this.groupBox1.Controls.Add(this.FilledFromTopRadio);
            this.groupBox1.Controls.Add(this.Label1);
            this.groupBox1.Controls.Add(this.PercentEdit);
            this.groupBox1.Location = new System.Drawing.Point(24, 50);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(266, 104);
            this.groupBox1.TabIndex = 29;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "Specifying a fraction of maximum available water";
            // 
            // EvenlyDistributedRadio
            // 
            this.EvenlyDistributedRadio.Location = new System.Drawing.Point(115, 63);
            this.EvenlyDistributedRadio.Name = "EvenlyDistributedRadio";
            this.EvenlyDistributedRadio.Size = new System.Drawing.Size(113, 20);
            this.EvenlyDistributedRadio.TabIndex = 20;
            this.EvenlyDistributedRadio.Text = "Evenly distributed";
            this.EvenlyDistributedRadio.CheckedChanged += new System.EventHandler(this.OnPercentChanged);
            // 
            // FilledFromTopRadio
            // 
            this.FilledFromTopRadio.Checked = true;
            this.FilledFromTopRadio.Location = new System.Drawing.Point(115, 37);
            this.FilledFromTopRadio.Name = "FilledFromTopRadio";
            this.FilledFromTopRadio.Size = new System.Drawing.Size(127, 20);
            this.FilledFromTopRadio.TabIndex = 19;
            this.FilledFromTopRadio.TabStop = true;
            this.FilledFromTopRadio.Text = "Filled from top";
            this.FilledFromTopRadio.CheckedChanged += new System.EventHandler(this.OnPercentChanged);
            // 
            // Label1
            // 
            this.Label1.AutoSize = true;
            this.Label1.Location = new System.Drawing.Point(70, 40);
            this.Label1.Name = "Label1";
            this.Label1.Size = new System.Drawing.Size(31, 13);
            this.Label1.TabIndex = 18;
            this.Label1.Text = "% full";
            // 
            // PercentEdit
            // 
            this.PercentEdit.Location = new System.Drawing.Point(9, 37);
            this.PercentEdit.Name = "PercentEdit";
            this.PercentEdit.Size = new System.Drawing.Size(55, 20);
            this.PercentEdit.TabIndex = 17;
            this.PercentEdit.Value = new decimal(new int[] {
            100,
            0,
            0,
            0});
            this.PercentEdit.ValueChanged += new System.EventHandler(this.OnPercentChanged);
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(8, 25);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(184, 13);
            this.label5.TabIndex = 27;
            this.label5.Text = "You can specify the starting water by:";
            // 
            // splitter1
            // 
            this.splitter1.BackColor = System.Drawing.SystemColors.Control;
            this.splitter1.Location = new System.Drawing.Point(296, 16);
            this.splitter1.Name = "splitter1";
            this.splitter1.Size = new System.Drawing.Size(3, 701);
            this.splitter1.TabIndex = 21;
            this.splitter1.TabStop = false;
            // 
            // SoilGraph
            // 
            this.SoilGraph.AutoScroll = true;
            this.SoilGraph.BackColor = System.Drawing.SystemColors.Window;
            this.SoilGraph.Dock = System.Windows.Forms.DockStyle.Fill;
            this.SoilGraph.HelpText = "";
            this.SoilGraph.Location = new System.Drawing.Point(299, 16);
            this.SoilGraph.Name = "SoilGraph";
            this.SoilGraph.Size = new System.Drawing.Size(444, 701);
            this.SoilGraph.TabIndex = 22;
            this.SoilGraph.UserHasChangedProperties = false;
            // 
            // InitWaterUI
            // 
            this.Controls.Add(this.SoilGraph);
            this.Controls.Add(this.splitter1);
            this.Controls.Add(this.panel1);
            this.Name = "InitWaterUI";
            this.Size = new System.Drawing.Size(743, 717);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.panel1, 0);
            this.Controls.SetChildIndex(this.splitter1, 0);
            this.Controls.SetChildIndex(this.SoilGraph, 0);
            this.panel1.ResumeLayout(false);
            this.panel1.PerformLayout();
            this.groupBox3.ResumeLayout(false);
            this.groupBox3.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.PercentEdit)).EndInit();
            this.ResumeLayout(false);

        }
        #endregion

        private System.Windows.Forms.Panel panel1;
        private System.Windows.Forms.Splitter splitter1;
        private System.Windows.Forms.GroupBox groupBox3;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.Label Label3;
        private System.Windows.Forms.TextBox DepthEdit;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.RadioButton EvenlyDistributedRadio;
        private System.Windows.Forms.RadioButton FilledFromTopRadio;
        private System.Windows.Forms.Label Label1;
        private System.Windows.Forms.NumericUpDown PercentEdit;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.ComboBox RelativeToCombo;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.Label Label2;
        private System.Windows.Forms.TextBox PAWEdit;
        private Graph.SoilGraphUI SoilGraph;
    }
}
