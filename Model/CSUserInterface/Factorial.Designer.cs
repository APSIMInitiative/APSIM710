namespace CSUserInterface
{
    partial class Factorial
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(Factorial));
            System.Windows.Forms.TreeNode treeNode1 = new System.Windows.Forms.TreeNode("15 Oct");
            System.Windows.Forms.TreeNode treeNode2 = new System.Windows.Forms.TreeNode("15 Nov");
            System.Windows.Forms.TreeNode treeNode3 = new System.Windows.Forms.TreeNode("15 Dec");
            System.Windows.Forms.TreeNode treeNode4 = new System.Windows.Forms.TreeNode("15 Jan");
            System.Windows.Forms.TreeNode treeNode5 = new System.Windows.Forms.TreeNode("Sorghum Sowing Rule: Sow Date", 2, 2, new System.Windows.Forms.TreeNode[] {
            treeNode1,
            treeNode2,
            treeNode3,
            treeNode4});
            System.Windows.Forms.TreeNode treeNode6 = new System.Windows.Forms.TreeNode("solid");
            System.Windows.Forms.TreeNode treeNode7 = new System.Windows.Forms.TreeNode("single");
            System.Windows.Forms.TreeNode treeNode8 = new System.Windows.Forms.TreeNode("double");
            System.Windows.Forms.TreeNode treeNode9 = new System.Windows.Forms.TreeNode("Sorghum Sowing Rule: skip row", 2, 2, new System.Windows.Forms.TreeNode[] {
            treeNode6,
            treeNode7,
            treeNode8});
            System.Windows.Forms.TreeNode treeNode10 = new System.Windows.Forms.TreeNode("3.5");
            System.Windows.Forms.TreeNode treeNode11 = new System.Windows.Forms.TreeNode("5");
            System.Windows.Forms.TreeNode treeNode12 = new System.Windows.Forms.TreeNode("7.5");
            System.Windows.Forms.TreeNode treeNode13 = new System.Windows.Forms.TreeNode("Sorghum Sowing Rule: Density", 2, 2, new System.Windows.Forms.TreeNode[] {
            treeNode10,
            treeNode11,
            treeNode12});
            System.Windows.Forms.TreeNode treeNode14 = new System.Windows.Forms.TreeNode("15 Sep");
            System.Windows.Forms.TreeNode treeNode15 = new System.Windows.Forms.TreeNode("15 Oct");
            System.Windows.Forms.TreeNode treeNode16 = new System.Windows.Forms.TreeNode("15 Nov");
            System.Windows.Forms.TreeNode treeNode17 = new System.Windows.Forms.TreeNode("15 Dec");
            System.Windows.Forms.TreeNode treeNode18 = new System.Windows.Forms.TreeNode("15 Jan");
            System.Windows.Forms.TreeNode treeNode19 = new System.Windows.Forms.TreeNode("Sorghum Sowing Rule: Sow Date", 2, 2, new System.Windows.Forms.TreeNode[] {
            treeNode14,
            treeNode15,
            treeNode16,
            treeNode17,
            treeNode18});
            System.Windows.Forms.TreeNode treeNode20 = new System.Windows.Forms.TreeNode("FactorLevel", 1, 1, new System.Windows.Forms.TreeNode[] {
            treeNode19});
            System.Windows.Forms.TreeNode treeNode21 = new System.Windows.Forms.TreeNode("Factorial", 3, 3, new System.Windows.Forms.TreeNode[] {
            treeNode5,
            treeNode9,
            treeNode13,
            treeNode20});
            this.btnGenerate = new System.Windows.Forms.Button();
            this.imageList1 = new System.Windows.Forms.ImageList(this.components);
            this.treeSims = new System.Windows.Forms.TreeView();
            this.label1 = new System.Windows.Forms.Label();
            this.txtTotalSims = new System.Windows.Forms.TextBox();
            this.groupBox1 = new System.Windows.Forms.GroupBox();
            this.radCount = new System.Windows.Forms.RadioButton();
            this.radDesc = new System.Windows.Forms.RadioButton();
            this.groupBox2 = new System.Windows.Forms.GroupBox();
            this.radMultiple = new System.Windows.Forms.RadioButton();
            this.radSingle = new System.Windows.Forms.RadioButton();
            this.groupBox1.SuspendLayout();
            this.groupBox2.SuspendLayout();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
//            this.MyHelpLabel.Text = "Factorial";
            // 
            // btnGenerate
            // 
            this.btnGenerate.Location = new System.Drawing.Point(569, 19);
            this.btnGenerate.Name = "btnGenerate";
            this.btnGenerate.Size = new System.Drawing.Size(86, 23);
            this.btnGenerate.TabIndex = 3;
            this.btnGenerate.Text = "Load Details";
            this.btnGenerate.UseVisualStyleBackColor = true;
            this.btnGenerate.Visible = false;
            // 
            // imageList1
            // 
            this.imageList1.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("imageList1.ImageStream")));
            this.imageList1.TransparentColor = System.Drawing.Color.Transparent;
            this.imageList1.Images.SetKeyName(0, "document16.png");
            this.imageList1.Images.SetKeyName(1, "cube_green16.png");
            this.imageList1.Images.SetKeyName(2, "cube_yellow16.png");
            // 
            // treeSims
            // 
            this.treeSims.ImageIndex = 0;
            this.treeSims.ImageList = this.imageList1;
            this.treeSims.Location = new System.Drawing.Point(3, 56);
            this.treeSims.Name = "treeSims";
            treeNode1.Name = "Node7";
            treeNode1.Text = "15 Oct";
            treeNode2.Name = "Node8";
            treeNode2.Text = "15 Nov";
            treeNode3.Name = "Node9";
            treeNode3.Text = "15 Dec";
            treeNode4.Name = "Node10";
            treeNode4.Text = "15 Jan";
            treeNode5.ImageIndex = 2;
            treeNode5.Name = "Node4";
            treeNode5.SelectedImageIndex = 2;
            treeNode5.Text = "Sorghum Sowing Rule: Sow Date";
            treeNode6.Name = "Node13";
            treeNode6.Text = "solid";
            treeNode7.Name = "Node14";
            treeNode7.Text = "single";
            treeNode8.Name = "Node15";
            treeNode8.Text = "double";
            treeNode9.ImageIndex = 2;
            treeNode9.Name = "Node2";
            treeNode9.SelectedImageIndex = 2;
            treeNode9.Text = "Sorghum Sowing Rule: skip row";
            treeNode10.Name = "Node17";
            treeNode10.Text = "3.5";
            treeNode11.Name = "Node18";
            treeNode11.Text = "5";
            treeNode12.Name = "Node19";
            treeNode12.Text = "7.5";
            treeNode13.ImageIndex = 2;
            treeNode13.Name = "Node3";
            treeNode13.SelectedImageIndex = 2;
            treeNode13.Text = "Sorghum Sowing Rule: Density";
            treeNode14.Name = "Node6";
            treeNode14.Text = "15 Sep";
            treeNode15.ForeColor = System.Drawing.Color.DimGray;
            treeNode15.Name = "Node7";
            treeNode15.Text = "15 Oct";
            treeNode16.ForeColor = System.Drawing.Color.DimGray;
            treeNode16.Name = "Node8";
            treeNode16.Text = "15 Nov";
            treeNode17.ForeColor = System.Drawing.Color.DimGray;
            treeNode17.Name = "Node9";
            treeNode17.Text = "15 Dec";
            treeNode18.ForeColor = System.Drawing.Color.DimGray;
            treeNode18.Name = "Node10";
            treeNode18.Text = "15 Jan";
            treeNode19.ForeColor = System.Drawing.Color.DimGray;
            treeNode19.ImageIndex = 2;
            treeNode19.Name = "Node5";
            treeNode19.SelectedImageIndex = 2;
            treeNode19.Text = "Sorghum Sowing Rule: Sow Date";
            treeNode20.ImageIndex = 1;
            treeNode20.Name = "Node4";
            treeNode20.SelectedImageIndex = 1;
            treeNode20.Text = "FactorLevel";
            treeNode20.ToolTipText = "FactorLevel";
            treeNode21.ImageIndex = 3;
            treeNode21.Name = "Node0";
            treeNode21.SelectedImageIndex = 3;
            treeNode21.Text = "Factorial";
            this.treeSims.Nodes.AddRange(new System.Windows.Forms.TreeNode[] {
            treeNode21});
            this.treeSims.SelectedImageIndex = 0;
            this.treeSims.Size = new System.Drawing.Size(255, 417);
            this.treeSims.TabIndex = 10;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(4, 32);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(90, 13);
            this.label1.TabIndex = 11;
            this.label1.Text = "Total Simulations:";
            // 
            // txtTotalSims
            // 
            this.txtTotalSims.Location = new System.Drawing.Point(100, 29);
            this.txtTotalSims.Name = "txtTotalSims";
            this.txtTotalSims.ReadOnly = true;
            this.txtTotalSims.Size = new System.Drawing.Size(75, 20);
            this.txtTotalSims.TabIndex = 12;
            this.txtTotalSims.TextAlign = System.Windows.Forms.HorizontalAlignment.Center;
            // 
            // groupBox1
            // 
            this.groupBox1.Controls.Add(this.radDesc);
            this.groupBox1.Controls.Add(this.radCount);
            this.groupBox1.Location = new System.Drawing.Point(275, 50);
            this.groupBox1.Name = "groupBox1";
            this.groupBox1.Size = new System.Drawing.Size(168, 76);
            this.groupBox1.TabIndex = 13;
            this.groupBox1.TabStop = false;
            this.groupBox1.Text = "File Name Options";
            // 
            // radCount
            // 
            this.radCount.AutoSize = true;
            this.radCount.Checked = true;
            this.radCount.Location = new System.Drawing.Point(6, 16);
            this.radCount.Name = "radCount";
            this.radCount.Size = new System.Drawing.Size(123, 17);
            this.radCount.TabIndex = 0;
            this.radCount.TabStop = true;
            this.radCount.Text = "Use simulation count";
            this.radCount.UseVisualStyleBackColor = true;
            // 
            // radDesc
            // 
            this.radDesc.AutoSize = true;
            this.radDesc.Location = new System.Drawing.Point(6, 39);
            this.radDesc.Name = "radDesc";
            this.radDesc.Size = new System.Drawing.Size(113, 17);
            this.radDesc.TabIndex = 1;
            this.radDesc.Text = "Use Factor/Levels";
            this.radDesc.UseVisualStyleBackColor = true;
            // 
            // groupBox2
            // 
            this.groupBox2.Controls.Add(this.radMultiple);
            this.groupBox2.Controls.Add(this.radSingle);
            this.groupBox2.Location = new System.Drawing.Point(275, 138);
            this.groupBox2.Name = "groupBox2";
            this.groupBox2.Size = new System.Drawing.Size(168, 76);
            this.groupBox2.TabIndex = 14;
            this.groupBox2.TabStop = false;
            this.groupBox2.Text = "Title Line Options";
            // 
            // radMultiple
            // 
            this.radMultiple.AutoSize = true;
            this.radMultiple.Location = new System.Drawing.Point(6, 39);
            this.radMultiple.Name = "radMultiple";
            this.radMultiple.Size = new System.Drawing.Size(106, 17);
            this.radMultiple.TabIndex = 1;
            this.radMultiple.Text = "Use multiple lines";
            this.radMultiple.UseVisualStyleBackColor = true;
            // 
            // radSingle
            // 
            this.radSingle.AutoSize = true;
            this.radSingle.Checked = true;
            this.radSingle.Location = new System.Drawing.Point(6, 16);
            this.radSingle.Name = "radSingle";
            this.radSingle.Size = new System.Drawing.Size(102, 17);
            this.radSingle.TabIndex = 0;
            this.radSingle.TabStop = true;
            this.radSingle.Text = "Use a single line";
            this.radSingle.UseVisualStyleBackColor = true;
            // 
            // Factorial
            // 
            this.Controls.Add(this.groupBox2);
            this.Controls.Add(this.groupBox1);
            this.Controls.Add(this.txtTotalSims);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.treeSims);
            this.Controls.Add(this.btnGenerate);
            this.HelpText = "Factorial";
            this.Name = "Factorial";
            this.Size = new System.Drawing.Size(655, 476);
            this.Controls.SetChildIndex(this.btnGenerate, 0);
//            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.treeSims, 0);
            this.Controls.SetChildIndex(this.label1, 0);
            this.Controls.SetChildIndex(this.txtTotalSims, 0);
            this.Controls.SetChildIndex(this.groupBox1, 0);
            this.Controls.SetChildIndex(this.groupBox2, 0);
            this.groupBox1.ResumeLayout(false);
            this.groupBox1.PerformLayout();
            this.groupBox2.ResumeLayout(false);
            this.groupBox2.PerformLayout();
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private FarPoint.Win.Spread.FpSpread fpSpread1;
        private System.Windows.Forms.Button btnGenerate;
        private System.Windows.Forms.ImageList imageList1;
        private System.Windows.Forms.TreeView treeSims;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.TextBox txtTotalSims;
        private System.Windows.Forms.GroupBox groupBox1;
        private System.Windows.Forms.RadioButton radDesc;
        private System.Windows.Forms.RadioButton radCount;
        private System.Windows.Forms.GroupBox groupBox2;
        private System.Windows.Forms.RadioButton radMultiple;
        private System.Windows.Forms.RadioButton radSingle;
    }
}
