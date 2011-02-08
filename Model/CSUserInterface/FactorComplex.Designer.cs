namespace CSUserInterface
{
    partial class FactorComplex
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
            System.Windows.Forms.TreeNode treeNode1 = new System.Windows.Forms.TreeNode("Black Vertosol 75mm");
            System.Windows.Forms.TreeNode treeNode2 = new System.Windows.Forms.TreeNode("Black Vertosol 125mm");
            System.Windows.Forms.TreeNode treeNode3 = new System.Windows.Forms.TreeNode("Black Vertosol 175mm");
            System.Windows.Forms.TreeNode treeNode4 = new System.Windows.Forms.TreeNode("Soil", new System.Windows.Forms.TreeNode[] {
            treeNode1,
            treeNode2,
            treeNode3});
            System.Windows.Forms.TreeNode treeNode5 = new System.Windows.Forms.TreeNode("15 Oct");
            System.Windows.Forms.TreeNode treeNode6 = new System.Windows.Forms.TreeNode("15 Nov");
            System.Windows.Forms.TreeNode treeNode7 = new System.Windows.Forms.TreeNode("15 Dec");
            System.Windows.Forms.TreeNode treeNode8 = new System.Windows.Forms.TreeNode("15 Jan");
            System.Windows.Forms.TreeNode treeNode9 = new System.Windows.Forms.TreeNode("Enter sowing window START date (dd-mmm) :", new System.Windows.Forms.TreeNode[] {
            treeNode5,
            treeNode6,
            treeNode7,
            treeNode8});
            System.Windows.Forms.TreeNode treeNode10 = new System.Windows.Forms.TreeNode("solid");
            System.Windows.Forms.TreeNode treeNode11 = new System.Windows.Forms.TreeNode("single");
            System.Windows.Forms.TreeNode treeNode12 = new System.Windows.Forms.TreeNode("double");
            System.Windows.Forms.TreeNode treeNode13 = new System.Windows.Forms.TreeNode("Skip row:", new System.Windows.Forms.TreeNode[] {
            treeNode10,
            treeNode11,
            treeNode12});
            System.Windows.Forms.TreeNode treeNode14 = new System.Windows.Forms.TreeNode("3.5");
            System.Windows.Forms.TreeNode treeNode15 = new System.Windows.Forms.TreeNode("5");
            System.Windows.Forms.TreeNode treeNode16 = new System.Windows.Forms.TreeNode("7.5");
            System.Windows.Forms.TreeNode treeNode17 = new System.Windows.Forms.TreeNode("Enter sowing density  (plants/m2) :", new System.Windows.Forms.TreeNode[] {
            treeNode14,
            treeNode15,
            treeNode16});
            System.Windows.Forms.TreeNode treeNode18 = new System.Windows.Forms.TreeNode("Sorghum Sowing Rule", new System.Windows.Forms.TreeNode[] {
            treeNode9,
            treeNode13,
            treeNode17});
            this.treeView1 = new System.Windows.Forms.TreeView();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Visible = true;
            // 
            // treeView1
            // 
            this.treeView1.CheckBoxes = true;
            this.treeView1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.treeView1.Location = new System.Drawing.Point(0, 16);
            this.treeView1.Name = "treeView1";
            treeNode1.BackColor = System.Drawing.Color.White;
            treeNode1.Checked = true;
            treeNode1.ForeColor = System.Drawing.Color.Gray;
            treeNode1.Name = "Node1";
            treeNode1.Text = "Black Vertosol 75mm";
            treeNode2.Checked = true;
            treeNode2.ForeColor = System.Drawing.Color.Gray;
            treeNode2.Name = "Node2";
            treeNode2.Text = "Black Vertosol 125mm";
            treeNode3.Checked = true;
            treeNode3.ForeColor = System.Drawing.Color.Gray;
            treeNode3.Name = "Node3";
            treeNode3.Text = "Black Vertosol 175mm";
            treeNode4.Checked = true;
            treeNode4.ForeColor = System.Drawing.Color.Gray;
            treeNode4.Name = "Node0";
            treeNode4.Text = "Soil";
            treeNode5.Checked = true;
            treeNode5.Name = "Node7";
            treeNode5.Text = "15 Oct";
            treeNode6.Checked = true;
            treeNode6.Name = "Node8";
            treeNode6.Text = "15 Nov";
            treeNode7.Checked = true;
            treeNode7.Name = "Node9";
            treeNode7.Text = "15 Dec";
            treeNode8.Checked = true;
            treeNode8.Name = "Node10";
            treeNode8.Text = "15 Jan";
            treeNode9.Checked = true;
            treeNode9.Name = "Node5";
            treeNode9.Text = "Enter sowing window START date (dd-mmm) :";
            treeNode10.Checked = true;
            treeNode10.Name = "Node13";
            treeNode10.Text = "solid";
            treeNode11.Checked = true;
            treeNode11.Name = "Node14";
            treeNode11.Text = "single";
            treeNode12.Checked = true;
            treeNode12.Name = "Node15";
            treeNode12.Text = "double";
            treeNode13.Checked = true;
            treeNode13.Name = "Node12";
            treeNode13.Text = "Skip row:";
            treeNode14.Checked = true;
            treeNode14.Name = "Node17";
            treeNode14.Text = "3.5";
            treeNode15.Checked = true;
            treeNode15.Name = "Node18";
            treeNode15.Text = "5";
            treeNode16.Checked = true;
            treeNode16.Name = "Node19";
            treeNode16.Text = "7.5";
            treeNode17.Checked = true;
            treeNode17.Name = "Node16";
            treeNode17.Text = "Enter sowing density  (plants/m2) :";
            treeNode18.Checked = true;
            treeNode18.Name = "Node4";
            treeNode18.Text = "Sorghum Sowing Rule";
            this.treeView1.Nodes.AddRange(new System.Windows.Forms.TreeNode[] {
            treeNode4,
            treeNode18});
            this.treeView1.Size = new System.Drawing.Size(655, 525);
            this.treeView1.TabIndex = 8;
            // 
            // FactorialLevel
            // 
            this.Controls.Add(this.treeView1);
            this.Name = "FactorialLevel";
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.treeView1, 0);
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TreeView treeView1;
    }
}
