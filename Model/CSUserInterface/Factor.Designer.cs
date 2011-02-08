namespace CSUserInterface
{
    partial class Factor
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
            this.label1 = new System.Windows.Forms.Label();
            this.comboBox1 = new System.Windows.Forms.ComboBox();
            this.pnlVariable = new System.Windows.Forms.Panel();
            this.pnlManager = new System.Windows.Forms.Panel();
            this.listBox1 = new System.Windows.Forms.ListBox();
            this.listBox2 = new System.Windows.Forms.ListBox();
            this.FactorTargets = new CSUserInterface.FactorTargets();
            this.pnlVariable.SuspendLayout();
            this.pnlManager.SuspendLayout();
            this.SuspendLayout();
            // 
            // MyHelpLabel
            // 
            this.MyHelpLabel.Text = "Factor";
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(0, 25);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(86, 13);
            this.label1.TabIndex = 3;
            this.label1.Text = "Factorial Method";
            // 
            // comboBox1
            // 
            this.comboBox1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList;
            this.comboBox1.FormattingEnabled = true;
            this.comboBox1.Items.AddRange(new object[] {
            "Component Replacement",
            "Variable Replacement",
            "Manager Variable Replacement"});
            this.comboBox1.Location = new System.Drawing.Point(93, 21);
            this.comboBox1.Name = "comboBox1";
            this.comboBox1.Size = new System.Drawing.Size(257, 21);
            this.comboBox1.TabIndex = 4;
            // 
            // pnlVariable
            // 
            this.pnlVariable.Controls.Add(this.listBox1);
            this.pnlVariable.Location = new System.Drawing.Point(0, 221);
            this.pnlVariable.Name = "pnlVariable";
            this.pnlVariable.Size = new System.Drawing.Size(317, 320);
            this.pnlVariable.TabIndex = 5;
            // 
            // pnlManager
            // 
            this.pnlManager.Controls.Add(this.listBox2);
            this.pnlManager.Location = new System.Drawing.Point(323, 221);
            this.pnlManager.Name = "pnlManager";
            this.pnlManager.Size = new System.Drawing.Size(332, 320);
            this.pnlManager.TabIndex = 6;
            // 
            // listBox1
            // 
            this.listBox1.FormattingEnabled = true;
            this.listBox1.Location = new System.Drawing.Point(14, 41);
            this.listBox1.Name = "listBox1";
            this.listBox1.Size = new System.Drawing.Size(206, 264);
            this.listBox1.TabIndex = 0;
            // 
            // listBox2
            // 
            this.listBox2.FormattingEnabled = true;
            this.listBox2.Location = new System.Drawing.Point(18, 41);
            this.listBox2.Name = "listBox2";
            this.listBox2.Size = new System.Drawing.Size(206, 264);
            this.listBox2.TabIndex = 1;
            // 
            // FactorTargets
            // 
            this.FactorTargets.AutoScroll = true;
            this.FactorTargets.BackColor = System.Drawing.SystemColors.Window;
            this.FactorTargets.HelpText = "";
            this.FactorTargets.Location = new System.Drawing.Point(0, 54);
            this.FactorTargets.Margin = new System.Windows.Forms.Padding(0);
            this.FactorTargets.Name = "FactorTargets";
            this.FactorTargets.Size = new System.Drawing.Size(350, 150);
            this.FactorTargets.TabIndex = 2;
            // 
            // Factor
            // 
            this.Controls.Add(this.pnlManager);
            this.Controls.Add(this.comboBox1);
            this.Controls.Add(this.pnlVariable);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.FactorTargets);
            this.HelpText = "Factor";
            this.Name = "Factor";
            this.Controls.SetChildIndex(this.FactorTargets, 0);
            this.Controls.SetChildIndex(this.label1, 0);
            this.Controls.SetChildIndex(this.pnlVariable, 0);
            this.Controls.SetChildIndex(this.comboBox1, 0);
            this.Controls.SetChildIndex(this.MyHelpLabel, 0);
            this.Controls.SetChildIndex(this.pnlManager, 0);
            this.pnlVariable.ResumeLayout(false);
            this.pnlManager.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private FactorTargets FactorTargets;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.ComboBox comboBox1;
        private System.Windows.Forms.Panel pnlVariable;
        private System.Windows.Forms.Panel pnlManager;
        private System.Windows.Forms.ListBox listBox1;
        private System.Windows.Forms.ListBox listBox2;


    }
}
