namespace GraphUserInterface
    {
    partial class SOIUI
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
            this.PhaseList = new System.Windows.Forms.CheckedListBox();
            this.label3 = new System.Windows.Forms.Label();
            this.MonthDropDown = new System.Windows.Forms.ComboBox();
            this.label2 = new System.Windows.Forms.Label();
            this.FileNameEdit = new System.Windows.Forms.TextBox();
            this.label1 = new System.Windows.Forms.Label();
            this.GroupBox.SuspendLayout();
            this.SuspendLayout();
            // 
            // GroupBox
            // 
            this.GroupBox.BackColor = System.Drawing.SystemColors.Window;
            this.GroupBox.Controls.Add(this.PhaseList);
            this.GroupBox.Controls.Add(this.label3);
            this.GroupBox.Controls.Add(this.MonthDropDown);
            this.GroupBox.Controls.Add(this.label2);
            this.GroupBox.Controls.Add(this.FileNameEdit);
            this.GroupBox.Controls.Add(this.label1);
            this.GroupBox.Dock = System.Windows.Forms.DockStyle.Fill;
            this.GroupBox.Location = new System.Drawing.Point(0, 18);
            this.GroupBox.Name = "GroupBox";
            this.GroupBox.Size = new System.Drawing.Size(127, 245);
            this.GroupBox.TabIndex = 2;
            this.GroupBox.TabStop = false;
            this.GroupBox.Text = "GroupBox";
            // 
            // PhaseList
            // 
            this.PhaseList.CheckOnClick = true;
            this.PhaseList.FormattingEnabled = true;
            this.PhaseList.Items.AddRange(new object[] {
            "Negative",
            "Positive",
            "Falling",
            "Rising",
            "Zero",
            "AllYears",
            "AllOtherYears"});
            this.PhaseList.Location = new System.Drawing.Point(9, 133);
            this.PhaseList.Name = "PhaseList";
            this.PhaseList.Size = new System.Drawing.Size(112, 109);
            this.PhaseList.TabIndex = 6;
            this.PhaseList.ItemCheck += new System.Windows.Forms.ItemCheckEventHandler(this.OnItemCheck);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(4, 115);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(40, 13);
            this.label3.TabIndex = 5;
            this.label3.Text = "Phase:";
            // 
            // MonthDropDown
            // 
            this.MonthDropDown.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.MonthDropDown.FormattingEnabled = true;
            this.MonthDropDown.Items.AddRange(new object[] {
            "January",
            "February",
            "March",
            "April",
            "May",
            "June",
            "July",
            "August",
            "September",
            "October",
            "November",
            "December"});
            this.MonthDropDown.Location = new System.Drawing.Point(7, 82);
            this.MonthDropDown.Name = "MonthDropDown";
            this.MonthDropDown.Size = new System.Drawing.Size(114, 21);
            this.MonthDropDown.TabIndex = 3;
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(7, 66);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(40, 13);
            this.label2.TabIndex = 2;
            this.label2.Text = "Month:";
            // 
            // FileNameEdit
            // 
            this.FileNameEdit.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left)
                        | System.Windows.Forms.AnchorStyles.Right)));
            this.FileNameEdit.Location = new System.Drawing.Point(7, 35);
            this.FileNameEdit.Name = "FileNameEdit";
            this.FileNameEdit.Size = new System.Drawing.Size(114, 20);
            this.FileNameEdit.TabIndex = 1;
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(4, 19);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(82, 13);
            this.label1.TabIndex = 0;
            this.label1.Text = "Phase filename:";
            // 
            // SOIUI
            // 
            this.Controls.Add(this.GroupBox);
            this.Name = "SOIUI";
            this.Size = new System.Drawing.Size(127, 263);
            this.Controls.SetChildIndex(this.GroupBox, 0);
            this.GroupBox.ResumeLayout(false);
            this.GroupBox.PerformLayout();
            this.ResumeLayout(false);

            }

        #endregion

        private System.Windows.Forms.GroupBox GroupBox;
        private System.Windows.Forms.TextBox FileNameEdit;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.ComboBox MonthDropDown;
        private System.Windows.Forms.Label label2;
       private System.Windows.Forms.Label label1;
       private System.Windows.Forms.CheckedListBox PhaseList;
        }
    }
