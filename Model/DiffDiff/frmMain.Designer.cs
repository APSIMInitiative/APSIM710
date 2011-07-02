namespace DiffDiff
{
    partial class frmMain
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
            this.rtxtOutput = new System.Windows.Forms.RichTextBox();
            this.btnOrig = new System.Windows.Forms.Button();
            this.btnNew = new System.Windows.Forms.Button();
            this.label1 = new System.Windows.Forms.Label();
            this.label2 = new System.Windows.Forms.Label();
            this.txtOrig = new System.Windows.Forms.TextBox();
            this.txtNew = new System.Windows.Forms.TextBox();
            this.label3 = new System.Windows.Forms.Label();
            this.lbxCols = new System.Windows.Forms.ListBox();
            this.label4 = new System.Windows.Forms.Label();
            this.txtTol = new System.Windows.Forms.TextBox();
            this.radPct = new System.Windows.Forms.RadioButton();
            this.radAbs = new System.Windows.Forms.RadioButton();
            this.radDP = new System.Windows.Forms.RadioButton();
            this.btnGo = new System.Windows.Forms.Button();
            this.btnSave = new System.Windows.Forms.Button();
            this.openFile = new System.Windows.Forms.OpenFileDialog();
            this.saveFile = new System.Windows.Forms.SaveFileDialog();
            this.btnCmdHelp = new System.Windows.Forms.Button();
            this.SuspendLayout();
            // 
            // rtxtOutput
            // 
            this.rtxtOutput.Font = new System.Drawing.Font("Consolas", 8.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.rtxtOutput.Location = new System.Drawing.Point(12, 333);
            this.rtxtOutput.Name = "rtxtOutput";
            this.rtxtOutput.Size = new System.Drawing.Size(992, 348);
            this.rtxtOutput.TabIndex = 0;
            this.rtxtOutput.Text = "";
            this.rtxtOutput.WordWrap = false;
            // 
            // btnOrig
            // 
            this.btnOrig.Location = new System.Drawing.Point(929, 12);
            this.btnOrig.Name = "btnOrig";
            this.btnOrig.Size = new System.Drawing.Size(75, 23);
            this.btnOrig.TabIndex = 1;
            this.btnOrig.Text = "Browse";
            this.btnOrig.UseVisualStyleBackColor = true;
            this.btnOrig.Click += new System.EventHandler(this.btnBrowse_Click);
            // 
            // btnNew
            // 
            this.btnNew.Location = new System.Drawing.Point(929, 41);
            this.btnNew.Name = "btnNew";
            this.btnNew.Size = new System.Drawing.Size(75, 23);
            this.btnNew.TabIndex = 3;
            this.btnNew.Text = "Browse";
            this.btnNew.UseVisualStyleBackColor = true;
            this.btnNew.Click += new System.EventHandler(this.btnBrowse_Click);
            // 
            // label1
            // 
            this.label1.AutoSize = true;
            this.label1.Location = new System.Drawing.Point(12, 17);
            this.label1.Name = "label1";
            this.label1.Size = new System.Drawing.Size(42, 13);
            this.label1.TabIndex = 4;
            this.label1.Text = "Original";
            // 
            // label2
            // 
            this.label2.AutoSize = true;
            this.label2.Location = new System.Drawing.Point(12, 46);
            this.label2.Name = "label2";
            this.label2.Size = new System.Drawing.Size(29, 13);
            this.label2.TabIndex = 5;
            this.label2.Text = "New";
            // 
            // txtOrig
            // 
            this.txtOrig.Location = new System.Drawing.Point(60, 14);
            this.txtOrig.Name = "txtOrig";
            this.txtOrig.Size = new System.Drawing.Size(863, 20);
            this.txtOrig.TabIndex = 6;
            this.txtOrig.TextChanged += new System.EventHandler(this.txt_TextChanged);
            // 
            // txtNew
            // 
            this.txtNew.Location = new System.Drawing.Point(60, 43);
            this.txtNew.Name = "txtNew";
            this.txtNew.Size = new System.Drawing.Size(863, 20);
            this.txtNew.TabIndex = 7;
            this.txtNew.TextChanged += new System.EventHandler(this.txt_TextChanged);
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(12, 76);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(128, 13);
            this.label3.TabIndex = 8;
            this.label3.Text = "Compare These Columns:";
            // 
            // lbxCols
            // 
            this.lbxCols.FormattingEnabled = true;
            this.lbxCols.Location = new System.Drawing.Point(15, 92);
            this.lbxCols.MultiColumn = true;
            this.lbxCols.Name = "lbxCols";
            this.lbxCols.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended;
            this.lbxCols.Size = new System.Drawing.Size(989, 147);
            this.lbxCols.TabIndex = 9;
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(241, 253);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(225, 13);
            this.label4.TabIndex = 10;
            this.label4.Text = "Acceptable Tolerance (exclusive less than \'x\'):";
            // 
            // txtTol
            // 
            this.txtTol.Location = new System.Drawing.Point(471, 250);
            this.txtTol.Name = "txtTol";
            this.txtTol.Size = new System.Drawing.Size(80, 20);
            this.txtTol.TabIndex = 11;
            // 
            // radPct
            // 
            this.radPct.AutoSize = true;
            this.radPct.Location = new System.Drawing.Point(559, 251);
            this.radPct.Name = "radPct";
            this.radPct.Size = new System.Drawing.Size(33, 17);
            this.radPct.TabIndex = 12;
            this.radPct.Text = "%";
            this.radPct.UseVisualStyleBackColor = true;
            // 
            // radAbs
            // 
            this.radAbs.AutoSize = true;
            this.radAbs.Checked = true;
            this.radAbs.Location = new System.Drawing.Point(598, 251);
            this.radAbs.Name = "radAbs";
            this.radAbs.Size = new System.Drawing.Size(65, 17);
            this.radAbs.TabIndex = 13;
            this.radAbs.TabStop = true;
            this.radAbs.Text = "absolute";
            this.radAbs.UseVisualStyleBackColor = true;
            // 
            // radDP
            // 
            this.radDP.AutoSize = true;
            this.radDP.Location = new System.Drawing.Point(669, 251);
            this.radDP.Name = "radDP";
            this.radDP.Size = new System.Drawing.Size(106, 17);
            this.radDP.TabIndex = 14;
            this.radDP.Text = "num Dec. Places";
            this.radDP.UseVisualStyleBackColor = true;
            // 
            // btnGo
            // 
            this.btnGo.Location = new System.Drawing.Point(471, 304);
            this.btnGo.Name = "btnGo";
            this.btnGo.Size = new System.Drawing.Size(75, 23);
            this.btnGo.TabIndex = 15;
            this.btnGo.Text = "Diff";
            this.btnGo.UseVisualStyleBackColor = true;
            this.btnGo.Click += new System.EventHandler(this.btnGo_Click);
            // 
            // btnSave
            // 
            this.btnSave.Location = new System.Drawing.Point(929, 687);
            this.btnSave.Name = "btnSave";
            this.btnSave.Size = new System.Drawing.Size(75, 23);
            this.btnSave.TabIndex = 17;
            this.btnSave.Text = "Save To File";
            this.btnSave.UseVisualStyleBackColor = true;
            this.btnSave.Click += new System.EventHandler(this.btnSave_Click);
            // 
            // saveFile
            // 
            this.saveFile.FileName = "diff_results.txt";
            // 
            // btnCmdHelp
            // 
            this.btnCmdHelp.Location = new System.Drawing.Point(12, 687);
            this.btnCmdHelp.Name = "btnCmdHelp";
            this.btnCmdHelp.Size = new System.Drawing.Size(128, 23);
            this.btnCmdHelp.TabIndex = 18;
            this.btnCmdHelp.Text = "Show CMD Line Help";
            this.btnCmdHelp.UseVisualStyleBackColor = true;
            this.btnCmdHelp.Click += new System.EventHandler(this.btnCmdHelp_Click);
            // 
            // frmMain
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(1016, 715);
            this.Controls.Add(this.btnCmdHelp);
            this.Controls.Add(this.btnSave);
            this.Controls.Add(this.btnGo);
            this.Controls.Add(this.radDP);
            this.Controls.Add(this.radAbs);
            this.Controls.Add(this.radPct);
            this.Controls.Add(this.txtTol);
            this.Controls.Add(this.label4);
            this.Controls.Add(this.lbxCols);
            this.Controls.Add(this.label3);
            this.Controls.Add(this.txtNew);
            this.Controls.Add(this.txtOrig);
            this.Controls.Add(this.label2);
            this.Controls.Add(this.label1);
            this.Controls.Add(this.btnNew);
            this.Controls.Add(this.btnOrig);
            this.Controls.Add(this.rtxtOutput);
            this.Name = "frmMain";
            this.Text = "DiffDiff";
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.RichTextBox rtxtOutput;
        private System.Windows.Forms.Button btnOrig;
        private System.Windows.Forms.Button btnNew;
        private System.Windows.Forms.Label label1;
        private System.Windows.Forms.Label label2;
        private System.Windows.Forms.TextBox txtOrig;
        private System.Windows.Forms.TextBox txtNew;
        private System.Windows.Forms.Label label3;
        private System.Windows.Forms.ListBox lbxCols;
        private System.Windows.Forms.Label label4;
        private System.Windows.Forms.TextBox txtTol;
        private System.Windows.Forms.RadioButton radPct;
        private System.Windows.Forms.RadioButton radAbs;
        private System.Windows.Forms.RadioButton radDP;
        private System.Windows.Forms.Button btnGo;
        private System.Windows.Forms.Button btnSave;
        private System.Windows.Forms.OpenFileDialog openFile;
        private System.Windows.Forms.SaveFileDialog saveFile;
        private System.Windows.Forms.Button btnCmdHelp;
    }
}

