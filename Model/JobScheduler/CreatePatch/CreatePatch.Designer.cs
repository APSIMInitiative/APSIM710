
partial class MainForm
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
        System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(MainForm));
        this.ListView = new System.Windows.Forms.ListView();
        this.columnHeader1 = ((System.Windows.Forms.ColumnHeader)(new System.Windows.Forms.ColumnHeader()));
        this.SelectCheckBox = new System.Windows.Forms.CheckBox();
        this.OKButton = new System.Windows.Forms.Button();
        this.CancelButton = new System.Windows.Forms.Button();
        this.SaveFileDialog = new System.Windows.Forms.SaveFileDialog();
        this.SuspendLayout();
        // 
        // ListView
        // 
        this.ListView.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom)
                    | System.Windows.Forms.AnchorStyles.Left)
                    | System.Windows.Forms.AnchorStyles.Right)));
        this.ListView.CheckBoxes = true;
        this.ListView.Columns.AddRange(new System.Windows.Forms.ColumnHeader[] {
            this.columnHeader1});
        this.ListView.Location = new System.Drawing.Point(13, 13);
        this.ListView.Name = "ListView";
        this.ListView.ShowGroups = false;
        this.ListView.Size = new System.Drawing.Size(680, 490);
        this.ListView.TabIndex = 0;
        this.ListView.UseCompatibleStateImageBehavior = false;
        this.ListView.View = System.Windows.Forms.View.Details;
        // 
        // columnHeader1
        // 
        this.columnHeader1.Text = "Path";
        this.columnHeader1.Width = 667;
        // 
        // SelectCheckBox
        // 
        this.SelectCheckBox.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Left)));
        this.SelectCheckBox.AutoSize = true;
        this.SelectCheckBox.Location = new System.Drawing.Point(13, 521);
        this.SelectCheckBox.Name = "SelectCheckBox";
        this.SelectCheckBox.Size = new System.Drawing.Size(120, 17);
        this.SelectCheckBox.TabIndex = 1;
        this.SelectCheckBox.Text = "Select / deselect all";
        this.SelectCheckBox.UseVisualStyleBackColor = true;
        this.SelectCheckBox.CheckedChanged += new System.EventHandler(this.OnSelectCheckBoxChanged);
        // 
        // OKButton
        // 
        this.OKButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
        this.OKButton.DialogResult = System.Windows.Forms.DialogResult.OK;
        this.OKButton.Location = new System.Drawing.Point(528, 521);
        this.OKButton.Name = "OKButton";
        this.OKButton.Size = new System.Drawing.Size(75, 23);
        this.OKButton.TabIndex = 2;
        this.OKButton.Text = "OK";
        this.OKButton.UseVisualStyleBackColor = true;
        this.OKButton.Click += new System.EventHandler(this.OnOKButtonClick);
        // 
        // CancelButton
        // 
        this.CancelButton.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
        this.CancelButton.DialogResult = System.Windows.Forms.DialogResult.Cancel;
        this.CancelButton.Location = new System.Drawing.Point(618, 521);
        this.CancelButton.Name = "CancelButton";
        this.CancelButton.Size = new System.Drawing.Size(75, 23);
        this.CancelButton.TabIndex = 3;
        this.CancelButton.Text = "Cancel";
        this.CancelButton.UseVisualStyleBackColor = true;
        this.CancelButton.Click += new System.EventHandler(this.OnCancelButtonClick);
        // 
        // SaveFileDialog
        // 
        this.SaveFileDialog.DefaultExt = "zip";
        this.SaveFileDialog.Filter = "Zip files|*.zip";
        this.SaveFileDialog.RestoreDirectory = true;
        this.SaveFileDialog.Title = "Enter name of patch zip file.";
        // 
        // MainForm
        // 
        this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
        this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
        this.ClientSize = new System.Drawing.Size(705, 553);
        this.Controls.Add(this.CancelButton);
        this.Controls.Add(this.OKButton);
        this.Controls.Add(this.SelectCheckBox);
        this.Controls.Add(this.ListView);
        this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
        this.Name = "MainForm";
        this.Text = "Create Patch";
        this.Shown += new System.EventHandler(this.OnMainFormShown);
        this.ResumeLayout(false);
        this.PerformLayout();

    }

    #endregion

    private System.Windows.Forms.ListView ListView;
    private System.Windows.Forms.CheckBox SelectCheckBox;
    private System.Windows.Forms.Button OKButton;
    private System.Windows.Forms.Button CancelButton;
    private System.Windows.Forms.ColumnHeader columnHeader1;
    private System.Windows.Forms.SaveFileDialog SaveFileDialog;
}


