

//This is used in in UIBits project in ReorderForm.cs, OptionsForm.cs 
//in CSUserInterface project in SoilUI
//in VBUserInterface project in RuleUI
//in GraphUserInterface project in ChartPageUI
//in Actions project in BaseActions.vb

namespace UIBits
{

    public class InputDialog : System.Windows.Forms.Form
    {


        internal System.Windows.Forms.Button btnOK;
        internal System.Windows.Forms.Button btnCancel;
        internal System.Windows.Forms.TextBox txtValue;
        internal System.Windows.Forms.Label lblPrompt;


        #region " Windows Form Designer generated code "

        public InputDialog()
            : base()
        {

            //This call is required by the Windows Form Designer. 
            InitializeComponent();

            //Add any initialization after the InitializeComponent() call 
        }


        //Required by the Windows Form Designer 
        private System.ComponentModel.IContainer components;

        //Form overrides dispose to clean up the component list. 
        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if ((components != null))
                {
                    components.Dispose();
                }
            }
            base.Dispose(disposing);
        }



        //NOTE: The following procedure is required by the Windows Form Designer 
        //It can be modified using the Windows Form Designer. 
        //Do not modify it using the code editor. 

        [System.Diagnostics.DebuggerStepThrough()]

        private void InitializeComponent()
        {
            this.components = new System.ComponentModel.Container();

            this.btnOK = new System.Windows.Forms.Button();
            this.btnCancel = new System.Windows.Forms.Button();
            this.txtValue = new System.Windows.Forms.TextBox();
            this.lblPrompt = new System.Windows.Forms.Label();
            this.SuspendLayout();
            // 
            //btnOK 
            // 
            this.btnOK.Anchor = (System.Windows.Forms.AnchorStyles)(System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right);
            this.btnOK.DialogResult = System.Windows.Forms.DialogResult.OK;
            this.btnOK.Location = new System.Drawing.Point(232, 84);
            this.btnOK.Name = "btnOK";
            this.btnOK.Size = new System.Drawing.Size(72, 24);
            this.btnOK.TabIndex = 6;
            this.btnOK.Text = "&OK";
            // 
            //btnCancel 
            // 
            this.btnCancel.Anchor = (System.Windows.Forms.AnchorStyles)(System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right);
            this.btnCancel.DialogResult = System.Windows.Forms.DialogResult.Cancel;
            this.btnCancel.Location = new System.Drawing.Point(312, 84);
            this.btnCancel.Name = "btnCancel";
            this.btnCancel.Size = new System.Drawing.Size(72, 24);
            this.btnCancel.TabIndex = 5;
            this.btnCancel.Text = "&Cancel";
            // 
            //txtValue 
            // 
            this.txtValue.Anchor = (System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) | System.Windows.Forms.AnchorStyles.Right);
            this.txtValue.Location = new System.Drawing.Point(8, 48);
            this.txtValue.Name = "txtValue";
            this.txtValue.Size = new System.Drawing.Size(376, 20);
            this.txtValue.TabIndex = 3;
            this.txtValue.Text = "";
            // 
            //lblPrompt 
            // 
            this.lblPrompt.Anchor = (System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) | System.Windows.Forms.AnchorStyles.Right);
            this.lblPrompt.Location = new System.Drawing.Point(8, 8);
            this.lblPrompt.Name = "lblPrompt";
            this.lblPrompt.Size = new System.Drawing.Size(376, 32);
            this.lblPrompt.TabIndex = 4;
            // 
            //InputDialog 
            // 
            this.AcceptButton = this.btnOK;
            this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
            this.CancelButton = this.btnCancel;
            this.ClientSize = new System.Drawing.Size(392, 113);
            this.Controls.Add(this.btnOK);
            this.Controls.Add(this.btnCancel);
            this.Controls.Add(this.txtValue);
            this.Controls.Add(this.lblPrompt);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "InputDialog";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent;
            this.ResumeLayout(false);

        }

        #endregion





        public string Prompt
        {
            get { return lblPrompt.Text; }
            set { lblPrompt.Text = value; }
        }

        public string Value
        {
            get { return txtValue.Text.Trim(); }
            set
            {
                txtValue.Text = value.Trim();
                // preselect the text, and give the focus to this control 
                txtValue.SelectAll();
                txtValue.Focus();
            }
        }

        // create an InputDialog window, and return the typed text 
        public static string InputBox(string prompt, string title, string defaultVal, bool Password)
        {
            InputDialog dlg = new InputDialog();
            dlg.Text = title;
            dlg.Prompt = prompt;
            dlg.Value = defaultVal;
            dlg.txtValue.UseSystemPasswordChar = Password;

            if (dlg.ShowDialog() == System.Windows.Forms.DialogResult.OK)
            {
                return dlg.Value;
            }
            else
            {
                return defaultVal;
            }
        }
    }
}