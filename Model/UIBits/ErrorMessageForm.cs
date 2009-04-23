
using System;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;


namespace UIBits
	{
	// ------------------------------------------
	// Simple form for displaying error messages
	// ------------------------------------------
	public class ErrorMessageForm : System.Windows.Forms.Form
		{
		private System.Windows.Forms.TextBox TextBox;
		private System.Windows.Forms.Button button1;
		private System.ComponentModel.Container components = null;

		public ErrorMessageForm()
			{
			//
			// Required for Windows Form Designer support
			//
			InitializeComponent();
			}

		protected override void Dispose( bool disposing )
			{
			if( disposing )
				{
				if(components != null)
					{
					components.Dispose();
					}
				}
			base.Dispose( disposing );
			}

		#region Windows Form Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.TextBox = new System.Windows.Forms.TextBox();
			this.button1 = new System.Windows.Forms.Button();
			this.SuspendLayout();
			// 
			// TextBox
			// 
			this.TextBox.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
				| System.Windows.Forms.AnchorStyles.Left) 
				| System.Windows.Forms.AnchorStyles.Right)));
			this.TextBox.Location = new System.Drawing.Point(0, 0);
			this.TextBox.Multiline = true;
			this.TextBox.Name = "TextBox";
			this.TextBox.ReadOnly = true;
			this.TextBox.ScrollBars = System.Windows.Forms.ScrollBars.Both;
			this.TextBox.Size = new System.Drawing.Size(528, 736);
			this.TextBox.TabIndex = 0;
			this.TextBox.Text = "";
			this.TextBox.WordWrap = false;
			// 
			// button1
			// 
			this.button1.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Right)));
			this.button1.DialogResult = System.Windows.Forms.DialogResult.OK;
			this.button1.Location = new System.Drawing.Point(544, 24);
			this.button1.Name = "button1";
			this.button1.TabIndex = 1;
			this.button1.Text = "&Ok";
			this.button1.Click += new System.EventHandler(this.button1_Click);
			// 
			// ErrorMessageForm
			// 
			this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
			this.ClientSize = new System.Drawing.Size(640, 734);
			this.Controls.Add(this.button1);
			this.Controls.Add(this.TextBox);
			this.Name = "ErrorMessageForm";
			this.Text = "Errors encountered in soils";
			this.ResumeLayout(false);

		}
		#endregion


		public void SetText(string Message)
			{
			TextBox.Text = Message;
			}

		private void button1_Click(object sender, System.EventArgs e)
			{
			this.Close();
			}
		}
	}
