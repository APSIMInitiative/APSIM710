
using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;

using Controllers;

namespace CSUserInterface
{
	/// <summary>
	/// Summary description for MemoControl.
	/// </summary>
	public class MemoControl : System.Windows.Forms.UserControl 
	{
		private System.Windows.Forms.ImageList ToolbarImages;
		private System.Windows.Forms.ToolBar tlbMain;
		private System.Windows.Forms.StatusBar statusBar1;
		private System.Windows.Forms.ToolBarButton Separator1;
		private System.Windows.Forms.ToolBarButton Separator2;
		private System.Windows.Forms.ToolBarButton btnAdd;
		private System.Windows.Forms.ToolBarButton btnExport;
		private System.Windows.Forms.ToolBarButton btnBold;
		private System.Windows.Forms.ToolBarButton btnItalics;
		private System.Windows.Forms.ToolBarButton btnUnderline;
		private System.Windows.Forms.ToolBarButton btnColour;
		private System.Windows.Forms.ToolBarButton btnFont;
		private System.Windows.Forms.SaveFileDialog dlgExport;
		private System.Windows.Forms.ColorDialog dlgColour;
		private System.Windows.Forms.FontDialog dlgFont;
		private System.Windows.Forms.RichTextBox rtxtResolution;
		private System.ComponentModel.IContainer components;

		public MemoControl()
		{
			// This call is required by the Windows.Forms Form Designer.
			InitializeComponent();

		}

		/// <summary> 
		/// Clean up any resources being used.
		/// </summary>
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

		#region Component Designer generated code
		/// <summary> 
		/// Required method for Designer support - do not modify 
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.components = new System.ComponentModel.Container();
			System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(MemoControl));
			this.tlbMain = new System.Windows.Forms.ToolBar();
			this.btnAdd = new System.Windows.Forms.ToolBarButton();
			this.btnExport = new System.Windows.Forms.ToolBarButton();
			this.Separator1 = new System.Windows.Forms.ToolBarButton();
			this.btnBold = new System.Windows.Forms.ToolBarButton();
			this.btnItalics = new System.Windows.Forms.ToolBarButton();
			this.btnUnderline = new System.Windows.Forms.ToolBarButton();
			this.Separator2 = new System.Windows.Forms.ToolBarButton();
			this.btnColour = new System.Windows.Forms.ToolBarButton();
			this.btnFont = new System.Windows.Forms.ToolBarButton();
			this.ToolbarImages = new System.Windows.Forms.ImageList(this.components);
			this.statusBar1 = new System.Windows.Forms.StatusBar();
			this.dlgExport = new System.Windows.Forms.SaveFileDialog();
			this.dlgColour = new System.Windows.Forms.ColorDialog();
			this.dlgFont = new System.Windows.Forms.FontDialog();
			this.rtxtResolution = new System.Windows.Forms.RichTextBox();
			this.SuspendLayout();
			// 
			// tlbMain
			// 
			this.tlbMain.Appearance = System.Windows.Forms.ToolBarAppearance.Flat;
			this.tlbMain.Buttons.AddRange(new System.Windows.Forms.ToolBarButton[] {
																					   this.btnAdd,
																					   this.btnExport,
																					   this.Separator1,
																					   this.btnBold,
																					   this.btnItalics,
																					   this.btnUnderline,
																					   this.Separator2,
																					   this.btnColour,
																					   this.btnFont});
			this.tlbMain.ButtonSize = new System.Drawing.Size(48, 48);
			this.tlbMain.DropDownArrows = true;
			this.tlbMain.ImageList = this.ToolbarImages;
			this.tlbMain.Location = new System.Drawing.Point(0, 0);
			this.tlbMain.Name = "tlbMain";
			this.tlbMain.ShowToolTips = true;
			this.tlbMain.Size = new System.Drawing.Size(544, 50);
			this.tlbMain.TabIndex = 0;
			this.tlbMain.ButtonClick += new System.Windows.Forms.ToolBarButtonClickEventHandler(this.tlbMain_ButtonClick);
			// 
			// btnAdd
			// 
			this.btnAdd.ImageIndex = 0;
			this.btnAdd.Text = "&Add";
			this.btnAdd.ToolTipText = "Add a date/time stamp";
			// 
			// btnExport
			// 
			this.btnExport.ImageIndex = 1;
			this.btnExport.Text = "&Export ...";
			this.btnExport.ToolTipText = "Export to an RTF file.";
			// 
			// Separator1
			// 
			this.Separator1.Style = System.Windows.Forms.ToolBarButtonStyle.Separator;
			// 
			// btnBold
			// 
			this.btnBold.ImageIndex = 2;
			this.btnBold.Style = System.Windows.Forms.ToolBarButtonStyle.ToggleButton;
			this.btnBold.Text = "&Bold";
			// 
			// btnItalics
			// 
			this.btnItalics.ImageIndex = 3;
			this.btnItalics.Style = System.Windows.Forms.ToolBarButtonStyle.ToggleButton;
			this.btnItalics.Text = "&Italics";
			// 
			// btnUnderline
			// 
			this.btnUnderline.ImageIndex = 4;
			this.btnUnderline.Style = System.Windows.Forms.ToolBarButtonStyle.ToggleButton;
			this.btnUnderline.Text = "&Underline";
			// 
			// Separator2
			// 
			this.Separator2.Style = System.Windows.Forms.ToolBarButtonStyle.Separator;
			// 
			// btnColour
			// 
			this.btnColour.ImageIndex = 5;
			this.btnColour.Text = "&Colour ...";
			// 
			// btnFont
			// 
			this.btnFont.ImageIndex = 6;
			this.btnFont.Text = "Font ...";
			// 
			// ToolbarImages
			// 
			this.ToolbarImages.ColorDepth = System.Windows.Forms.ColorDepth.Depth32Bit;
			this.ToolbarImages.ImageSize = new System.Drawing.Size(24, 24);
			this.ToolbarImages.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("ToolbarImages.ImageStream")));
			this.ToolbarImages.TransparentColor = System.Drawing.Color.Transparent;
			// 
			// statusBar1
			// 
			this.statusBar1.Location = new System.Drawing.Point(0, 432);
			this.statusBar1.Name = "statusBar1";
			this.statusBar1.Size = new System.Drawing.Size(544, 24);
			this.statusBar1.SizingGrip = false;
			this.statusBar1.TabIndex = 2;
			// 
			// dlgExport
			// 
			this.dlgExport.DefaultExt = "rtf";
			this.dlgExport.Filter = "Rich text files (*.rtf)|*.rtf|All files (*.*)|*.*";
			// 
			// rtxtResolution
			// 
			this.rtxtResolution.Dock = System.Windows.Forms.DockStyle.Fill;
			this.rtxtResolution.Location = new System.Drawing.Point(0, 50);
			this.rtxtResolution.Name = "rtxtResolution";
			this.rtxtResolution.Size = new System.Drawing.Size(544, 382);
			this.rtxtResolution.TabIndex = 3;
			this.rtxtResolution.Text = "";
			// 
			// MemoControl
			// 
			this.Controls.Add(this.rtxtResolution);
			this.Controls.Add(this.statusBar1);
			this.Controls.Add(this.tlbMain);
			this.Name = "MemoControl";
			this.Size = new System.Drawing.Size(544, 456);
			this.ResumeLayout(false);

		}
		#endregion








		public void TextPlain( string text)
		{
			this.rtxtResolution.Text = text;
		}

		public string TextPlain()
		{
			return this.rtxtResolution.Text;

		}

		public void TextRichFormat(string richTextFormat)
		{
			this.rtxtResolution.Rtf = richTextFormat;

		}

		public string TextRichFormat()
		{
			return this.rtxtResolution.Rtf;

		}
		/// <summary>
		/// Catches which button the user pressed.
		/// </summary>
		/// <param name="sender">Toolbar</param>
		/// <param name="e">ButtonClick event arguments</param>
		private void tlbMain_ButtonClick(object sender, System.Windows.Forms.ToolBarButtonClickEventArgs e)
		{
			if( e.Button == this.btnAdd ) 
			{

				this.AddStamp();
				return;
			}

			if( e.Button == this.btnBold ) 
			{
				SetNewSelectionStyle(FontStyle.Bold);
				return;
			}

			if( e.Button == this.btnItalics )
			{
				SetNewSelectionStyle(FontStyle.Italic);
				return;
			}

			if( e.Button == this.btnUnderline )
			{
				SetNewSelectionStyle(FontStyle.Underline);
				return;
			}

			if( e.Button == this.btnExport )
				if(dlgExport.ShowDialog() == DialogResult.OK )
					this.rtxtResolution.SaveFile(dlgExport.FileName);

			if(e.Button == this.btnColour )
				if( this.dlgColour.ShowDialog() == DialogResult.OK )				
					this.rtxtResolution.SelectionColor = this.dlgColour.Color;

			if(e.Button == this.btnFont )
				if( this.dlgFont.ShowDialog() == DialogResult.OK )
					this.rtxtResolution.SelectionFont = this.dlgFont.Font;
		}
				
		private void SetNewSelectionStyle( FontStyle style )
		{
			// loops through each character in the selected text setting its new style
			int selectionStart = this.rtxtResolution.SelectionStart;
			int selectionEnd = this.rtxtResolution.SelectionLength;

			if( this.rtxtResolution.SelectionLength == 0 ) return;
				
			for( int i = 0; i < selectionEnd; i++ )
			{
				this.rtxtResolution.Select( selectionStart + i, 1);
				Font charFont = this.rtxtResolution.SelectionFont;
				FontStyle charStyle = charFont.Style;

				charStyle = charStyle ^ style;

				try
				{
					this.rtxtResolution.SelectionFont = new Font(charFont.FontFamily, charFont.Size, charStyle);
				}
				catch( System.Exception ){}

			}

			this.rtxtResolution.SelectionStart = selectionStart;
			this.rtxtResolution.SelectionLength = selectionEnd;

		}		
		
		/// <summary>
		/// Adds a date time stamp to the end of the rich text box text.
		/// </summary>
		private void AddStamp()
		{
			String carriageReturns = "\r\n";
			String loggedBy = DateTime.Now + " -";

			this.rtxtResolution.SelectionStart = this.rtxtResolution.SelectionLength;
				
			if( !this.rtxtResolution.Text.Equals( "" )) carriageReturns += "\r\n";
			this.rtxtResolution.AppendText( carriageReturns);
			this.rtxtResolution.AppendText(loggedBy + "\r\n");
			this.rtxtResolution.SelectionStart = this.rtxtResolution.Text.Length - loggedBy.Length - 1;
			this.rtxtResolution.SelectionLength = loggedBy.Length;
			this.rtxtResolution.SelectionColor = Color.Blue;
			this.rtxtResolution.SelectionStart = this.rtxtResolution.Text.Length + 1;


		}

		/// <summary>
		/// Toggles the bold, underline and italics buttons as the user moves through the text in the
		/// RichTextBox
		/// </summary>
		/// <param name="sender">RichTextBox</param>
		/// <param name="e">Selection Changed event arguments</param>
		private void rtxtResolution_SelectionChanged(object sender, System.EventArgs e)
		{
			RichTextBox rtb = (RichTextBox) sender;
		
			if( rtb.SelectionFont != null )
			{
				this.btnBold.Pushed = rtb.SelectionFont.Bold;
				this.btnItalics.Pushed = rtb.SelectionFont.Italic;
				this.btnUnderline.Pushed = rtb.SelectionFont.Underline;
			}
		}

	}
}
