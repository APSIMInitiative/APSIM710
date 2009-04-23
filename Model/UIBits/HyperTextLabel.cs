

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Data;
using System.Windows.Forms;

using UIUtility;


namespace UIBits
{
	/// <summary>
	/// The HyperTextLabel class works in a similar way to a System.Windows.Forms.LinkLabel, where hyperlinks
	/// are drawn like a skin over the label's text.  This object was developed to allow hyperlink text to change
	/// at runtime, remapping the hyperlinks to the underlying text.  the location of each hyperlink's text is 
	/// maintained via a collection of HyperText.HyperTextLink objects.
	/// 
	/// To use ths object add a reference to it in your project and set the objects 'Text' property.  You then
	/// create on or more HyperText.HyperTextLink objects via this classes 'AddHyperText' method, passing in text found
	/// in the label and hyperlink data such as URL.
	/// <seealso cref="System.Windows.Foms.LinkLabel"/>
	/// </summary>
	public class HyperTextLabel : System.Windows.Forms.UserControl
	{

		#region Component Designer generated code
		/// <summary>
		/// Required method for Designer support - do not modify 
		/// the contents of this method with the code editor.
		/// </summary>
		private void InitializeComponent()
		{
			this.lblHyperText = new System.Windows.Forms.LinkLabel();
			this.SuspendLayout();
			// 
			// lblHyperText
			// 
			this.lblHyperText.Dock = System.Windows.Forms.DockStyle.Fill;
			this.lblHyperText.Location = new System.Drawing.Point(0, 0);
			this.lblHyperText.Name = "lblHyperText";
			this.lblHyperText.Size = new System.Drawing.Size(216, 40);
			this.lblHyperText.TabIndex = 0;
			this.lblHyperText.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.lblHyperText_LinkClicked);
			// 
			// HyperTextLabel
			// 
			this.Controls.Add(this.lblHyperText);
			this.Name = "HyperTextLabel";
			this.Size = new System.Drawing.Size(216, 40);
			this.ResumeLayout(false);

		}
		#endregion

		#region "Events and Delegates"
		
		/// <summary>
		/// The delegate for the 'LinkClicked' event for this class.
		/// </summary>
		public delegate void OnLinkClicked( object sender, HyperTextLink link );
		public event OnLinkClicked LinkClicked;

		# endregion

		# region "Properties"

		/// <summary>
		/// The object used to display hyperlinks.
		/// </summary>
		private System.Windows.Forms.LinkLabel lblHyperText;

		/// <summary>
		/// The list of HyperText.HyperTextLink objects.
		/// </summary>
		private System.Collections.ArrayList HyperTextArray;

		
		/// <summary>
		/// Required designer variable.
		/// </summary>
		private System.ComponentModel.Container components = null;

		public HyperTextLabel()
		{
			// This call is required by the Windows.Forms Form Designer.
			InitializeComponent();

			this.HyperTextArray = new ArrayList();
			this.lblHyperText.Text = this.Name;
			

		}


		/// <summary>
		/// Sets or gets the Text for this control.
		/// </summary>
		[
		Browsable(true),
		Category("Appearance"),
		Description("Gets or sets the label text."),
		DesignerSerializationVisibility(DesignerSerializationVisibility.Visible)
		]
		public override string Text
		{
			get
			{
				return this.lblHyperText.Text;
				

			}
			set
			{
				// Each time the text changes clear out the LinkLabels Link collection.
				this.lblHyperText.Links.Clear();
				this.lblHyperText.Text = value;

				// Loop through each HyperText.HyperTextLink, recreating the LinkLabelLink collection.
                foreach (HyperTextLink link in this.GetHyperTextArray())
				{
						link.TextPosition = this.Text.IndexOf( link.Text, link.StartPosition );
						this.lblHyperText.Links.Add( link.StartPosition, link.Text.Length, link.HyperTextData );
				}
			}
		}



		/// <summary>
		/// How the links appear within the caption.
		/// </summary>
		[
		Category("Appearance"),
		Description("Gets or sets the behaviour of the link when drawn or as the user interacts.")
		]
		public System.Windows.Forms.LinkBehavior LinkBehaviour 
		{
			get 
			{
				return this.lblHyperText.LinkBehavior;
			}
			set 
			{
				this.lblHyperText.LinkBehavior = value;
			}


		}

		/// <summary>
		/// The alignment of the text.
		/// </summary>
		[
		Category("Appearance"),
		Description("Gets or sets the alignment of hyper text.")
		]
		public System.Drawing.ContentAlignment TextAlign
		{
			get
			{
				return this.lblHyperText.TextAlign;
			}
			set
			{
				this.lblHyperText.TextAlign = value;
			}

		}
		
		/// <summary>
		/// Set or Get the colour of the link.
		/// </summary>
		[
		Category("Appearance"),
		Description("Gets or sets the link colour.")
		]
		public System.Drawing.Color LinkColour
		{
			get
			{
				return this.lblHyperText.LinkColor;
			}
			set 
			{
				this.lblHyperText.LinkColor = value;
			}
		}
			
			

		/// <summary>
		/// Set or get the background colour of the control.
		/// </summary>
		[
		Category("Appearance"),
		Description("Gets or sets the background colour.")
		]
		public override Color BackColor
		{
			get
			{
				return base.BackColor;
			}
			set
			{
				base.BackColor = value;
				this.lblHyperText.BackColor = value;

			}
		}



		/// <summary>
		/// Set or get the control's current font.
		/// </summary>
		[
		Category("Appearance"),
		Description("Gets or sets the control's font.")
		]
		public override Font Font
		{
			get
			{
				return base.Font;
			}
			set
			{
				base.Font = value;
				this.lblHyperText.Font = value;
			}
		}

		/// <summary>
		/// Clean up any resources being used.
		/// </summary>
		protected override void Dispose( bool disposing )
		{
			if( disposing )
			{
				if( components != null )
					components.Dispose();
			}
			base.Dispose( disposing );
		}

		# endregion

		#region "Methods"

		/// <summary>
		/// Used privately to retrieve the current list of HyperText.HyperTextLink objects.
		/// </summary>
		/// <returns>A System.Collections.ArrayList of HyperText.HyperTextLink objects.</returns>
		private System.Collections.ArrayList GetHyperTextArray()
		{
			return this.HyperTextArray;
		}

		/// <summary>
		/// Retrieve an Array of HyperText.HyperTextLink objects.
		/// </summary>
		/// <returns>An array of HyperText.HyperTextLink objects.</returns>
		public HyperTextLink[] HyperTextCollection()
		{
			HyperTextLink[] arrHyperText = new HyperTextLink[ this.GetHyperTextArray().Count ];

			// Loop through the System.Collections.ArrayList adding it a standard array object.
			for( int i = 0; i < this.GetHyperTextArray().Count; i++ )
			{
				arrHyperText[i] = (HyperTextLink) this.GetHyperTextArray()[i];

			}

			return arrHyperText;

		}

		
		/// <summary>
		/// Creates a hyperlink within this label's text.
		/// </summary>
		/// <param name="text">Matches a portion of text defined within the 'Text' property.</param>
		/// <param name="hyperTextData">The underlying value of the hyperlink, eg a URL.</param>
		/// <returns>HyperText.HyperTextLink object.</returns>
		public HyperTextLink AddHyperText(string text, string hyperTextData)
		{
			HyperTextLink newLink = new HyperTextLink();
			newLink.Text = text ;
			newLink.HyperTextData = hyperTextData;

			this.GetHyperTextArray().Add( newLink );
			this.Text = this.lblHyperText.Text;

			return newLink;

		}


		/// <summary>
		/// Creates a hyperlink within this label's text.
		/// </summary>
		/// <param name="text">Matches a portion of text defined within the 'Text' property.</param>
		/// <param name="hyperTextData">The underlying value of the hyperlink, eg a URL.</param>
		/// <param name="startPosition">The starting position from which to locate the given text within the 'Text' property</param>
		/// <returns>HyperText.HyperTextLink object.</returns>
		public HyperTextLink AddHyperText(string text, string hyperTextData, int startPosition )
		{
			
			HyperTextLink newLink = new HyperTextLink();
			newLink.Text = text ;
			newLink.HyperTextData = hyperTextData;
			newLink.TextPosition = startPosition;

			this.GetHyperTextArray().Add( newLink );
			this.Text = this.lblHyperText.Text;

			return newLink;


		}

		/// <summary>
		/// Add a HyperTextLink at the specified index
		/// </summary>
		/// <param name="link">The new link.</param>
		/// <param name="index">The point in the HyperTexLink array to add the link.</param>
		/// <returns></returns>
		public HyperTextLink AddHyperText(HyperTextLink link, int index)
		{
			this.GetHyperTextArray()[index] = link;
			return link;

		}

		/// <summary>
		/// Remove the given link from the the 'Text' property.
		/// </summary>
		/// <param name="link">The HyperText.HyperTextLink object to remove.</param>
		public void RemoveHyperText(HyperTextLink link )
		{
			this.GetHyperTextArray().Remove(link);
			this.Text = this.Text;

		}

		/// <summary>
		/// Replace an existing link with new details.
		/// </summary>
		/// <param name="oldLink">The existing HyperText.HyperTextLink object to replace.</param>
		/// <param name="newText">The new text portion of the HyperText.HyperTextLink.</param>
		/// <param name="newHyperTextData">the underlying hyperlink value.</param>
		public HyperTextLink ReplaceLink( HyperTextLink oldLink, string newText, string newHyperTextData )
		{
			// Get the current postion of the text about to be replaced.
			int oldTextStartPosition = this.Text.IndexOf( oldLink.Text, oldLink.StartPosition);
			int oldLinkIndex = -1;
			
			// Get the difference in the length between the old and new text
			int balanceText = newText.Length - oldLink.Text.Length;
			
			// Remove the old text and insert the new text at the same point.
			string newLabelText = this.Text.Remove( oldTextStartPosition, oldLink.Text.Length);
			newLabelText = newLabelText.Insert(oldTextStartPosition, newText);


			//loop through current links and adjust the start position of each according to the balance of each
			foreach( HyperTextLink alink in this.GetHyperTextArray() )
			{
				// reset the starting positions for all links found after the old link.
				if( !alink.Equals( oldLink ) && alink.TextPosition >= oldTextStartPosition ) 
					alink.TextPosition += balanceText;

				if( alink.Equals(oldLink)) oldLinkIndex = this.GetHyperTextArray().IndexOf(oldLink);

			}

			// Update the labels text, forcing a refresh of the labels link locations.
			HyperTextLink newLink = new HyperTextLink();
			newLink.Text = newText;
			newLink.HyperTextData = newHyperTextData;
			newLink.TextPosition = oldTextStartPosition;
			
			HyperTextLink replacedLink = this.AddHyperText( newLink, oldLinkIndex );

			this.Text = newLabelText;

			return replacedLink;
			
		}

		public void ClearHyperTextLinks()
		{
			this.HyperTextArray.Clear();
			this.lblHyperText.Links.Clear();

		}

		
		/// <summary>
		/// Catch the LinkClicked event of the System.Windows.Forms.LinkLabel object.
		/// </summary>
		/// <param name="sender">The object that fired the event.</param>
		/// <param name="e">The link that was clicked by the User.</param>
		private void lblHyperText_LinkClicked(object sender, System.Windows.Forms.LinkLabelLinkClickedEventArgs e)
		{
			if( this.LinkClicked == null) return;

			//find which HyperText.HyperTextLink was clicked and fire this event for registered objects.
			foreach( HyperTextLink alink in this.GetHyperTextArray() )
			{
				if( alink.HyperTextData.Equals( e.Link.LinkData ) )
				{
					this.LinkClicked( this, alink );
					return;
				}
			}
		}

	}

# endregion

}
