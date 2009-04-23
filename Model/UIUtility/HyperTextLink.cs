using System;

namespace UIUtility
{
	/// <summary>
	/// Instances of the HyperTextLink class are typically instanciated via the 'AddHyperText' method
	/// of the HyperText.HyperTextLabel class.  Its properties and methods are used to maintain the location
	/// of an individual link within a HyperTextlabel
	/// <seealso cref="System.Windows.Forms.LinkLabel.Link"
	/// </summary>
	public class HyperTextLink
	{
		private string text;
		private string hyperTextData;
		private int startPosition;
		private object tag;
		

		public HyperTextLink(){
			text = "";
			hyperTextData = "";
			startPosition = 0;
			tag = null;

		
		}

		/// <summary>
		/// The portion of text found in the HyperText label.
		/// </summary>
		public string Text
		{
			get
			{
				return this.text;
			}
			set
			{
				this.text = value;
			}
		}

		
		/// <summary>
		/// The underlying value for the link.
		/// </summary>
		public string HyperTextData
		{
			get
			{
				return this.hyperTextData;
			}
			set
			{
				this.hyperTextData = value;
			}
		}


		/// <summary>
		/// The public read only start position of the text portion in the HyperText label.
		/// </summary>
		public int StartPosition
		{
			get
			{
				return this.startPosition;
			}

		}

		
		/// <summary>
		/// The property used to set the start position for this portion of text.
		/// </summary>
		public int TextPosition
		{
			get
			{
				return this.startPosition;
			}
			set 
			{
				this.startPosition = value;
			}
		}


		/// <summary>
		/// A propety to store extra details about an instance of this class.
		/// </summary>
		public object Tag
		{
			get
			{
				return this.tag;
			}
			set
			{
				this.tag = value;
			}
		}


	}
}
