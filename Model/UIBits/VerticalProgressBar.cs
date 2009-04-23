

using System;
using System.Collections;
using System.ComponentModel;
using System.Drawing;
using System.Windows.Forms;


namespace UIBits
{
	public enum Styles
	{
		Classic, // same as ProgressBar
		Solid
	}

	public enum BorderStyles
	{
		Classic, // same as ProgressBar
		None
	}
	/// <summary>
	/// Represents a Windows vertical progress bar control.
	/// </summary>
	[Description("Vertical Progress Bar")]
	[ToolboxBitmap(typeof(ProgressBar))]
	[Browsable(false)]
	public sealed class VerticalProgressBar : System.Windows.Forms.UserControl
	{

		private System.ComponentModel.Container components = null;

		private int m_Value = 50;
		private int m_Minimum = 0;
		private int m_Maximum = 100;
		private int m_Step = 10;
		
		private Styles m_Style = Styles.Classic; //Bar Style
		private BorderStyles m_BorderStyle = BorderStyles.Classic; 
		private Color m_Color = Color.Blue; //Bar color

		public VerticalProgressBar()
		{
			InitializeComponent();

            // ***** avoid flickering
            this.SetStyle(ControlStyles.AllPaintingInWmPaint, true);
            this.SetStyle(ControlStyles.UserPaint, true);
            this.SetStyle(ControlStyles.DoubleBuffer, true);

			this.Name = "VerticalProgressBar";
			this.Size = new Size(10, 120);
		}

        [Description( "VerticalProgressBar Maximum Value")]
		[Category( "VerticalProgressBar" )]
		[RefreshProperties(RefreshProperties.All)]
		public int Maximum
		{
			get
			{
				return m_Maximum;
			}
			set
			{
				m_Maximum = value;
				if(m_Maximum < m_Minimum)
					m_Minimum=m_Maximum;
				if(m_Maximum < m_Value)
					m_Value = m_Maximum;
				Invalidate();
			}
		}
		[Description( "VerticalProgressBar Minimum Value")]
		[Category( "VerticalProgressBar" )]
		[RefreshProperties(RefreshProperties.All)]
		public int Minimum
		{
			get
			{
				return m_Minimum;
			}
			set
			{
				m_Minimum = value;
				if(m_Minimum > m_Maximum)
					m_Maximum = m_Minimum;
				if(m_Minimum > m_Value)
					m_Value = m_Minimum;
				Invalidate();
			}
		}
		[Description( "VerticalProgressBar Step")]
		[Category( "VerticalProgressBar" )]
		[RefreshProperties(RefreshProperties.All)]
		public int Step
		{
			get
			{
				return m_Step;
			}
			set
			{
				m_Step = value;
			}
		}
		[Description( "VerticalProgressBar Current Value")]
		[Category( "VerticalProgressBar" )]
		public int Value
		{
			get
			{
				return m_Value;
			}
			set
			{
				m_Value = value;
				if(m_Value > m_Maximum)
					m_Value = m_Maximum;
				if(m_Value < m_Minimum)
					m_Value = m_Minimum;
				Invalidate();
			}
		}
		[Description( "VerticalProgressBar Color")]
		[Category( "VerticalProgressBar" )]
		[RefreshProperties(RefreshProperties.All)]
		public System.Drawing.Color Color
		{
			get
			{
				return m_Color;
			}
			set
			{
				m_Color = value;
				Invalidate();
			}
		}
		[Description( "VerticalProgressBar Border Style")]
		[Category( "VerticalProgressBar" )]
		public new BorderStyles BorderStyle
		{
			get
			{
				return m_BorderStyle;
			}
			set
			{
				m_BorderStyle = value;
				Invalidate();
			}
		}
		[Description( "VerticalProgressBar Style")]
		[Category( "VerticalProgressBar" )]
		public Styles Style
		{
			get
			{
				return m_Style;
			}
			set
			{
				m_Style = value;
				Invalidate();
			}
		}
		
		public void PerformStep()
		{
			m_Value+=m_Step;

			if(m_Value > m_Maximum)
				m_Value = m_Maximum;
			if(m_Value < m_Minimum)
				m_Value = m_Minimum;
			
			Invalidate();
			return;
		}

		public void Increment(int value)
		{
			m_Value+=value;

			if(m_Value > m_Maximum)
				m_Value = m_Maximum;
			if(m_Value < m_Minimum)
				m_Value = m_Minimum;
			
			Invalidate();
			return;
		}


		private void drawBorder(Graphics dc)
		{
			if(m_BorderStyle == BorderStyles.Classic)
			{
				Color darkColor = ControlPaint.Dark(this.BackColor);
				Color brightColor = ControlPaint.Dark(this.BackColor);
				Pen p = new Pen(darkColor, 1);
				dc.DrawLine(p, this.Width, 0, 0, 0);
				dc.DrawLine(p, 0,0, 0, this.Height);
				p = new Pen(brightColor,1);
				dc.DrawLine(p, 0, this.Height, this.Width,this.Height);
				dc.DrawLine(p, this.Width,this.Height, this.Width,0);
			}
		}

		private void drawBar(Graphics dc)
		{
			if(m_Minimum == m_Maximum || (m_Value - m_Minimum) == 0)
				return;

			int width;		// the bar width
			int height;		// the bar height
			int x;				// the bottom-left x pos of the bar
			int y;				// the bottom-left y pos of the bar

			if(m_BorderStyle == BorderStyles.None)
			{
				width = this.Width;
				x = 0;
				y = this.Height;
			}
			else
			{
				if(this.Width > 4 || this.Height > 2)
				{
					width = this.Width - 4;
					x = 2;
					y = this.Height - 1; 
				}
				else
					return; // Cannot draw
			}

            height = (m_Value - m_Minimum) * this.Height / (m_Maximum - m_Minimum); // the bar height

			if(m_Style == Styles.Solid)
			{
				drawSolidBar(dc, x, y, width, height);
			}
			if(m_Style == Styles.Classic)
			{
				drawClassicBar(dc, x, y, width, height);
			}
		}
		private void drawSolidBar(Graphics dc, int x, int y, int width, int height)
		{
			dc.FillRectangle(new SolidBrush(m_Color), x, y-height, width, height);
		}
		private void drawClassicBar(Graphics dc, int x, int y, int width, int height)
		{
			int valuepos_y = y - height;			// The pos y of value

			int blockheight = width * 3 / 4;        // The height of the block

            if (blockheight <= -1) return; // make sure blockheight is larger than -1 in order not to have the infinite loop.
			
			for(int currentpos = y; currentpos > valuepos_y; currentpos -= blockheight+1)
			{
				dc.FillRectangle(new SolidBrush(m_Color), x, currentpos - blockheight, width, blockheight);
			}
		}
		protected override void OnPaint(System.Windows.Forms.PaintEventArgs e)
		{
			Graphics dc = e.Graphics;
			
			//Draw Bar
			drawBar(dc);
			
			//Draw Border
			drawBorder(dc);

			base.OnPaint(e);
		}

		protected override void OnSizeChanged(EventArgs e)
		{
			base.OnSizeChanged (e);
			Invalidate();
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
			components = new System.ComponentModel.Container();
		}
		#endregion
	}
}
