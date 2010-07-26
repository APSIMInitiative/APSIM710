using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using Curves;

namespace CSUserInterface
{
    public partial class RotationsUI : Controllers.BaseView
    {
        public RotationsUI()
        {
            InitializeComponent();
        }
        protected override void OnLoad()
        {
            BuildGraphDisplay();
            panel2.Height = panel2.MaxHeight;
            panel2.Width = panel2.MaxWidth;
        }
        public override void OnRefresh()
        {
        }
        public override void OnSave()
        {
        }
        private void BuildGraphDisplay()
        {
            //panel2.Nodes.Clear();
            //panel2.Arcs.Clear();
            //should only have to load it once.
            if (panel2.Nodes.Count == 0)
            {
                XmlNodeList nodes = Data.SelectNodes("//node");
                foreach (XmlNode tmpNode in nodes)
                {
                    GDNode tmpGDNode = ReadGDNode(tmpNode);
                    panel2.AddNode(tmpGDNode);
                }
                nodes = Data.SelectNodes("//arc");
                foreach (XmlNode tmpNode in nodes)
                {
                    GDArc tmpGArc = ReadGDArc(tmpNode);
                    panel2.AddArc(tmpGArc);
                }
            }
        }
        public GDNode ReadGDNode(XmlNode tmpNode)
        {
            GDNode gd = new GDNode();
            
            string sName = ReadChildNodeText(tmpNode, "name");
            int x1 = ReadChildNodeValue(tmpNode, "x1");
            int y1 = ReadChildNodeValue(tmpNode, "y1");
            gd.Name = sName;

            gd.Start.X = x1;
            gd.Start.Y = y1;
            int x2 = ReadChildNodeValue(tmpNode, "x2");
            int y2 = ReadChildNodeValue(tmpNode, "y2");
            gd.End.X = x2;
            gd.End.Y = y2;

            gd.Mid.X = gd.Start.X + (gd.End.X - gd.Start.X) / 2;
            gd.Mid.Y = gd.Start.Y + (gd.End.Y - gd.Start.Y) / 2;

            gd.Width = x2 - x1;
            gd.Height = y2 - y1;
            return gd;
        }
        public GDArc ReadGDArc(XmlNode tmpNode)
        {
            GDArc ga = new GDArc();

            string sName = ReadChildNodeText(tmpNode, "name");
            string sSource = ReadChildNodeText(tmpNode, "source");
            string sTarget = ReadChildNodeText(tmpNode, "target");
            int x = ReadChildNodeValue(tmpNode, "x");
            int y = ReadChildNodeValue(tmpNode, "y");
            ga.Name = sName;
            ga.Source = sSource;
            ga.Target = sTarget;
            ga.Location.X = x;
            ga.Location.Y = y;
            //actions and rules
            return ga;
        }
        public int ReadChildNodeValue(XmlNode tmpNode, string sChild)
        {
            if (tmpNode != null)
            {
                return ReadNodeValue(tmpNode.SelectSingleNode(sChild));
                //return ReadNodeValue(tmpNode.SelectSingleNode("//" + sChild));
            }
            return 0;
        }
        public string ReadChildNodeText(XmlNode tmpNode, string sChild)
        {
            if (tmpNode != null)
            {
                XmlNode chNode = tmpNode.SelectSingleNode(sChild);
                if (chNode != null)
                    return chNode.InnerText;
            }
            return "";
        }
        public int ReadNodeValue(XmlNode tmpNode)
        {
            if (tmpNode != null)
            {
                string sReturn = tmpNode.InnerText;
                if (sReturn != "")
                {
                    try
                    {
                        return (int)Convert.ToSingle(sReturn);
                    }
                    catch (Exception ex) 
                    {}
                }
            }
            return 0;
        }
        public int ReadAttValue(XmlNode tmpNode, string sAtt)
        {
            string sReturn = ReadAttText(tmpNode, sAtt);
            if (sReturn != "")
            {
                try
                {
                    return (int)Convert.ToSingle(sReturn);
                }catch(Exception ex){}
            }
            return 0;
        }
        public string ReadAttText(XmlNode tmpNode, string sAtt)
        {
            XmlElement tmp = tmpNode as XmlElement;
            if (tmp != null)
            {
                if (tmp.Attributes != null)
                {
                    XmlAttribute tmpAtt = tmp.Attributes[sAtt];
                    if (tmpAtt != null)
                    {
                        return tmpAtt.Value;
                    }
                }
            }
            return "";
        }

        
    }

    public class GDObject
    {
        public virtual bool Selected { get; set; }
        public virtual int Left { get; set; }
        public virtual int Right { get; set; }
        public virtual int Top { get; set; }
        public virtual int Bottom { get; set; }

        public virtual void Paint(Graphics GraphicsObject) { }
        public virtual void Move(int x, int y) { }
        public virtual void Update() { }
        public virtual bool Clicked(int x, int y) { return false; }
        public virtual double GetDistance(Point point1, Point point2)
        {
            //pythagoras theorem c^2 = a^2 + b^2
            //thus c = square root(a^2 + b^2)
            double a = (double)(point2.X - point1.X);
            double b = (double)(point2.Y - point1.Y);

            return Math.Sqrt(a * a + b * b);
        }

    }
    public class GDArc : GDObject
    {
        public List<string> Rules = new List<string>();
        public List<string> Actions = new List<string>();
        public Point Location = new Point();

        public string Name;
        public string Source = "";
        public string Target = "";
        public Color Line;
        private int clickTolerence = 3;

        public GDNode SourceNode = null;
        public GDNode TargetNode = null;

        private BezierCurve bezCurve = new BezierCurve();
        public List<Point> BezPoints = new List<Point>();
        private double[] bezParameters = new double[8];

        #region interface properties
        //these aren't technically correct as their bounds are also described by the nodes they connect
        //used in conjunction with the others this should work though
        public override int Left { get { return Location.X; } }
        public override int Right { get { return Location.X; } }
        public override int Top { get { return Location.Y; } }
        public override int Bottom { get { return Location.Y; } }
        #endregion

        public override void Paint(Graphics GraphicsObject)
        {
            if (BezPoints.Count == 0)
                CalcBezPoints();

            if (SourceNode != null && TargetNode != null)
            {
                Pen pen = new Pen(Color.Black);
                if (Selected)
                    pen.Color = Color.Blue;

                GraphicsObject.DrawBezier(pen, SourceNode.Mid, Location, Location, TargetNode.Mid);

                //find closest point in the bezPoints to the intersection point that is outside the target
                //work backwards through BezPoints array and use the first one that is outside the target
                for (int i = BezPoints.Count - 1; i >= 0; --i)
                {
                    Point arrowHead; 
                    if (!TargetNode.Clicked(BezPoints[i].X, BezPoints[i].Y))
                    {
                        arrowHead = BezPoints[i];
                        --i;
                        //keep moving along the line until distance = ??
                        for (; i >= 0; --i)
                        {
                            double dist = GetDistance(BezPoints[i], arrowHead);
                            if (dist > 10)
                            {
                                //Pen p = new Pen(Color.Red, 10);
                                //ArrowAnchor = Point            
                                pen.StartCap = System.Drawing.Drawing2D.LineCap.ArrowAnchor;
                                //Flat = Flat side            
                                pen.EndCap = System.Drawing.Drawing2D.LineCap.Flat;
                                pen.Width = 10;
                                GraphicsObject.DrawLine(pen, arrowHead, BezPoints[i]);
                                break;
                            }
                        }
                        break;
                    }
                }
            }
        }
/*        int PointInPolygon(Point *polygon,int N,Point p)
        {
          int counter = 0;
          int i;
          double xinters;
          Point p1,p2;

          p1 = polygon[0];
          for (i=1;i<=N;i++) {
            p2 = polygon[i % N];
            if (p.y > MIN(p1.y,p2.y)) {
              if (p.y <= MAX(p1.y,p2.y)) {
                if (p.x <= MAX(p1.x,p2.x)) {
                  if (p1.y != p2.y) {
                    xinters = (p.y-p1.y)*(p2.x-p1.x)/(p2.y-p1.y)+p1.x;
                    if (p1.x == p2.x || p.x <= xinters)
                      counter++;
                  }
                }
              }
            }
            p1 = p2;
          }

          return (counter % 2 != 0);
        }
 */

        public override void Move(int x, int y)
        {
            Location.X = Location.X + x;
            Location.Y = Location.Y + y;
            if (x != 0 || y != 0)
                CalcBezPoints();
        }
        public override void Update()
        {
            //a signal to recalc - forced by a move event on the targetnode
            //needs to be seperate from a moveall event
            CalcBezPoints();
        }
        public override bool Clicked(int x, int y)
        { 
            foreach(Point tmpPoint in BezPoints)
            {
                if (tmpPoint.X > x - clickTolerence && tmpPoint.X < x + clickTolerence)
                {
                    if (tmpPoint.Y > y - clickTolerence && tmpPoint.Y < y + clickTolerence)
                        return true;
                }
            }
            return false;
        }
        private void CalcBezPoints()
        {
            int iStart = Math.Min(SourceNode.Mid.X, TargetNode.Mid.X);
            int iEnd = Math.Max(SourceNode.Mid.X, TargetNode.Mid.X);
            int xPoints = iEnd - iStart;
            iStart = Math.Min(SourceNode.Mid.Y, TargetNode.Mid.Y);
            iEnd = Math.Max(SourceNode.Mid.Y, TargetNode.Mid.Y);
            int yPoints = iEnd - iStart;
            
            //will calc a min of 20 points
            int points = Math.Max(Math.Max(xPoints, yPoints), 10) * 2;
            double[] output = new double[points];

            bezParameters[0] = SourceNode.Mid.X;
            bezParameters[1] = SourceNode.Mid.Y;
            bezParameters[2] = Location.X;
            bezParameters[3] = Location.Y;
            bezParameters[4] = Location.X;
            bezParameters[5] = Location.Y;
            bezParameters[6] = TargetNode.Mid.X;
            bezParameters[7] = TargetNode.Mid.Y;

            BezPoints.Clear();
            bezCurve.Bezier2D(bezParameters, (points) / 2, output);
            for (int i = 0; i < points - 2; i += 2)
            {
                BezPoints.Add(new Point((int)output[i], (int)output[i+1]));
            }
        }
        private int FindLineCircleIntersections(float cx, float cy, float radius,
            Point point1, Point point2, out PointF intersection1, out PointF intersection2)
        {
            float dx, dy, A, B, C, det, t;

            dx = point2.X - point1.X;
            dy = point2.Y - point1.Y;

            A = dx * dx + dy * dy;
            B = 2 * (dx * (point1.X - cx) + dy * (point1.Y - cy));
            C = (point1.X - cx) * (point1.X - cx) + (point1.Y - cy) * (point1.Y - cy) - radius * radius;

            det = B * B - 4 * A * C;
            if ((A <= 0.0000001) || (det < 0))
            {
                // No real solutions.
                intersection1 = new PointF(float.NaN, float.NaN);
                intersection2 = new PointF(float.NaN, float.NaN);
                return 0;
            }
            else if (det == 0)
            {
                // One solution.
                t = -B / (2 * A);
                intersection1 = new PointF(point1.X + t * dx, point1.Y + t * dy);
                intersection2 = new PointF(float.NaN, float.NaN);
                return 1;
            }
            else
            {
                // Two solutions.
                t = (float)((-B + Math.Sqrt(det)) / (2 * A));
                intersection1 = new PointF(point1.X + t * dx, point1.Y + t * dy);
                t = (float)((-B - Math.Sqrt(det)) / (2 * A));
                intersection2 = new PointF(point1.X + t * dx, point1.Y + t * dy);
                return 2;
            }
        }

    }
    public class GDNode : GDObject
    {
        public Point Start = new Point();
        public Point End = new Point();
        public Point Mid = new Point();

        public List<GDArc> Arcs = new List<GDArc>();

        public string Name;
        public Color Fill;

        public int Height { get; set; }
        public int Width { get; set; }
        public override int Left { get { return Start.X; }}
        public override int Right { get { return Start.X + Width; }}
        public override int Top { get { return Start.Y; } }
        public override int Bottom { get { return Start.Y + Height; }}

        public override void Paint(Graphics GraphicsObject)
        {
            // Create point for upper-left corner of drawing.
            Rectangle tmp = new System.Drawing.Rectangle();
            tmp.X = Start.X;
            tmp.Y = Start.Y;
            tmp.Height = Height;
            tmp.Width = Width;
            //tmp.Height = End.Y - Start.Y;
            //tmp.Width = End.X - Start.X; ;

            SolidBrush drawBrush = new SolidBrush(Color.Beige);
            Pen pen = new Pen(Color.Black);
            if (Selected)
            {
                pen.Color = Color.Blue;
                pen.Width = 3;
            }

            GraphicsObject.FillEllipse(drawBrush, tmp);
            GraphicsObject.DrawEllipse(pen, tmp);
        }
        public override void Move(int x, int y)
        {
            Start.X = Start.X + x;
            Start.Y = Start.Y + y;

            Mid.X = Start.X + Width / 2;
            Mid.Y = Start.Y + Width / 2;

            foreach (GDArc arc in Arcs)
                arc.Update();
        }
        public override bool Clicked(int x, int y)
        {
            Point clickPoint = new Point(x, y);
            double dist = GetDistance(Mid, clickPoint);
            return dist < (Width / 2);
        }

    }
    public partial class GraphDisplayObject : System.Windows.Forms.Panel
    {
        public List<GDNode> Nodes = new List<GDNode>();
        public List<GDArc> Arcs = new List<GDArc>();
        public int MaxHeight = 0;
        public int MaxWidth = 0;

        private GDObject m_SelectedObject = null;
        private bool mouseDown = false;
        private Point LastPos;

        public GraphDisplayObject()
        {
            InitializeComponent();
        }
        private void InitializeComponent()
        {
            SetStyle(ControlStyles.UserPaint, true);
            SetStyle(ControlStyles.AllPaintingInWmPaint, true);
            SetStyle(ControlStyles.OptimizedDoubleBuffer, true);

            this.MouseDown += this.OnMouseDown;
            this.MouseUp += this.OnMouseUp;
            this.MouseMove += this.OnMouseMove;
        }
        public GDObject SelectedObject
        {
            get { return m_SelectedObject; }
            set 
            {
                if (m_SelectedObject != null && value != m_SelectedObject)
                    m_SelectedObject.Selected = false;
                m_SelectedObject = value;
                if (m_SelectedObject != null)
                {
                    m_SelectedObject.Selected = true;
                }
            }
        }

        public void AddNode(GDNode tmpNode)
        {
            Nodes.Add(tmpNode);
            MaxHeight = Math.Max(MaxHeight, tmpNode.End.Y);
            MaxWidth = Math.Max(MaxWidth, tmpNode.End.X);
        }
        public void RemoveNode()
        {
        }
        public GDNode FindNode(string sName)
        {
            foreach (GDNode tmp in Nodes)
            {
                if (tmp.Name == sName)
                    return tmp;
            }
            return null;
        }
        public void AddArc(GDArc tmpArc)
        {
            //link arc with nodes
            tmpArc.SourceNode = FindNode(tmpArc.Source);
            if (tmpArc.SourceNode != null)
                tmpArc.SourceNode.Arcs.Add(tmpArc);

            tmpArc.TargetNode = FindNode(tmpArc.Target);
            if (tmpArc.TargetNode != null)
                tmpArc.TargetNode.Arcs.Add(tmpArc);
            
            Arcs.Add(tmpArc);
        }
        public void RemoveArc()
        {
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            UpdateLimits();
            if(mouseDown)
                e.Graphics.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.HighQuality;
            else
                e.Graphics.SmoothingMode = System.Drawing.Drawing2D.SmoothingMode.HighQuality;

            foreach (GDArc tmpArc in Arcs)
            {
                tmpArc.Paint(e.Graphics);
            }
            foreach (GDNode tmpNode in Nodes)
            {
                tmpNode.Paint(e.Graphics);
            }
        }
        private void UpdateLimits()
        {
            //calc min left, max right, mintop, maxbottom values for width & height
            int iLeft = Width, iRight = 0, iTop = Height, iBottom = 0;
            if (Nodes.Count > 0)
            {
                foreach (GDNode tmpNode in Nodes)
                {
                    iLeft = Math.Min(iLeft, tmpNode.Left);
                    iRight = Math.Max(iRight, tmpNode.Right);
                    iTop = Math.Min(iTop, tmpNode.Top);
                    iBottom = Math.Max(iBottom, tmpNode.Bottom);
                }
                int iLeftAdj = 0;
                int iTopAdj = 0;
                if (iLeft > Margin.Left || iLeft < Margin.Left)
                {
                    iLeftAdj = 0 - (iLeft - Margin.Left);
                    iRight = iRight - iLeftAdj;
                }
                if (iTop > Margin.Top || iTop < Margin.Top)
                {
                    iTopAdj = 0 - (iTop - Margin.Top);
                    iBottom = iBottom - iTopAdj; //works because of double -ve
                }
                if (iLeftAdj != 0 || iTopAdj != 0)
                {
                    MoveAll(iLeftAdj, iTopAdj);
                }
                Width = iRight + Margin.Right + Margin.Left;
                Height = iBottom + Margin.Top + Margin.Bottom;
            }
            else
            {
                Width = 0;
                Height = 0;
            }
        }
        protected void MoveAll(int x, int y)
        {
            foreach (GDNode tmpNode in Nodes)
            {
                tmpNode.Move(x, y);
            }
            foreach (GDArc tmpArc in Arcs)
            {
                tmpArc.Move(x, y);
            }
        }
        private void OnMouseDown(object sender, MouseEventArgs e)
        {
            //if an object is under the mouse then select it
            //SelectedObject = Nodes[0];
            GDObject tmpObject = null;
            //reverse order as last drawn will be on top
            for(int i = Nodes.Count-1; i >= 0; --i)
            {
                if(Nodes[i].Clicked(e.X, e.Y))
                {
                    tmpObject = Nodes[i];
                    break;
                }
            }
            if (tmpObject == null)
            {
                foreach (GDArc tmpArc in Arcs)
                {
                    if(tmpArc.Clicked(e.X, e.Y))
                        tmpObject = tmpArc;
                }
            }
            SelectedObject = tmpObject;

            if (SelectedObject != null)
            {
                mouseDown = true;
                LastPos.X = e.X;
                LastPos.Y = e.Y;
            }
            this.Invalidate();
        }
        private void OnMouseUp(object sender, MouseEventArgs e)
        {
            //if an object is selected, then 
            if (SelectedObject != null)
            {
                mouseDown = false;
                this.Invalidate();
            }
        }
        private void OnMouseMove(object sender, MouseEventArgs e)
        {
            //if an object is under the mouse then select it
            if (mouseDown && SelectedObject != null)
            {
                int x = e.X - LastPos.X;
                int y = e.Y - LastPos.Y;
                LastPos.X = e.X;
                LastPos.Y = e.Y;
                SelectedObject.Move(x, y);
                this.Invalidate();
            }
        }

    }
}
