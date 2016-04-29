using System;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;
using System.Xml;
using Curves;
using CSGeneral;


namespace CSUserInterface
{
    public partial class RotPlotUI : Controllers.BaseView
    {
        bool m_Loading = true;
        bool m_OldVersion = false;
        public string GraphName;
        public List<GDPaddock> ManagedPaddocks = new List<GDPaddock>();
        public List<GDPaddock> AvailablePaddocks = new List<GDPaddock>();
        private List<ComponentVE> MyComponents = new List<ComponentVE>();
        private string language = "TCL";
        public RotPlotUI()
        {
            InitializeComponent();
            GraphDisplay.ChangeSelection += new EventHandler<GDChangeEventArgs>(OnChanging);
            GraphDisplay.SelectObject += new EventHandler<GDSelectEventArgs>(OnSelected);
            pnlHints.Dock = DockStyle.Fill;
            lstHints.Dock = DockStyle.Fill;
            lstHints.Visible = true;
        }
        protected override void OnLoad()
        {
        }
        public override void OnRefresh()
        {
            //if it is old version
            //load all data using rotnode - save routine will look after it
            //flag creation of subnode "RugPlot"
            XmlNodeList nodes = Data.SelectNodes("//rotnode");
            m_OldVersion = nodes.Count > 0;
            BuildGraphDisplay();

            nodes = Data.SelectNodes("//language");
            if (nodes.Count == 0)
                language = "TCL";
            else
                language = nodes[0].InnerText;  //XmlHelper.Value(nodes[0]);
            if (language != "TCL" && language != "C#")
                throw new Exception("Language " + language + " is unknown");

            GraphDisplay.Height = GraphDisplay.MaxHeight;
            GraphDisplay.Width = GraphDisplay.MaxWidth;
            if (GraphDisplay.Height == 0 && GraphDisplay.Width == 0)
            {
                GraphDisplay.Height = panel1.Height - 10;
                GraphDisplay.Width = panel1.Width - 10;
            }

            pnlFlowLayout.Controls.Clear();
            List<string> states = new List<string>();
            foreach (GDNode node in GraphDisplay.Nodes)
            {
                states.Add(node.Name);
            }
            foreach (GDPaddock paddock in ManagedPaddocks)
            {
                PaddockState ps = new PaddockState();
                ps.chkPaddock.Text = paddock.Name;
                ps.chkPaddock.Checked = paddock.Managed;
                ps.cboState.Items.AddRange(states.ToArray());
                ps.cboState.Text = paddock.InitialState;
                pnlFlowLayout.Controls.Add(ps);
            }
            GraphDisplay.SelectedObject = null;
            if (language == "C#")
            {
                label2.Text = "Rules (get from system)";
                label3.Text = "Actions (publish to system)";
            }
            GraphDisplay.Refresh();

            ComponentVE.GetVisibleComponents(Controller.ApsimData.Find(NodePath), ref MyComponents);
            PopulateComponentFilter();
            PopulateVariableListView("Variables");
            RuleBoxSelected = true;
            txtRules.BackColor = Color.LightPink;
            ActionBoxSelected = false;
            txtActions.BackColor = Color.White; // TextBox.DefaultBackColor;
        }
        public override void OnSave()
        {
            string graphName = XmlHelper.Name(Data);
            Data.RemoveAll();
            XmlHelper.SetName(Data, graphName);

            WriteSettings();
            WriteNodes();
            WriteArcs();
            WritePaddocks();
            WriteGraphRule();
        }
        public string BuildRuleTCL()
        {
            string sRule = "package require struct\n";
            sRule += "::struct::graph " + GraphName + "\n";

            foreach(GDNode node in GraphDisplay.Nodes)
            {
                sRule += GraphName + " node insert \"" + node.Name + "\"\n";
                //should be able to remove this line
                sRule += "set colour(" + node.Name + ") \"" + node.Colour.ToArgb().ToString() + "\"\n";
            }
            foreach (GDArc arc in GraphDisplay.Arcs)
            {
                sRule += GraphName + " arc insert \"" + arc.SourceNode.Name + "\"";
                sRule += " \"" + arc.TargetNode.Name + "\"";
                sRule += " \"" + arc.Name + "\"\n";

                foreach (string sAction in arc.Actions)
                {
                    if (sAction != "")
                    {
                        sRule += GraphName + " arc lappend \"" + arc.Name + "\" actions {";
                        sRule += sAction;
                        sRule += "}\n";
                    }
                }
                if(arc.Actions.Count == 0)
                    sRule += GraphName + " arc lappend \"" + arc.Name + "\" actions {}\n";

                foreach (string rule in arc.Rules)
                {
                    if (rule != "")
                    {
                        sRule += GraphName + " arc lappend \"" + arc.Name + "\" rules {";
                        sRule += rule;
                        sRule += "}\n";
                    }
                }
            }
            foreach (GDPaddock paddock in ManagedPaddocks)
            {
                sRule += "if {[info exists config(" + paddock.Name + ",graphNames)]}";
                sRule += " {lappend config(" + paddock.Name + ",graphNames) " + GraphName + "}";
                sRule += " else {set config(" + paddock.Name + ",graphNames) " + GraphName + "}\n";

                sRule += "set config(" + paddock.Name + ",initialState) \"" + paddock.InitialState + "\"\n";
            }
            return sRule;
        }
        public string BuildRuleCSharp()
        {
            string sRule = "using System;\n";
            sRule += "using System.Collections.Generic;\n";
            sRule += "using System.Text;\n";
            sRule += "using ModelFramework;\n";
            sRule += "using CSGeneral;\n";

            sRule += "public class Script {\n";
            sRule += "[Link()]  public Paddock MyPaddock;\n";

            sRule += "private Graph g = new Graph();\n";
            sRule += "private string _currentState = \"\";\n";
            sRule += "[Output()] public string currentState {\n";
            sRule += "  get {return _currentState;} \n";
            sRule += "  set {\n";
            sRule += "    if (_currentState!= \"\") MyPaddock.Publish(\"transition_from_\" + _currentState);\n";
            sRule += "    MyPaddock.Publish(\"transition\");\n";
            sRule += "    _currentState = value;\n";
            sRule += "    MyPaddock.Publish(\"transition_to_\" + _currentState);\n";
            sRule += "  }\n";
            sRule += "}\n";

            sRule += "[Output()] public string [] states = new string[0]; \n";

            sRule += "[EventHandler] public void OnInitialised()\n";
            sRule += "{\n";
            sRule += "g = new Graph();\n";

            sRule += "states = new string[" + GraphDisplay.Nodes.Count.ToString() + "];\n";
            sRule += "int i = 0;\n";
            foreach (GDNode node in GraphDisplay.Nodes)
            {
                sRule += "g.AddNode(\"" + node.Name + "\");\n";
                sRule += "states[i++] = \"" + node.Name + "\";\n";
            }
            foreach (GDArc arc in GraphDisplay.Arcs)
            {
                string rName = arc.Name;
                sRule += "ruleAction " + rName + " = new ruleAction();\n";
                foreach (string rule in arc.Rules)
					sRule += rName + ".testCondition.Add(\"" + rule + "\");\n";
                
                foreach (string action in arc.Actions)
                    sRule += rName + ".action.Add(\"" + action + "\");\n";
                sRule += "g.AddDirectedEdge(\"" + arc.SourceNode.Name + "\",\"" + arc.TargetNode.Name + "\"," + rName + ");\n";
            }
            sRule += "}\n";
            sRule += "[EventHandler] public void OnStart_Simulation()\n";
            sRule += "{\n";
            foreach (var c in pnlFlowLayout.Controls) {
				if (c is PaddockState) {
				   PaddockState p = (PaddockState) c;
				   if (p.chkPaddock.Text == "<self>") {
                      sRule += "currentState = \"" + p.cboState.Text + "\";\n";
                   }
                   else
                   {
                    /*sRule += "set config(" + paddock.Name + ",initialState) \"" + p.cboState.Text + "\"\n"*/; //FIXME				}
				   }
				}
			}


            sRule += "}\n";
            sRule += "[EventHandler] public void OnProcess()\n";
            sRule += "{\n";
            sRule += "   bool more = true;\n";
            sRule += "   while (more) {\n";
            sRule += "      more = false;\n";
            sRule += "      Node s = g.FindNode(currentState);\n";
            sRule += "      //Console.WriteLine(\" state=\" + s.Name);\n";
            sRule += "      double bestScore = -1.0; string bestArc = \"\";\n";
            sRule += "      foreach (string _arc in s.arcs.Keys) {\n";
            sRule += "         double score = 1;\n";
			sRule += "         foreach (string testCondition in s.arcs[_arc].testCondition){\n";
			sRule += "            double c = 0.0;\n";
            sRule += "            if (MyPaddock.Get(testCondition, out c))\n";
            sRule += "               score *= c;\n";
            sRule += "            else\n";
            sRule += "               throw new Exception(\"Nothing returned from expression '\" + testCondition + \"'\");\n";
            sRule += "         } //Console.WriteLine(\" a=\" + _arc + \" score=\" + score);\n";
            sRule += "         if (score > bestScore) {\n";
            sRule += "            bestScore = score;\n";
            sRule += "            bestArc = _arc;\n";
            sRule += "         }\n";
            sRule += "      }\n";
            sRule += "      if (bestScore > 0.0) {\n";
            sRule += "          //Console.WriteLine(\" best=\" + bestTarget + \" action=\" + s.arcs[bestTarget].action);\n";
            sRule += "          currentState = g.FindArcTarget(bestArc);\n";
            sRule += "          foreach (string action in s.arcs[bestArc].action )\n";
            sRule += "             MyPaddock.Publish(action);\n";
            sRule += "          more = true;\n";
            sRule += "      }\n";
            sRule += "   }\n";
            sRule += "}\n";

            sRule += "  public class ruleAction\n";
            sRule += "  {\n";
            sRule += "     public string Target;\n";
            sRule += "     public List<string> testCondition = new List<string>();\n";
            sRule += "     public List<string> action = new List<string>();\n";
            sRule += "  }\n";

            sRule += "   public class Node : IEquatable<Node>\n";
            sRule += "   {\n";
            sRule += "      private string data /*, alias*/;\n";
            sRule += "      public Node(string _data) { data = _data; }\n";
            sRule += "      public Dictionary<string, ruleAction> arcs = new Dictionary<string, ruleAction>();\n";
            sRule += "      public bool Equals(Node other)\n";
            sRule += "      {\n";
            sRule += "         return ( this.data == other.data );\n";
            sRule += "      }\n";
            sRule += "      public string Name { get { return (data); } }\n";
            sRule += "   }\n";
            sRule += "\n";
            sRule += "   class Graph\n";
            sRule += "   {\n";
            sRule += "      public List<Node> vertices;\n";

            sRule += "      public void print() {\n";
            sRule += "         foreach (Node n in vertices) {\n";
            sRule += "            Console.WriteLine(n.Name + \":\"); \n";
            sRule += "            foreach (string k in n.arcs.Keys) {\n";
            sRule += "               Console.WriteLine(\"  \" + k); \n";
            sRule += "            }\n";
            sRule += "         }\n";
            sRule += "      }\n";
            sRule += "      public Graph() { vertices = new List<Node>(); }\n";
            sRule += "      public Node AddNode(Node node)\n";
            sRule += "      {\n";
            sRule += "         if (vertices.Find(delegate(Node n) { return (n.Equals(node)); }) == null)\n";
            sRule += "            vertices.Add(node);\n";
            sRule += "         return (vertices.Find(delegate(Node n) { return (n.Equals(node)); }));\n";
            sRule += "      }\n";

            sRule += "      public Node AddNode(string value)\n";
            sRule += "      {\n";
            sRule += "         return (AddNode(new Node(value)));\n";
            sRule += "      }\n";
            sRule += "      public Node FindNode(string value)\n";
            sRule += "      {\n";
            sRule += "         return (FindNode(AddNode(value)));\n";
            sRule += "      }\n";
            sRule += "      public Node FindNode(Node node)\n";
            sRule += "      {\n";
            sRule += "         return (vertices.Find(delegate(Node n) { return (n.Equals(node)); }));\n";
            sRule += "      }\n";

            sRule += "      public void AddDirectedEdge(string source, string dest, ruleAction value)\n";
            sRule += "      {\n";
            sRule += "         int i = 0;\n";
            sRule += "         foreach (Node n in vertices) {\n";
            sRule += "            foreach (string arc in n.arcs.Keys) {\n";
            sRule += "               i++;\n";
            sRule += "            }\n";
            sRule += "         }\n";
            sRule += "         Node node1 = AddNode(source);\n";
            sRule += "         AddNode(dest);\n";
            sRule += "         value.Target = dest;\n";
            sRule += "         node1.arcs.Add(\"arc\" + i.ToString(), value);\n";
            sRule += "      }\n";
            sRule += "      public string FindArcTarget(string arc)\n";
            sRule += "      {\n";
            sRule += "         foreach (Node n in vertices) {\n";
            sRule += "            foreach (string a in n.arcs.Keys) {\n";
            sRule += "               if (a == arc) { return (n.arcs[a].Target); }\n";
            sRule += "            }\n";
            sRule += "         }\n";
            sRule += "         Console.WriteLine(\"Aiee - no target for arc \" + arc ); \n";
            sRule += "         return(\"\");\n";
            sRule += "      }\n";


            sRule += "   }\n";
            sRule += "}\n";


            return sRule;
        }
        public void WriteGraphRule()
        {
            if (language == "TCL")
            {
                XmlElement elem = Data.AppendChild(Data.OwnerDocument.CreateElement("rule")) as XmlElement;
                XmlHelper.SetName(elem, GraphName + " Init rule");
                elem.SetAttribute("invisible", "yes");
                elem.SetAttribute("condition", "init");
                elem.AppendChild(Data.OwnerDocument.CreateCDataSection(BuildRuleTCL()));
            }
            else if (language == "C#")
            {
                XmlNode node = Data.AppendChild(Data.OwnerDocument.CreateElement("component"));
                XmlHelper.SetName(node, GraphName + " Initialisation");
                XmlHelper.SetAttribute(node, "executable", "%apsim%/Model/Manager2.dll");
                XmlHelper.SetValue(node, "executable", "%apsim%/Model/Manager2.dll");
                node = node.AppendChild(Data.OwnerDocument.CreateElement("initdata"));
                node = node.AppendChild(Data.OwnerDocument.CreateElement("text"));
                node.AppendChild(Data.OwnerDocument.CreateCDataSection(BuildRuleCSharp()));
            }
        }
        private void WriteSettings()
        {
            XmlNode node = Data.AppendChild(Data.OwnerDocument.CreateElement("canvas_height"));
            node.InnerText = panel1.Height.ToString();
            node = Data.AppendChild(Data.OwnerDocument.CreateElement("rules_height"));
            node.InnerText = pnlHints.Height.ToString();
            node = Data.AppendChild(Data.OwnerDocument.CreateElement("graph_name"));
            node.InnerText = GraphName;
            node = Data.AppendChild(Data.OwnerDocument.CreateElement("language"));
            node.InnerText = language;
        }
        private void WriteNodes()
        {
            foreach (GDNode node in GraphDisplay.Nodes)
            {
                XmlNode parentNode = Data.AppendChild(Data.OwnerDocument.CreateElement("node"));
                XmlNode childNode = parentNode.AppendChild(Data.OwnerDocument.CreateElement("name"));
                childNode.InnerText = node.Name;

                childNode = parentNode.AppendChild(Data.OwnerDocument.CreateElement("x1"));
                childNode.InnerText = node.Left.ToString();
                childNode = parentNode.AppendChild(Data.OwnerDocument.CreateElement("y1"));
                childNode.InnerText = node.Top.ToString();

                childNode = parentNode.AppendChild(Data.OwnerDocument.CreateElement("desc"));
                childNode.InnerText = node.Description;
                childNode = parentNode.AppendChild(Data.OwnerDocument.CreateElement("fill"));
                childNode.InnerText = node.Colour.ToArgb().ToString();
            }
        }
        private void WriteArcs()
        {
            foreach (GDArc arc in GraphDisplay.Arcs)
            {
                XmlNode parentNode = Data.AppendChild(Data.OwnerDocument.CreateElement("arc"));
                XmlNode childNode = parentNode.AppendChild(Data.OwnerDocument.CreateElement("name"));
                childNode.InnerText = arc.Name;
                childNode = parentNode.AppendChild(Data.OwnerDocument.CreateElement("source"));
                childNode.InnerText = arc.SourceNode.Name;
                childNode = parentNode.AppendChild(Data.OwnerDocument.CreateElement("target"));
                childNode.InnerText = arc.TargetNode.Name;
                childNode = parentNode.AppendChild(Data.OwnerDocument.CreateElement("x"));
                childNode.InnerText = arc.Location.X.ToString();
                childNode = parentNode.AppendChild(Data.OwnerDocument.CreateElement("y"));
                childNode.InnerText = arc.Location.Y.ToString();

                foreach (string action in arc.Actions)
                {
                    if (action != "")
                    {
                        childNode = parentNode.AppendChild(Data.OwnerDocument.CreateElement("actions"));
                        childNode.InnerText = action;
                    }
                }
                foreach (string rule in arc.Rules)
                {
                    if (rule != "")
                    {
                        childNode = parentNode.AppendChild(Data.OwnerDocument.CreateElement("rules"));
                        childNode.InnerText = rule;
                    }
                }
            }
        }
        private void WritePaddocks()
        {
			foreach (var c in pnlFlowLayout.Controls) {
				if (c is PaddockState) {
				   PaddockState p = (PaddockState) c;
				   if (p.chkPaddock.Text == "<self>") {
                       XmlNode initStateNode = Data.AppendChild(Data.OwnerDocument.CreateElement("InitialState"));
				       initStateNode.InnerText = p.cboState.Text;
				   } else {
//                   XmlNode parentNode = Data.AppendChild(Data.OwnerDocument.CreateElement("paddock"));
//                   XmlNode childNode = parentNode.AppendChild(Data.OwnerDocument.CreateElement("name"));
//                   childNode.InnerText = paddock.Name;
//                   childNode = parentNode.AppendChild(Data.OwnerDocument.CreateElement("isManaged"));
//                   childNode.InnerText = paddock.Managed ? "1" : "0";
//                   childNode = parentNode.AppendChild(Data.OwnerDocument.CreateElement("initialState"));
//                   childNode.InnerText = paddock.InitialState;
//                    /*sRule += "set config(" + paddock.Name + ",initialState) \"" + p.cboState.Text + "\"\n"*/; //FIXME				}
				   }
				}
			}
        }

        private void UpdateAvailablePaddocks()
        {

        }
        private void BuildGraphDisplay()
        {
            GraphDisplay.ClearAll();
            ManagedPaddocks.Clear();
            AvailablePaddocks.Clear();
            //should only have to load it once.
            if (GraphDisplay.Nodes.Count == 0)
            {
                XmlNodeList nodes = Data.SelectNodes("//node");
                foreach (XmlNode tmpNode in nodes)
                {
                    GDNode tmpGDNode = ReadGDNode(tmpNode);
                    GraphDisplay.AddNode(tmpGDNode);

                }
                nodes = Data.SelectNodes("//arc");
                foreach (XmlNode tmpNode in nodes)
                {
                    GDArc tmpGArc = ReadGDArc(tmpNode);
                    GraphDisplay.AddArc(tmpGArc);
                }
                nodes = Data.SelectNodes("//paddock");
                foreach (XmlNode tmpNode in nodes)
                {
                    GDPaddock tmpPaddock = ReadGDPaddock(tmpNode);
                    if (tmpPaddock != null)
                        ManagedPaddocks.Add(tmpPaddock);    
                }
                nodes = Data.SelectNodes("//graph_name");
                if (nodes.Count == 1)
                {
                    GraphName = nodes[0].InnerText;
                }
				// Fudge a paddock if needed (ie reading an old style .apsim file)
				if (ManagedPaddocks.Count == 0) 
				{
					GDPaddock t = new GDPaddock();
					t.Name = "<self>";
					t.Managed = true;
					t.InitialState = ReadChildNodeText(Data, "InitialState");
					if (t.InitialState == null) t.InitialState = "Fallow";
					ManagedPaddocks.Add(t); 
				}
            }
        }
        public GDNode ReadGDNode(XmlNode tmpNode)
        {
            GDNode gd = new GDNode();
            
            string sName = ReadChildNodeText(tmpNode, "name");
            string sDesc = ReadChildNodeText(tmpNode, "desc");
            int x1 = ReadChildNodeValue(tmpNode, "x1");
            int y1 = ReadChildNodeValue(tmpNode, "y1");
            gd.Name = sName;
            gd.Description = sDesc;

            gd.Start.X = x1;
            gd.Start.Y = y1;

            int iColour = ReadChildNodeValue(tmpNode, "fill");
            if(iColour != 0)
            {
                gd.Colour = Color.FromArgb(iColour);
            }
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
            foreach (XmlNode node in tmpNode.ChildNodes)
            {
                if (node.Name == "actions")
                {
                    ga.Actions.Add(node.InnerText);
                }
                else if (node.Name == "rules")
                {
                    ga.Rules.Add(node.InnerText);
                }
            }
            return ga;
        }
        public GDPaddock ReadGDPaddock(XmlNode tmpNode)
        {
            GDPaddock gp = new GDPaddock();

            string sName = ReadChildNodeText(tmpNode, "name");
            string sManaged = ReadChildNodeText(tmpNode, "isManaged");
            string sInitial = ReadChildNodeText(tmpNode, "initialState");

            if(sManaged == "1")
            {
                gp.Name = sName;
                gp.Managed = true;
                gp.InitialState = sInitial;
                return gp;
            }
            return null;
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
                    catch (Exception ) 
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
                }catch(Exception ){}
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

        public void OnChanging(object sender, GDChangeEventArgs e)
        {
        }

        public void OnSelected(object sender, GDSelectEventArgs e)
        {
            m_Loading = true;
            if (e.SelectedObject == null)
            {
                lstHints.Dock = DockStyle.Fill;
                lstHints.Visible = true;
                pnlArcProperties.Visible = false;
                pnlNodeProperties.Visible = false;

                lblStatus.Text = "No Item Selected.";

                //display hints
            }
            else
            {
                GDArc arc = e.SelectedObject as GDArc;
                if (arc != null)
                {
                    pnlArcProperties.Dock = DockStyle.Fill;
                    pnlArcProperties.Visible = true;
                    pnlNodeProperties.Visible = false;
                    lstHints.Visible = false;

                    txtRules.Lines = arc.Rules.ToArray();
                    txtActions.Lines = arc.Actions.ToArray();

                    lblStatus.Text = "Transition from " + arc.Source + " to " + arc.Target;
                }
                else
                {
                    GDNode node = e.SelectedObject as GDNode;
                    if (node != null)
                    {
                        pnlNodeProperties.Dock = DockStyle.Fill;
                        pnlNodeProperties.Visible = true;
                        pnlArcProperties.Visible = false;
                        lstHints.Visible = false;

                        lblStatus.Text = "State: " + node.Name;
                        txtName.Text = node.Name;
                        lblInvalidName.Visible = false;
                        txtDesc.Text = node.Description;
                        lblColour.BackColor = node.Colour;
                    }
                }
                m_Loading = false;
            }
        }
        private void label2_Click(object sender, EventArgs e)
        {
            if (dlgColour.ShowDialog() == DialogResult.OK)
            {
                lblColour.BackColor = dlgColour.Color;
                GraphDisplay.SelectedObject.Colour = dlgColour.Color;
                GraphDisplay.Invalidate();
            }
        }
        private void txtName_TextChanged(object sender, EventArgs e)
        {
            if (m_Loading)
                return;
            TextBox txt = sender as TextBox;
            if (txt != null)
            {
                if (txt.Text == "")
                {
                    lblInvalidName.Text = "Name cannot be blank";
                    lblInvalidName.Visible = true;
                }
                else
                {
                    bool bExists = false;
                    foreach (GDNode node in GraphDisplay.Nodes)
                    {
                        if (node != GraphDisplay.SelectedObject)
                        {
                            if (node.Name == txt.Text)
                            {
                                lblInvalidName.Text = "Name already exists";
                                bExists = true;
                                break;
                            }
                        }
                    }
                    lblInvalidName.Visible = bExists;
                }
                if (!lblInvalidName.Visible)
                {
                    GraphDisplay.SelectedObject.Name = txt.Text;
                    Refresh();
                }
            }
        }
        private void txtName_KeyDown(object sender, KeyEventArgs e)
        {
            if (m_Loading)
                return;
            if (e.KeyCode == Keys.Escape && lblInvalidName.Visible)
            {
                txtName.Text = GraphDisplay.SelectedObject.Name;
            }
        }
        private void txtDesc_TextChanged(object sender, EventArgs e)
        {
            if (m_Loading)
                return;
            TextBox txt = sender as TextBox;
            if (txt != null)
            {
                GraphDisplay.SelectedObject.Name = txt.Text;
            }
        }
        private void txtRules_TextChanged(object sender, EventArgs e)
        {
            if (m_Loading)
                return;
            TextBox txt = sender as TextBox;
            if (txt != null)
            {
                GDArc arc = GraphDisplay.SelectedObject as GDArc;
                if (arc != null)
                {
                    arc.Rules.Clear();
                    arc.Rules.AddRange(txt.Lines);
                }
            }
        }
    private void txtActions_TextChanged(object sender, EventArgs e)
        {
            if (m_Loading)
                return;
            TextBox txt = sender as TextBox;
            if (txt != null)
            {
                GDArc arc = GraphDisplay.SelectedObject as GDArc;
                if (arc != null)
                {
                    arc.Actions.Clear();
                    arc.Actions.AddRange(txt.Lines);
                }
            }

        }
        private void PopulateVariableListView(string what)
        {
            // ----------------------------------------------
            // Populate the variable list view box
            // ----------------------------------------------

            if ((ComponentFilter.SelectedIndex >= 0) && (ComponentFilter.SelectedIndex < MyComponents.Count))
            {
                VariableListView.BeginUpdate();
                VariableListView.Groups.Clear();
                VariableListView.Items.Clear();
                VariableListView.Columns.Clear();
                AddThingsToListView(MyComponents[ComponentFilter.SelectedIndex], what);
                if (what == "Events")
                {
                    ColumnHeader1.Text = "Event name";
                    VariableListView.Columns.Add(ColumnHeader1);
                    VariableListView.Columns.Add(ColumnHeader4);
                }
                else
                {
                    ColumnHeader1.Text = "Variable name";
                    VariableListView.Columns.Add(ColumnHeader1);
                    VariableListView.Columns.Add(ColumnHeader2);
                    VariableListView.Columns.Add(ColumnHeader3);
                    VariableListView.Columns.Add(ColumnHeader4);
                }
                VariableListView.EndUpdate();
                VariableListView.Columns[0].AutoResize(ColumnHeaderAutoResizeStyle.ColumnContent);
            }
        }
        private void AddThingsToListView(ComponentVE c, string what)
        {
            ListViewGroup NewGroup = new ListViewGroup(c.name + " " + what);
            VariableListView.Groups.Add(NewGroup);

            StringCollection hidden = new StringCollection();
            hidden.AddRange(new string[] { "active", "author", "name", "state", "type", "version" });
            foreach (Types.MetaDataInfo thing in c.ModelInfo(what))
            {
                if (hidden.Contains(thing.Name.ToLower()))
                    continue;
                ListViewItem ListItem = new ListViewItem(thing.Name);
                ListItem.Group = NewGroup;
                if (what == "Variables")
                {
                if (thing.IsArray)
                {
                    ListItem.SubItems.Add("Yes");
                }
                else
                {
                    ListItem.SubItems.Add("No");
                }
                ListItem.SubItems.Add(thing.Units);
                }
                ListItem.SubItems.Add(thing.Description);
                VariableListView.Items.Add(ListItem);
            }
        }
        private void PopulateComponentFilter()
        {
            // ----------------------------------------
            // Populate the component filter drop down
            // ----------------------------------------
            ComponentFilter.Items.Clear();
            foreach (ComponentVE component in MyComponents)
               ComponentFilter.Items.Add(component.name);

            if (ComponentFilter.Items.Count > 0)
                {
                ComponentFilter.SelectedIndex = 0;
                }
        }

        #region "Drag / Drop methods"

        private void ListViewItemDrag(object sender, System.Windows.Forms.ItemDragEventArgs e)
        {
            // --------------------------------------------------------
            // User is trying to initiate a drag - allow drag operation
            // --------------------------------------------------------
            VariableListView.DoDragDrop("xx", DragDropEffects.All);
        }
        private void VariablesGridDragEnter(System.Object sender, System.Windows.Forms.DragEventArgs e)
        {
            e.Effect = DragDropEffects.Copy;
        }
        private void VariablesGridDragOver(System.Object sender, System.Windows.Forms.DragEventArgs e)
        {
            e.Effect = DragDropEffects.Copy;
        }
        private void VariablesGridDragDrop(System.Object sender, System.Windows.Forms.DragEventArgs e)
        {
            // --------------------------------------------------
            // User has dropped a variable onto the variable grid
            // --------------------------------------------------
            AddVariablesToGrid();
        }

        private void VariableListView_DoubleClick(object sender, System.EventArgs e)
        {
            // ----------------------------------------------------------
            // On a double click do exact the same thing as when you drop
            // ----------------------------------------------------------
            AddVariablesToGrid();
        }
        private void AddVariablesToGrid()
        {
            string componentName = MyComponents[ComponentFilter.SelectedIndex].name;
            foreach (ListViewItem vname in VariableListView.SelectedItems)
            {
                string fullname = componentName + "." + vname.Text;
                if (RuleBoxSelected)
                {
                    if (txtRules.Text.Length > 0) txtRules.AppendText("\n");
                    txtRules.AppendText(fullname);
                }
                if (ActionBoxSelected)
                {
                    if (txtActions.Text.Length > 0) txtActions.AppendText("\n");
                    txtActions.AppendText(fullname);
                }
            }
        }
        #endregion
        private void ComponentFilter_TextChanged(object sender, EventArgs e)
        {
            if (RuleBoxSelected)
                PopulateVariableListView("Variables");
            else if (ActionBoxSelected) 
                PopulateVariableListView("Events");
        }
        private bool RuleBoxSelected = false;
        private bool ActionBoxSelected = false;
        private void txtRules_Click(object sender, EventArgs e)
        {
            if (ActionBoxSelected) ActionBoxSelected = false;
            txtActions.BackColor = Color.White; //TextBox.DefaultBackColor;
            RuleBoxSelected = !RuleBoxSelected;
            if (RuleBoxSelected)
            {
                txtRules.BackColor = Color.LightPink;
                //int currentComponent = ComponentFilter.SelectedIndex;
                PopulateVariableListView("Variables");
            }
            else
                txtRules.BackColor = Color.White; //TextBox.DefaultBackColor;TextBox.DefaultBackColor;
        }
        private void txtActions_Click(object sender, EventArgs e)
        {
            if (RuleBoxSelected) RuleBoxSelected = false;
            txtRules.BackColor = Color.White; //TextBox.DefaultBackColor;TextBox.DefaultBackColor;
            ActionBoxSelected = !ActionBoxSelected;
            if (ActionBoxSelected)
            {
                txtActions.BackColor = Color.LightPink;
                PopulateVariableListView("Events");
            }
            else
                txtActions.BackColor = Color.White; //TextBox.DefaultBackColor;TextBox.DefaultBackColor;
        }
    }
    public class GDPaddock
    {
		public GDPaddock() {Name = ""; InitialState = ""; Managed = true;}
        public string Name { get; set; }
        public string InitialState { get; set; } //Set to a GDNode Name
        public bool Managed { get; set; }
    }

    public class GDObject
    {
        public virtual bool Selected { get; set; }
        public virtual int Left { get; set; }
        public virtual int Right { get; set; }
        public virtual int Top { get; set; }
        public virtual int Bottom { get; set; }

        public string Name { get; set; }
        public Color Colour { get; set; }

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
        private int clickTolerence = 3;

        public List<string> Rules = new List<string>();
        public List<string> Actions = new List<string>();
        public Point Location = new Point();

        public string Source { get; set; }
        public string Target { get; set; }

        public GDNode SourceNode = null;
        public GDNode TargetNode = null;

        private BezierCurve bezCurve = new BezierCurve();
        public List<Point> BezPoints = new List<Point>();
        private double[] bezParameters = new double[8];

        private int m_DefaultNodeSize = 100;

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

            if (BezPoints.Count != 0)
            {
                Pen pen = new Pen(Color.Black);
                if (Selected)
                    pen.Color = Color.Blue;

                GraphicsObject.DrawBezier(pen, BezPoints[0], Location, Location, BezPoints[BezPoints.Count-1]);

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
        #region PointinPolygonCalc
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
        #endregion

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
            BezPoints.Clear();
            if (SourceNode == null || TargetNode == null) return;
            Point ep1 = new Point();
            Point ep2 = new Point();
            if (SourceNode != TargetNode)
            {
                ep1 = SourceNode.Mid;
                ep2 = TargetNode.Mid;
            }
            else
            {
                double d = m_DefaultNodeSize / 4;
                double m;
                if ((SourceNode.Mid.X - Location.X) != 0)
                    m = Math.Atan((SourceNode.Mid.Y - Location.Y) / (double)(SourceNode.Mid.X - Location.X));
                else
                    if (SourceNode.Mid.Y > Location.Y)
                        m = Math.PI * 0.5;
                    else
                        m = Math.PI * 1.5;
                double m1 = m - Math.PI / 2;
                double m2 = m + Math.PI / 2;
                ep1.X = SourceNode.Mid.X + (int)(d * Math.Cos(m1));
                ep1.Y = SourceNode.Mid.Y + (int)(d * Math.Sin(m1));
                ep2.X = SourceNode.Mid.X + (int)(d * Math.Cos(m2));
                ep2.Y = SourceNode.Mid.Y + (int)(d * Math.Sin(m2));
            }

			int iStart = Math.Min(ep1.X, ep2.X);
            int iEnd = Math.Max(ep1.X, ep2.X);
            int xPoints = iEnd - iStart;
            iStart = Math.Min(ep1.Y, ep2.Y);
            iEnd = Math.Max(ep1.Y, ep2.Y);
            int yPoints = iEnd - iStart;
            
            //will calc a min of 100 points
            int points = Math.Max(Math.Max(xPoints, yPoints), 50) * 2;
            double[] output = new double[points];
                bezParameters[0] = ep1.X;
                bezParameters[1] = ep1.Y;
                bezParameters[2] = Location.X ;
                bezParameters[3] = Location.Y ;
                bezParameters[4] = Location.X ;
                bezParameters[5] = Location.Y ;
                bezParameters[6] = ep2.X;
                bezParameters[7] = ep2.Y;

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

        public string Description { get; set; }
        public int Height { get; set; }
        public int Width { get; set; }
        public override int Left { get { return Start.X; }}
        public override int Right { get { return Start.X + Width; }}
        public override int Top { get { return Start.Y; } }
        public override int Bottom { get { return Start.Y + Height; }}

        public GDNode()
        {
            this.Colour = Color.Beige;
        }
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

            SolidBrush drawBrush = new SolidBrush(this.Colour);
            Pen pen = new Pen(Color.Black);
            if (Selected)
            {
                pen.Color = Color.Blue;
                pen.Width = 3;
            }

            GraphicsObject.FillEllipse(drawBrush, tmp);
            GraphicsObject.DrawEllipse(pen, tmp);

            //Write text
            using (StringFormat stringFormat = new StringFormat())
            {
                stringFormat.Alignment = StringAlignment.Center;
                stringFormat.LineAlignment = StringAlignment.Center;

                using(Font txtFont = new Font("Tahoma", 8))
                {
                    GraphicsObject.DrawString(this.Name, txtFont, Brushes.Black, tmp, stringFormat);
                };
            };
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
        public bool ArcIsAttached(GDArc tmp)
        {
            return tmp.SourceNode == this || tmp.TargetNode == this;
        }
    }

    public class GDSelectEventArgs : EventArgs
    {
        public GDSelectEventArgs() { }
        public GDSelectEventArgs(GDObject obj)
        {
            SelectedObject = obj;
        }
        public GDObject SelectedObject {get; set;}
    }
    public class GDChangeEventArgs : EventArgs
    {
        public GDChangeEventArgs() { }
        public GDChangeEventArgs(GDObject oldobj, GDObject newobj)
        {
            OldObject = oldobj;
            NewObject = newobj;
        }
        public GDObject OldObject { get; set; }
        public GDObject NewObject { get; set; }

    }
    

    public partial class GraphDisplayObject : System.Windows.Forms.Panel
    {
        public List<GDNode> Nodes = new List<GDNode>();
        public List<GDArc> Arcs = new List<GDArc>();
        
        public int MaxHeight = 0;
        public int MaxWidth = 0;
        private int m_DefaultSize = 100;

        private GDObject m_SelectedObject = null;
        private GDObject m_RightClickedObject = null;
        private bool mouseDown = false;
        private Point LastPos;

        public event EventHandler<GDChangeEventArgs> ChangeSelection;
        public event EventHandler<GDSelectEventArgs> SelectObject;

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
                {
                    GDChangeEventArgs evt = new GDChangeEventArgs(m_SelectedObject, value);
                    if(ChangeSelection != null)
                        ChangeSelection(this, evt);
                    m_SelectedObject.Selected = false;
                }
                m_SelectedObject = value;
                if (m_SelectedObject != null)
                {
                    m_SelectedObject.Selected = true;
                }
                GDSelectEventArgs selevt = new GDSelectEventArgs(m_SelectedObject);
                if (SelectObject != null)
                    SelectObject(this, selevt);
            }
        }

        public void ClearAll()
        {
            m_SelectedObject = null;
            m_RightClickedObject = null;
            Nodes.Clear();
            Arcs.Clear();
        }
        public void AddNode(GDNode tmpNode)
        {
            Nodes.Add(tmpNode);

            tmpNode.End.X = tmpNode.Start.X + m_DefaultSize;
            tmpNode.End.Y = tmpNode.Start.Y + m_DefaultSize;

            tmpNode.Mid.X = tmpNode.Start.X + (tmpNode.End.X - tmpNode.Start.X) / 2;
            tmpNode.Mid.Y = tmpNode.Start.Y + (tmpNode.End.Y - tmpNode.Start.Y) / 2;
            tmpNode.Width = tmpNode.End.X - tmpNode.Start.X;
            tmpNode.Height = tmpNode.End.Y - tmpNode.Start.Y;

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
            EnsureArcNameIsUnique(tmpArc);
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
        public GDArc FindArc(string sName)
        {
            foreach (GDArc tmp in Arcs)
            {
                if (tmp.Name == sName)
                    return tmp;
            }
            return null;
        }
        public string getUniqueNodeName(string sSeedName)
        {
            for (int i = 0; i < 100000; ++i)
            {
                GDNode tmpNode = FindNode(sSeedName + i.ToString());
                if (tmpNode == null)
                    return sSeedName + i.ToString();
            }
            return "UniqueNameNotFound";
        }
        public string getUniqueArcName(string sSeedName)
        {
            for (int i = 0; i < 100000; ++i)
            {
                GDArc tmpArc = FindArc(sSeedName + i.ToString());
                if (tmpArc == null)
                    return sSeedName + i.ToString();
            }
            return "UniqueNameNotFound";
        }
        private void EnsureArcNameIsUnique(GDArc newArc)
        {
            GDArc existingArc = FindArc(newArc.Name);
            if (existingArc != null)
            {
                newArc.Name = getUniqueArcName("arc");
            }
        }
        protected override void OnPaint(PaintEventArgs e)
        {
            UpdateLimits();
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
                //reset to just inside parent, so there is something to click on
                if(Parent.Width > 10)
                    Width = Parent.Width-10;
                else
                    Width = Parent.Width;
                if(Parent.Width > 10)
                    Height = Parent.Height - 10;
                else
                    Height = Parent.Height;
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
            m_RightClickedObject = null;
            if (e.Button == MouseButtons.Right)
            {
                //menu is going to popup regardless
                this.ContextMenuStrip.Items.Clear();
                
                if (tmpObject != null)
                {
                    m_RightClickedObject = tmpObject;
                    //if previous object is the same as the current object - duplicate/delee
                    if (SelectedObject == tmpObject)
                    {
                        ToolStripItem mnuItem = this.ContextMenuStrip.Items.Add("Delete");
                        mnuItem.Click += new EventHandler(DeleteGDObjectMenuClicked);

                        mnuItem = this.ContextMenuStrip.Items.Add("Duplicate");
                        mnuItem.Click += new EventHandler(DuplicateGDObjectMenuClicked);

                        mnuItem = this.ContextMenuStrip.Items.Add("Add arc from " + tmpObject.Name + " to " + tmpObject.Name);
                        mnuItem.Click += new EventHandler(addArcMenuClicked);
                    }
                    else
                    {
                        GDNode tmpPrev = SelectedObject as GDNode;
                        if (tmpPrev != null)
                        {
                            GDNode tmpCurr = tmpObject as GDNode;
                            if (tmpCurr != null)
                            {
                                //if last object was a state and new object is a state and right mouse clicked
                                //add arc
                                ToolStripItem mnuItem = this.ContextMenuStrip.Items.Add("Add arc from " + tmpPrev.Name + " to " + tmpCurr.Name);
                                mnuItem.Click += new EventHandler(addArcMenuClicked);
                            }
                            else
                            {
                                ToolStripItem mnuItem = this.ContextMenuStrip.Items.Add("Add State");
                                mnuItem.Click += new EventHandler(addStateMenuClicked);
                            }
                        }
                    }
                }
                else
                {
                    ToolStripItem mnuItem = this.ContextMenuStrip.Items.Add("Add State");
                    mnuItem.Click += new EventHandler(addStateMenuClicked);
                }
            }
            else
            {
                if (tmpObject != null)
                {
                    mouseDown = true;
                    LastPos.X = e.X;
                    LastPos.Y = e.Y;
                }
                SelectedObject = tmpObject;
            }
            this.Invalidate();
        }

        private void addStateMenuClicked(object sender, EventArgs e)
        {
            ToolStripItem mnuItem = sender as ToolStripItem;
            GDNode gd = new GDNode();
            gd.Name = getUniqueNodeName("State");

            if (mnuItem != null)
            {
                Point loc = PointToClient(mnuItem.Owner.Location);
                gd.Start.X = loc.X - m_DefaultSize / 3;
                gd.Start.Y = loc.Y - m_DefaultSize / 3;
            }
            AddNode(gd);
            SelectedObject = gd;
            Invalidate();
        }
        private void addArcMenuClicked(object sender, EventArgs e)
        {
            ToolStripItem mnuItem = sender as ToolStripItem;
            if (m_RightClickedObject != null 
                && SelectedObject != null
                && mnuItem != null)
            {
                GDArc ga = new GDArc();
                ga.Name = getUniqueArcName("arc");
                ga.Source = SelectedObject.Name;
                ga.Target = m_RightClickedObject.Name;
                Point loc = PointToClient(mnuItem.Owner.Location);
                
                AddArc(ga);
                //this should set source and target so we can use them to cal the midpoint
                if (ga.TargetNode != ga.SourceNode)
                {
                    int dist = ga.TargetNode.Mid.X - ga.SourceNode.Mid.X;
                    ga.Location.X = ga.SourceNode.Mid.X + dist / 2;
                    dist = ga.TargetNode.Mid.Y - ga.SourceNode.Mid.Y;
                    ga.Location.Y = ga.SourceNode.Mid.Y + dist / 2;
                }
                else
                {
                    ga.Location.X = ga.SourceNode.Mid.X + m_DefaultSize ;
                    ga.Location.Y = ga.SourceNode.Mid.Y + m_DefaultSize ;
                }
                SelectedObject = ga;
                Invalidate();
            }
        }
        private void DeleteGDObjectMenuClicked(object sender, EventArgs e)
        {
            GDArc arc = m_RightClickedObject as GDArc;
            if (arc != null)
            {
                Arcs.Remove(arc);
            }
            else
            {
                GDNode node = m_RightClickedObject as GDNode;
                Arcs.RemoveAll(node.ArcIsAttached);
                SelectedObject = null;
                Nodes.Remove(node);
            }
            Invalidate();
        }
        private void DuplicateGDObjectMenuClicked(object sender, EventArgs e)
        {
            GDArc existingArc = m_RightClickedObject as GDArc;
            if (existingArc != null)
            {
                GDArc ga = DupeArc(existingArc);
                AddArc(ga);
                SelectedObject = ga;
                Invalidate();
            }
            else
            {
                GDNode node = m_RightClickedObject as GDNode;
                if(node != null)
                {
                    GDNode newNode = new GDNode();
                    newNode.Name = getUniqueNodeName("State");

                    newNode.Start.X = node.Start.X;
                    newNode.Start.Y = node.Start.Y;
                    AddNode(newNode);
                    //duplicating a node includes arcs
                    List<GDArc> newArcs = new List<GDArc>();

                    foreach (GDArc arc in node.Arcs)
                    {
                        if (arc.SourceNode == node)
                        {
                            GDArc ga = DupeArc(arc);
                            ga.Source = newNode.Name;
                            newArcs.Add(ga);
                        }
                        if(arc.TargetNode == node)
                        {
                            GDArc ga = DupeArc(arc);
                            ga.Target = newNode.Name;
                            newArcs.Add(ga);
                        }
                    }
                    foreach (GDArc arc in newArcs)
                    {
                        AddArc(arc);
                    }
                    SelectedObject = newNode;
                    Invalidate();
                }
            }
        }
        private GDArc DupeArc(GDArc arc)
        {
            GDArc ga = new GDArc();
            ga.Name = getUniqueArcName("arc");
            ga.Source = arc.Source;
            ga.Target = arc.Target;
            ga.Location.X = arc.Location.X + 10;
            ga.Location.Y = arc.Location.Y + 10;
            ga.Rules = new List<string>(arc.Rules);
            ga.Actions = new List<string>(arc.Actions);
            return ga;
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
