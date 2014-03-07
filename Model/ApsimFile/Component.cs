using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using CSGeneral;
using System.Collections;
using System.Collections.Specialized;
using System.IO;
using System.Data;
using System.Threading;
using System.Threading.Tasks;

namespace ApsimFile
{
    public class Component : IComparable
    {
        // Some private stuff
        private string MyName;
        private string MyType;
        private string MyContents = "";
        private string MyDescription = "";
        private bool MyEnabled = true;
        private List<Component> MyChildNodes = new List<Component>();
        private Component MyShortCutTo = null;
        private ApsimFile MyFile = null;
        private Component MyParent = null;
        private string TempShortCutName = "";

        private void EnsureNameIsUnique()
        {
            // -------------------------------------------------------------
            // Make sure our name is unique amongst our siblings.
            // -------------------------------------------------------------
            string BaseName = Name;
            string UniqueChildName = BaseName;

            // Fix for bug: http://www.apsim.info/BugTracker/edit_bug.aspx?id=1599:
            //     Currently when you use the duplicate paddock function from the GUI it isn't very smart 
            //     with the way it numbers the resulting paddocks.
            if (UniqueChildName.Length > 0 && UniqueChildName[UniqueChildName.Length - 1] == '1')
                BaseName = BaseName.Remove(UniqueChildName.Length - 1);

            if (Parent != null)
            {
                for (int i = 1; i != 10000; i++)
                {
                    int Count = 0;
                    foreach (Component Sibling in Parent.ChildNodes)
                    {
                        if (Sibling.Name.ToLower() == UniqueChildName.ToLower())
                            Count++;
                    }
                    if (Count == 1)
                        return;
                    UniqueChildName = BaseName + i.ToString();
                    MyName = UniqueChildName;
                }
                throw new Exception("Cannot find a unique name for component: " + BaseName);

            }
        }
        private int ChildNameToIndex(string Name)
        {
            // ---------------------------------------------------------------------
            // Find a child component by name and return it's index. Returns -1 if 
            // not found.
            // ---------------------------------------------------------------------
            for (int i = 0; i != ChildNodes.Count; i++)
                if (ChildNodes[i].Name == Name)
                    return i;
            return -1;
        }
        internal Component(ApsimFile File, Component Parent)
        {
            this.MyFile = File;
            this.MyParent = Parent;
        }
        internal Component(ApsimFile File, Component Parent, Component Source)
        {
            this.MyFile = File;
            Name = Source.Name;
            Type = Source.Type;
            this.MyParent = Parent;
            MyShortCutTo = Source;
        }
        public void Read(XmlNode Node)
        {
            // ------------------------------------------------------
            // Read this component from the specified XmlNode. Also
            // create child components as necessary.
            // ------------------------------------------------------
            Type = Node.Name;
            if (Type.ToLower() == "simulations")
                Type = "folder";

            Name = XmlHelper.Name(Node);
            if (XmlHelper.Attribute(Node, "shortcut") != "")
                TempShortCutName = XmlHelper.Attribute(Node, "shortcut");
            if (XmlHelper.Attribute(Node, "enabled") == "no")
                MyEnabled = false;

            foreach (XmlNode Child in Node.ChildNodes)
            {
                if (Types.Instance.IsVisible(Child.Name) && XmlHelper.Attribute(Child, "invisible") != "yes")
                {
                    Component ChildComponent = new Component(MyFile, this);
                    ChildNodes.Add(ChildComponent);
                    ChildComponent.Read(Child);

                }
                else
                    MyContents += Child.OuterXml;
            }
            CheckOutputFileNode();
        }
        internal void ResolveShortcuts()
        {
            // ------------------------------------------------------
            // Now that reading is fully done we can go and resolve
            // any shortcuts that we found during reading. Do
            // this recursively for all children.
            //  Some "compound" objects shouldnt be resolved - eg. outputfile, area, but their children should be
            // ------------------------------------------------------
            if (TempShortCutName != "") 
            {
                MyShortCutTo = MyFile.Find(TempShortCutName);
                TempShortCutName = "";
                CheckOutputFileNode();
            }
            foreach (Component Child in ChildNodes)
                Child.ResolveShortcuts();
        }
        public void Write(XmlNode Node)
        {
            // ------------------------------------------------------
            // Write this component to the specified XmlNode. Also
            // create child nodes as necessary.
            // ------------------------------------------------------
            XmlHelper.SetName(Node, Name);
            if (!Enabled && ShortCutTo == null)
                XmlHelper.SetAttribute(Node, "enabled", "no");
            if (ShortCutTo != null)
                XmlHelper.SetAttribute(Node, "shortcut", ShortCutTo.FullPath);
            Node.InnerXml = MyContents;
            foreach (Component Child in ChildNodes)
            {
                XmlNode NewNode = Node.AppendChild(Node.OwnerDocument.CreateElement(Child.Type));
                Child.Write(NewNode);
            }
        }

        // Public properties and methods.
        public string Name
        {
            get { return MyName; }
            set
            {
                CheckFactorialsForRenamedSimulation(MyName, value);
                MyName = value;
                EnsureNameIsUnique();
                CheckOutputFileNode();
                MyFile.PublishComponentChanged(this.Parent);
            }
        }
        public string Type
        {
            get { return MyType; }
            set
            {
                MyType = value;
                MyFile.PublishComponentChanged(this);
            }
        }
        public string Description
        {
            get { return MyDescription; }
            set
            {
                MyDescription = value;
                MyFile.PublishComponentChanged(this);
            }
        }
        public string Contents
        {
            get
            {
                string OuterContents = "<" + Type;
                if (Name != Type)
                    OuterContents += " name=\"" + Name + "\"";
                OuterContents += ">";

                // Get the absolute base component.
                // For output files, don't follow any shortcuts because we want to read
                // the title and filename from this node, not the shortcut node.
                Component BaseComponent = this;
                if (Type != "outputfile")
                {
                    while (BaseComponent.MyShortCutTo != null)
                        BaseComponent = BaseComponent.MyShortCutTo;
                }
                OuterContents += BaseComponent.MyContents;
                OuterContents += "</" + Type + ">";
                return OuterContents;
            }
            set
            {
                if (MyShortCutTo == null)
                {
                    XmlDocument Doc = new XmlDocument();
                    Doc.LoadXml(value);
                    string InnerContents = Doc.DocumentElement.InnerXml;
                    if (MyContents != InnerContents)
                    {
                        MyContents = InnerContents;
                        MyFile.PublishContentChanged(this);
                    }
                }
                else
                    MyShortCutTo.Contents = value;
                CheckOutputFileNode();
            }
        }
        public XmlNode ContentsAsXML
        {
            get
            {
                XmlDocument Doc = new XmlDocument();
                Doc.LoadXml(Contents);
                return Doc.DocumentElement;
            }
        }
        public const char Delimiter = '/';
        public string FullPath
        {
            get
            {
                // The path returned is of the form: /RootComponent/Child/SubChild
                string Path = Name;
                Component Comp = Parent;
                while (Comp != null)
                {
                    Path = Comp.Name + Delimiter + Path;
                    Comp = Comp.Parent;
                }
                return Delimiter + Path;
            }
        }
        public bool Enabled
        {
            get
            {
                if (ShortCutTo == null)
                    return MyEnabled;
                else
                    return ShortCutTo.Enabled && MyEnabled;
            }
            set
            {
                MyEnabled = value;
                MyFile.PublishComponentChanged(this);
            }
        }
        public Component Parent
        {
            get { return MyParent; }
        }
        public Component ShortCutTo
        {
            get { return MyShortCutTo; }
        }
        public int CompareTo(object rhs)
        {
            return Name.CompareTo((rhs as Component).Name);
        }
        public Component Find(string RelativePath)
        {
            // ------------------------------------------------------
            // Will look for a child component using the relative
            // path passed in. e.g. Child/SubChild
            // ------------------------------------------------------
            string ChildName, Remainder;
            int PosDelimiter = RelativePath.IndexOf(Delimiter);
            if (PosDelimiter != -1)
            {
                ChildName = RelativePath.Substring(0, PosDelimiter);
                Remainder = RelativePath.Substring(PosDelimiter + 1);
            }
            else
            {
                ChildName = RelativePath;
                Remainder = "";
            }
            if (ChildName == "..")
            {
                if (Remainder == "")
                    return Parent;
                else
                    return Parent.Find(Remainder);
            }
            foreach (Component Child in ChildNodes)
            {
                if (Child.Name.ToLower() == ChildName.ToLower())
                {
                    if (Remainder == "")
                        return Child;
                    else
                        return Child.Find(Remainder);
                }
            }
            return null;
        }
        public List<Component> ChildNodes
        {
            get { return MyChildNodes; }
        }
        public Component Add(string Xml)
        {
            return Add(Xml, true);
        }
        public Component Add(string Xml, bool AddInitWaterIfNecessary)
        {
            // ---------------------------------------------------------------------
            // Add the specified xml as children. The xml might be multiple children
            // i.e. have multiple root nodes and so not valid XML. Add a dummy
            // root node around the xml so that we can parse it. Returns the first
            // child component
            // ---------------------------------------------------------------------
            XmlDocument Doc;

            try
            {
                Doc = new XmlDocument();
                Doc.LoadXml("<dummy>" + Xml + "</dummy>");
                if (AddInitWaterIfNecessary &&
                    Doc.DocumentElement.ChildNodes.Count == 1 &&
                    Doc.DocumentElement.ChildNodes[0].Name.ToLower() == "soil")
                {
                    // A special test for when the user drops a soil on a component node or when
                    // the user pastes a soil node. We want to make sure there are <initwater> and
                    // <initnitrogen> nodes.
                    XmlNode SoilNode = Doc.DocumentElement.ChildNodes[0];
                    XmlNode InitWaterNode = XmlHelper.FindByType(SoilNode, "InitWater");
                    XmlNode InitNitrogenNode = XmlHelper.FindByType(SoilNode, "InitNitrogen");
                    XmlNode SoilSampleNode = XmlHelper.FindByType(SoilNode, "Sample");
                    if (InitWaterNode == null && InitNitrogenNode == null && SoilSampleNode == null)
                    {
                        InitWaterNode = SoilNode.AppendChild(SoilNode.OwnerDocument.CreateElement("InitWater"));
                        InitWaterNode.InnerXml =
                            "<percentmethod>" +
                            "  <percent>1</percent>" +
                            "  <distributed>filled from top</distributed>" +
                            "</percentmethod>";

                        InitNitrogenNode = SoilNode.AppendChild(SoilNode.OwnerDocument.CreateElement("Sample"));
                        XmlHelper.SetName(InitNitrogenNode, "Initial nitrogen");
                        InitNitrogenNode.InnerXml = "<Date type=\"date\" description=\"Sample date:\" />" +
                                                     "<Layer>" +
                                                     "   <Thickness units=\"mm\">100</Thickness>" +
                                                     "   <NO3 units=\"ppm\">0</NO3>" +
                                                     "   <NH4 units=\"ppm\">0</NH4>" +
                                                     "   <SW units=\"mm/mm\">0</SW>" +
                                                     "</Layer>";

                    }
                }

            }
            catch (Exception)
            {
                return null;
            }

            Component FirstChildComponent = null;
            MyFile.BeginUpdate();
            foreach (XmlNode Child in Doc.DocumentElement.ChildNodes)
            {
                if (Types.Instance.AllowComponentAdd(Child.Name, Type) || Type == "factor" || Type == "factorComplex")
                {
                    Component ChildComponent = new Component(MyFile, this);
                    MyChildNodes.Add(ChildComponent);
                    ChildComponent.Read(Child);
                    ChildComponent.ResolveShortcuts();
                    ChildComponent.EnsureNameIsUnique();
                    if (FirstChildComponent == null)
                        FirstChildComponent = ChildComponent;
                }
            }
            MyFile.EndUpdate();
            MyFile.PublishComponentChanged(this);
            return FirstChildComponent;
        }
        public bool AllowAdd(string Xml)
        {
            // -------------------------------------------------------------- 
            // Do we allow the specified XML to be added to this node?
            // -------------------------------------------------------------- 
            if (MyFile.IsReadOnly)
                return false;
            else
            {
                // Do we allow the specified xml to be added to the selected node? 
                XmlDocument TempDoc = new XmlDocument();
                TempDoc.LoadXml("<dummy>" + Xml + "</dummy>");
                bool ok = true;
                foreach (XmlNode Child in XmlHelper.ChildNodes(TempDoc.DocumentElement, ""))
                    ok = ok & Types.Instance.AllowComponentAdd(Child.Name, Type);
                return ok & TempDoc.DocumentElement.ChildNodes.Count > 0;
            }
        }
        public string[] ChildNames
        {
            get
            {
                string[] names = new string[MyChildNodes.Count];
                for (int i = 0; i != MyChildNodes.Count; i++)
                    names[i] = MyChildNodes[i].Name;
                return names;
            }
        }
        public Component AddShortCut(Component SourceComponent)
        {
            // ---------------------------------------------------------------------
            // Add the source component as a child shortcut component.
            // ---------------------------------------------------------------------
            MyFile.BeginUpdate();
            Component ShortCutComponent = new Component(MyFile, this, SourceComponent);
            MyChildNodes.Add(ShortCutComponent);
            ShortCutComponent.EnsureNameIsUnique();

            // Now we need to go through all child nodes of the shortcut and make 
            // them shortcuts as well.
            foreach (Component Child in SourceComponent.ChildNodes)
                ShortCutComponent.AddShortCut(Child);

            MyFile.EndUpdate();
            MyFile.PublishComponentChanged(this);
            CheckOutputFileNode();
            return ShortCutComponent;
        }
        public void Delete(Component ComponentToDelete)
        {
            // ---------------------------------------------------------------------
            // Delete the specified child component. We need to scan all components
            // recursively looking for a shortcut that points to this component or
            // any of our children that we're about to delete. If found then make 
            // the shortcut node concrete.
            // ---------------------------------------------------------------------
            List<Component> AllChildNodes = new List<Component>();
            ComponentToDelete.ChildNodesRecursively(AllChildNodes);
            AllChildNodes.Add(ComponentToDelete);
            MyFile.RootComponent.MakeShortCutsConrete(AllChildNodes);
            ChildNodes.Remove(ComponentToDelete);
            MyFile.PublishComponentChanged(this);
        }
        public Component Duplicate(Component ComponentToDuplicate, bool AsAShortCut)
        {
            XmlDocument Doc = new XmlDocument();
            Doc.AppendChild(Doc.CreateElement(ComponentToDuplicate.Type));
            ComponentToDuplicate.Write(Doc.DocumentElement);
            if (!AsAShortCut && XmlHelper.Attribute(Doc.DocumentElement, "shortcut") == "")
                return Add(Doc.DocumentElement.OuterXml);
            else
                return AddShortCut(ComponentToDuplicate);
        }
        public void ChildNodesRecursively(List<Component> AllChildNodes)
        {
            // ---------------------------------------------------------------------
            // Fill the specified list will all child nodes recursively.
            // ---------------------------------------------------------------------
            foreach (Component Child in ChildNodes)
            {
                AllChildNodes.Add(Child);
                Child.ChildNodesRecursively(AllChildNodes);
            }
        }

        private void MakeShortCutsConrete(List<Component> Nodes)
        {
            // ---------------------------------------------------------------------
            // Scan all children looking for a shortcut that points to any node in
            // the specified list of nodes. When found then make the shortcut concete.
            // ---------------------------------------------------------------------
            if (ShortCutTo != null && Nodes.Contains(ShortCutTo))
            {
                XmlDocument Doc = new XmlDocument();
                Doc.LoadXml(ShortCutTo.Contents);
                MyContents = Doc.DocumentElement.InnerXml;
                Enabled = ShortCutTo.Enabled;
                MyShortCutTo = null;
                CheckOutputFileNode();
                MyFile.PublishComponentChanged(this);
            }
            foreach (Component Child in ChildNodes)
                Child.MakeShortCutsConrete(Nodes);
        }
        public void Sort()
        {
            // ---------------------------------------------------------------------
            // Sort the childnodes alphabetically
            // ---------------------------------------------------------------------
            ChildNodes.Sort();
            MyFile.PublishComponentChanged(this);
        }
        public void MoveUp(List<string> NamesToMoveUp)
        {
            // ---------------------------------------------------------------------
            // Move the specified child component names up one spot.
            // ---------------------------------------------------------------------
            for (int i = 0; i != NamesToMoveUp.Count; i++)
            {
                int ReferencePosition = ChildNameToIndex(NamesToMoveUp[i]);
                if (ReferencePosition > 0)
                {
                    Component ComponentToMove = ChildNodes[ReferencePosition];
                    ChildNodes.Remove(ComponentToMove);
                    ChildNodes.Insert(ReferencePosition - 1, ComponentToMove);
                }
                else
                    return;
            }
            MyFile.PublishComponentChanged(this);
        }
        public void MoveDown(List<string> NamesToMoveDown)
        {
            // ---------------------------------------------------------------------
            // Move the specified child component names down one spot.
            // ---------------------------------------------------------------------
            for (int i = NamesToMoveDown.Count - 1; i >= 0; i--)
            {
                int ReferencePosition = ChildNameToIndex(NamesToMoveDown[i]);
                if (ReferencePosition < ChildNodes.Count - 1)
                {
                    Component ComponentToMove = ChildNodes[ReferencePosition];
                    ChildNodes.Remove(ComponentToMove);
                    ChildNodes.Insert(ReferencePosition + 1, ComponentToMove);
                }
                else
                    return;
            }
            MyFile.PublishComponentChanged(this);
        }
        public void MakeConcrete()
        {
            // ---------------------------------------------------------------------
            // Convert this component from a shortcut node to a concrete node.
            // ---------------------------------------------------------------------
            if (ShortCutTo != null)
            {
                List<Component> ShortCuts = new List<Component>();
                ShortCuts.Add(ShortCutTo);
                MakeShortCutsConrete(ShortCuts);
                MyFile.PublishComponentChanged(this);
            }
        }
        public void MakeConcreteRecursively()
        {
            // ---------------------------------------------------------------------
            // Convert this component from a shortcut node to a concrete node
            // PLUS all child components.
            // ---------------------------------------------------------------------
            MakeConcrete();
            foreach (Component Child in ChildNodes)
                Child.MakeConcreteRecursively();
        }
        public Component FindContainingPaddock()
        {
            Component Paddock = this;
            while (Paddock != null && Paddock.Type != "area" && Paddock.Type != "simulation")
                Paddock = Paddock.Parent;
            return Paddock;
        }

        public Component FindRecursively(string ComponentName, string ComponentType)
        {
            // ------------------------------------------------------
            // Recursively locates a component with the specified name
            // ------------------------------------------------------

            if (ComponentName == "")
                throw new Exception("Cannot call FindRecursively with a blank name");
            if (ComponentName.ToLower() == Name.ToLower() &&
                (ComponentType == "" || ComponentType.ToLower() == Type.ToLower()))
                return this;

            foreach (Component Child in ChildNodes)
            {
                Component FoundComponent = Child.FindRecursively(ComponentName, ComponentType);
                if (FoundComponent != null)
                    return FoundComponent;
            }
            return null;
        }
        public void FindRecursively(string ComponentType, ref List<Component> Matches)
        {
            // ------------------------------------------------------
            // Recursively locates all component of a specified type
            // ------------------------------------------------------

            if (ComponentType.ToLower() == Type.ToLower())
                Matches.Add(this);

            foreach (Component Child in ChildNodes)
                Child.FindRecursively(ComponentType, ref Matches);
        }
        public void SetValue(string valueName, string value)
        {
            XmlDocument doc = new XmlDocument();
            doc.LoadXml("<dummy>" + MyContents + "</dummy>");
            if (value == "")
                XmlHelper.DeleteValue(doc.DocumentElement, valueName);
            else
                XmlHelper.SetValue(doc.DocumentElement, valueName, value);
            MyContents = doc.DocumentElement.InnerXml;
        }
        public void Replace(string Xml)
        {
            // -------------------------------------------------------
            // We need to replace 'this' component with the one
            // passed in - need to be mindfull of shortcuts.
            // -------------------------------------------------------
            Component TempComponent = Parent.Add(Xml);
            Replace(TempComponent);
            Parent.Delete(TempComponent);
            MyFile.PublishComponentChanged(this);
        }
        private void Replace(Component Rhs)
        {
            // -------------------------------------------------------
            // Replace 'this' component with 'Rhs'
            // -------------------------------------------------------
            Contents = Rhs.Contents;
            string[] Children = ChildNames;
            foreach (Component TempChild in Rhs.MyChildNodes)
            {
                int Pos = CSGeneral.StringManip.IndexOfCaseInsensitive(Children, TempChild.Name);
                if (Pos == -1)
                    Add(TempChild.Contents);
                else
                    MyChildNodes[Pos].Replace(TempChild);
            }

            // Remove unwanted children.
            for (int i = ChildNames.Length - 1; i >= 0; i--)
            {
                bool Found = false;
                foreach (Component RhsChild in Rhs.MyChildNodes)
                {
                    if (ChildNames[i].ToLower() == RhsChild.Name.ToLower())
                        Found = true;
                }
                if (!Found)
                    Delete(MyChildNodes[i]);
            }
        }
        public string FullXML()
        {
            XmlDocument Doc = new XmlDocument();
            Doc.AppendChild(Doc.CreateElement(Type));
            Write(Doc.DocumentElement);
            return Doc.OuterXml;
        }
        public string FullXMLNoShortCuts()
        {
            // ------------------------------------------------------
            // Write this component to the specified XmlNode. Also
            // create child nodes as necessary.
            // ------------------------------------------------------
            XmlDocument Doc = new XmlDocument();
            Doc.AppendChild(Doc.CreateElement(Type));
            FullXMLNoShortCutsInternal(Doc.DocumentElement);
            return Doc.OuterXml;
        }
        private void FullXMLNoShortCutsInternal(XmlNode Node)
        {
            XmlHelper.SetName(Node, Name);
            if (!Enabled && ShortCutTo == null)
                XmlHelper.SetAttribute(Node, "enabled", "no");
            if (ShortCutTo != null)
                Node.InnerXml = ShortCutTo.ContentsAsXML.InnerXml;
            else
                Node.InnerXml = MyContents;
            foreach (Component Child in ChildNodes)
            {
                XmlNode NewNode = Node.AppendChild(Node.OwnerDocument.CreateElement(Child.Type));
                Child.FullXMLNoShortCutsInternal(NewNode);
            }
        }
        public Component FindComponentInPaddock(Component To, string Name)
        {
            // Find a component in the paddockthe nearest component to "To" that has the specified "Name".
            // It won't look down the simulation hierarchy and won't look
            // outside the paddock.
            Component Paddock = FindContainingPaddock();
            if (Paddock != null)
            {
                List<Component> ComponentsInPaddock = new List<Component>();
                Paddock.ChildNodesRecursively(ComponentsInPaddock);
                foreach (Component ChildComponent in ComponentsInPaddock)
                {
                    if (ChildComponent.Name.ToLower() == Name.ToLower() && ChildComponent.Type != "SoilCrop")
                        return ChildComponent;
                }
            }
            return null;
        }



        public void ConvertToShortcutsUsingBase(Component Base)
        {
            if (ShortCutTo == null)
            {
                // If this node is a simulation or paddock then link the simulation and 
                // not worry about the different name. The call to IsEqual below will 
                // test the name attributes and find them different.
                if (Type == "area" || Type == "simulation" || XmlHelper.IsEqual(ContentsAsXML, Base.ContentsAsXML))
                {
                    MyContents = "";
                    MyShortCutTo = Base;
                }
            }
            foreach (Component Child in ChildNodes)
            {
                Component RelatedBaseComponent = Base.Find(Child.Name);
                if (RelatedBaseComponent != null)
                    Child.ConvertToShortcutsUsingBase(RelatedBaseComponent);
            }
        }

        private void CheckOutputFileNode()
        {
            if (Type == "outputfile" && MyChildNodes.Count > 0)
            {
                XmlNode Node = ContentsAsXML;
                string FileName = ComponentUtility.CalcFileName(this);
                XmlHelper.SetValue(Node, "filename", FileName);
                XmlHelper.SetValue(Node, "title", Path.GetFileNameWithoutExtension(FileName));
                XmlNode FileNameNode = XmlHelper.Find(Node, "filename");
                XmlHelper.SetAttribute(FileNameNode, "output", "yes");
                MyContents = Node.InnerXml;
            }
            foreach (Component Child in ChildNodes)
                Child.CheckOutputFileNode();
        }

        /// <summary>
        /// When a simulation is renamed in the GUI, we need to check the factors in factorial
        /// and rename the targets in them.
        /// </summary>
        private void CheckFactorialsForRenamedSimulation(string oldSimName, string newSimName)
        {
            if (Type == "simulation" && oldSimName != null && MyFile != null && MyFile.RootComponent != null)
            {
                string oldFullPath = FullPath;
                string newFullPath = FullPath.Replace(oldSimName, newSimName);

                Component factorial = MyFile.RootComponent.Find("Factorials");
                if (factorial != null)
                {
                    foreach (Component factor in factorial.ChildNodes)
                    {
                        XmlNode factorNode = factor.ContentsAsXML;
                        XmlNode targetsNode = XmlHelper.Find(factorNode, "targets");
                        foreach (XmlNode targetNode in XmlHelper.ChildNodes(targetsNode, "Target"))
                        {
                            targetNode.InnerText = targetNode.InnerText.Replace(oldFullPath, newFullPath);
                        }
                        factor.Contents = factorNode.OuterXml;
                    }
                }
            }
        }
        
        public bool IsAncestorOf(Component comp)
        // Returns true if comp is this component, or one of its ancestors
        {
            Component ancestor = comp; 
            while (ancestor != null)
            {
                if (ancestor == this)
                    return true;
                ancestor = ancestor.Parent;
            }
            return false;
        }
    }
}
