using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.IO;
using System.Collections;

namespace CSGeneral
   {
   public class XmlHelper
      {
      public const char Delimiter = '/';

      public static XmlNode CreateNode(XmlDocument Document, string Type, string Name)
         {
         // -----------------------------------------------------------------
         // Create a node with the specified type and name.
         // -----------------------------------------------------------------
         XmlNode NewNode = Document.CreateElement(Type);
         if (Name != "")
            SetAttribute(NewNode, "name", Name);
         return NewNode;
         }
      public static string Name(XmlNode Node)
         {
         // --------------------------------------
         // Return the name attribute or if not 
         // found then the element name
         // i.e. 'Type'
         // --------------------------------------
         if (Attribute(Node, "name") == "")
            return Type(Node);
         else
            return Attribute(Node, "name");
         }
      public static void SetName(XmlNode Node, string Name)
         {
         if (Name != XmlHelper.Name(Node))
            SetAttribute(Node, "name", Name);
         }
      public static string Type(XmlNode Node)
         {
         // ---------------------------------------
         // Return the 'type' of the specified node
         // ---------------------------------------
         return Node.Name;
         }
      public static XmlNode ChangeType(XmlNode Node, string NewType)
         {
         if (Node.ParentNode != null)
            {
            XmlNode NewNode = Node.OwnerDocument.CreateElement(NewType);

            // Copy all attributes
            while (Node.Attributes.Count > 0)
               NewNode.Attributes.Append(Node.Attributes[0]);

            // Copy all child nodes
            while (Node.ChildNodes.Count > 0)
               NewNode.AppendChild(Node.ChildNodes[0]);

            Node.ParentNode.ReplaceChild(NewNode, Node);
            return NewNode;
            }
         return null;
         }

      public static XmlNode Parent(XmlNode Node)
         {
         // ---------------------------------------
         // Return the parent of the specified node
         // ---------------------------------------
         if (Node.ParentNode == null || Node.ParentNode.Name == "#document")
            return null;
         else
            return Node.ParentNode;
         }

      public static string FullPath(XmlNode Node)
         {
         // --------------------------------------------------------
         // Return a full path for the specified node. Paths are 
         // of the form: /RootNode/ChildNode/SubChildNode
         // --------------------------------------------------------
         XmlNode LocalData = Node;
         string Path = Name(LocalData);
         LocalData = Parent(LocalData);
         while (LocalData != null)
            {
            Path = Name(LocalData) + Delimiter + Path;
            LocalData = Parent(LocalData);
            }
         return Delimiter + Path;
         }
      public static string ParentPath(string NodePath)
         {
         int PosDelimiter = NodePath.LastIndexOf(Delimiter);
         if (PosDelimiter == -1)
            throw new Exception("Cannot get the parent of the specified node: " + NodePath);
         string ParentName = NodePath.Remove(PosDelimiter);
         if (ParentName == "")
            throw new Exception("Cannot get the parent of the root node");
         return ParentName;
         }
      public static XmlNode Find(XmlNode Node, string NamePath)
         {
         // ----------------------------------------------------
         // Find a child with the specified NamePath. NamePath
         // can be a single child name or a path delimited with
         // '/' characters e.g. ChildNode/SubChildNode or /RootNode/ChildNode
         // Returns null if no child found. 
         // ----------------------------------------------------
         if (Node == null)
             return null;
         if (NamePath == "")
            throw new Exception("Cannot call FindByName with a blank path");
         if (NamePath[0] == Delimiter)
            {
            Node = Node.OwnerDocument.DocumentElement;
            int Pos = NamePath.IndexOf(Delimiter, 1);
            string RootName;
            if (Pos == -1)
               {
               RootName = NamePath.Substring(1);
               NamePath = "";
               }
            else
               {
               RootName = NamePath.Substring(1, Pos - 1);
               NamePath = NamePath.Substring(Pos + 1);
               }
            if (RootName.ToLower() != Name(Node).ToLower())
               return null;
            if (NamePath == "")
               return Node;
            }

         string ChildName, Remainder;
         int PosDelimiter = NamePath.IndexOf(Delimiter);
         if (PosDelimiter != -1)
            {
            ChildName = NamePath.Substring(0, PosDelimiter);
            Remainder = NamePath.Substring(PosDelimiter + 1);
            }
         else
            {
            ChildName = NamePath;
            Remainder = "";
            }
         foreach (XmlNode Child in Node.ChildNodes)
            {
            if (Name(Child).ToLower() == ChildName.ToLower())
               {
               if (Remainder == "")
                  return Child;
               else
                  return Find(Child, Remainder);
               }
            }
         return null;
         }
      public static XmlNode FindByType(XmlNode Node, string TypePath)
         {
         // ----------------------------------------------------
         // Find a child with the specified TypePath. TypePath
         // can be a single child type or a path delimited with
         // '/' characters e.g. ChildNode/SubChildNode or /RootNode/ChildNode
         // Returns null if no child found. 
         // ----------------------------------------------------
         if (Node == null)
             return null;
         if (TypePath == "")
            throw new Exception("Cannot call FindByType with a blank path");
         if (TypePath[0] == Delimiter)
            {
            Node = Node.OwnerDocument.DocumentElement;
            int Pos = TypePath.IndexOf(Delimiter, 1);
            string RootName = TypePath.Substring(1, Pos - 1);
            if (RootName.ToLower() != Type(Node).ToLower())
               return null;
            TypePath = TypePath.Substring(Pos + 1);
            }

         string ChildType, Remainder;
         int PosDelimiter = TypePath.IndexOf(Delimiter);
         if (PosDelimiter != -1)
            {
            ChildType = TypePath.Substring(0, PosDelimiter);
            Remainder = TypePath.Substring(PosDelimiter + 1);
            }
         else
            {
            ChildType = TypePath;
            Remainder = "";
            }
         foreach (XmlNode Child in Node.ChildNodes)
            {
            if (Type(Child).ToLower() == ChildType.ToLower())
               {
               if (Remainder == "")
                  return Child;
               else
                  return FindByType(Child, Remainder);
               }
            }
         return null;
         }
      public static XmlNode ChildByTypeAndValue(XmlNode Node, string TypeFilter, string ValueFilter)
         {
         // ----------------------------------------------------
         // Find a child with the specified Type and value. 
         // Returns null if no child found. 
         // ----------------------------------------------------
         foreach (XmlNode Child in Node.ChildNodes)
            {
            if (Type(Child).ToLower() == TypeFilter.ToLower() && Child.InnerText == ValueFilter)
               return Child;
            }
         return null;
         }
      public static List<XmlNode> ChildNodes(XmlNode Node, string TypeFilter)
         {
         // ----------------------------------------------------
         // Return an array of children that match the specified
         // filter. The filter can be an empty string to match
         // all child XmlNodes
         // ----------------------------------------------------
         List<XmlNode> MatchingChildren = new List<XmlNode>();
         if (Node != null)
            {
            foreach (XmlNode Child in Node.ChildNodes)
               {
               if (Child.Name != "#text" && Child.Name != "#comment" && Child.Name != "#cdata-section" &&
                   TypeFilter == "" || Type(Child).ToLower() == TypeFilter.ToLower())
                  MatchingChildren.Add(Child);
               }
            }
         return MatchingChildren;
         }
      public static string[] ChildNames(XmlNode Node, string TypeFilter)
         {
         List<XmlNode> Children = ChildNodes(Node, TypeFilter);
         string[] Names = new string[Children.Count];
         for (int i = 0; i != Children.Count; i++)
            Names[i] = Name(Children[i]);
         return Names;
         }
      public static string Value(XmlNode Child, string NamePath)
         {
         XmlNode FoundNode;
         if (NamePath == "")
            FoundNode = Child;
         else
            FoundNode = Find(Child, NamePath);
         if (FoundNode != null)
            return FoundNode.InnerText;
         else
            return "";
         }
      public static void SetValue(XmlNode Node, string NamePath, string Value)
         {
         XmlNode ValueNode = EnsureNodeExists(Node, NamePath);

         char[] InvalidChars = { '&', '<', '>' };
         if (Value.IndexOfAny(InvalidChars) != -1)
            {
            XmlCDataSection cdata = Node.OwnerDocument.CreateCDataSection(Value);
            ValueNode.InnerXml = cdata.OuterXml;
            }
         else
            ValueNode.InnerText = Value;
         }
      public static List<string> Values(XmlNode Node, string TypeFilter)
            {
            int PosDelimiter = TypeFilter.LastIndexOf('/');
            if (PosDelimiter != -1)
               {
               Node = Find(Node, TypeFilter.Substring(0, PosDelimiter));
               TypeFilter = TypeFilter.Substring(PosDelimiter+1);
               }

            List<string> ReturnValues = new List<string>();
            foreach (XmlNode Child in ChildNodes(Node, TypeFilter))
                ReturnValues.Add(Child.InnerText);
            return ReturnValues;
            }
      public static List<string> ValuesRecursive(XmlNode Node, string TypeFilter)
         {
         int PosDelimiter = TypeFilter.LastIndexOf('/');
         if (PosDelimiter != -1)
            {
            Node = Find(Node, TypeFilter.Substring(0, PosDelimiter));
            TypeFilter = TypeFilter.Substring(PosDelimiter + 1);
            }

         List<string> ReturnValues = new List<string>();
         foreach (XmlNode Child in ChildNodes(Node, ""))
            {
            if (Child.Name == TypeFilter)
               ReturnValues.Add(Child.InnerText);
            ReturnValues.AddRange(ValuesRecursive(Child, TypeFilter)); // recursion
            }
         return ReturnValues;
         }
      public static void SetValues(XmlNode Node, string NamePath, List<string> Values)
         {
         int PosDelimiter = NamePath.LastIndexOf('/');
         if (PosDelimiter != -1)
            {
            Node = Find(Node, NamePath.Substring(0, PosDelimiter));
            NamePath = NamePath.Substring(PosDelimiter + 1);
            }

         EnsureNumberOfChildren(Node, NamePath, "", Values.Count);

         int i = 0;
         foreach (XmlNode Child in ChildNodes(Node, NamePath))
            {
            SetValue(Child, "", Values[i]);
            i++;
            }
         }
      public static string Attribute(XmlNode Node, string AttributeName)
         {
         // -----------------------------------------------------------------
         // Return the specified attribute or "" if not found
         // -----------------------------------------------------------------
         if (Node.Attributes != null)
            {
            XmlNode A = Node.Attributes.GetNamedItem(AttributeName);
            if (A != null)
               return A.InnerText;
            }
         return "";
         }
      public static void SetAttribute(XmlNode Node, string AttributeName, string AttributeValue)
         {
         // ----------------------------------------
         // Set the value of the specified attribute
         // ----------------------------------------
         if (Attribute(Node, AttributeName) != AttributeValue)
            {
            XmlNode attr = Node.OwnerDocument.CreateNode(XmlNodeType.Attribute, AttributeName, "");
            attr.Value = AttributeValue;
            Node.Attributes.SetNamedItem(attr);
            }
         }
      public static void DeleteAttribute(XmlNode Node, string AttributeName)
         {
         // ----------------------------------------
         // Delete the specified attribute
         // ----------------------------------------
         XmlAttribute A = (XmlAttribute)Node.Attributes.GetNamedItem(AttributeName);
         if (A != null)
            {
            Node.Attributes.Remove(A);
            }
         }
      public static string FormattedXML(string Xml)
         {
         // -------------------------------------------------
         // Format the specified XML using indentation etc.
         // -------------------------------------------------
         XmlDocument Doc = new XmlDocument();
         Doc.LoadXml("<dummy>" + Xml + "</dummy>");
         StringWriter TextWriter = new StringWriter();
         XmlTextWriter Out = new XmlTextWriter(TextWriter);
         Out.Formatting = Formatting.Indented;
         Doc.DocumentElement.WriteContentTo(Out);
         return TextWriter.ToString();
         }
      public static void EnsureNodeIsUnique(XmlNode Node)
         {
         // -------------------------------------------------------------
         // Make sure the node's name is unique amongst it's siblings.
         // -------------------------------------------------------------
         string BaseName = Name(Node);
         string UniqueChildName = BaseName;
         for (int i = 1; i != 10000; i++)
            {
            int Count = 0;
            foreach (XmlNode Sibling in Node.ParentNode.ChildNodes)
               {
               if (Name(Sibling).ToLower() == UniqueChildName.ToLower())
                  Count++;
               }
            if (Count == 1)
               return;
            UniqueChildName = BaseName + i.ToString();
            SetAttribute(Node, "name", UniqueChildName);
            }
         throw new Exception("Cannot find a unique name for child: " + Name(Node));
         }
      public static void EnsureNumberOfChildren(XmlNode Node, string ChildType, string ChildName, int NumChildren)
         {
         // -------------------------------------------------------------------------
         // Ensure there are the specified number of children with the speciifed type
         // -------------------------------------------------------------------------
         string[] ChildrenNames = ChildNames(Node, ChildType);
         int NumChildrenToAdd = NumChildren - ChildrenNames.Length;
         int NumChildrenToDelete = ChildrenNames.Length - NumChildren;
         for (int i = 1; i <= NumChildrenToAdd; i++)
            Node.AppendChild(CreateNode(Node.OwnerDocument, ChildType, ChildName));

         if (NumChildrenToDelete > 0)
            {
            List<XmlNode> ChildsToDelete = ChildNodes(Node, ChildType);
            ChildsToDelete.RemoveRange(0, ChildsToDelete.Count - NumChildrenToDelete);
            foreach (XmlNode ChildToDelete in ChildsToDelete)
               Node.RemoveChild(ChildToDelete);
            }
         }

      private class XmlNodeComparer : System.Collections.IComparer
         {
         // Calls CaseInsensitiveComparer.Compare with the parameters reversed.
         int System.Collections.IComparer.Compare(Object x, Object y)
            {
            XmlNode yNode = (XmlNode)y;
            XmlNode xNode = (XmlNode)x;
            return ((new CaseInsensitiveComparer()).Compare(Name(xNode), Name(yNode)));
            }

         }
      public static void Sort(XmlNode Node, IComparer Comparer)
         {
         XmlNode[] SortedNodes = new XmlNode[Node.ChildNodes.Count];
         for (int i = 0; i != Node.ChildNodes.Count; i++)
            {
            SortedNodes[i] = Node.ChildNodes[i];
            }
         if (Comparer == null)
            Array.Sort(SortedNodes, new XmlNodeComparer());
         else
            Array.Sort(SortedNodes, Comparer);
         foreach (XmlNode Child in ChildNodes(Node, ""))
            Child.ParentNode.RemoveChild(Child);
         foreach (XmlNode Child in SortedNodes)
            Node.AppendChild(Child);
         }

      public static XmlNode EnsureNodeExists(XmlNode Node, string NodePath)
         {
         // --------------------------------------------------------
         // Ensure a node exists by creating nodes as necessary
         // for the specified node path.
         // --------------------------------------------------------

         if (NodePath.Length == 0)
            return Node;

         int PosDelimiter = NodePath.IndexOf(XmlHelper.Delimiter);
         string ChildNameToMatch = NodePath;
         if (PosDelimiter != -1)
            ChildNameToMatch = NodePath.Substring(0, PosDelimiter);

         foreach (XmlNode Child in Node.ChildNodes)
            {
            if (Name(Child).ToLower() == ChildNameToMatch.ToLower())
               {
               if (PosDelimiter == -1)
                  return Child;
               else
                  return EnsureNodeExists(Child, NodePath.Substring(PosDelimiter + 1));
               }
            }

         // Didn't find the child node so add one and continue.
         XmlNode NewChild = Node.AppendChild(Node.OwnerDocument.CreateElement(ChildNameToMatch));
         if (PosDelimiter == -1)
            return NewChild;
         else
            return EnsureNodeExists(NewChild, NodePath.Substring(PosDelimiter + 1));
         }

      }
   }

