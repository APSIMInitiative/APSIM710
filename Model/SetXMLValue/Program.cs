using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;
using System.Xml;

namespace Tools
   {
   class SetXMLValue
      {
      static int Main(string[] args)
         {
         try
            {
            if (args.Length != 3)
               throw new Exception("SetXMLValue FileName XMLPath NewValue");
            else
               {
               XmlDocument Doc = new XmlDocument();
               Doc.Load(args[0]);

               string ParentNodeName = args[1];
               string ChildNodeName = "";
               int PosDelimiter = ParentNodeName.LastIndexOf('/');
               if (PosDelimiter != -1)
                  {
                  ChildNodeName = ParentNodeName.Substring(PosDelimiter + 1);
                  ParentNodeName = ParentNodeName.Substring(0, PosDelimiter);
                  }
               if (ParentNodeName[0] == '@')
                  {
                  ChildNodeName = ParentNodeName;
                  ParentNodeName = "";
                  }

               XmlNode Node;
               if (ParentNodeName == "")
                  Node = Doc.DocumentElement;
               else
                  Node = XmlHelper.Find(Doc.DocumentElement, ParentNodeName);
               if (Node != null)
                  {
                  if (ChildNodeName[0] == '@')
                     {
                     ChildNodeName = ChildNodeName.Substring(1);
                     XmlHelper.SetAttribute(Node, ChildNodeName, args[2]);
                     }
                  else
                     XmlHelper.SetValue(Node, ChildNodeName, args[2]);
                  }
               Doc.Save(args[0]);
               }
            }
         catch (Exception err)
            {
            Console.Error.WriteLine(err.Message);
            return 1;
            }
         return 0;
         }
      }
   }
