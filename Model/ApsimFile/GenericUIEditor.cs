using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using CSGeneral;
using System.Data;


public interface GenericUIView
   {
   // ---------------------------------------------------------------
   // This interface class outlines how the genericUI editor class below
   // talks to the view.
   // ---------------------------------------------------------------

   void SetTable(DataTable Table);
   }






public class GenericUIEditor
   {
   // ---------------------------------------------------------------
   // This class is given an XML data node and a view. It then passes
   // to the view data to be displayed and looks after the reading and
   // writing of all XML data.
   // ---------------------------------------------------------------
   private XmlNode _Data;
   private GenericUIView _View;

   public GenericUIEditor(XmlNode Data, GenericUIView View)
      {
      _Data = Data;
      _View = View;
      }

   public void RefreshView()
      {
      // ---------------------------------------------------------------
      // The view wants to refresh itself. Go read all properties from 
      // the XML, put into a table and call view.
      // ---------------------------------------------------------------

      DataTable Table = new DataTable();
      Table.Columns.Add("Name", typeof(string));
      Table.Columns.Add("Type", typeof(string));
      Table.Columns.Add("List items (CSV)", typeof(string));
      Table.Columns.Add("Description", typeof(string));
      Table.Columns.Add("Value", typeof(string));

      foreach (XmlNode Child in _Data.ChildNodes)
         {
         if (Child.ChildNodes.Count <= 1 && Child.Name != "TeeChartFormat")
            {
            DataRow NewRow = Table.NewRow();
            Table.Rows.Add(NewRow);
            if (Child.Name == "category")
               {
               NewRow[0] = "category";
               NewRow[1] = "category";
               NewRow[3] = XmlHelper.Attribute(Child, "description");
               }
            else
               {
               NewRow[0] = Child.Name;
               if (XmlHelper.Attribute(Child, "type") == "")
                  NewRow[1] = "text";
               else
                  NewRow[1] = XmlHelper.Attribute(Child, "type");
               NewRow[2] = XmlHelper.Attribute(Child, "listvalues");
               if (XmlHelper.Attribute(Child, "description") == "")
                  NewRow[3] = XmlHelper.Name(Child);
               else
                  NewRow[3] = XmlHelper.Attribute(Child, "description");

               NewRow[4] = Child.InnerText;
               }
            }
         }
      _View.SetTable(Table);
      }
   public void OnDataChanged(DataTable Table)
      {
      // ---------------------------------------------------------------
      // The view has changed the data. Save back to XML.
      // ---------------------------------------------------------------

      // Remove existing XML nodes that don't have any children.
      for (int i = _Data.ChildNodes.Count - 1; i >= 0; i--)
         {
         if (_Data.ChildNodes[i].ChildNodes.Count <= 1)
            _Data.RemoveChild(_Data.ChildNodes[i]);
         }

      // Add new child nodes.
      foreach (DataRow Row in Table.Rows)
         {
         XmlNode NewProperty = _Data.OwnerDocument.CreateElement(Row["Name"].ToString());
         XmlHelper.SetAttribute(NewProperty, "type", Row["Type"].ToString());
         if (Row["List items (CSV)"].ToString() != "")
            XmlHelper.SetAttribute(NewProperty, "listvalues", Row["List items (CSV)"].ToString());
         if (Row["Description"].ToString() != "")
            XmlHelper.SetAttribute(NewProperty, "description", Row["Description"].ToString());
         if (Row["Value"].ToString() != "")
            NewProperty.InnerText = Row["Value"].ToString();

         _Data.AppendChild(NewProperty);
         }
      }
   }