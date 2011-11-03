using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.IO;
using System.Xml;

using ApsimFile;
using Controllers;
using CSGeneral;
using CMPServices;

namespace CPIUserInterface
{
    //=====================================================================
    /// <summary>
    /// Specialised interface for mvCotton component. 
    /// This mvCottonUI class is used to create a initsection for Cotton
    /// from the component's getDescription.
    /// Cultivar details listed in the model.xml (apsim) are read and used to 
    /// populate the cultivar array in the initSection.
    /// Other details are set to default values from the getDescription().
    /// Cultivar, sow depth, row spacing, plants density and skip row are 
    /// specified/adjusted in the sow command (manager).
    /// </summary>
    //=====================================================================
    public partial class mvCottonUI : CPIBaseView
    {

        private List<TTypedValue> typedvals;

        public mvCottonUI() : base()
        {
            InitializeComponent();
            typedvals = new List<TTypedValue>();
        }
        //=====================================================================
        /// <summary>
        /// When the UI is created and loaded
        /// </summary>
        //=====================================================================
        protected override void OnLoad()
        {
            base.HelpText = " Cotton component";
            InitFromComponentDescription(); //fills the propertyList with init properties
        }
        //=====================================================================
        /// <summary>
        /// 
        /// </summary>
        //=====================================================================
        private void mvCottonUI_Load(object sender, EventArgs e)
        {
            //Fill the property fields
            XmlNode initSection = XmlHelper.Find(Data, "initsection");

            if (initSection != null)
            {
                TInitParser initPsr = new TInitParser(initSection.OuterXml);

                for (int i = 1; i <= initPsr.initCount(); i++)
                {
                    String initText = initPsr.initText((uint)i);
                    TSDMLValue sdmlinit = new TSDMLValue(initText, "");
                    typedvals.Add(sdmlinit);
                }
            }
        }
        //=====================================================================
        /// <summary>
        /// Load an image and set the labels
        /// </summary>
        //=====================================================================
        public override void OnRefresh()
        {
            label1.Text = XmlHelper.Type(Data);
            this.HelpText = "This module does not have any editable properties.";

            String imagefile = Types.Instance.MetaData(Data.Name, "image");
            if (File.Exists(imagefile))
            {
                pictureBox1.Image = Image.FromFile(imagefile);
            }
            else
            {
                pictureBox1.Image = null;
            }
            label2.Text = Types.Instance.MetaData(Data.Name, "description");
        }
        //=====================================================================
        /// <summary>
        /// Save the changes from the form.
        /// </summary>
        //=====================================================================
        public override void OnSave()
        {
           //************** //StoreControls();
            String newXML = WriteInitsectionXml();

            //now store the new xml by replacing the old xmlnode in Data
            XmlNode initSection = XmlHelper.Find(Data, "initsection");

            if (initSection != null)
                Data.RemoveChild(initSection);

            XmlDocument doc = new XmlDocument();
            doc.LoadXml(newXML);

            //add new data about the cultivars as an array of cultivars (only add the ones that have extra details)
            String CultivarSpecs = WriteCultivars();
            XmlDocument cultivar = new XmlDocument();
            cultivar.LoadXml(CultivarSpecs);
            doc.DocumentElement.AppendChild(doc.ImportNode(cultivar.DocumentElement, true));

            Data.AppendChild(Data.OwnerDocument.ImportNode(doc.DocumentElement, true));
        }
        //=====================================================================
        /// <summary>
        /// The ddml type string for the cultivar type that will be in the init section of the component
        /// </summary>
        private const String CULTIVARTYPE = "<type name=\"cultivars\" kind=\"defined\" array=\"T\">" +
                                              "<element>" +
                                                "<field name=\"cultivar\" kind=\"string\"/>" +
                                                "<field name=\"percent_l\" kind=\"double\"/>" +
                                                "<field name=\"scboll\" kind=\"double\"/>" +
                                                "<field name=\"respcon\" kind=\"double\"/>" +
                                                "<field name=\"sqcon\" kind=\"double\"/>" +
                                                "<field name=\"fcutout\" kind=\"double\"/>" +
                                                "<field name=\"flai\" kind=\"double\"/>" +
                                                "<field name=\"DDISQ\" kind=\"double\"/>" +
                                                "<field name=\"TIPOUT\" kind=\"double\"/>" +
                                                "<field name=\"FRUDD\" kind=\"double\" array=\"T\"/>" +
                                                "<field name=\"BLTME\" kind=\"double\" array=\"T\"/>" +
                                                "<field name=\"WT\" kind=\"double\" array=\"T\"/>" +
                                                "<field name=\"dlds_max\" kind=\"double\"/>" +
                                                "<field name=\"rate_emergence\" kind=\"double\" units=\"mm/dd\"/>" +
                                                "<field name=\"popcon\" kind=\"double\"/>" +
                                                "<field name=\"fburr\" kind=\"double\"/>" +
                                                "<field name=\"ACOTYL\" kind=\"double\" units=\"mm2\"/>" +
                                                "<field name=\"RLAI\" kind=\"double\"/>" +
                                                "<field name=\"BckGndRetn\" kind=\"double\"/>" +
                                              "</element>" +
                                            "</type>";

        //=====================================================================
        /// <summary>
        /// Scan through the cultivar list in mvCotton.xml and find any that have
        /// parameters and form them into an XML document based on TSDMLValue.
        /// </summary>
        /// <returns>The cultivar list as XML.</returns>
        //=====================================================================
        private String WriteCultivars()
        {
            //create new TSDMLValue for the cultivar details
            TDDMLValue CultivarValues = new TDDMLValue(CULTIVARTYPE, "");
            CultivarValues.setElementCount(0);

            uint cultivarCount = 0;
            XmlDocument ModelDoc = new XmlDocument();
            String ComponentType = Controller.ApsimData.Find(NodePath).Type;
            ModelDoc.LoadXml("<Model>" + Types.Instance.ModelContents(ComponentType) + "</Model>");
            foreach (XmlNode Child in ModelDoc.DocumentElement.ChildNodes)
            {
                if (XmlHelper.Attribute(Child, "cultivar").ToLower() == "yes")
                {
                    if (Child.ChildNodes.Count > 0)
                    {
                        XmlDocument CultivarDoc = new XmlDocument();
                        CultivarDoc.LoadXml(Child.OuterXml);
                        CultivarValues.setElementCount(++cultivarCount);
                        TTypedValue arrayitem = CultivarValues.item(cultivarCount);
                        arrayitem.member("cultivar").setValue(CultivarDoc.DocumentElement.Name);   //read root tag name
                        foreach (XmlNode param in CultivarDoc.DocumentElement.ChildNodes)  //for each child of cultivardoc
                        {
                            if (param.NodeType == XmlNodeType.Element)
                            {
                                TTypedValue field = arrayitem.member(param.Name);
                                if (field != null)
                                {
                                    if (param.Name == "FRUDD" || param.Name == "BLTME" | param.Name == "WT")
                                    {
                                        //add array node values
                                        String arrayField = param.InnerText;
                                        if (arrayField.Contains(" "))
                                        {
                                            string[] VariableNameBits = arrayField.Split(" ".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
                                            double[] values = MathUtility.StringsToDoubles(VariableNameBits);
                                            uint count = 0;
                                            foreach (double val in values)
                                            {
                                                field.setElementCount(++count);
                                                field.item(count).setValue(val);
                                            }
                                        }
                                    }
                                    else
                                    {
                                        double val = Convert.ToDouble(param.InnerText);     //these are all doubles
                                        field.setValue(val);        //copy child to new doc
                                    }
                                }
                                else
                                    throw new Exception("Cannot set init value for " + param.Name + " in WriteCultivars()");
                            }
                        }
                    }
                }
            }
            TSDMLValue writer = new TSDMLValue("<type/>", "");
            return writer.getText(CultivarValues, 0, 2);
        }

    }
}
