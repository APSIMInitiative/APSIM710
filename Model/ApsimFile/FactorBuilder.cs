using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using CSGeneral;

namespace ApsimFile
{
    public class FactorItem
    {
        public FactorItem NextItem = null;
        public List<string> Targets = null;

        public Component FactorComponent = null;
        public FactorBuilder Builder = null;
        public FactorItem(FactorBuilder b)
        {
            Builder = b;
        }
        public virtual string getDesc()
        {
            return FactorComponent.Name;
        }
        //getCount returns the number of parameters for this factor only
        public virtual int getCount()
        {
            if(FactorComponent != null)
                return FactorComponent.ChildNodes.Count;
            return 0;
        }
        //CalcCount returns the number of parameters for the chain of factors
        public virtual int CalcCount()
        {
            if (NextItem != null)
                return NextItem.CalcCount() * FactorComponent.ChildNodes.Count;
            return FactorComponent.ChildNodes.Count; 
        }
        public virtual void CalcFactorialList(List<String> factorials, string factorsList, ref int counter, int totalCount)
        {
           if (factorsList != "")
              factorsList += ", ";
           foreach (Component child in FactorComponent.ChildNodes)
           {
              if (NextItem != null)
              {
                 //call next factor in the list
                 NextItem.CalcFactorialList(factorials, factorsList + FactorComponent.Name + " = " + child.Name, ref counter, totalCount * getCount());
              }
              else
              {
                 ++counter;
                 factorials.Add(factorsList + FactorComponent.Name + " = " + child.Name);
              }
           }
        }

        public virtual void Process(List<SimFactorItem> SimFiles, Component Simulation, string SimulationPath, string factorsList, ref int counter, int totalCount, Configuration.architecture arch)
        {
            if (factorsList != "")
                factorsList += ";";
            foreach (Component child in FactorComponent.ChildNodes)
            {
                //replace each target that is within the provided simulation with the child's xml
                foreach (string target in Targets)
                {
                    //need to remove the path of this simulation from the target to get the relative path
                    string relativePath = target.Substring(Simulation.FullPath.Length+1);
                    Component targetComp = Simulation.Find(relativePath);
                    if (targetComp != null)
                    {
                       //replace target nodes with factor nodes - add child factor nodes if they don't exist 
                       //don't remove any children from the target
                       ReplaceComponent(targetComp, child);
                    }
                }
                if (NextItem != null)
                {
                    //call next factor in the list
                    NextItem.Process(SimFiles, Simulation, SimulationPath, factorsList + FactorComponent.Name + "=" + child.Name, ref counter, totalCount * getCount(), arch);
                }
                else
                {
                    ++counter;
                    CreateJobFromSimulation(SimFiles, Simulation, factorsList + FactorComponent.Name + "=" + child.Name, ref counter, totalCount * getCount(), arch);
                }
            }
        }
        public void ReplaceComponent(Component targetComp, Component Source)
        {
           //replace the contents (innerxml).This doesn't affect child xml nodes that belong to child components
           targetComp.Contents = Source.Contents;
           //targetComp.Name = Source.Name;
           targetComp.ChildNodes.Clear();

           foreach (Component comp in Source.ChildNodes)
           {
                 //add a component
              Component targetchild = targetComp.Add(comp.Contents);
              ReplaceComponent(targetchild, comp);
           }
        }

        public void CreateJobFromSimulation(List<SimFactorItem> SimFiles, Component Simulation, string factorsList, ref int counter, int totalCount, Configuration.architecture arch)
      {
         string sInitialName = Simulation.Name;
         if (Builder.SaveExtraInfoInFilename)
         {
            Simulation.Name = factorsList;
         }
         else
         {
            //write a spacer to list sims in order eg: 01 or 001 or 0001 depending on count
            string sPad = "";
            double tot = Math.Floor(Math.Log10(totalCount) + 1);
            double file = Math.Floor(Math.Log10(counter) + 1);
            for (int i = 0; i < (int)(tot - file); ++i)
               sPad += "0";

            Simulation.Name = sInitialName + "_" + sPad + counter.ToString();
         }
         //add title line into simulation
         //find all components that are outputfiles... or use xml?
         List<Component> outputfiles = new List<Component>();
         AddOutputFilesToList(Simulation, outputfiles);
         foreach (Component comp in outputfiles)
         {
            // Ensure the output name is correct for this simulation
            XmlNode compNode = comp.ContentsAsXML;
            XmlNode fileNode = compNode.SelectSingleNode("//filename");
            if (fileNode != null)
                fileNode.InnerText = ComponentUtility.CalcFileName(comp);
            else
                throw new Exception("Cant find an outputfile filename node!");
   
            if (Builder.TitleIsSingleLine)
            {
                //Title node is now read only, so need to add this as a constant 
                //XmlNode titleNode = compNode.SelectSingleNode("//title");
                //if (titleNode != null)
                //    titleNode.InnerText = factorsList;
                //else
                //    throw new Exception("Cant find an outputfile title node!");
                Component constantsComponent = null;  
                foreach (Component c in comp.ChildNodes)
                if (c.Type == "variables")
                    constantsComponent = c;
                if (constantsComponent == null) 
                    throw new Exception("No variables in outputfile!");

                XmlNode componentNode = constantsComponent.ContentsAsXML; 
                XmlNode constantsNode = componentNode.SelectSingleNode("//constants");
                if (constantsNode == null)
                {
                    constantsNode = componentNode.OwnerDocument.CreateElement("constants");
                    componentNode.AppendChild(constantsNode);
                }
                XmlNode factorNode = constantsNode.SelectSingleNode("//constant[@name='factors']");
                if (factorNode == null)
                {
                    factorNode = constantsNode.OwnerDocument.CreateElement("constant");
                    constantsNode.AppendChild(factorNode);
                }
                XmlHelper.SetAttribute(factorNode, "name", "factors");
                factorNode.InnerText = factorsList;
                constantsComponent.Contents = componentNode.OuterXml;
            }
            else
            {
               //Find the variables node (should be a child of the output file - it will find and use the first one 
               Component constantsComponent = null;  
                foreach (Component c in comp.ChildNodes)
                    if (c.Type == "variables")
                        constantsComponent = c;
                if (constantsComponent == null) throw new Exception("No variables in outputfile!");

                XmlNode componentNode = constantsComponent.ContentsAsXML; 
                XmlNode constantsNode = componentNode.SelectSingleNode("//constants");
                if (constantsNode == null)
                {
                   constantsNode = componentNode.OwnerDocument.CreateElement("constants");
                   componentNode.AppendChild(constantsNode);
                }
                else
                {
                   //clean out existing nodes - will remove existing constants
                   constantsNode.RemoveAll();
                }
                List<string> factors = new List<string>(factorsList.Split(';'));

                foreach (string factor in factors)
                  {
                     List<string> nameValue = new List<string>(factor.Split('='));
                     if (nameValue.Count > 1)
                     {
                        XmlNode factorNode = constantsNode.OwnerDocument.CreateElement("constant");
                        constantsNode.AppendChild(factorNode);
                        XmlHelper.SetAttribute(factorNode, "name", nameValue[0]);
                        factorNode.InnerText = nameValue[1];
                     }
                  }
                constantsComponent.Contents = componentNode.OuterXml;
            }
            comp.Contents = compNode.OuterXml;
         }
         string SimFileName;
         SimFileName = ApsimToSim.WriteSimFile(Simulation, arch);
         SimFactorItem itm = new SimFactorItem(Simulation.Name, SimFileName);
         SimFiles.Add(itm);    
          //return simulation name to it's original
          Simulation.Name = sInitialName;
      }
      public void AddOutputFilesToList(Component node, List<Component> outputfiles)
      {
         if (node.Type == "outputfile")
            outputfiles.Add(node);
         foreach (Component comp in node.ChildNodes)
         {
            AddOutputFilesToList(comp, outputfiles);
         }
      }
   }
    public class ManagerFactorItem : FactorItem
    {
        public ManagerFactorItem(FactorBuilder b)
        :base(b){}

        public XmlNode Variable = null;
        public List<string> Parameters = null;
        public override string getDesc()
        {
            if (Variable != null)
                return FactorComponent.Name + "."+XmlHelper.Name(Variable);
            return FactorComponent.Name;
        }
        //getCount returns the number of parameters for this factor only
        public override int getCount()
        {
            if(Parameters != null)
                return Parameters.Count;
            return 0;
        }
        //CalcCount returns the number of parameters for the chain of factors
        public override int CalcCount()
        {
            if (NextItem != null)
                return NextItem.CalcCount() * Parameters.Count;
            return Parameters.Count;
        }
        public override void CalcFactorialList(List<String> factorials, string factorsList, ref int counter, int totalCount)
        {
            if (factorsList != "")
                factorsList += ", ";

            foreach (string par in Parameters)
            {
              if (NextItem != null)
              {
                 //call next factor in the list
                 NextItem.CalcFactorialList(factorials, factorsList + Variable.Name + " = " + par, ref counter, totalCount * getCount());
              }
              else
              {
                 ++counter;
                 factorials.Add(factorsList + Variable.Name + " = " + par);
              }
           }
        }
       
        public override void Process(List<SimFactorItem> SimFiles, Component Simulation, string SimulationPath, string factorsList, ref int counter, int totalCount, Configuration.architecture arch)
        {
            if (factorsList != "")
                factorsList += ";";
            foreach (string par in Parameters)
            {
                //replace each target that is within the provided simulation with the child's xml
                foreach (string target in Targets)
                {
                   string sRelativeTarget = target.Substring(SimulationPath.Length+1);
                   Component targetComp = Simulation.Find(sRelativeTarget);
                    if (targetComp != null)
                    {
                        //find name of var in ui or CustomUI nodes
                       XmlNode variablesNode = targetComp.ContentsAsXML;
                       XmlNode varNode = variablesNode.SelectSingleNode("//ui/" + Variable.Name + " | " + "//CustomUI/" + Variable.Name);
                        if (varNode != null)
                        {
                           varNode.InnerText = par;
                        }
                        else
                        {
                           string sSelect = "//property[@name='" + Variable.Name + "']";
                           varNode = variablesNode.SelectSingleNode(sSelect);
                           if (varNode != null)
                           {
                              varNode.InnerText = par;
                           }//TODO if else then record as an error
                        }
                        targetComp.Contents = variablesNode.OuterXml;
                    }
                }
                if (NextItem != null)
                {
                    //call next factor in the list
                    NextItem.Process(SimFiles, Simulation, SimulationPath, factorsList + Variable.Name + "=" + par, ref counter, totalCount * getCount(), arch);
                }
                else
                {
                    ++counter;
                    CreateJobFromSimulation(SimFiles, Simulation, factorsList + Variable.Name + "=" + par, ref counter, totalCount * getCount(), arch);
                }
            }
        }

    }
    public class SimFactorItem
    {
        public SimFactorItem() { }
        public SimFactorItem(string name, string filename)
        {
            SimName = name;
            SimFileName = filename;
        }
        public string SimName { get; set; }
        public string SimFileName { get; set; }
        
    }

    public class FactorBuilder
    {
        public bool TitleIsSingleLine { get; set; }
        public bool SaveExtraInfoInFilename { get; set; }

        public List<FactorItem> BuildFactorItems(Component factorial, string SimulationPath)
        {
            //read file saving options - 2 bools at this stage - single line title, add factor/level to filename
            XmlNode varNode = factorial.ContentsAsXML.SelectSingleNode("//settings");
            TitleIsSingleLine = true;
            SaveExtraInfoInFilename = false;

            string s = "";
            if(varNode != null)
               s = XmlHelper.Attribute(varNode, "fn");
            if (s == "1")
                SaveExtraInfoInFilename = true;

            s = "";
            if(varNode != null) 
               s = XmlHelper.Attribute(varNode, "tl");
            if (s == "1")
                TitleIsSingleLine = false;

            FactorItem lastItem = null;
            List<FactorItem> items = new List<FactorItem>();
            ProcessFactorNodes(factorial, ref lastItem, ref items, SimulationPath);
            return items;
        }
        public void LoadVariableTypes(XmlNode metNode, List<string> variableTypes)
        {
           if (metNode != null && variableTypes != null)
           {
              foreach (XmlNode varNode in XmlHelper.ChildNodes(metNode, "facvar"))
              {
                 variableTypes.Add(varNode.InnerText.ToLower());
              }
           }
        }
        public void ProcessFactorNodes(Component parentComponent, ref FactorItem lastItem, ref List<FactorItem> items, string SimulationPath)
        {
            foreach (Component comp in parentComponent.ChildNodes)
            {
                if (comp.Type == "factor")
                {
                    List<string> targets = new List<string>();
                    XmlNode varNode = comp.ContentsAsXML.SelectSingleNode("//targets");
                    if (varNode != null)
                    {
                        foreach (XmlNode target in varNode.ChildNodes)
                        {
                            string spath = SimulationPath + "/";
                            if (target.InnerText.Contains(spath))
                                targets.Add(target.InnerText);
                        }
                    }
                    if (targets.Count > 0)
                    {
 		 				List<string> variableTypes = new List<string>();
	 		 			LoadVariableTypes(Types.Instance.MetaDataNode("Factor", "FactorVariables"), variableTypes);
                        if (comp.ChildNodes.Count == 1 && variableTypes.Contains(comp.ChildNodes[0].Type.ToLower()))//(comp.ChildNodes[0].Type == "manager" || comp.ChildNodes[0].Type == "rule" || comp.ChildNodes[0].Type == "cropui"))
                        {
                            //process variables within manager code
                            XmlNodeList varNodes = comp.ContentsAsXML.SelectNodes("//vars/*");
                            foreach (XmlNode node in varNodes)
                            {
                                ManagerFactorItem item = new ManagerFactorItem(this);
                                item.Targets = targets;
                                item.FactorComponent = comp;
                                item.Variable = node;
                                string[] pars = node.InnerText.Split(",".ToCharArray());
                                List<string> parameters = new List<string>();
                                parameters.AddRange(pars);
                                item.Parameters = parameters;
                                if (lastItem == null)
                                    items.Add(item);
                                else
                                    lastItem.NextItem = item;
                                lastItem = item;
                            }
                        }
                        else
                        {
                            FactorItem item = new FactorItem(this);
                            item.Targets = targets;
                            item.FactorComponent = comp;
                            if (lastItem == null)
                                items.Add(item);
                            else
                                lastItem.NextItem = item;
                            lastItem = item;
                        }
                    }
                }
                else
                {
                    ProcessFactorNodes(comp, ref lastItem, ref items, SimulationPath);
                }
            }
        }
    }
    public class Factor
    {
        public static void ProcessSimulationFactorials(List<SimFactorItem> SimFiles, ApsimFile copiedFile, Component FactorComponent, string SimulationPath)
        {
            if (FactorComponent == null)
                throw new Exception("Error initialising Factorials");

            if (FactorComponent.ChildNodes.Count > 0)
            {
                Component Simulation = copiedFile.Find(SimulationPath);
                try
                {
                    FactorBuilder builder = new FactorBuilder();
                    List<FactorItem> items = builder.BuildFactorItems(FactorComponent, SimulationPath);
                    foreach (FactorItem item in items)
                    {
                        int counter = 0;
                        string factorsList = "";

                        item.Process(SimFiles, Simulation, SimulationPath, factorsList, ref counter, 1, Configuration.getArchitecture());
                    }
                }
                catch (Exception ex)
                {
                    throw new Exception("Error encountered creating Factorials\n" + ex.Message);
                }
            }
        }

    }
}
