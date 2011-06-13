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

        public virtual void Process(List<SimFactorItem> SimFiles, Component Simulation, string SimulationPath, string factorsList, ref int counter, int totalCount)
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
                    NextItem.Process(SimFiles, Simulation, SimulationPath, factorsList + FactorComponent.Name + "=" + child.Name, ref counter, totalCount * getCount());
                }
                else
                {
                    ++counter;
                    CreateJobFromSimulation(SimFiles, Simulation, factorsList + FactorComponent.Name + "=" + child.Name, ref counter, totalCount * getCount());
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

      public void CreateJobFromSimulation(List<SimFactorItem> SimFiles, Component Simulation, string factorsList, ref int counter, int totalCount)
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
            Component varComp = comp.Find("Variables");
            if (varComp != null)
            {
               //add/update constants details
               XmlNode variablesNode = varComp.ContentsAsXML;
               XmlNode constantsNode = variablesNode.SelectSingleNode("//Constants");
               if (constantsNode == null)
               {
                  constantsNode = variablesNode.AppendChild(variablesNode.OwnerDocument.CreateElement("Constants"));
               }
               if (Builder.TitleIsSingleLine)
               {
                  //find existing node and replace - if it doesn't exist, create
                  XmlNode titleNode = constantsNode.SelectSingleNode("//Title");
                  if (titleNode == null)
                  {
                     titleNode = constantsNode.OwnerDocument.CreateElement("Title");
                     constantsNode.PrependChild(titleNode);
                  }
                  else
                  {
                     titleNode.RemoveAll();
                  }
                  titleNode.InnerText = factorsList;//will overwrite any existing values
               }
               else
               {
                  //add a node for each factor
                  constantsNode.RemoveAll();
                  string[] factors = factorsList.Split(';');
                  foreach (string factor in factors)
                  {
                     XmlNode xNode = constantsNode.OwnerDocument.CreateElement("line");
                     constantsNode.AppendChild(xNode);
                     xNode.InnerText = factor;
                  }
               }

               varComp.Contents = variablesNode.OuterXml;
            }
         }
         string SimFileName;
         SimFileName = ApsimToSim.WriteSimFile(Simulation);
         SimFactorItem itm = new SimFactorItem(Simulation.Name, SimFileName);
         SimFiles.Add(itm);    
          //return simulation name to it's original
          Simulation.Name = sInitialName;
      }
      public void AddOutputFilesToList(Component parent, List<Component> outputfiles)
      {
         if (parent.Type == "outputfile")
            outputfiles.Add(parent);
         foreach (Component comp in parent.ChildNodes)
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

        public override void Process(List<SimFactorItem> SimFiles, Component Simulation, string SimulationPath, string factorsList, ref int counter, int totalCount)
        {
            if (factorsList != "")
                factorsList += ";";
            foreach (string par in Parameters)
            {
                //replace each target that is within the provided simulation with the child's xml
                foreach (string target in Targets)
                {
                    Component targetComp = Simulation.Find(target);
                    if (targetComp != null)
                    {
                        //find name of var in ui or CustomUI nodes
                        XmlNode varNode = targetComp.ContentsAsXML.SelectSingleNode("//ui/" + Variable.Name + " | " + "//CustomUI/" + Variable.Name);
                        if (varNode != null)
                        {
                           varNode.InnerText = target;
                        }
                        else
                        {
                           varNode = targetComp.ContentsAsXML.SelectSingleNode("//"+Variable.Name);
                           if (varNode != null)
                           {
                              varNode.InnerText = target;
                           }
                        }
                    }
                }
                if (NextItem != null)
                {
                    //call next factor in the list
                    NextItem.Process(SimFiles, Simulation, SimulationPath, factorsList + Variable.Name + "=" + par, ref counter, totalCount * getCount());
                }
                else
                {
                    ++counter;
                    CreateJobFromSimulation(SimFiles, Simulation, factorsList + Variable.Name + "=" + par, ref counter, totalCount * getCount());
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

            string s = XmlHelper.Attribute(varNode, "fn");
            if (s == "1")
                SaveExtraInfoInFilename = true;

            s = XmlHelper.Attribute(varNode, "tl");
            if (s == "1")
                TitleIsSingleLine = false;

            FactorItem lastItem = null;
            List<FactorItem> items = new List<FactorItem>();
            ProcessFactorNodes(factorial, ref lastItem, ref items, SimulationPath);
            return items;
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
                            if (target.InnerText.Contains(SimulationPath))
                                targets.Add(target.InnerText);
                        }
                    }
                    if (targets.Count > 0)
                    {
                        if (comp.ChildNodes.Count == 1 && (comp.ChildNodes[0].Type == "manager" || comp.ChildNodes[0].Type == "rule" || comp.ChildNodes[0].Type == "cropui"))
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
}
