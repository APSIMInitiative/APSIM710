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

        public virtual void Process(JobRunner jobRunner, Component Simulation, string SimulationPath, string filename)
        {
            foreach (Component child in FactorComponent.ChildNodes)
            {
                //replace each target that is within the provided simulation with the child's xml
                foreach (string target in Targets)
                {
                    Component targetComp = Simulation.Find(target);
                    if (targetComp != null)
                    {
                        targetComp.ContentsAsXML.InnerXml = child.ContentsAsXML.InnerXml;
                    }
                }
                if (NextItem != null)
                {
                    //call next factor in the list
                    NextItem.Process(jobRunner, Simulation, SimulationPath, filename + "_" + child.Name);
                }
                else
                {
                    CreateJobFromSimulation(jobRunner, Simulation, filename + "_" + child.Name);
                }
            }
        }
       public void CreateJobFromSimulation(JobRunner jobRunner, Component Simulation, string filename)
       {
           Simulation.Name = filename;

           string SimFileName;
           SimFileName = ApsimToSim.WriteSimFile(Simulation);

           RunApsimJob NewJob = new RunApsimJob(Simulation.Name, jobRunner);
           NewJob.SimFileName = SimFileName;
           jobRunner.Add(NewJob);
       }

    }
    public class ManagerFactorItem : FactorItem
    {
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

        public override void Process(JobRunner jobRunner, Component Simulation, string SimulationPath, string filename)
        {
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
                    }
                    if (NextItem != null)
                    {
                        //call next factor in the list
                        NextItem.Process(jobRunner, Simulation, SimulationPath, filename + "_" + Variable.Name + "=" + par);
                    }
                    else
                    {
                        CreateJobFromSimulation(jobRunner, Simulation, filename + "_" + Variable.Name + "=" + par);
                    }
                }
            }
        }

    }

    public class FactorBuilder
    {
        public List<FactorItem> BuildFactorItems(Component factorial, string SimulationPath)
        {
            //change to a list of lists when adding levels?
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
                        if (comp.ChildNodes.Count == 1 && (comp.ChildNodes[0].Type == "manager" || comp.ChildNodes[0].Type == "cropui"))
                        {
                            //process variables within manager code
                            XmlNodeList varNodes = comp.ContentsAsXML.SelectNodes("//vars/*");
                            foreach (XmlNode node in varNodes)
                            {
                                ManagerFactorItem item = new ManagerFactorItem();
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
                            FactorItem item = new FactorItem();
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
