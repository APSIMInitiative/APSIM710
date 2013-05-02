using System.Xml.Serialization;
using System.Xml;
using System.Reflection;
using System;
using CSGeneral;
using System.Collections.Generic;
using System.Collections;

namespace ModelFramework
{
    //=========================================================================
    /// <summary>
    /// A generic system that can have children
    /// </summary>
    public class SystemComponent
    {
        private Simulation Simulation = null;

        [XmlAttribute("name")]
        public string Name;

        [XmlElement("Component")]
        [Link]
        public List<object> Children = null;

        /// <summary>
        /// Resolve all [Link] and call OnInitialise on all child objects.
        /// </summary>
        protected void ResolveLinksAndInitialise(Simulation Simulation)
        {
            this.Simulation = Simulation;
            ResolveLinksAndInitialiseRecursively(Simulation);
        }

        /// <summary>
        /// Recursively resolve all [Link] fields and call OnInitialise on all child objects.
        /// </summary>
        private void ResolveLinksAndInitialiseRecursively(object Obj)
        {
            Console.WriteLine("------- Initialising " + Obj.GetType().Name + " --------------------------------------------------");

            foreach (FieldInfo Field in Utility.GetAllFields(Obj.GetType(), BindingFlags.Instance |
                                                                            BindingFlags.Public |
                                                                            BindingFlags.NonPublic |
                                                                            BindingFlags.DeclaredOnly))
            {
                if (Field.IsDefined(typeof(Link), false))
                {
                    object Value = Field.GetValue(Obj);
                    if (Value is IList)
                    {
                        IList List = (IList)Value;
                        if (List.Count > 0 && List[0].GetType().IsClass)
                            for (int i = 0; i < List.Count; i++)
                            {
                                if (List[i] is Paddock)
                                {
                                    Paddock SubPaddock = List[i] as Paddock;
                                    SubPaddock.Simulation = Simulation;
                                    SubPaddock.ResolveLinksAndInitialiseRecursively(List[i]);
                                }
                                else
                                    ResolveLinksAndInitialiseRecursively(List[i]);
                            }
                    }
                    else if (Field.FieldType == typeof(Simulation))
                        Field.SetValue(Obj, Simulation);
                    else if (Field.FieldType == typeof(SystemComponent))
                        Field.SetValue(Obj, this); 
                    else if (Field.FieldType == typeof(Paddock))
                        Field.SetValue(Obj, this);  //not sure about this ?
                    else
                    {
                        object LinkedObject = FindObject(Field.FieldType);
                        if (LinkedObject == null)
                        {
                            if (Value == null)
                                throw new Exception("Cannot resolve link: " + Field.FieldType.Name);
                            else
                            {
                                // This might be a child object that has already been deserialised e.g. In SoilWater.cs:
                                //     public SoilWatTillageType SoilWatTillageType;
                                // Go see if it has an OnInitialised method that we should call.
                                MethodInfo Initialised = Value.GetType().GetMethod("OnInitialised");
                                if (Initialised != null)
                                    Initialised.Invoke(Value, null);
                            }
                        }
                        else
                            Field.SetValue(Obj, LinkedObject);
                    }
                }
            }
            MethodInfo OnInitialised = Obj.GetType().GetMethod("OnInitialised");
            if (OnInitialised != null)
                OnInitialised.Invoke(Obj, null);
        }

        /// <summary>
        /// Find an object of the specified type in this area. Returns null if not found.
        /// </summary>
        private object FindObject(Type type)
        {
            foreach (object Obj in Children)
                if (Obj.GetType() == type)
                    return Obj;

            // If we get this far then not found. Check the simulation.
            if (this is Simulation)
                return null;
            else
                return Simulation.FindObject(type);
        }

        /// <summary>
        /// Return a model or variable from the simulation or null if not found.
        /// </summary>
        public object Get(string VariableName)
        {
            foreach (object Child in Children)
            {
                if (Child.GetType().Name.Equals(VariableName, StringComparison.CurrentCultureIgnoreCase))
                    return Child;

                object Value = Utility.GetValueOfFieldOrProperty(VariableName, Child);
                if (Value != null)
                    return Value;
            }

            // If we get this far then search the simulation
            if (this is Simulation)
                return null;
            else
                return Simulation.Get(VariableName);
        }

        /// <summary>
        /// Add a new model to this area.
        /// </summary>
        public void Add(object Model)
        {
            ResolveLinksAndInitialiseRecursively(Model);
            Children.Add(Model);
        }

    }

    //=========================================================================
    public class Simulation : SystemComponent
    {
        public event NullTypeDelegate Commence;

        /// <summary>
        /// Run the simulation
        /// </summary>
        public void Run()
        {
            // Write summary to summary file.
            WriteSummary();

            // Go satisfy all [Link]
            ResolveLinksAndInitialise(this);

            // Raise a commence event.
            Console.WriteLine("------- Start of simulation --------------------------------------------------");

            if (Commence != null)
                Commence.Invoke();
        }

        private void WriteSummary()
        {

            Console.WriteLine(@"              _____   _____ _____ __  __ ");
            Console.WriteLine(@"        /\   |  __ \ / ____|_   _|  \/  |");
            Console.WriteLine(@"       /  \  | |__) | (___   | | | \  / |");
            Console.WriteLine(@"      / /\ \ |  ___/ \___ \  | | | |\/| |");
            Console.WriteLine(@"     / ____ \| |     ____) |_| |_| |  | |");
            Console.WriteLine(@"    /_/    \_\_|    |_____/|_____|_|  |_|");
            Console.WriteLine();
            Console.WriteLine("The Agricultural Production Systems Simulator");
            Console.WriteLine("Copyright(c) APSRU");
            Console.WriteLine();
            Console.WriteLine("Version: " + ApsimFile.Configuration.Instance.ApsimVersion() + "x");
            Console.WriteLine();
            Console.WriteLine("Simulation " + Name);
            foreach (object Child in Children)
            {
                Console.WriteLine(" |- " + Child.GetType().Name);
                if (Child is Paddock)
                {
                    Paddock Area = Child as Paddock;
                    foreach (object AreaChild in Area.Children)
                        Console.WriteLine("    |- " + AreaChild.GetType().Name);
                }

            }
            Console.WriteLine();
        }
    }

    //=========================================================================
    /// <summary>
    /// Paddock objects can have crop children
    /// </summary>
    [XmlType("area")]
    public class Paddock : SystemComponent
    {
        [XmlIgnore]
        public Crop[] Crops
        {
            get
            {
                return new Crop[0];
            }
        }
    }

    [XmlType("summaryfile")]
    public class SummaryFile
    {

    }

    public class Folder
    {

    }

}