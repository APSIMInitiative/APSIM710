using System;
using System.Text;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Runtime.InteropServices;
using ModelFramework;
using CMPServices;
using CSGeneral;

namespace ModelFramework
{

    public class Paddock : Component
    {
        
        // --------------------------------------------------------------------
        /// <summary>
        /// Encapsulates an APSIM paddock in a simulation.
        /// </summary>
        /// <param name="Nam">Name of the paddock or system</param>
        /// <param name="component">The hosting component</param>
        // --------------------------------------------------------------------
        public Paddock(String Nam, ApsimComponent component)
            : base(Nam, component)
        {
            NamePrefix = "";
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// 
        /// </summary>
        /// <param name="In">Instance of a root/leaf/shoot/phenology</param>
        // --------------------------------------------------------------------
        public Paddock(Instance In)
            : base(In)
        {
            NamePrefix = "";
        }

        // --------------------------------------------------------------------
        /// <summary>
        /// Return the parent paddock of this paddock or null if no parent found.
        /// </summary>
        // --------------------------------------------------------------------
        public Paddock Parent
        {
            get
            {
                string ParentName = AddMasterPM(StringManip.ParentName(FullName));
                return new Paddock(ParentName, HostComponent);
            }
        }


        // --------------------------------------------------------------------
        /// <summary>
        /// Return a list of all child paddock components to caller.
        /// </summary>
        // --------------------------------------------------------------------
        public List<Paddock> ChildPaddocks
        {
            get
            {
                queryChildComponents();
                List<Paddock> Children = new List<Paddock>();
                foreach (KeyValuePair<uint, TComp> pair in ChildComponents)
                {
                    if (pair.Value.CompClass.ToLower() == "paddock" || pair.Value.CompClass.ToLower() == "protocolmanager")
                    {
                        Paddock C = new Paddock(pair.Value.name, HostComponent);
                        Children.Add(C);
                    }
                }
                return Children;
            }
        }


        // --------------------------------------------------------------------
        /// <summary>
        /// Return a list of all child paddock components to caller.
        /// </summary>
        // --------------------------------------------------------------------
        public override List<Component> Children
        {
            get
            {
                List<Component> Children = new List<Component>();
                queryChildComponents();
                foreach (KeyValuePair<uint, TComp> pair in ChildComponents)
                {
                    Component C = new Component(pair.Value.name, HostComponent);
                    Children.Add(C);
                }
                return Children;
            }
        }

        /// <summary>
        /// Returns a reference to a variable.
        /// <param name="VariableName"></param>
        /// </summary>
        protected override Variable Variable(String VariableName)
        {
            return new Variable(HostComponent, VariableName);
        }

        // --------------------------------------------------------------------
        /// <summary>
        /// Return a list of all child crops to caller.
        /// </summary>
        // --------------------------------------------------------------------
        public List<Component> Crops
        {
            get
            {
                List<Component> Children = new List<Component>();
                List<TIDSpec> entityList = new List<TIDSpec>();
                queryChildComponents();
                foreach (KeyValuePair<uint, TComp> pair in ChildComponents)
                {
                    Component ChildComponent = new Component(pair.Value.name, HostComponent);
                    // How can we determine what is a "crop"?
                    // Currently, all "crops" have cover_green as an output
                    // However, the AusFarm "Paddock" component also has this as an output. Might this be a problem?
                    // It shouldn't be, if we're looking only at children, and not descendants further down the tree
                    String sSearchName = AddMasterPM(ChildComponent.FullName) + ".cover_green";    
                    HostComponent.Host.queryEntityInfo(sSearchName, TypeSpec.KIND_OWNED, ref entityList);
                    if (entityList.Count > 0)
                    {
                        Children.Add(ChildComponent);
                        entityList.Clear();
                    }
                }
                return Children;
            }
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public class MyPaddock
    {
        private static Paddock _Singleton = null;
        public Paddock this[String ComponentName]
        {
            get
            {
                if (_Singleton == null)
                    _Singleton = new Paddock(ComponentName, null);
                return _Singleton;
            }
        }
    }
}