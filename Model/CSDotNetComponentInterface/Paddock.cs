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
    //=========================================================================
    /// <summary>
    /// Generic system component that is a base class for Paddock and Simulation
    /// </summary>
    public class SystemComponent : Component
    {
        internal ApsimComponent HostComponent;

        // --------------------------------------------------------------------
        /// <summary>
        /// Encapsulates an APSIM paddock in a simulation.
        /// </summary>
        /// <param name="Nam">Name of the paddock or system</param>
        /// <param name="component">The hosting component</param>
        // --------------------------------------------------------------------
        public SystemComponent(String Nam, object component)
            : base(Nam, component)
        {
            //NamePrefix = "";
            HostComponent = component as ApsimComponent;
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// Encapsulates an APSIM paddock in a simulation.
        /// When the component Class is known.
        /// </summary>
        /// <param name="Nam">Name of the paddock or system</param>
        /// <param name="CompClass"></param>
        /// <param name="component">The hosting component</param>
        public SystemComponent(String Nam, String CompClass, object component)
            : base(Nam, CompClass, component)
        {
            //NamePrefix = "";
            HostComponent = component as ApsimComponent;
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// 
        /// </summary>
        /// <param name="In">Instance of a root/leaf/shoot/phenology</param>
        // --------------------------------------------------------------------
        internal SystemComponent(Instance In)
            : base(In)
        {
            //NamePrefix = "";
            HostComponent = In.ParentComponent();
        }

        // --------------------------------------------------------------------
        /// <summary>
        /// Return the parent paddock of this paddock or null if no parent found.
        /// </summary>
        // --------------------------------------------------------------------
        public SystemComponent Parent
        {
            get
            {
                string ParentName = StringManip.ParentName(FullName);
                return new SystemComponent(ParentName, HostComponent);
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
                        Paddock C = new Paddock(pair.Value.name, pair.Value.CompClass, HostComponent);
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
        public List<Component> Children
        {
            get
            {
                List<Component> Children = new List<Component>();
                queryChildComponents();
                foreach (KeyValuePair<uint, TComp> pair in ChildComponents)
                {
                    Component C = new Component(pair.Value.name, pair.Value.CompClass, HostComponent);
                    Children.Add(C);
                }
                return Children;
            }
        } 
        // --------------------------------------------------------------------
        /// <summary>
        /// Return the fully-qualified name of a component, given its ID
        /// This implementation only works for components "owned" by this paddock
        /// It could be extended to be more general...
        /// </summary>
        // --------------------------------------------------------------------
        public string SiblingNameFromId(int compId)
        {
            queryChildComponents();
            TComp aComp;
            if (ChildComponents.TryGetValue((uint)compId, out aComp))
                return aComp.name;
            else
                return "";
        }
    }

    //=========================================================================
    /// <summary>
    /// Paddock class is a specialised system that can have crop type children.
    /// </summary>
    public class Paddock : SystemComponent
    {
        protected Dictionary<String, Boolean> ChildCrops;   //cache the .cover_green response for child components

        public Paddock(String Nam, object component)
            : base(Nam, component)
        {
            NamePrefix = "";
            ChildCrops = new Dictionary<string, bool>();
        }
        public Paddock(String Nam, String CompClass, object component)
            : base(Nam, CompClass, component)
        {
            NamePrefix = "";
            ChildCrops = new Dictionary<string, bool>();
        }
        internal Paddock(Instance In)
            : base(In)
        {
            NamePrefix = "";
            ChildCrops = new Dictionary<string, bool>();
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
                    Component ChildComponent = new Component(pair.Value.name, pair.Value.CompClass, HostComponent);
                    // How can we determine what is a "crop"?
                    // Once upon a time, all "crops" had cover_green as an output
                    // That has now been changed to CoverLive in APSIM, but not (yet) in AusFarm. Ugh.
                    // Should we look for both???
                    // The AusFarm "Paddock" component also has cover_green as an output. Might this be a problem?
                    // It shouldn't be, if we're looking only at children, and not descendants further down the tree
                    for (int pass = 0; pass <= 1; ++pass)
                    {
                        String sSearchName = ChildComponent.FullName + (pass == 0 ? ".CoverLive" : ".cover_green");
                        if (ChildCrops.ContainsKey(sSearchName))
                        {
                            if (ChildCrops[sSearchName])
                            {
                                Children.Add(ChildComponent); //is crop
                            }
                        }
                        else
                        {
                            HostComponent.Host.queryEntityInfo(sSearchName, TypeSpec.KIND_OWNED, ref entityList);
                            if (entityList.Count > 0)
                            {
                                Children.Add(ChildComponent);
                                entityList.Clear();
                                ChildCrops.Add(sSearchName, true);
                            }
                            else
                            {
                                ChildCrops.Add(sSearchName, false);
                            }
                        }
                    }
                }
                return Children;
            }
        }
    } //paddock

    //=========================================================================
    /// <summary>
    /// Simulation that can have child paddocks (no crops)
    /// It also treats it's name differently to a paddock as it's
    /// name does not appear in a FQN path.
    /// </summary>
    public class Simulation : SystemComponent
    {
        public Simulation(String Nam, object component)
            : base(Nam, component)
        {
            NamePrefix = "";
        }
        public Simulation(String Nam, String CompClass, object component)
            : base(Nam, CompClass, component)
        {
            NamePrefix = "";
        }
        internal Simulation(Instance In)
            : base(In)
        {
            NamePrefix = "";
        }
        // --------------------------------------------------------------------
        /// <summary>
        /// Go looking for child components of this simulation.
        /// </summary>
        // --------------------------------------------------------------------
        protected override void queryChildComponents()
        {
            if (ChildComponents == null)
            {
                ChildComponents = new Dictionary<uint, TComp>();

                String sSearchName = "*";

                List<TComp> comps = new List<TComp>();
                HostComponent.Host.queryCompInfo(sSearchName, TypeSpec.KIND_COMPONENT, ref comps);
                ChildComponents.Clear();
                for (int i = 0; i < comps.Count; i++)
                {
                    ChildComponents.Add(comps[i].compID, comps[i]);
                }

            }
        }
    }

}

