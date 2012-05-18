using System;
using System.Text;
using System.Collections.Generic;
using System.Collections.Specialized;
using System.Runtime.InteropServices;
using ModelFramework;
using System.Xml;
using System.Reflection;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;

namespace ModelFramework
{

    [Serializable]
    public class Paddock : Component
    {
       // private ModelInstance Instance;


        /// <summary>
        /// Encapsulates an APSIM paddock in a simulation.
        /// </summary>
        internal Paddock(ModelInstance Inst)
            : base(Inst)
        {
            Instance = Inst;
        }

        /// <summary>
        /// Returns the parent Paddock or null if there is no parent.
        /// </summary>
        public Paddock Parent
        {
            get
            {
                if (Instance.Parent == null)
                    return null;
                else
                    return new Paddock(Instance.Parent);
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
                List<Paddock> Children = new List<Paddock>();
                //foreach (KeyValuePair<uint, TComp> pair in ChildComponents)
                //{
                //    if (pair.Value.CompClass.ToLower() == "paddock" || pair.Value.CompClass.ToLower() == "protocolmanager")
                //    {
                //        Paddock C = new Paddock(pair.Value.name, HostComponent);
                //        Children.Add(C);
                //    }
                //}
                return Children;
            }
        }
        //=========================================================================
        /// <summary>
        /// Returns a component that is a child of the paddock
        /// </summary>
        //=========================================================================
        public Component ComponentByType(String TypeToFind)
        {
            throw new NotImplementedException();
            
        }

        /// <summary>
        /// Returns a reference to a variable.
        /// <param name="VariableName"></param>
        /// </summary>
        public override Variable Variable(String VariableName)
        {
            throw new NotImplementedException();
        }
        //=========================================================================
        /// <summary>
        /// Publish a notification event i.e. one that doesn't have any data 
        /// associated with it. This event is broadcast to all components within scope.
        /// </summary>
        //=========================================================================
        public override void Publish(String EventName)
        {
            Instance.Publish(EventName);
        }

        public void Activate(string ComponentName)
        {
            ModelInstance I = Instance.FindModelInstance(ComponentName);
            if (I == null)
                throw new Exception("Cannot find component: " + ComponentName + ". Cannot activate it.");
            I.Activate();
        }

        public void Deactivate(string ComponentName)
        {
            ModelInstance I = Instance.FindModelInstance(ComponentName);
            if (I == null)
                throw new Exception("Cannot find component: " + ComponentName + ". Cannot deactivate it.");
            I.Deactivate();
        }


        public string Checkpoint(string ComponentName)
        {
            ModelInstance I = Instance.FindModelInstance(ComponentName);
            if (I == null)
                throw new Exception("Cannot find component: " + ComponentName + ". Cannot checkpoint it.");

            return I.Checkpoint();
        }

        public void RestoreFromCheckpoint(string ComponentName, string Checkpoint)
        {
            ModelInstance I = Instance.FindModelInstance(ComponentName);
            if (I == null)
                throw new Exception("Cannot find component: " + ComponentName + ". Cannot RestoreFromCheckpoint.");

            I.RestoreFromCheckpoint(Checkpoint);
        }

        public void InitialiseFromCheckpoint(string ComponentName, string Checkpoint)
        {
            ModelInstance I = Instance.FindModelInstance(ComponentName);
            if (I == null)
                throw new Exception("Cannot find component: " + ComponentName + ". Cannot RestoreFromCheckpoint.");

            I.InitialiseFromCheckpoint(Checkpoint);
        }

        public void OutputDiagnostics()
        {
            Instance.Root.OutputDiagnostics();
        }

        //=========================================================================
        /// <summary>
        /// Publish an event that has associated data. This event is broadcast to all components within scope.
        /// </summary>
        /// <param name="EventName"></param>
        /// <param name="Data"></param>
        //=========================================================================
        //public override void Publish(String EventName/*, ApsimType Data*/)
        //{
        //    throw new NotImplementedException();
        //}

        /// <summary>
        /// Return a list of all child crops to caller.
        /// </summary>
        public List<Component> Crops
        {
            get
            {
                List<Component> crops = new List<Component>();
                foreach (ModelInstance crop in Instance.Crops)
                    crops.Add(new Component(crop));
                return crops;
            }
        }

       public List<Component> ComponentList
         {
         get {throw new NotImplementedException();}
         set{throw new NotImplementedException();}
         }


  

    }

    /// <summary>
    /// 
    /// </summary>
    //public class MyPaddock
    //{
    //    private static Paddock _Singleton = null;
    //    public Paddock this[String ComponentName]
    //    {
    //        get
    //        {
    //            if (_Singleton == null)
    //                _Singleton = new Paddock(ComponentName, null);
    //            return _Singleton;
    //        }
    //    }
    //}
}
