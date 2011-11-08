using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CMPServices;
using System.Reflection;
using ModelFramework;
using System.Xml;

public class ModelEnvironment
{

    private Instance In;
    public Dictionary<uint, TComp> ChildComponents = null;

    /// <summary>
    /// Constructor - created by LinkRef
    /// </summary>
    public ModelEnvironment(Instance Obj)
    {
        In = Obj;
    }


    /// <summary>
    /// Query the system for all child components.
    /// </summary>
    protected void QueryChildComponents()
    {
        if (ChildComponents == null)
        {
            string sSearchName = In.ParentComponent().Name + ".*";    //search comp.*

            List<TComp> comps = new List<TComp>();
            In.ParentComponent().Host.queryCompInfo(sSearchName, TypeSpec.KIND_COMPONENT, ref comps);
            ChildComponents = new Dictionary<uint,TComp>();
            for (int i = 0; i < comps.Count; i++)
                ChildComponents.Add(comps[i].compID, comps[i]);
        }
    }

    /// <summary>
    /// A helper function for getting the system name from the specified
    /// fully qualified name passed in.
    /// </summary>
    public static string SystemName(string Name)
    {
        int PosLastPeriod = Name.LastIndexOf('.');
        return Name.Substring(0, PosLastPeriod);
    }

    /// <summary>
    /// Property for returning the name of this instance.
    /// </summary>
    public string Name
    {
        get
        {
            int PosLastPeriod = In.InstanceName.LastIndexOf('.');
            if (PosLastPeriod == -1)
                return In.InstanceName;
            else
                return In.InstanceName.Substring(PosLastPeriod + 1);

        }
    }

    /// <summary>
    /// Property for returning the name of this instance.
    /// </summary>
    public string FullName
    {
        get
        {
            string SystemName = In.ParentComponent().Name;
            SystemName = SystemName.Remove(SystemName.LastIndexOf('.'));  // remove instance name
            return SystemName + "." + In.InstanceName;
        }
    }

    /// <summary>
    /// Returns a list of fully qualified child model names for the specified system path. 
    /// The returned list may be zero length but will never be null.
    /// </summary>
    /// <returns></returns>
    public string[] ChildModelNames()
    {
        return ChildModelNames(In);
    }

    /// <summary>
    /// Returns a list of fully qualified child model names for the specified system path. 
    /// The returned list may be zero length but will never be null.
    /// </summary>
    /// <returns></returns>
    public string[] SystemNames(string SystemPath)
    {
        List<string> ReturnNames = new List<string>();

        string sSearchName = SystemPath + ".*";    //search comp.*

        List<TComp> comps = new List<TComp>();
        In.ParentComponent().Host.queryCompInfo(sSearchName, TypeSpec.KIND_COMPONENT, ref comps);
        ChildComponents = new Dictionary<uint, TComp>();
        for (int i = 0; i < comps.Count; i++)
        {
            if (comps[i].isSystem)
                ReturnNames.Add(comps[i].name);
        }
        return ReturnNames.ToArray();
    }

    /// <summary>
    /// Returns a list of fully qualified child model names for the specified system path. 
    /// The returned list may be zero length but will never be null.
    /// </summary>
    /// <returns></returns>
    public string[] ChildModelNames(string SystemPath)
    {
        Instance Obj = (Instance) LinkField.FindInstanceObject(In, SystemPath, "Instance");
        if (Obj != null)
            return ChildModelNames(Obj);
        else
            return new string[0];
    }

    /// <summary>
    /// Returns a list of fully qualified child model names for the specified system path. 
    /// The returned list may be zero length but will never be null.
    /// </summary>
    /// <returns></returns>
    public static string[] ChildModelNames(object Obj)
    {
        string[] Names;
        if (Obj is Instance)
        {
            Instance I = (Instance)Obj;
            if (I.Children.Count > 0)
            {
                Names = new string[I.Children.Count];
                for (int i = 0; i < I.Children.Count; i++)
                {
                    if (I.Children[i] is Instance)
                    {
                        Instance Child = (Instance)I.Children[i];
                        Names[i] = SystemName(I.ParentComponent().Name) + "." + Child.InstanceName;
                    }
                    else
                        throw new Exception("Invalid child found: " + I.Children[i].Name);
                }
            }
            else
            {
                string sSearchName = I.ParentComponent().Name + ".*";    //search comp.*

                List<TComp> comps = new List<TComp>();
                I.ParentComponent().Host.queryCompInfo(sSearchName, TypeSpec.KIND_COMPONENT, ref comps);
                Names = new string[comps.Count];
                for (int i = 0; i < comps.Count; i++)
                    Names[i] = comps[i].name;
            }
            return Names;
        }
        else
            return new string[0];
    }

    public delegate void NullFunction ();
    public void Subscribe(string EventPath, NullFunction F)
    {
        RuntimeEventHandler.NullFunction Fn = new RuntimeEventHandler.NullFunction(F);
        RuntimeEventHandler Event = new RuntimeEventHandler(EventPath, Fn);
        In.ParentComponent().Subscribe(Event);
    }


    public void Publish(string EventPath, ApsimType Data = null)
    {
        if (Data == null)
            Data = new NullType();
        In.ParentComponent().Publish(EventPath, Data);
    }

    public object ModelByName(string NamePath)
    {
        for (int i = 0; i < In.Children.Count; i++)
        {
            string NameToFind = NamePath.Replace(SystemName(In.ParentComponent().Name) + ".", "");
            if (In.Children[i] is Instance)
            {
                Instance Child = (Instance)In.Children[i];
                if (Child.InstanceName == NameToFind)
                    return Child.Model;
            }
            else
                throw new Exception("Invalid child found: " + In.Children[i].Name);
        }

        return LinkField.FindApsimObject(null, NamePath, SystemName(NamePath), In.ParentComponent());
    }
    public bool Get(string NamePath, out double Data)
    {
        WrapBuiltInVariable<double> Value = new WrapBuiltInVariable<double>();
        if (GetInternal<double>(NamePath, Value))
        {
            Data = Value.Value;
            return true;
        }
        else
        {
            Data = Double.NaN;
            return false;
        }
    }

    public bool Get(string NamePath, out double[] Data)
    {
        WrapBuiltInVariable<double[]> Value = new WrapBuiltInVariable<double[]>();
        if (GetInternal<double[]>(NamePath, Value))
        {
            Data = Value.Value;
            return true;
        }
        else
        {
            Data = null;
            return false;
        }
    }

    public object Get(string NamePath)
    {
        // Try and convert namePath into a relative path.
        NamePath = NamePath.Replace(FullName + ".", "");

        object E = FindInternalEntity(NamePath, In);
        if (E != null)
        {
            if (E is Entity)
                return (E as Entity).Get();
            else if (E is Instance)
                return (E as Instance).Model;
        }
        return E;
    }
    
    /// <summary>
    /// Locate a variable that matches the specified path and return its value. Returns null
    /// if not found. e.g. NamePath:
    ///     "Plant"   - no path specified so look in scope
    ///     "Phenology.CurrentPhase" - relative path specified so look for matching child
    ///     ".met.maxt" - absolute path specified so look for exact variable.
    /// </summary>    
    
    private bool GetInternal<T>(string NamePath, WrapBuiltInVariable<T> Data)
    {
        if (NamePath.Length > 0 && NamePath[0] == '.')
        {
            // absolute path.
            return In.ParentComponent().Get(NamePath, Data, true);

        }
        else if (NamePath.Contains('.'))
        {
            // relative path.
            // assume internal entity.
            object E = FindInternalEntity(NamePath, In);
            if (E != null && E is Entity)
            {
                Data.setValue( (E as Entity).Get());
                return true;
            }
            else
                return false;
        }
        else
        {
            // no path
            // First look for an internal entity.
            object E = FindInternalEntity(NamePath, In);
            if (E != null && E is Entity)
                Data.setValue((E as Entity).Get());
            else
            {
                // not an internal entity so look for an external one.
                return In.ParentComponent().Get(NamePath, Data, true);
            }
        }

        return false;
    }

    /// <summary>
    /// Set the value of a variable.
    /// </summary>
    public bool Set(string NamePath, double Data)
    {
        return SetInternal<double>(NamePath, Data);
    }


    /// <summary>
    /// Set the value of a variable.
    /// </summary>
    private bool SetInternal<T>(string NamePath, T Value)
    {
        WrapBuiltInVariable<T> Data = new WrapBuiltInVariable<T>();
        Data.Value = Value;
        if (NamePath.Length > 0 && NamePath[0] == '.')
        {
            // absolute path.
            In.ParentComponent().Set(NamePath, Data);
            return true;
        }
        else if (NamePath.Contains('.'))
        {
            // relative path.
            object E = FindInternalEntity(NamePath, In);
            if (E != null && E is Entity)
                return (E as Entity).Set(Data);
            else
                return false;
        }
        else
        {
            // no path
            // First look for an internal entity.
            object E = FindInternalEntity(NamePath, In);
            if (E != null && E is Entity)
            {
                Data.setValue( (E as Entity).Get());
                return true;
            }
            else
            {
                // not an internal entity so look for an external one.
                In.ParentComponent().Set(NamePath, Data);
                return true;
            }

        }

    }

    class Entity
    {
        public MemberInfo MI;
        public object Obj;

        public object Get()
        {
            if (MI is FieldInfo)
            {
                FieldInfo FI = MI as FieldInfo;
                return  FI.GetValue(Obj);
            }
            else
            {
                PropertyInfo PI = MI as PropertyInfo;
                return PI.GetValue(Obj, null);
            }



        }
        public bool Set(object Value)
        {
            if (MI is FieldInfo)
            {
                FieldInfo FI = MI as FieldInfo;
                FI.SetValue(Obj, Value);
            }
            else
            {
                PropertyInfo PI = MI as PropertyInfo;
                PI.SetValue(Obj, Value, null);
            }
            return true;
        }
    }


    /// <summary>
    /// Return the value (using Reflection) of the specified property on the specified object.
    /// Returns null if not found.
    /// </summary>
    private static object FindInternalEntity(string NamePath, Instance RelativeTo)
    {
        object Value = null;
        do
        {
            Value = FindChildEntity(NamePath, RelativeTo);
            RelativeTo = RelativeTo.Parent;
        }
        while (Value == null && RelativeTo != null);
        return Value;
    }

    /// <summary>
    /// Return the value (using Reflection) of the specified property on the specified object.
    /// Returns null if not found.
    /// </summary>
    private static object FindChildEntity(string NamePath, Instance RelativeTo)
    {
        string[] Bits = NamePath.Split(".".ToCharArray(), StringSplitOptions.RemoveEmptyEntries);
        for (int i = 0; i < Bits.Length; i++)
        {
            Instance MatchingChild = null;
            for (int j = 0; j < RelativeTo.Children.Count; j++)
            {
                if (RelativeTo.Children[j] is Instance)
                {
                    Instance Child = (Instance)RelativeTo.Children[j];
                    if (Child.Name.ToLower() == Bits[i].ToLower())
                    {
                        MatchingChild = Child;
                        break;
                    }
                }
                else
                    throw new Exception("Invalid child found: " + RelativeTo.Children[j].Name);
            }

            if (MatchingChild == null)
            {
                // If we get this far then we didn't find the child. If it's the last bit of the name path
                // then perhaps it is a field or property - go have a look.
                if (i == Bits.Length - 1)
                {
                    FieldInfo FI = RelativeTo.Model.GetType().GetField(Bits[i], BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
                    if (FI == null)
                    {
                        PropertyInfo PI = RelativeTo.Model.GetType().GetProperty(Bits[i], BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
                        if (PI != null)
                            return new Entity { MI = PI, Obj = RelativeTo.Model };
                    }
                    else
                        return new Entity { MI = FI, Obj = RelativeTo.Model };
                }
                return null;
            }
            else
                RelativeTo = MatchingChild;
        }

        // If we get this far then we've found a match.
        return RelativeTo;
    }

    /// <summary>
    /// Add a new model to the simulation. The ModelDescription describes the parameterisation of
    /// the model. The ModelAssembly contains the model.
    /// </summary>
    public void AddModel(XmlNode ModelDescription, Assembly ModelAssembly)
    {
        In.ParentComponent().BuildObjects(ModelDescription, ModelAssembly);
    }

}
