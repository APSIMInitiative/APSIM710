using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CMPServices;
using System.Reflection;

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
    private static string SystemName(string Name)
    {
        int PosLastPeriod = Name.LastIndexOf('.');
        if (PosLastPeriod == -1)
            return null;
        else
            return Name.Substring(0, PosLastPeriod);
    }

    /// <summary>
    /// Property for returning the name of this instance.
    /// </summary>
    public string Name
    {
        get { return In.Name; }
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
    /// <summary>
    /// Locate a variable that matches the specified path and return its value. Returns null
    /// if not found. e.g. NamePath:
    ///     "Plant"   - no path specified so look in scope
    ///     "Phenology.CurrentPhase" - relative path specified so look for matching child
    ///     ".met.maxt" - absolute path specified so look for exact variable.
    /// </summary>    
    public object Get(string NamePath)
    {
        if (NamePath.Length > 0 && NamePath[0] == '.')
        {
            // absolute path.
            WrapBuiltIn Data = new WrapBuiltIn();
            if (In.ParentComponent().Get(NamePath, Data, true))
                return Data.Value;

        }
        else if (NamePath.Contains('.'))
        {
            // relative path.
            return FindInternalEntity(NamePath, In);
        }
        else
        {
            // no path
            // first see if it is a relative path.
            object Value = FindInternalEntity(NamePath, In);
            if (Value != null)
                return Value;
            else
            {
                // not a relative path so go look for it.
            }
        }

        return null;      
    }


    /// <summary>
    /// Return the value (using Reflection) of the specified property on the specified object.
    /// Returns null if not found.
    /// </summary>
    private static object FindInternalEntity(string NamePath, Instance RelativeTo)
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
                    object v = null;
                    if (FI == null)
                    {
                        PropertyInfo PI = RelativeTo.Model.GetType().GetProperty(Bits[i], BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Public);
                        if (PI != null)
                            v = PI.GetValue(RelativeTo.Model, null);
                    }
                    else
                        v = FI.GetValue(RelativeTo.Model);
                    return v;
                }
                else
                    return null;
            }
            else
                RelativeTo = MatchingChild;
        }
        return null;
    }


}


// --------------------------------------------------------------------
// This class wraps a FactoryProperty and a built in type (e.g. Single, 
// Double etc). It then makes it look like an ApsimType with pack,
// unpack methods etc.
// --------------------------------------------------------------------
public class WrapBuiltIn : TypeInterpreter, ApsimType
{
    /*This class is a quick way to wrap the TTypedValue into a generic class
        * that handles scalars and arrays of scalars - NH
        I think this class should be superceded by TypeInterpreter or TDDMLValue
        */
    protected Type tType;
    public object Value;
    //protected override TDDMLValue DDMLValue; */
    public WrapBuiltIn()
    {
    }
    public override void pack(out byte[] messageData)
    {
        throw new NotImplementedException();
    }
    public override void unpack(byte[] messageData)
    {
        //::unpackWithConverter(messageData, Value);
        DDMLValue.setData(messageData, messageData.Length, 0);
       /* if (DDMLValue.typeName() == typeof(Boolean))
        {
            Value = Convert.ChangeType(DDMLValue.asBool(), typeof(bool));
        }
        else if (tType == typeof(Int32))
        {
            Value = Convert.ChangeType(DDMLValue.asInt(), typeof(int));
        }
        else if (tType == typeof(Single))
        {
            Value = Convert.ChangeType(DDMLValue.asSingle(), typeof(Single));
        }
        else if (tType == typeof(double))
        {
            Value = Convert.ChangeType(DDMLValue.asDouble(), typeof(double));
        }
        else if (tType == typeof(String))
        {
            Value = Convert.ChangeType(DDMLValue.asStr(), typeof(String));
        }
        else if (tType == typeof(Boolean[]))
        {
            Value = Convert.ChangeType(DDMLValue.asBooleanArray(), typeof(Boolean[]));
        }
        else if (tType == typeof(Int32[]))
        {
            Value = Convert.ChangeType(DDMLValue.asIntArray(), typeof(Int32[]));
        }
        else if (tType == typeof(Single[]))
        {
            Value = Convert.ChangeType(DDMLValue.asSingleArray(), typeof(Single[]));
        }
        else if (tType == typeof(double[]))
        {
            Value = Convert.ChangeType(DDMLValue.asDoubleArray(), typeof(double[]));
        }
        else if (tType == typeof(String[]))
        {
            Value = Convert.ChangeType(DDMLValue.asStringArray(), typeof(String[]));
        }
        else if (tType == typeof(DateTime))
        {
            double JulianDate = DDMLValue.asDouble();               //stored as a double
            DateTime jDate = TTimeValue.JDToDateTime(JulianDate);
            Value = Convert.ChangeType(jDate, typeof(DateTime));
        }*/
    }
    public override uint memorySize()
    {
        throw new NotImplementedException();
    }
    public override String DDML()
    {
        return DDML(Value);
    }

    public String DDML(object Value)
    {
        throw new NotImplementedException();
    }
}







