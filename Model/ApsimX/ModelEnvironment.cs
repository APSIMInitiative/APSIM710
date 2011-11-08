using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;
using System.Reflection;

public interface ModelAPIInterface
{
    object Get(string Name);
    void Subscribe(string Name, NullTypeDelegate f);
    void Publish(string Name, object Data);
}


public class ModelEnvironment : ModelAPIInterface
{
    private ModelInstance Instance;
    internal ModelEnvironment(ModelInstance Inst) { Instance = Inst; }
    Dictionary<string, VariableBase> Gets = new Dictionary<string, VariableBase>();

    public string Name
    {
        get
        {
            throw new NotImplementedException();
        }
    }

    public string FullName
    {
        get
        {
            throw new NotImplementedException();
        }
    }

    /// <summary>
    /// A helper function for getting the system name from the specified
    /// fully qualified name passed in.
    /// </summary>
    public static string SystemName(string Name)
    {
        throw new NotImplementedException();
    }

    public string[] ChildModelNames()
    {
        throw new NotImplementedException();
    }

    /// <summary>
    /// Returns a list of fully qualified child model names for the specified system path. 
    /// The returned list may be zero length but will never be null.
    /// </summary>
    /// <returns></returns>
    public string[] SystemNames(string SystemPath)
    {
        throw new NotImplementedException();
    }

    public object ModelByName(string NamePath)
    {
        throw new NotImplementedException();
    }
    public bool Get(string Name, out double Value)
    {
        throw new NotImplementedException();
    }
    public bool Get(string Name, out double[] Value)
    {
        throw new NotImplementedException();
    }

    public object Get(string Name)
    {
        VariableBase V = null;
        if (Gets.TryGetValue(Name, out V))
            return V.Value;
        else
        {
            if (Name.Contains("."))
            {
                string ModelName = Name.Substring(0, Name.LastIndexOf('.'));
                string VariableName = Name.Substring(Name.LastIndexOf('.') + 1);
                ModelInstance Inst = Instance.FindModelInstance(ModelName);
                if (Inst == null)
                    throw new Exception("Cannot find a model called " + ModelName + " while trying to do a get for variable " + VariableName);

                // See if the instance has the output we want.
                foreach (VariableBase Output in Inst.Outputs)
                {
                    if (Output.Name.ToLower() == VariableName.ToLower())
                    {
                        V = Output;
                        break;
                    }
                }
            }
            else
            {
                V = Instance.Root.FindOutput(Name);
            }
            if (V == null)
                throw new Exception("Cannot find a value for the variable " + Name);
            Gets.Add(Name, V);
            return V.Value;
        }
    }

    /// <summary>
    /// Set the value of a variable.
    /// </summary>
    public bool Set(string NamePath, double Data)
    {
        throw new NotImplementedException();
    }
    public void Subscribe(string Name, NullTypeDelegate f)
    {
        throw new NotImplementedException();
    }

    public void Publish(string Name, object Data = null)
    {
        throw new NotImplementedException();
    }

    public void AddModel(XmlNode ModelDescription, Assembly ModelAssembly)
    {
        throw new NotImplementedException();
    }
}
