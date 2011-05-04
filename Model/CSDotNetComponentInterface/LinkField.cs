using System;
using System.Collections.Generic;
using System.Text;
using System.Reflection;
using System.Collections.Specialized;

public class LinkField
{
    private FieldInfo Field;
    private Link LinkAttr;
    private Instance In;
    ApsimComponent Comp;
    private static StringBuilder Data = new StringBuilder(10000);
    /// <summary>
    ///  Go find a component IN SCOPE that matches the specified type.
    /// IN SCOPE means a component that is a sibling or in the paddock
    /// above.
    /// </summary>
    /// <param name="TypeToFind"></param>
    /// <param name="OurName"></param>
    /// <returns></returns>
    private String FindComponentByType(String TypeToFind, String OurName)
    {
        String PaddockName = OurName.Substring(0, OurName.LastIndexOf('.'));

        // Get a list of all paddock children.
        ////////getChildren(PaddockName, Data);
        StringCollection ChildNames = CSGeneral.StringManip.SplitStringHonouringQuotes(Data.ToString(), ",");

        // Go through all children and find a component that has the specified type.
        foreach (String ChildName in ChildNames)
        {
            String ChildNameNoQuotes = ChildName.Replace("\"", "");
            ///////getComponentType(ChildNameNoQuotes, Data);
            String ChildType = Data.ToString();

            if (ChildType.ToLower() == TypeToFind.ToLower())
                return ChildNameNoQuotes;
        }

        // If we get this far then we need to go to our parent and search it's children.
        int PosLastPeriod = OurName.LastIndexOf('.');
        if (PosLastPeriod == -1)
            return "";
        String ParentName = OurName.Substring(0, PosLastPeriod);
        if (ParentName == ".MasterPM")
            return "";
        return FindComponentByType(TypeToFind, ParentName);

    }
    private String FindComponentByName(String NameToFind, String OurName)
    {
        String Delimiter = ".";

        if (NameToFind.Contains(".MasterPM."))
            return NameToFind;   // absolute reference.
        else
        {
            // relative reference.

            String ParentName = "";
            int PosLastPeriod = OurName.LastIndexOf('.');
            if (PosLastPeriod == -1)
                throw new Exception("Invalid component name found: " + OurName);
            ParentName = OurName.Substring(0, PosLastPeriod);
            return ParentName + "." + NameToFind;
        }
    }
    public LinkField(Instance _In, FieldInfo _Field, Link _LinkAttr)
    {
        In = _In;
        Field = _Field;
        LinkAttr = _LinkAttr;
        Comp = In.ParentComponent();
    }
    /// <summary>
    /// Creates any classes that are of type [Link] in the model
    /// </summary>
    public void Resolve()
    {
        {
            if (Field.GetValue(In) == null)
            {
                // Work out our name and the name of our containing paddock.
                String OurName = Comp.GetName();

                // Load in the probe info assembly.
                Assembly ProbeInfo = Types.GetProbeInfoAssembly();

                bool TypeIsInCSDotNetComponentInterface = false;
                String FQN = null;
                if (LinkAttr._Path == null)
                {
                    String TypeToFind = Field.FieldType.Name;

                    // If the type to find is "Paddock" then don't go looking for children.
                    if (TypeToFind == "Paddock")
                    {
                        String PaddockName = OurName.Substring(0, OurName.LastIndexOf('.'));
                        FQN = PaddockName;
                        TypeIsInCSDotNetComponentInterface = true;
                    }
                    else if (TypeToFind == "Component")
                    {
                        FQN = OurName;
                        TypeIsInCSDotNetComponentInterface = true;
                    }

                    else
                    {
                        FQN = FindComponentByType(TypeToFind, OurName);
                        if (FQN == "")
                        {
                            if (LinkAttr._IsOptional == IsOptional.Yes)
                                return;
                            throw new Exception("Cannot find [Link] for type: " + TypeToFind);
                        }
                    }
                }
                else
                {
                    // The path must be a name.
                    FQN = FindComponentByName(LinkAttr._Path, OurName);
                }

                // Now go create a proxy object
                String ProxyTypeString = Field.FieldType.Name;
                Type ProxyType;
                if (TypeIsInCSDotNetComponentInterface)
                    ProxyType = Assembly.GetExecutingAssembly().GetType(ProxyTypeString);
                else
                    ProxyType = Types.GetProbeInfoAssembly().GetType("ModelFramework." + ProxyTypeString);

                if (ProxyType == null)
                    throw new Exception("Cannot find proxy reference: " + ProxyTypeString);
                Object[] Parameters = new Object[2];
                Parameters[0] = FQN;
                Parameters[1] = Comp;
                Object ReferencedObject = Activator.CreateInstance(ProxyType, Parameters);

                // Set the value of the [Link] field to the newly created proxy object.
                Field.SetValue(In, ReferencedObject);
            }

        }
    }
}

