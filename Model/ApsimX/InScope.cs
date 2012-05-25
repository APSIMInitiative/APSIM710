using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Collections;

/// <summary>
/// Provides methods for locating variables or models using either a path string or APSIM
/// scoping rules.
/// Format of paths:  .model.submodel.subsubmodel
///     A leading . indicates an absolute address. e.g. .simulation.paddock1.soilwat
///     No dot indicates a child.                  e.g. leaf 
/// Model scoping rules (links):
///   This class will look at child models (not deeply) and then 
///       recurse up to the parent model. It is not a deep search.
/// Variable scoping rules:
///   This class will firstly search deeply for the variable in all 
///       child models and then all models within an 'Area'. If not found then
///       it will search through all child models of the parent. It won't look inside of other 
///       'Area' components i.e. it respects paddock boundaries and won't look inside of other
///       paddocks.
/// </summary>
class InScope
{
    private static SortedSet<string> AlreadyChecked = new SortedSet<string>();
    public static IEnumerable Inputs(ModelInstance I) { return I.Inputs; }
    public static IEnumerable Outputs(ModelInstance I) { return I.Outputs; }
    public static IEnumerable States(ModelInstance I) { return I.States; }
    public static IEnumerable Params(ModelInstance I) { return I.Params; }

    #region Model searching
    /// <summary>
    /// Go find a specific model instance using the specified NamePath. This name can
    /// have path information. Returns null if not found.
    /// </summary>
    public static ModelInstance FindModel(string NamePath, ModelInstance ModelInstance)
    {
        if (NamePath.Contains("."))
        {
            string[] Paths = NamePath.Split(".".ToCharArray());
            int PathIndex = 0;
            while (PathIndex < Paths.Length)
            {
                string Path = Paths[PathIndex];
                if (Path == "")
                {
                    // Must be a reference to the root node.
                    while (ModelInstance.Parent != null)
                        ModelInstance = ModelInstance.Parent;
                }
                else
                {
                    ModelInstance = ModelInstance.Children.Find(x => string.Equals(x.Name, Path, StringComparison.CurrentCultureIgnoreCase));
                    if (ModelInstance == null)
                        throw new Exception("Cannot find model: " + NamePath);
                }
                PathIndex++;
            }
            return ModelInstance;
        }
        else
            return FindModel(Model => string.Equals(Model.Name, NamePath, StringComparison.CurrentCultureIgnoreCase), ModelInstance);
        
    }

    /// <summary>
    /// Go find a model instance using the specified predicate. Returns null if not found. Uses
    /// scoping rules.
    /// </summary>
    public static ModelInstance FindModel(Predicate<ModelInstance> Pred,
                                          ModelInstance ModelInstance)
    {
        AlreadyChecked.Clear();   
        do
        {
            if (Pred(ModelInstance))
                return ModelInstance;

            // Check children.
            foreach (ModelInstance Instance in ModelInstance.Children)
                if (Pred(Instance))
                    return Instance;

            //ModelInstance Model = FindModelInChildren(Pred, ModelInstance, AlreadyChecked);
            //if (Model != null)
            //    return Model;

            AlreadyChecked.Add(ModelInstance.FullName);

            if (ModelInstance.Type.Name == "Simulation")
                return null; // didn't find the variable.
            else
                ModelInstance = ModelInstance.Parent;
        }
        while (true);
    }

    /// <summary>
    /// Go find a model instance using the specified predicate looking through all children.
    /// Returns null if not found. 
    /// </summary>
    private static ModelInstance FindModelInChildren(Predicate<ModelInstance> Pred,
                                                     ModelInstance ModelInstance,
                                                     SortedSet<string> AlreadyChecked)
    {
        foreach (ModelInstance Instance in ModelInstance.Children)
        {
            if (!AlreadyChecked.Contains(Instance.FullName))
            {
                if (Pred(Instance))
                    return Instance;

                if (Instance.Type.Name != "Area") // don't look inside areas.
                {
                    ModelInstance FoundInstance = FindModelInChildren(Pred, Instance, AlreadyChecked);  // DEPTH FIRST RECUSION
                    if (FoundInstance != null)
                        return FoundInstance;
                }
            }
        }
        return null;
    }
    #endregion


    #region Variable searching

    /// <summary>
    /// Will find a variable using the specified path. If the path has a '.' character then a specific
    /// variable will be found, otherwise variable scoping rules will be used to locate the best match.
    /// null is returned if not found.
    /// </summary>
    /// <returns></returns>
    public static ClassVariable FindVariable(string NamePath, ModelInstance ModelInstance, Func<ModelInstance, IEnumerable> List)
    {
        int PosPeriod = NamePath.LastIndexOf('.');
        if (PosPeriod != -1)
        {
            string ModelName = NamePath.Substring(0, PosPeriod);
            string VariableName = NamePath.Substring(PosPeriod+1);
            ModelInstance = FindModel(ModelName, ModelInstance);
            if (ModelInstance == null)
                return null;
            foreach (ClassVariable Var in List(ModelInstance))
                if (string.Equals(Var.Name, VariableName, StringComparison.CurrentCultureIgnoreCase))
                    return Var;
            return null;
        }
        else
            return FindVariable(Var => string.Equals(Var.Name, NamePath, StringComparison.CurrentCultureIgnoreCase), 
                                ModelInstance, List);
    }

    /// <summary>
    /// Find a variable in scope using the specified predicate. Example usages:
    /// Look for output variable with specified name:
    ///    InScope.FindVariable(Var => Var.Name == NameToFind, this, InScope.Outputs);
    /// Look for state variable with specified type:
    ///    InScope.FindVariable(Var => Var.Type == TypeToFind, this, InScope.States);
    /// </summary>
    public static ClassVariable FindVariable(Predicate<ClassVariable> Pred,
                                             ModelInstance ModelInstance,
                                             Func<ModelInstance, IEnumerable> List)
    {
        AlreadyChecked.Clear();
        do
        {
            ClassVariable Var = FindVariableInChildren(Pred, ModelInstance, List, AlreadyChecked);

            if (Var != null)
                return Var;
            AlreadyChecked.Add(ModelInstance.FullName);

            if (ModelInstance.Type.Name == "Simulation")
                return null; // didn't find the variable.
            else
                ModelInstance = ModelInstance.Parent;
        }
        while (ModelInstance != null);
        return null;
    }

    /// <summary>
    /// Try and find a variable in all child models recursively. Returns the variable if found or
    /// null otherwise.
    /// </summary>
    private static ClassVariable FindVariableInChildren(Predicate<ClassVariable> Pred,
                                                        ModelInstance ModelInstance,
                                                        Func<ModelInstance, IEnumerable> List,
                                                        SortedSet<string> AlreadyChecked)
    {
        foreach (ClassVariable Var in List(ModelInstance))
            if (Pred(Var))
                return Var;

        foreach (ModelInstance Instance in ModelInstance.Children)
        {
            // don't look inside areas.
            if (!AlreadyChecked.Contains(Instance.FullName) && Instance.Type.Name != "Area")
            {
                ClassVariable FoundVariable = FindVariableInChildren(Pred, Instance, List, AlreadyChecked);  // DEPTH FIRST RECUSION
                if (FoundVariable != null)
                    return FoundVariable;
            }
        }

        return null;
    }


    #endregion

}
