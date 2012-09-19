using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;
using System.Diagnostics;
using CSGeneral;
using System.Collections;
using System.Reflection;
using ModelFramework;

    class Util
    {
        public static StreamWriter Dbg = null;
        
        [Conditional("DEBUG")]
        public static void Debug(string format, object value)
        {
            if (Dbg == null)
                Dbg = new StreamWriter("plant2.debug");
            format = format.Replace("%f0", "{0:0}");
            format = format.Replace("%f2", "{0:0.00}");
            format = format.Replace("%f", "{0:0.000}");
            format = format.Replace("%i", "{0:0}");
            format = format.Replace("%s", "{0}");
            Dbg.WriteLine(string.Format(format, value));
        }

        [Conditional("DEBUG")]
        public static void DebugArray(string format, double[] value, int NumElements)
        {
            for (int i = 0; i < NumElements; i++)
                Debug(format, value[i]);
        }

        /// <summary>
        /// Return the value (using Reflection) of the specified property on the specified object.
        /// Returns null if not found. Examples of Names that can be found.
        ///     Pod
        ///     Environment.MeanT
        ///     Organs[]
        ///     Organs[AboveGround].Live
        ///     Organs[AboveGround].Live.Wt
        ///     Leaf.Leaves[Leaf.CurrentRank].CoverAbove
        ///  Can return an Instance, an Entity or an object[] when an array specifier is present.
        /// </summary>
        public static object GetVariable(string NamePath, object RelativeTo)
        {
            Component My = null;
            if (RelativeTo is Component)
                My = RelativeTo as Component;

            if (!NamePath.Contains("[") && My != null)
            {
                object v;
                My.Get(NamePath, out v);
                return v;
            }

            string[] Bits = StringManip.SplitStringHonouringBrackets(NamePath, '.', '[', ']');
            for (int i = 0; i < Bits.Length; i++)
            {
                bool ArrayFound = Bits[i].Contains("[");
                string ArraySpecifier = StringManip.SplitOffBracketedValue(ref Bits[i], '[', ']');

                object MatchingChild;
                if (RelativeTo is Component)
                {
                    MatchingChild = (RelativeTo as Component).LinkByName(Bits[i]);  // Try for a model name first. e.g. Root
                    if (MatchingChild == null)
                        (RelativeTo as Component).Get(Bits[i], out MatchingChild);  // may be a variable. e.g. Organs
                }
                else
                    MatchingChild = Utility.GetValueOfFieldOrProperty(Bits[i], RelativeTo);

                if (MatchingChild == null)
                    throw new Exception("Cannot find variable: " + NamePath);

                // Look for array spec
                if (ArrayFound)
                {
                    if (!(MatchingChild is IList))
                        throw new Exception("Cannot specify an array on a non array variable. Name: " + NamePath);
                    IList Array = MatchingChild as IList;

                    // First try and treat the ArraySpecifier as an integer index.
                    // If that's not possible, then assume it is a reference to an integer variable
                    // somewhere in the system.
                    // If that's not possible then assume it is a type name e.g. AboveGround.
                    int ArrayIndex;

                    bool ok = int.TryParse(ArraySpecifier, out ArrayIndex);

                    if (ArraySpecifier != "" && !ok && My != null)
                    {
                        object ArraySpec;
                        ok = My.Get(ArraySpecifier, out ArraySpec);  // Assume it is a simulation variable.
                        if (ok && (ArraySpec is Int32 || ArraySpec is Double))
                            ArrayIndex = Convert.ToInt32(ArraySpec);
                        else
                            ok = false;
                    }

                    if (ok)
                    {
                        if (ArrayIndex < 0 || ArrayIndex >= Array.Count)
                            throw new Exception("Invalid index of " + ArrayIndex.ToString() + " found while indexing into variable: " + NamePath);
                        MatchingChild = Array[ArrayIndex];
                    }
                    else
                    {
                        // Must be a type name. Go collect an array of objects of that type.
                        List<object> ArrayOfType = new List<object>();

                        // Construct a name remainder.
                        string RestOfName = null;
                        for (int j = i + 1; j < Bits.Length; j++)
                        {
                            if (RestOfName != null)
                                RestOfName += ".";
                            RestOfName += Bits[j];
                        }

                        foreach (object o in Array)
                            if (ArraySpecifier == "" || Utility.IsOfType(o.GetType(), ArraySpecifier))
                            {
                                if (RestOfName == null)
                                    ArrayOfType.Add(o);
                                else
                                {
                                    object ChildObject = GetVariable(RestOfName, o);  // recursion
                                    if (ChildObject != null)
                                        ArrayOfType.Add(ChildObject);
                                }
                            }
                        return ArrayOfType.ToArray();
                    }
                }

                RelativeTo = MatchingChild;
            }

            // If we get this far then we've found a match.
            return RelativeTo;
        }        
        

    }
