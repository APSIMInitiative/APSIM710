﻿using System;
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
        protected static Dictionary<String, List<String>> VarParts;
        
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
            {
                My = RelativeTo as Component;
                if ((My != null) && !NamePath.Contains("["))
                {
                    object v;
                    My.GetObject(NamePath, out v);
                    return v;
                }
            }

            if (VarParts == null)
                VarParts = new Dictionary<string, List<string>>();

            string[] Bits;
            //cache the list of names that are parsed here
            List<String> Parts;
            if (VarParts.TryGetValue(NamePath, out Parts))
                Bits = Parts.ToArray();
            else
            {
                Bits = StringManip.SplitStringHonouringBrackets(NamePath, '.', '[', ']');
                VarParts.Add(NamePath, Bits.ToList());
            }

            for (int i = 0; i < Bits.Length; i++)
            {
                string ArraySpecifier = "";
                bool ArrayFound = Bits[i].Contains("[");
                if (ArrayFound)
                    ArraySpecifier = StringManip.SplitOffBracketedValue(ref Bits[i], '[', ']');

                object MatchingChild;
                if (RelativeTo is Component)
                {
                    MatchingChild = (RelativeTo as Component).LinkByName(Bits[i]);  // Try for a model name first. e.g. Root
                    if (MatchingChild == null)
                        (RelativeTo as Component).GetObject(Bits[i], out MatchingChild);  // may be a variable. e.g. Organs
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
                        ok = My.GetObject(ArraySpecifier, out ArraySpec);  // Assume it is a simulation variable.
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

        public static void ZeroArray(double[] Arr)
        {
            for (int i = 0; i < Arr.Length; i++)
                Arr[i] = 0;
        }
        /// <summary>
        /// Find the first element of an array where a given value
        /// is contained with the cumulative sum_of of the elements.
        /// If sum_of is not reached by the end of the array, then it
        /// is ok to set it to the last element. This will take
        /// account of the case of the number of levels being 0.
        /// </summary>
        public static int GetCumulativeIndex(double cum_sum, double[] array, int NumElements)
        {
            double progressive_sum = 0.0;
            int indx;

            for (indx = 0; indx < NumElements; indx++)
            {
                progressive_sum = progressive_sum + array[indx];
                if (progressive_sum >= cum_sum)
                    break;
            }
            if (indx == NumElements)
                return (indx - 1); // last element in array
            return indx;
        }

        /// <summary>
        /// Accumulates a value in an array, at the specified index.
        /// If the increment in index value changes to a new index, the value
        /// is distributed proportionately between the two indices of the array.
        /// </summary>
        /// <param name="value">value to add to array</param>
        /// <param name="array">array to split</param>
        /// <param name="p_index">current p_index no</param>
        /// <param name="dlt_index">increment in p_index no</param>
        public static void Accumulate(double value, double[] array, double p_index, double dlt_index)
        {
            double fract_in_old;           // fraction of value in last index
            int new_index;                 // number of index just starting ()
            double portion_in_new;         // portion of value in next index
            double portion_in_old;         // portion of value in last index

            int current_index = (int)Math.Truncate(p_index);

            // make sure the index is something we can work with
            if (current_index >= 0)
            {
                // fraction_of of current index elapsed ()
                double index_devel = p_index - Math.Truncate(p_index) + dlt_index;
                if (index_devel >= 1.0)
                {
                    // now we need to divvy
                    new_index = (int)(p_index + Math.Min(1.0, dlt_index));
                    if (MathUtility.FloatsAreEqual(Math.IEEERemainder(p_index, 1.0), 0.0))
                    {
                        fract_in_old = 1.0 - MathUtility.Divide(index_devel - 1.0, dlt_index, 0.0);
                        portion_in_old = fract_in_old * (value + array[current_index]) -
                                             array[current_index];
                    }
                    else
                    {
                        fract_in_old = 1.0f - MathUtility.Divide(index_devel - 1.0, dlt_index, 0.0);
                        portion_in_old = fract_in_old * value;
                    }
                    portion_in_new = value - portion_in_old;
                    array[current_index] = array[current_index] + portion_in_old;
                    array[new_index] = array[new_index] + portion_in_new;
                }
                else
                {
                    array[current_index] = array[current_index] + value;
                }
            }

            else
                throw new Exception("Accumulate index < 0!!");
        }

        public static int IncreaseSizeOfBiomassRemoved(BiomassRemovedType BiomassRemoved)
        {
            // Make sure the BiomassRemoved structure has enough elements in it.
            if (BiomassRemoved.dm_type == null)
            {
                BiomassRemoved.dm_type = new string[1];
                BiomassRemoved.fraction_to_residue = new float[1];
                BiomassRemoved.dlt_crop_dm = new float[1];
                BiomassRemoved.dlt_dm_n = new float[1];
                BiomassRemoved.dlt_dm_p = new float[1];
            }
            else
            {
                int NewSize = BiomassRemoved.dm_type.Length + 1;
                Array.Resize(ref BiomassRemoved.dm_type, NewSize);
                Array.Resize(ref BiomassRemoved.fraction_to_residue, NewSize);
                Array.Resize(ref BiomassRemoved.dlt_crop_dm, NewSize);
                Array.Resize(ref BiomassRemoved.dlt_dm_n, NewSize);
                Array.Resize(ref BiomassRemoved.dlt_dm_p, NewSize);
            }
            return BiomassRemoved.dm_type.Length - 1;
        }

        public static void CalcNDemand(double dltDm, double dltDmPotRue, double n_conc_crit, double n_conc_max,
                                         Biomass Growth, Biomass Green, double RetranslocationN,
                                         double n_deficit_uptake_fraction,
                                         ref double NDemand, ref double NMax)
        {
            double part_fract = MathUtility.Divide(Growth.Wt, dltDm, 0.0);
            double dlt_dm_pot = dltDmPotRue * part_fract;         // potential dry weight increase (g/m^2)
            dlt_dm_pot = MathUtility.Constrain(dlt_dm_pot, 0.0, dltDmPotRue);

            if (Green.Wt > 0.0)
            {
                // get N demands due to difference between
                // actual N concentrations and critical N concentrations
                double N_crit = Green.Wt * n_conc_crit;         // critical N amount (g/m^2)
                double N_potential = Green.Wt * n_conc_max;     // maximum N uptake potential (g/m^2)

                // retranslocation is -ve for outflows
                double N_demand_old = N_crit                       // demand for N by old biomass (g/m^2)
                                   - (Green.N + RetranslocationN);
                if (N_demand_old > 0.0)                             // Don't allow demand to satisfy all deficit
                    N_demand_old *= n_deficit_uptake_fraction;

                double N_max_old = N_potential                  // N required by old biomass to reach  N_conc_max  (g/m^2)
                                   - (Green.N + RetranslocationN);
                if (N_max_old > 0.0)
                    N_max_old *= n_deficit_uptake_fraction;         // Don't allow demand to satisfy all deficit

                // get potential N demand (critical N) of potential growth
                double N_demand_new = dlt_dm_pot * n_conc_crit;     // demand for N by new growth (g/m^2)
                double N_max_new = dlt_dm_pot * n_conc_max;      // N required by new growth to reach N_conc_max  (g/m^2)

                NDemand = N_demand_old + N_demand_new;
                NMax = N_max_old + N_max_new;

                NDemand = MathUtility.Constrain(NDemand, 0.0, double.MaxValue);
                NMax = MathUtility.Constrain(NMax, 0.0, double.MaxValue);
            }
            else
                NDemand = NMax = 0.0;
        }

        public static AvailableToAnimalelementType[] AvailableToAnimal(string PlantName, string OrganName, double PlantHeight,
                                                                       Biomass Live, Biomass Dead)
        {
            AvailableToAnimalelementType[] Available = new AvailableToAnimalelementType[2];
            Available[0] = new AvailableToAnimalelementType();
            Available[0].CohortID = PlantName;
            Available[0].Organ = OrganName;
            Available[0].AgeID = "green";
            Available[0].Bottom = 0.0;
            Available[0].Top = PlantHeight;
            Available[0].Chem = "digestible";
            Available[0].Weight = Live.Wt * Conversions.gm2kg / Conversions.sm2ha;
            Available[0].N = Live.N * Conversions.gm2kg / Conversions.sm2ha;
            Available[0].P = 0.0; //Live.P * Conversions.gm2kg / Conversions.sm2ha;
            Available[0].S = 0.0;
            Available[0].AshAlk = 0.0;

            Available[1] = new AvailableToAnimalelementType();
            Available[1].CohortID = PlantName;
            Available[1].Organ = OrganName;
            Available[1].AgeID = "senesced";
            Available[1].Bottom = 0.0;
            Available[1].Top = PlantHeight;
            Available[1].Chem = "digestible";
            Available[1].Weight = Dead.Wt * Conversions.gm2kg / Conversions.sm2ha;
            Available[1].N = Dead.N * Conversions.gm2kg / Conversions.sm2ha;
            Available[1].P = 0.0; //Dead.P * Conversions.gm2kg / Conversions.sm2ha;
            Available[1].S = 0.0;
            Available[1].AshAlk = 0.0;
            return Available;
        }

        /// <summary>
        /// Remove some dm. The fraction removed is calculation and put into the RemovedPool so that
        /// later one it can be removed from the actual Pool in the update routine.
        /// Routine was called giveDMGreenRemoved and giveDMSenescedRemoved in old Plant.
        /// </summary>
        internal static Biomass RemoveDM(double delta, Biomass Pool, string OrganName)
        {
            double fraction = MathUtility.Divide(delta, Pool.Wt, 0.0);
            Biomass RemovedPool = Pool * fraction;

            double error_margin = 1.0e-6f;
            if (delta > Pool.Wt + error_margin)
            {
                string msg;
                msg = "Attempting to remove more green " + OrganName + " biomass than available:-\r\n";
                msg += "Removing -" + delta.ToString() + " (g/m2) from " + Pool.Wt.ToString() + " (g/m2) available.";
                throw new Exception(msg);
            }
            return RemovedPool;
        }
    }