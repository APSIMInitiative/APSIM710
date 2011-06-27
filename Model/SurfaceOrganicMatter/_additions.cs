using System;
using System.Collections.Generic;
using System.Text;
using System.Xml;

public partial class SurfaceOrganicMatter : Instance
{
    const double acceptableErr = 1e-4;

    #region Residue/Tillage Types

    public class ResiduesType : Instance
    {
        Dictionary<string, ResidueType> residues;
        [Param]
        XmlNode xe = null;

        public override void Initialised()
        {
            base.Initialised();
            residues = new Dictionary<string, ResidueType>();

            foreach (XmlNode xn in xe.ChildNodes)
                if (xn.NodeType == XmlNodeType.Element)
                    residues.Add(xn.Name, new ResidueType(xn, ref residues));
        }

        public ResidueType getResidue(string name)
        {
            return residues.ContainsKey(name) ? residues[name] : null;
        }
    }

    public class ResidueType : BaseType
    {
        public string fom_type { get; private set; }
        public string derived_from { get; private set; }
        public float fraction_C { get; private set; }
        public float po4ppm { get; private set; }
        public float nh4ppm { get; private set; }
        public float no3ppm { get; private set; }
        public float specific_area { get; private set; }
        public int cf_contrib { get; private set; }
        public float pot_decomp_rate { get; private set; }
        public float[] fr_c { get; private set; }
        public float[] fr_n { get; private set; }
        public float[] fr_p { get; private set; }

        public ResidueType(XmlNode xn, ref Dictionary<string, ResidueType> residues)
        {
            XmlNode xn_derived_from = xn.SelectSingleNode("derived_from");

            try
            {
                if (xn_derived_from != null)
                    CloneParent(residues[xn_derived_from.FirstChild.Value]);
            }
            catch (KeyNotFoundException ex)
            {
                throw new Exception("Error attempting to get residue type '" + xn_derived_from.Value + "' to create derived type '" + xn.Name + "'\r\n\tPlease make sure that any new types you have defined are defined BELOW any types they may derive from");
            }

            try
            {
                foreach (XmlNode xnc in xn.ChildNodes)
                    if (xnc.NodeType == XmlNodeType.Element)
                        SetVariable(xnc.Name, xnc.FirstChild.Value);
            }
            catch (FormatException ex)
            {
                throw new Exception("Problem converting value inside Residue Types in SurfaceOM XML: " + ex.Message);
            }



        }

        void CloneParent(ResidueType parent)
        {
            this.fom_type = parent.fom_type;
            this.derived_from = parent.derived_from;
            this.fraction_C = parent.fraction_C;
            this.po4ppm = parent.po4ppm;
            this.nh4ppm = parent.nh4ppm;
            this.no3ppm = parent.no3ppm;
            this.specific_area = parent.specific_area;
            this.cf_contrib = parent.cf_contrib;
            this.pot_decomp_rate = parent.pot_decomp_rate;
            this.fr_c = parent.fr_c;
            this.fr_n = parent.fr_n;
            this.fr_p = parent.fr_p;
        }

        void SetVariable(string name, string value)
        {
            switch (name)
            {
                case "fom_type":
                    fom_type = value;
                    break;
                case "derived_from":
                    derived_from = value;
                    break;
                case "fraction_C":
                    fraction_C = float.Parse(value);
                    break;
                case "po4ppm":
                    po4ppm = float.Parse(value);
                    break;
                case "nh4ppm":
                    nh4ppm = float.Parse(value);
                    break;
                case "no3ppm":
                    no3ppm = float.Parse(value);
                    break;
                case "specific_area":
                    specific_area = float.Parse(value);
                    break;
                case "cf_contrib":
                    cf_contrib = int.Parse(value);
                    break;
                case "pot_decomp_rate":
                    pot_decomp_rate = float.Parse(value);
                    break;
                case "fr_c":
                    fr_c = strToArr(value);
                    break;
                case "fr_n":
                    fr_n = strToArr(value);
                    break;
                case "fr_p":
                    fr_p = strToArr(value);
                    break;
                default:
                    throw new Exception("Problem setting residue types from XML, surplus variable found: " + name);
            }
        }

    }

    public class SOMTillageType : BaseType
    {
        Dictionary<string, float[]> tillage_types;

        public void ReadFromXML(XmlElement xe)
        {
            tillage_types = new Dictionary<string, float[]>();
            foreach (XmlNode xnc in xe.ChildNodes)
                if (xnc.NodeType == XmlNodeType.Element)
                    tillage_types.Add(xnc.Name, strToArr(xnc.FirstChild.Value));

        }

        //public TillageType GetTillageData(string name)
        //{
            //return tillage_types.ContainsKey(name) ? new TillageType() { f_incorp = tillage_types[name][0], tillage_depth = tillage_types[name][1] } : null;
        //}
    }

    public class BaseType
    {
        protected float[] strToArr(string str)
        {
            string[] temp = str.Split(new char[] { ' ', '\t', ',', '\r', '\n' }, StringSplitOptions.RemoveEmptyEntries);
            float[] result = new float[temp.Length];

            for (int i = 0; i < result.Length; i++)
                result[i] = float.Parse(temp[i]);

            return result;
        }
    }
    
    #endregion

    #region Math Operations

    static float bound(float tobound, float lower, float upper)
    {
        return Math.Max(Math.Min(tobound, upper), lower);
    }

    int bound(int tobound, int lower, int upper)
    {
        return Math.Max(Math.Min(tobound, upper), lower);
    }

    static float divide(float numerator, float denominator, float on_denom_is_0)
    {
        return denominator == 0 ? on_denom_is_0 : numerator / denominator;
    }

    /// <summary>
    /// "cover1" and "cover2" are numbers between 0 and 1 which
    ///     indicate what fraction of sunlight is intercepted by the
    ///     foliage of plants.  This function returns a number between
    ///     0 and 1 indicating the fraction of sunlight intercepted
    ///     when "cover1" is combined with "cover2", i.e. both sets of
    ///     plants are present.
    /// </summary>
    /// <param name="cover1"></param>
    /// <param name="cover2"></param>
    /// <returns></returns>
    private float add_cover(float cover1, float cover2)
    {
        float bare = (1 - cover1) * (1 - cover2);
        return 1 - bare;
    }

    private int count_of_real_vals(float[] p, int max_layer)
    {
        throw new NotImplementedException();
    }

    private float l_bound(float val, float bound)
    {
        return Math.Max(val, bound);
    }


    const string apsim_bounds_warning_error =
@"     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                     APSIM  Warning  Error
                     ---------------------
     '{0}' out of bounds!
     {1} < {2} < {3} evaluates 'FALSE'
     Component name: SurfaceOMdotNET
     
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!";

    private void Bound_check_real_var(float value, float lower, float upper, string vname)
    {
        if (value + float.Epsilon < lower || value - float.Epsilon > upper)
            Console.WriteLine(apsim_bounds_warning_error, vname, lower, value, upper);
    }

    private bool reals_are_equal(float first, float second)
    {
        return Math.Abs(first - second) < 2 * float.Epsilon;
    }

    /// <summary>
    /// <para>+ Purpose</para>
    /// <para>
    /// Find the first element of an array where a given value
    /// is contained with the cumulative sum_of of the elements.
    /// If sum_of is not reached by the end of the array, then it
    /// is ok to set it to the last element. This will take
    /// account of the case of the number of levels being 0.
    /// </para>
    /// <para>Definition</para>
    /// <para>
    /// Returns ndx where ndx is the smallest value in the range
    /// 1.."size_of" such that the sum of "array"(j), j=1..ndx is
    /// greater than or equal to "cum_sum".  If there is no such
    /// value of ndx, then "size_of" will be returned.
    /// <para>
    /// <para>Mission Statement</para>
    /// <para>
    /// Find index for cumulative %2 = %1
    /// </para>
    /// </summary>
    /// <param name="cum_sum">sum_of to be found</param>
    /// <param name="array">array to be searched</param>
    /// <param name="size_of">size_of of array</param>
    /// <returns>Index for a 1-BASED ARRAY</returns>
    private int get_cumulative_index_real(float cum_sum, float[] array, int size_of)
    {

        float cum = 0;
        for (int i = 0; i < size_of; i++)
            if ((cum += array[i]) >= cum_sum)
                return i + 1;
        return size_of;
    }

    #endregion
}

