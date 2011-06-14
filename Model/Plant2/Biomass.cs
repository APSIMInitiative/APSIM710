using System;
using System.Collections.Generic;
using System.Text;
using CSGeneral;

public class Biomass : Instance
{
    private double _StructuralWt = 0;
    private double _NonStructuralWt = 0;
    private double _StructuralN = 0;
    private double _NonStructuralN = 0;
    private double _PotentialDMAllocation = 0;
    private double _MetabolicWt = 0;
    private double _MetabolicN = 0;

    [Output]
    [Units("g/m^2")]
    virtual public double NonStructuralN
    {
        get { return _NonStructuralN; }
        set
        {
            _NonStructuralN = MathUtility.RoundToZero(value);
            
        }
    }
    [Output]
    [Units("g/m^2")]
    virtual public double StructuralN
    {
        get { return _StructuralN; }
        set
        {
            _StructuralN = MathUtility.RoundToZero(value);
        }
    }
    [Output]
    [Units("g/m^2")]
    virtual public double NonStructuralWt
    {
        get { return _NonStructuralWt; }
        set
        {
            _NonStructuralWt = MathUtility.RoundToZero(value);
        }
    }
    [Output]
    [Units("g/m^2")]
    virtual public double StructuralWt
    {
        get { return _StructuralWt; }
        set
        {
            _StructuralWt = MathUtility.RoundToZero(value);
        }
    }
    [Output]
    [Units("g/m^2")]
    public double PotentialDMAllocation
    {
        get { return _PotentialDMAllocation; }
        set
        {
            _PotentialDMAllocation = MathUtility.RoundToZero(value);
        }
    } //FIXME  This was only added because it was the only way I could get potential DM allocation values into a root layer array.  need to pull back to the root module
    [Output]
    [Units("g/m^2")]
    virtual public double MetabolicWt
    {
        get { return _MetabolicWt; }
        set
        {
            _MetabolicWt = MathUtility.RoundToZero(value);
        }
    }
    [Output]
    [Units("g/m^2")]
    virtual public double MetabolicN
    {
        get { return _MetabolicN; }
        set
        {
            _MetabolicN = MathUtility.RoundToZero(value);
        }
    }
    [Output]
    [Units("g/m^2")]
    public double Wt
    {
        get
        {
            return StructuralWt + NonStructuralWt + MetabolicWt;
        }
    }
    [Output]
    [Units("g/m^2")]
    public double N
    {
        get
        {
            return StructuralN + NonStructuralN + MetabolicN;
        }
    }
    [Output]
    [Units("g/g")]
    public double NConc
    {
        get
        {
            double wt = (StructuralWt + NonStructuralWt + MetabolicWt);
            double n = (StructuralN + NonStructuralN + MetabolicN);
            if (wt > 0)
                return n / wt;
            else
                return 0.0;
        }
    }
    [Output]
    [Units("g/g")]
    public double StructuralNConc
    {
        get
        {
            if (StructuralWt > 0)
                return StructuralN / StructuralWt;
            else
                return 0.0;
        }
    }
    [Output]
    [Units("g/g")]
    public double NonStructuralNConc
    {
        get
        {
            if (NonStructuralWt > 0)
                return NonStructuralN / NonStructuralWt;
            else
                return 0.0;
        }
    }
    [Output]
    [Units("g/g")]
    public double MetabolicNConc
    {
        get
        {
            if (MetabolicWt > 0)
                return MetabolicN / MetabolicWt;
            else
                return 0.0;
        }
    }
    virtual public void Clear()
    {
        StructuralWt = 0;
        NonStructuralWt = 0;
        MetabolicWt = 0;
    }

}
   
