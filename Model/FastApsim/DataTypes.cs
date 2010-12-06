using System;
using System.Collections.Generic;
using System.Text;

public delegate void NullTypeDelegate();
public class NullType
   {
   }
//------ ManagerEventKey ------
public class ManagerEventKeyType
   {
   public string Name;
   public string Units;
   public string[] Values;
   };

//------ ManagerEvent ------
public class ManagerEventType
   {
   public ManagerEventKeyType[] Key;
   };
public delegate void ManagerEventDelegate(ManagerEventKeyType Data);


//------ NewMet ------
public class NewMetType
   {
   public double today;
   public float radn;
   public float maxt;
   public float mint;
   public float rain;
   public float vp;
   };
public delegate void NewMetDelegate(NewMetType Data);

public class PhenologyChangedType
   {
   public string OldPhaseName;
   public string NewPhaseName;
   };
public delegate void PhenologyChangedDelegate(PhenologyChangedType Data);

//------ NewPotentialGrowth ------
public class NewPotentialGrowthType
   {
   public string sender;
   public float frgr;
   };
public delegate void NewPotentialGrowthDelegate(NewPotentialGrowthType Data);

//------ NewCanopy ------
public class NewCanopyType
   {
   public string sender;
   public float height;
   public float depth;
   public float lai;
   public float lai_tot;
   public float cover;
   public float cover_tot;
   };
public delegate void NewCanopyDelegate(NewCanopyType Data);

//------ NewCrop ------
public class NewCropType
   {
   public string sender;
   public string crop_type;
   };
public delegate void NewCropDelegate(NewCropType Data);

//------ CanopyWaterBalanceCanopy ------
public class CanopyWaterBalanceCanopyType
   {
   public string name;
   public string CropType;
   public float PotentialEp;
   };
public delegate void CanopyWaterBalanceCanopyDelegate(CanopyWaterBalanceCanopyType Data);

//------ CanopyWaterBalance ------
public class CanopyWaterBalanceType
   {
   public CanopyWaterBalanceCanopyType[] Canopy;
   public float eo;
   public float interception;
   };
public delegate void CanopyWaterBalanceDelegate(CanopyWaterBalanceType Data);

//------ WaterChanged ------
public class WaterChangedType
   {
   public double[] DeltaWater;
   };
public delegate void WaterChangedDelegate(WaterChangedType Data);

//------ NitrogenChanged ------
public class NitrogenChangedType
   {
   public double[] DeltaNO3;
   public double[] DeltaNH4;
   }
public delegate void NitrogenChangedDelegate(NitrogenChangedType Data);

//------ KillLeaf ------
public class KillLeafType
   {
   public float KillFraction;
   };
public delegate void KillLeafDelegate(KillLeafType Data);

//------ Sow ------
public class SowType
   {
   public string Cultivar;
   public double Population;
   public double Depth;
   public double MaxCover;
   public double BudNumber;
   public double RowSpacing;
   };
public delegate void SowDelegate(SowType Data);

//------ WaterUptakesUptakes ------
public class WaterUptakesUptakesType
   {
   public string Name;
   public double[] Amount;
   };
public delegate void WaterUptakesUptakesDelegate(WaterUptakesUptakesType Data);

//------ WaterUptakes ------
public class WaterUptakesType
   {
   public WaterUptakesUptakesType[] Uptakes;
   };
public delegate void MWaterUptakesDelegate(WaterUptakesType Data);


