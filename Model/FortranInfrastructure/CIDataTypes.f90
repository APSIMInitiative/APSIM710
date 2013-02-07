module dataTypes
   integer, parameter :: max_array_size = 100

   !-------------------- Built in types
   character(len=*), parameter :: nullTypeDDML = '<type/>'
   character(len=*), parameter :: BooleanTypeDDML = &
      '<type name="Boolean" kind="boolean" />'

   character(len=*), parameter :: IntTypeDDML = &
      '<type name="Int" kind="integer4" boundable="T" />'

   character(len=*), parameter :: FloatTypeDDML = &
      '<type name="Float" kind="single" boundable="T" />'

   character(len=*), parameter :: DoubleTypeDDML = &
      '<type name="Double" kind="double" boundable="T" />'

   character(len=*), parameter :: StringTypeDDML = &
      '<type name="String" kind="string" />'

   character(len=*), parameter :: BoolArrayTypeDDML = &
      '<type name="BoolArray" kind="boolean" array="T" />'

   character(len=*), parameter :: IntArrayTypeDDML = &
      '<type name="IntArray" kind="integer4" array="T" boundable="T" />'

   character(len=*), parameter :: FloatArrayTypeDDML = &
      '<type name="FloatArray" kind="single" array="T" boundable="T" />'

   character(len=*), parameter :: DoubleArrayTypeDDML = &
      '<type name="DoubleArray" kind="double" array="T" boundable="T" />'

   character(len=*), parameter :: StringArrayTypeDDML = &
      '<type name="StringArray" kind="string" array="T" />'



!-------------------- ManagerEventKeyType
   character(len=*), parameter :: ManagerEventKeyTypeDDML = &
      '<type name="ManagerEventKey">' // &
      '   <field name="Name" kind="string" />' // &
      '   <field name="Units" kind="string" />' // &
      '   <field name="Values" kind="string" array="T" />' // &
      '   </type>'
   type ManagerEventKeyType
      sequence
      character(len=100) :: Name
      character(len=100) :: Units
      character(len=100) :: Values(max_array_size)
      integer :: num_Values
   end type ManagerEventKeyType

!-------------------- ManagerEventType
   character(len=*), parameter :: ManagerEventTypeDDML = &
      '<type name="ManagerEvent">' // &
      '   <field name="Key" array="T">' // &
      '   <element>' // &
      '   <field name="Name" kind="string" />' // &
      '   <field name="Units" kind="string" />' // &
      '   <field name="Values" kind="string" array="T" />' // &
      '   <field name="Name" kind="string" />' // &
      '   <field name="Units" kind="string" />' // &
      '   <field name="Values" kind="string" array="T" />' // &
      '   </element>' // &
      '   </field>' // &
      '   </type>'
   type ManagerEventType
      sequence
      type(ManagerEventKeyType):: Key(max_array_size)
      integer :: num_Key
   end type ManagerEventType

!-------------------- SowType
   character(len=*), parameter :: SowTypeDDML = &
      '<type name="Sow">' // &
      '   <field name="Cultivar" kind="string" />' // &
      '   <field name="Population" kind="double" />' // &
      '   <field name="Sowing_depth" kind="integer4" />' // &
      '   </type>'
   type SowType
      sequence
      character(len=100) :: Cultivar
      double precision :: Plants
      integer :: Sowing_depth
   end type SowType

!-------------------- ApsimVariantType
   character(len=*), parameter :: ApsimVariantTypeDDML = &
      '<type name="ApsimVariant">' // &
      '   <field name="param1_name" kind="string" />' // &
      '   <field name="param1_numbytes" kind="integer4" />' // &
      '   <field name="param1_code" kind="integer4" />' // &
      '   <field name="param1_isarray" kind="boolean" />' // &
      '   <field name="param1_value" kind="string" />' // &
      '   <field name="param2_name" kind="string" />' // &
      '   <field name="param2_numbytes" kind="integer4" />' // &
      '   <field name="param2_code" kind="integer4" />' // &
      '   <field name="param2_isarray" kind="boolean" />' // &
      '   <field name="param2_value" kind="string" />' // &
      '   <field name="param3_name" kind="string" />' // &
      '   <field name="param3_numbytes" kind="integer4" />' // &
      '   <field name="param3_code" kind="integer4" />' // &
      '   <field name="param3_isarray" kind="boolean" />' // &
      '   <field name="param3_value" kind="string" />' // &
      '   <field name="param4_name" kind="string" />' // &
      '   <field name="param4_numbytes" kind="integer4" />' // &
      '   <field name="param4_code" kind="integer4" />' // &
      '   <field name="param4_isarray" kind="boolean" />' // &
      '   <field name="param4_value" kind="string" />' // &
      '   <field name="param5_name" kind="string" />' // &
      '   <field name="param5_numbytes" kind="integer4" />' // &
      '   <field name="param5_code" kind="integer4" />' // &
      '   <field name="param5_isarray" kind="boolean" />' // &
      '   <field name="param5_value" kind="string" />' // &
      '   <field name="param6_name" kind="string" />' // &
      '   <field name="param6_numbytes" kind="integer4" />' // &
      '   <field name="param6_code" kind="integer4" />' // &
      '   <field name="param6_isarray" kind="boolean" />' // &
      '   <field name="param6_value" kind="string" />' // &
      '   <field name="param7_name" kind="string" />' // &
      '   <field name="param7_numbytes" kind="integer4" />' // &
      '   <field name="param7_code" kind="integer4" />' // &
      '   <field name="param7_isarray" kind="boolean" />' // &
      '   <field name="param7_value" kind="string" />' // &
      '   </type>'
   type ApsimVariantType
      sequence
      character(len=100) :: param1_name
      integer :: param1_numbytes
      integer :: param1_code
      logical :: param1_isarray
      character(len=100) :: param1_value
      character(len=100) :: param2_name
      integer :: param2_numbytes
      integer :: param2_code
      logical :: param2_isarray
      character(len=100) :: param2_value
      character(len=100) :: param3_name
      integer :: param3_numbytes
      integer :: param3_code
      logical :: param3_isarray
      character(len=100) :: param3_value
      character(len=100) :: param4_name
      integer :: param4_numbytes
      integer :: param4_code
      logical :: param4_isarray
      character(len=100) :: param4_value
      character(len=100) :: param5_name
      integer :: param5_numbytes
      integer :: param5_code
      logical :: param5_isarray
      character(len=100) :: param5_value
      character(len=100) :: param6_name
      integer :: param6_numbytes
      integer :: param6_code
      logical :: param6_isarray
      character(len=100) :: param6_value
      character(len=100) :: param7_name
      integer :: param7_numbytes
      integer :: param7_code
      logical :: param7_isarray
      character(len=100) :: param7_value
   end type ApsimVariantType

!-------------------- LayeredType
   character(len=*), parameter :: LayeredTypeDDML = &
      '<type name="Layered" description="Layered soil data">' // &
      '   <field name="layer" kind="double" array="T" />' // &
      '   <field name="value" kind="double" array="T" />' // &
      '   </type>'
   type LayeredType
      sequence
      double precision :: layer(max_array_size)
      integer :: num_layer
      double precision :: value(max_array_size)
      integer :: num_value
   end type LayeredType

!-------------------- TimeType
   character(len=*), parameter :: TimeTypeDDML = &
      '<type name="Time" description="Change in the simulation system time and the duration of the new time step">' // &
      '   <field name="startday" kind="integer4" description="Day number of the start of the timestep" />' // &
      '   <field name="startsec" kind="integer4" description="Seconds past midnight of the start of the timestep (0-86399)" />' // &
      '   <field name="startsecpart" kind="double" description="Fraction of a second of the start of the timestep (0-1)" />' // &
      '   <field name="endday" kind="integer4" description="Day number of the end of the timestep" />' // &
      '   <field name="endsec" kind="integer4" description="Seconds past midnight of the end of the timestep (0-86399)" />' // &
      '   <field name="endsecpart" kind="double" description="Fraction of a second of the end of the timestep (0-1)" />' // &
      '   </type>'
   type TimeType
      sequence
      integer :: startday
      integer :: startsec
      double precision :: startsecpart
      integer :: endday
      integer :: endsec
      double precision :: endsecpart
   end type TimeType

!-------------------- NewMetType
   character(len=*), parameter :: NewMetTypeDDML = &
      '<type name="NewMet">' // &
      '   <field name="today" kind="double" />' // &
      '   <field name="radn" kind="single" lower_bound="0.0" upper_bound="50.0" units="MJ/m^2" />' // &
      '   <field name="maxt" kind="single" lower_bound="-10.0" upper_bound="70.0" units="oC" />' // &
      '   <field name="mint" kind="single" lower_bound="-20.0" upper_bound="50.0" units="oC" />' // &
      '   <field name="rain" kind="single" lower_bound="0.0" upper_bound="1000.0" units="mm" />' // &
      '   <field name="vp" kind="single" units="hPa" />' // &
      '   </type>'
   type NewMetType
      sequence
      double precision :: today
      real :: radn
      real :: maxt
      real :: mint
      real :: rain
      real :: vp
   end type NewMetType

!-------------------- KillCropType
   character(len=*), parameter :: KillCropTypeDDML = &
      '<type name="KillCrop">' // &
      '   <field name="KillFraction" kind="single" />' // &
      '   </type>'
   type KillCropType
      sequence
      real :: KillFraction
   end type KillCropType

!-------------------- FertiliserApplicationType
   character(len=*), parameter :: FertiliserApplicationTypeDDML = &
      '<type name="FertiliserApplication">' // &
      '   <field name="Amount" kind="single" />' // &
      '   <field name="Depth" kind="single" />' // &
      '   <field name="Type" kind="string" />' // &
      '   </type>'
   type FertiliserApplicationType
      sequence
      real :: Amount
      real :: Depth
      character(len=100) :: Type
   end type FertiliserApplicationType

!-------------------- FOMType
   character(len=*), parameter :: FOMTypeDDML = &
      '<type name="FOM">' // &
      '   <field name="amount" kind="single" units="kg/ha" />' // &
      '   <field name="C" kind="single" units="kg/ha" />' // &
      '   <field name="N" kind="single" units="kg/ha" />' // &
      '   <field name="P" kind="single" units="kg/ha" />' // &
      '   <field name="AshAlk" kind="single" units="kg/ha" />' // &
      '   </type>'
   type FOMType
      sequence
      real :: amount
      real :: C
      real :: N
      real :: P
      real :: AshAlk
   end type FOMType

!-------------------- FOMLayerLayerType
   character(len=*), parameter :: FOMLayerLayerTypeDDML = &
      '<type name="FOMLayerLayer">' // &
      '   <field name="FOM">' // &
      '   <field name="amount" kind="single" units="kg/ha" />' // &
      '   <field name="C" kind="single" units="kg/ha" />' // &
      '   <field name="N" kind="single" units="kg/ha" />' // &
      '   <field name="P" kind="single" units="kg/ha" />' // &
      '   <field name="AshAlk" kind="single" units="kg/ha" />' // &
      '   </field>' // &
      '   <field name="CNR" kind="single" />' // &
      '   <field name="LabileP" kind="single" />' // &
      '   </type>'
   type FOMLayerLayerType
      sequence
      type(FOMType) :: FOM
      real :: CNR
      real :: LabileP
   end type FOMLayerLayerType

!-------------------- FOMLayerType
   character(len=*), parameter :: FOMLayerTypeDDML = &
      '<type name="FOMLayer">' // &
      '   <field name="Type" kind="string" />' // &
      '   <field name="Layer" array="T">' // &
      '   <element>' // &
      '   <field name="FOM">' // &
      '   <field name="amount" kind="single" units="kg/ha" />' // &
      '   <field name="C" kind="single" units="kg/ha" />' // &
      '   <field name="N" kind="single" units="kg/ha" />' // &
      '   <field name="P" kind="single" units="kg/ha" />' // &
      '   <field name="AshAlk" kind="single" units="kg/ha" />' // &
      '   </field>' // &
      '   <field name="CNR" kind="single" />' // &
      '   <field name="LabileP" kind="single" />' // &
      '   <field name="FOM">' // &
      '   <field name="amount" kind="single" units="kg/ha" />' // &
      '   <field name="C" kind="single" units="kg/ha" />' // &
      '   <field name="N" kind="single" units="kg/ha" />' // &
      '   <field name="P" kind="single" units="kg/ha" />' // &
      '   <field name="AshAlk" kind="single" units="kg/ha" />' // &
      '   </field>' // &
      '   <field name="CNR" kind="single" />' // &
      '   <field name="LabileP" kind="single" />' // &
      '   </element>' // &
      '   </field>' // &
      '   </type>'
   type FOMLayerType
      sequence
      character(len=100) :: Type
      type(FOMLayerLayerType):: Layer(max_array_size)
      integer :: num_Layer
   end type FOMLayerType

!-------------------- NewProfileType
   character(len=*), parameter :: NewProfileTypeDDML = &
      '<type name="NewProfile">' // &
      '   <field name="dlayer" kind="single" array="T" />' // &
      '   <field name="air_dry_dep" kind="single" array="T" />' // &
      '   <field name="ll15_dep" kind="single" array="T" />' // &
      '   <field name="dul_dep" kind="single" array="T" />' // &
      '   <field name="sat_dep" kind="single" array="T" />' // &
      '   <field name="sw_dep" kind="single" array="T" />' // &
      '   <field name="bd" kind="single" array="T" />' // &
      '   </type>'
   type NewProfileType
      sequence
      real :: dlayer(max_array_size)
      integer :: num_dlayer
      real :: air_dry_dep(max_array_size)
      integer :: num_air_dry_dep
      real :: ll15_dep(max_array_size)
      integer :: num_ll15_dep
      real :: dul_dep(max_array_size)
      integer :: num_dul_dep
      real :: sat_dep(max_array_size)
      integer :: num_sat_dep
      real :: sw_dep(max_array_size)
      integer :: num_sw_dep
      real :: bd(max_array_size)
      integer :: num_bd
   end type NewProfileType

!-------------------- ExternalMassFlowType
   character(len=*), parameter :: ExternalMassFlowTypeDDML = &
      '<type name="ExternalMassFlow">' // &
      '   <field name="PoolClass" kind="string" unit="-" />' // &
      '   <field name="FlowType" kind="string" unit="-" />' // &
      '   <field name="C" kind="single" unit="kg/ha" />' // &
      '   <field name="N" kind="single" unit="kg/ha" />' // &
      '   <field name="P" kind="single" unit="kg/ha" />' // &
      '   <field name="DM" kind="single" unit="kg/ha" />' // &
      '   <field name="SW" kind="single" unit="mm" />' // &
      '   </type>'
   type ExternalMassFlowType
      sequence
      character(len=100) :: PoolClass
      character(len=100) :: FlowType
      real :: C
      real :: N
      real :: P
      real :: DM
      real :: SW
   end type ExternalMassFlowType

end module dataTypes
