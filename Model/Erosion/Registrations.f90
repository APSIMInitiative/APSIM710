module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: sysinit
      integer :: end_run
      integer :: process
      integer :: dummy

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%dummy = add_reg(respondToGetReg, 'soil_loss', floatTypeDDML, 't/ha', '')
         id%dummy = add_reg(respondToGetReg, 'soil_loss_bed', floatTypeDDML, 't/ha', '')
         id%dummy = add_reg(respondToGetReg, 'soil_loss_susp', floatTypeDDML, 't/ha', '')
         id%dummy = add_reg(respondToGetReg, 'soil_loss_mm', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'sed_conc', floatTypeDDML, 'g/l', '')
         id%dummy = add_reg(respondToGetReg, 'sed_conc_bed', floatTypeDDML, 'g/l', '')
         id%dummy = add_reg(respondToGetReg, 'sed_conc_susp', floatTypeDDML, 'g/l', '')
         id%dummy = add_reg(respondToGetReg, 'bed_depth', floatTypeDDML, 'mm', '')
         id%dummy = add_reg(respondToGetReg, 'erosion_cover', floatTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'cover_extra', floatTypeDDML, '', '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%end_run = add_registration(respondToEventReg, 'end_run', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
      end subroutine
end module Registrations

