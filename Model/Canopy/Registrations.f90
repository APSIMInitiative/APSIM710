module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: sysinit
      integer :: prepare
      integer :: post
      integer :: dummy
   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%post = add_registration(respondToEventReg, 'post', nullTypeDDML, '')
         dummy = add_reg(respondToGetReg, 'cover_tot_sum', floatTypeDDML, '', '')
         dummy = add_reg(respondToGetReg, 'cover_tot_all', floatarrayTypeDDML, '', '')
         dummy = add_reg(respondToGetReg, 'cover_height_all', floatarrayTypeDDML, '', '')
         dummy = add_reg(respondToGetReg, 'cover_green_sum', floatTypeDDML, '', '')
         dummy = add_reg(respondToGetReg, 'cover_green_all', floatarrayTypeDDML, '', '')
         dummy = add_reg(respondToGetReg, 'cover_crops_all', intarrayTypeDDML, '', '')
         
      end subroutine
end module Registrations

