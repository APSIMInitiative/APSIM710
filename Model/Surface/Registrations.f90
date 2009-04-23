module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: swim_timestep_preparation
      integer :: pre_swim_timestep
      integer :: post_swim_timestep
      integer :: tillage

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%swim_timestep_preparation = add_registration(respondToEventReg, 'swim_timestep_preparation', nullTypeDDML, '')
         id%pre_swim_timestep = add_registration(respondToEventReg, 'pre_swim_timestep', nullTypeDDML, '')
         id%post_swim_timestep = add_registration(respondToEventReg, 'post_swim_timestep', nullTypeDDML, '')
         id%tillage = add_registration(respondToEventReg, 'tillage', nullTypeDDML, '')
      end subroutine
end module Registrations

