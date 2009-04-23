module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: create
      integer :: sysinit
      integer :: prepare
      integer :: process

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%create = add_registration(respondToEventReg, 'create', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%prepare = add_registration(respondToEventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(respondToEventReg, 'process', nullTypeDDML, '')
      end subroutine
end module Registrations

