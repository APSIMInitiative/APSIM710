module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: tick
      integer :: prepare
      integer :: process
      integer :: post
      integer :: report
      integer :: sysinit
      integer :: start
      integer :: pause
      integer :: cont
      integer :: end_run
      integer :: dummy

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%tick = add_registration(eventReg, 'tick', TimeTypeDDML, '')
         id%prepare = add_registration(eventReg, 'prepare', nullTypeDDML, '')
         id%process = add_registration(eventReg, 'process', nullTypeDDML, '')
         id%post = add_registration(eventReg, 'post', nullTypeDDML, '')
         id%report = add_registration(eventReg, 'report', nullTypeDDML, '')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%start = add_registration(respondToEventReg, 'start', nullTypeDDML, '')
         id%pause = add_registration(respondToEventReg, 'pause', nullTypeDDML, '')
         id%cont = add_registration(respondToEventReg, 'cont', nullTypeDDML, '')
         id%end_run = add_registration(respondToEventReg, 'end_run', nullTypeDDML, '')
         id%dummy = add_reg(respondToGetReg, 'day', intTypeDDML, 'day', '')
         id%dummy = add_reg(respondToGetReg, 'year', intTypeDDML, 'year', '')
         id%dummy = add_reg(respondToGetReg, 'timestep', intTypeDDML, 'min', '')
         id%dummy = add_reg(respondToGetReg, 'day_of_month', intTypeDDML, 'day', '')
         id%dummy = add_reg(respondToGetReg, 'month', intTypeDDML, 'month', '')
         id%dummy = add_reg(respondToGetReg, 'start_week', booleanTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'end_week', booleanTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'start_month', booleanTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'end_month', booleanTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'end_year', booleanTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'simulation_start_day', intTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'simulation_start_year', intTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'simulation_end_day', intTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'simulation_end_day', intTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'simulation_start_date', doubleTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'simulation_end_date', doubleTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'today', doubleTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'day_of_year', intTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'month_str', stringTypeDDML, '', '')

      end subroutine
end module Registrations

