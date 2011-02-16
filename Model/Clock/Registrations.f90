module Registrations
   use DataTypes
   type IDsType
      sequence
      integer :: tick
      integer :: prepare
      integer :: process
      integer :: post
      integer :: sysinit
      integer :: start
      integer :: pause
      integer :: cont
      integer :: end_run
      integer :: dummy
      integer :: start_simulation
      integer :: end_simulation
      integer :: start_week
      integer :: start_month
      integer :: start_year
      integer :: end_week
      integer :: end_month
      integer :: end_year
      integer :: end_day
      integer :: report

   end type IDsType

   contains

      subroutine doRegistrations(id)
         use Infrastructure
         type(IDsType) :: id

         id%tick = add_registration(eventReg, 'tick', TimeTypeDDML, '')
         id%prepare = add_reg(eventReg, 'prepare', nullTypeDDML, '', 'Beginning of a day')
         id%process = add_reg(eventReg, 'process', nullTypeDDML, '', 'Middle of a day')
         id%post = add_reg(eventReg, 'post', nullTypeDDML, '', 'End of a day')
         id%sysinit = add_registration(respondToEventReg, 'sysinit', nullTypeDDML, '')
         id%start = add_registration(respondToEventReg, 'start', nullTypeDDML, '')
         id%pause = add_registration(respondToEventReg, 'pause', nullTypeDDML, '')
         id%cont = add_registration(respondToEventReg, 'cont', nullTypeDDML, '')
         id%end_run = add_registration(respondToEventReg, 'end_run', nullTypeDDML, '')
         id%start_simulation = add_reg(eventReg, 'start_simulation', nullTypeDDML, '', 'Start of simulation')
         id%end_simulation = add_reg(eventReg, 'end_simulation', nullTypeDDML, '', 'End of simulation')
         id%start_week = add_reg(eventReg, 'start_week', nullTypeDDML, '', 'Start of a week')
         id%start_month = add_reg(eventReg, 'start_month', nullTypeDDML, '', 'Start of a month')
         id%start_year = add_reg(eventReg, 'start_year', nullTypeDDML, '', 'Start of a year')
         id%end_week = add_reg(eventReg, 'end_week', nullTypeDDML, '', 'End of a week')
         id%end_month = add_reg(eventReg, 'end_month', nullTypeDDML, '', 'End of a month')
         id%end_year = add_reg(eventReg, 'end_year', nullTypeDDML, '', 'End of a year')
         id%end_day = add_reg(eventReg, 'end_day', nullTypeDDML, '',  'End of a day')
         id%report = add_reg(eventReg, 'report', nullTypeDDML, '', 'End of day report')

         id%dummy = add_reg(respondToGetReg, 'day', intTypeDDML, 'day', 'Current day of year (1-365)')
         id%dummy = add_reg(respondToGetReg, 'year', intTypeDDML, 'year', 'Current year')
         id%dummy = add_reg(respondToGetReg, 'timestep', intTypeDDML, 'min', 'Timestep of simulation')
         id%dummy = add_reg(respondToGetReg, 'day_of_month', intTypeDDML, 'day', 'Day of current month')
         id%dummy = add_reg(respondToGetReg, 'month', intTypeDDML, 'month', 'Month')
         id%dummy = add_reg(respondToGetReg, 'month_str', stringTypeDDML, 'month', 'Month as string')
         id%dummy = add_reg(respondToGetReg, 'start_week', booleanTypeDDML, '', 'Whether today is the start of the week')
         id%dummy = add_reg(respondToGetReg, 'end_week', booleanTypeDDML, '', 'Whether today is the end of the week')
         id%dummy = add_reg(respondToGetReg, 'start_month', booleanTypeDDML, '', 'Whether today is the start of the month')
         id%dummy = add_reg(respondToGetReg, 'end_month', booleanTypeDDML, '', 'Whether today is the end of the month')
         id%dummy = add_reg(respondToGetReg, 'end_year', booleanTypeDDML, '', 'Whether today is the end of the year')
         id%dummy = add_reg(respondToGetReg, 'start_simulation', booleanTypeDDML, '', 'Whether today is the start of the simulation')
         id%dummy = add_reg(respondToGetReg, 'end_simulation', booleanTypeDDML, '', 'Whether today is the end of the simulation')
         id%dummy = add_reg(respondToGetReg, 'simulation_start_day', intTypeDDML, '', 'The day number of the start of the simulation (1-365)')
         id%dummy = add_reg(respondToGetReg, 'simulation_start_year', intTypeDDML, '', 'The year of the start of the simulation')
         id%dummy = add_reg(respondToGetReg, 'simulation_end_day', intTypeDDML, '', 'The julian day number of the end of the simulation')
         id%dummy = add_reg(respondToGetReg, 'simulation_end_year', intTypeDDML, '', 'Year of end of simulation')
         id%dummy = add_reg(respondToGetReg, 'simulation_days', intTypeDDML, '', 'The number of days since the start of the simulation')
         id%dummy = add_reg(respondToGetReg, 'simulation_start_date', doubleTypeDDML, '', 'Date of start of simulation')
         id%dummy = add_reg(respondToGetReg, 'simulation_end_date', doubleTypeDDML, '', 'Date of start of simulation')
         id%dummy = add_reg(respondToGetReg, 'today', doubleTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'day_of_year', intTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'month_str', stringTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'time', TimeTypeDDML, '', '')
         id%dummy = add_reg(respondToGetReg, 'dd/mm', stringTypeDDML, 'dd/mm', '')
         id%dummy = add_reg(respondToGetReg, 'dd/mm/yyyy', stringTypeDDML, 'dd/mm/yyyy', '')
         id%dummy = add_reg(respondToGetReg, 'mm/dd/yyyy', stringTypeDDML, 'mm/dd/yyyy', '')
         id%dummy = add_reg(respondToGetReg, 'dd_mm_yyyy', stringTypeDDML, 'dd_mm_yyyy', '')
         id%dummy = add_reg(respondToGetReg, 'dd/mmm/yyyy', stringTypeDDML, 'dd/mmm/yyyy', '')
         id%dummy = add_reg(respondToGetReg, 'mmm/dd/yyyy', stringTypeDDML, 'mmm/dd/yyyy', '')
         id%dummy = add_reg(respondToGetReg, 'dd_mmm', stringTypeDDML, 'dd_mmm', '')
		 

      end subroutine
end module Registrations

