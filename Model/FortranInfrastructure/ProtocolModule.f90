module ProtocolModule
   implicit none

   ! Some protocol definitions
   type Message
      sequence
      integer*2 version
      integer*2 messageType
      integer*4 from
      integer*4 to
      integer*4 messageID
      logical*4 toAcknowledge
      integer*4 nDataBytes
      integer*4 dataPtr           ! really a void*
   end type Message

   type MessagePtr
      type(Message), pointer :: ptr
   end type MessagePtr

   ! PROTOCOL MESSAGE Types
   integer, parameter :: MESSAGE_ActivateComponent = 1
   integer, parameter :: MESSAGE_AddComponent = 2
   integer, parameter :: MESSAGE_Checkpoint = 3
   integer, parameter :: MESSAGE_Commence = 4
   integer, parameter :: MESSAGE_Complete = 5
   integer, parameter :: MESSAGE_DeactivateComponent = 6
   integer, parameter :: MESSAGE_DeleteComponent = 7
   integer, parameter :: MESSAGE_Deregister = 8
   integer, parameter :: MESSAGE_Event = 9
   integer, parameter :: MESSAGE_GetValue = 10
   integer, parameter :: MESSAGE_Init1 = 11
   integer, parameter :: MESSAGE_Init2 = 12
   integer, parameter :: MESSAGE_NotifyAboutToDelete = 13
   integer, parameter :: MESSAGE_NotifyRegistrationChange = 14
   integer, parameter :: MESSAGE_NotifySetValueSuccess = 15
   integer, parameter :: MESSAGE_NotifyTermination = 16
   integer, parameter :: MESSAGE_PauseSimulation = 17
   integer, parameter :: MESSAGE_PublishEvent = 18
   integer, parameter :: MESSAGE_QueryInfo = 19
   integer, parameter :: MESSAGE_QueryType = 20
   integer, parameter :: MESSAGE_QuerySetValue = 40
   integer, parameter :: MESSAGE_QueryValue = 21
   integer, parameter :: MESSAGE_Register = 22
   integer, parameter :: MESSAGE_ReinstateCheckpoint = 23
   integer, parameter :: MESSAGE_RequestComponentID = 24
   integer, parameter :: MESSAGE_RequestSetValue = 25
   integer, parameter :: MESSAGE_ResumeSimulation = 26
   integer, parameter :: MESSAGE_ReturnComponentID = 27
   integer, parameter :: MESSAGE_ReturnInfo = 28
   integer, parameter :: MESSAGE_ReturnType = 29
   integer, parameter :: MESSAGE_ReturnValue = 30
   integer, parameter :: MESSAGE_TerminateSimulation = 31

   type QueryData
      integer id
      integer replyto
      integer replyid
   end type QueryData

end module ProtocolModule
