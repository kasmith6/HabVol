MODULE PARAM_MOD 
 
!  The Parameter Module reads in the include file, Input.inc, making the 
!  parameters declared within available to all the other modules.
 
IMPLICIT NONE 
PUBLIC 
SAVE 

  include 'HabitatVolumeModel.h'
  include 'Input.inc'

END MODULE PARAM_MOD 