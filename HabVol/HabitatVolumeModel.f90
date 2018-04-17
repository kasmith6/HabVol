program main
implicit none

  integer :: stepT, stepP

!   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   ~~~~~            HABITAT VOLUME MODEL             ~~~~~
!   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  call ini_HabitatVolumeModel()  !Initialize Model
  call run_HabitatVolumeModel()  !Run        Model
  call fin_HabitatVolumeModel()  !Finalize   Model

contains

! ~~~~~~~~~~~~~~~ INITIALIZE MODEL ~~~~~~~~~~~~~~~

  subroutine ini_HabitatVolumeModel()
  use param_mod,  only: days, dt, iprint
  use hydro_mod,  only: init_Grid
  use volume_mod, only: init_Output
  implicit none

    stepT = int(days*DBLE(86400)/dt)   ! number of external time steps
    stepP = int(iprint/dt)             ! interval at which to print volumes

    !Make sure iprint is a multiple of dt
    if(mod(iprint,dt)/=0)then
      write(*,*) 'Error: iprint must be a multiple of dt.'
      stop
    endif

    !Initialize the model grid
    call init_Grid()

    !Initialize Output Files
    call init_Output()

  end subroutine ini_HabitatVolumeModel


! ~~~~~~~~~~~~~~~~~~ RUN MODEL ~~~~~~~~~~~~~~~~~~~

  subroutine run_HabitatVolumeModel()
  use hydro_mod, only: init_Hydro, update_Hydro
  use volume_mod, only: getVolumes
  implicit none

    integer :: t

    do t=1,stepT

      !Read in hydrodynamic model data 
      if(t==1)then
        call init_Hydro()
      else
        call update_Hydro()
      endif

      !if this time step is being printed, get volumes
      if( mod(t,stepP) == 0 ) call getVolumes(t)

    enddo

  end subroutine run_HabitatVolumeModel


! ~~~~~~~~~~~~~~~~ FINALIZE MODEL ~~~~~~~~~~~~~~~~

  subroutine fin_HabitatVolumeModel()
  use hydro_mod,  only: fin_Grid
  use volume_mod, only: fin_Output
  implicit none

    !finalize the grid and output files
    call fin_Grid()
    call fin_Output()

  end subroutine fin_HabitatVolumeModel

end