! ******************************* LTRANS Include File *******************************

!*** TIME PARAMETERS ***
REAL,    PARAMETER :: days   = 360.0          ! Number of days to run the model
INTEGER, PARAMETER :: iprint = 43200        ! Print interval for output (s); 3600 = every hour
INTEGER, PARAMETER :: dt  = 3600            ! External time step (duration between hydro model predictions) (s)

!*** ROMS HYDRODYNAMIC MODULE PARAMETERS ***
INTEGER, PARAMETER :: tdim = 240            ! Number of time steps per ROMS hydro predictions file
REAL,    PARAMETER :: hc  = 2.5             ! Min Depth - used in ROMS S-level transformations
INTEGER, PARAMETER :: Vtransform = 1        ! 1-WikiRoms Eq. 1, 2-WikiRoms Eq. 2, 3-Song/Haidvogel 1994 Eq.

!*** IDEAL HABITAT INPUT SPECIFICATION PARAMETERS ***
LOGICAL, PARAMETER :: ton   = .TRUE.        ! Read-in model temperature
LOGICAL, PARAMETER :: son   = .TRUE.        ! Read-in model salinity
LOGICAL, PARAMETER :: don   = .TRUE.        ! Read-in model DO

!*** IDEAL HABITAT OUTPUT SPECIFICATION PARAMETERS ***
LOGICAL, PARAMETER :: bio_on     = .FALSE.  ! Bioenergetics on?
LOGICAL, PARAMETER :: b_vol_on   = .FALSE.  ! Output Volume of bioenergetics
LOGICAL, PARAMETER :: b_surf_on  = .FALSE.  ! Output Surface of bioenergetics
INTEGER, PARAMETER :: b_species  = 3        ! 1 = Atlantic Sturgeon Mortality
                                            ! 2 = Striped Bass Bioenergetics (see note below)
                                            ! 3 = Atlantic Sturgeon Bioenergetics
!Note that for Striped Bass Bioenergetics, fish mass (M) must be set at the beginning of 
!  the Striped Bass code (currently found on line 164) in the Constraints Module


LOGICAL, PARAMETER :: vol_col_on = .FALSE.  ! Output Volume in each water column? (in addition to total volume)

LOGICAL, PARAMETER :: s_vol_on   = .FALSE.  ! Output Volume of 'ideal' salinity
LOGICAL, PARAMETER :: t_vol_on   = .FALSE.  ! Output Volume of 'ideal' temperature
LOGICAL, PARAMETER :: d_vol_on   = .FALSE.  ! Output Volume of 'ideal' DO
LOGICAL, PARAMETER :: st_vol_on  = .TRUE.  ! Output Volume of 'ideal' temperature and salinity
LOGICAL, PARAMETER :: td_vol_on  = .FALSE.  ! Output Volume of 'ideal' temperature and DO
LOGICAL, PARAMETER :: sd_vol_on  = .FALSE.  ! Output Volume of 'ideal' salinity and DO
LOGICAL, PARAMETER :: std_vol_on = .TRUE.   ! Output Volume of 'ideal' temperature, salinity, and DO
LOGICAL, PARAMETER :: s_surf_on   = .FALSE. ! Output Surface of 'ideal' salinity
LOGICAL, PARAMETER :: t_surf_on   = .FALSE. ! Output Surface of 'ideal' temperature
LOGICAL, PARAMETER :: d_surf_on   = .FALSE. ! Output Surface of 'ideal' DO
LOGICAL, PARAMETER :: st_surf_on  = .FALSE. ! Output Surface of 'ideal' temperature and salinity
LOGICAL, PARAMETER :: td_surf_on  = .FALSE. ! Output Surface of 'ideal' temperature and DO
LOGICAL, PARAMETER :: sd_surf_on  = .FALSE. ! Output Surface of 'ideal' salinity and DO
LOGICAL, PARAMETER :: std_surf_on = .FALSE. ! Output Surface of 'ideal' temperature, salinity, and DO
! Note that the salt, temp, & do constraints are now specified in subroutine getConstraints of constraints_module.f90

LOGICAL, PARAMETER :: shallow = .FALSE.     ! Is the species only found in shallow water?
DOUBLE PRECISION, PARAMETER :: sdepth = 10. ! If shallow=.TRUE., the maximum depth cutoff
LOGICAL, PARAMETER :: deep    = .FALSE.     ! Is the species only found in deep water?
DOUBLE PRECISION, PARAMETER :: ddepth = 10. ! If deep=.TRUE., the minimum depth cutoff
! Note: shallow and deep cannot both be true

LOGICAL, PARAMETER :: atBottom = .FALSE.    ! Is the species only found near bottom?
DOUBLE PRECISION, PARAMETER :: offBott = 2. ! If atBottom=.TRUE., the distance off bottom to calculate


!*** LAT/LON CONSTRAINTS ***
LOGICAL, PARAMETER :: llconst = .TRUE.      !Are there lat/lon constraints?
CHARACTER(LEN=72), PARAMETER :: llConstFile = '/share/home/aschleng/Nmodel/HabitatVolumeModel/HabitatVolumeBoundary.csv'
INTEGER, PARAMETER :: numConstPts = 8       !# of edge points in llConstFile including repeated first point


!*** RIVER FORCING CORRECTIONS ***
!  This is to remove areas where rivers are first injected into the model, as the
!  salinity/temperature in these areas is inaccurate
INTEGER, PARAMETER :: numRiv = 8            !Number of river inputs in the model domain
INTEGER, PARAMETER :: sqSize = 5            !Size of area to skip (e.g. 5 means 5x5 square of rho nodes)
                                            !           iii , jjj
INTEGER, PARAMETER, DIMENSION(2*numRiv) :: rivLocs = (/   7 ,   7 , &
                                                         12 ,  27 , &
                                                          5 ,  60 , &
                                                          3 ,  83 , &
                                                         32 ,  78 , &
                                                         28 , 104 , &
                                                         26 , 120 , &
                                                         80 ,  78 /)

!*** CONVERSION MODULE PARAMETERS ***
DOUBLE PRECISION, PARAMETER :: PI = 3.14159265358979     ! Pi
DOUBLE PRECISION, PARAMETER :: RCF = 180.0 / PI          ! Radian conversion factor
DOUBLE PRECISION, PARAMETER :: Earth_Radius = 6378*1000  ! Equatorial radius

!*** INPUT FILE NAME AND LOCATION PARAMETERS ***; 
!ROMS NetCDF Model Grid file
CHARACTER(LEN=36), PARAMETER :: NCgridfile = '/share/home/aschleng/CPB_GRID_wUV.nc'
  !Note: the path to the file is necessary if the file is not in the same folder as the code
  !Note: if .nc file in separate folder in Linux, then include path. For example:
  !      CHARACTER(LEN=29), PARAMETER :: NCgridfile = '/share/enorth/CPB_GRID_wUV.nc' 
  !Note: if .nc file in separate folder in Windows, then include path. For example:
  !      CHARACTER(LEN=23), PARAMETER :: NCgridfile = 'D:\ROMS\CPB_GRID_wUV.nc'

!ROMS Predictions NetCDF Input File. Filename = prefix + filenum + suffix
CHARACTER(LEN=44), PARAMETER :: prefix='/share/home/aschleng/Nmodel/HVM_Sample/ly04_'   ! NetCDF Input Filename prefix
INTEGER, PARAMETER :: filenum = 0351                        ! Number in First NetCDF Input Filename
CHARACTER(LEN=3), PARAMETER :: suffix='.nc'                 ! NetCDF Input Filename suffix
  !Note: the path to the file is necessary if the file is not in the same folder as the code
  !Note: if .nc file in separate folder in Windows, then include path in prefix. For example:
  !      CHARACTER(LEN=15), PARAMETER :: prefix='D:\ROMS\y95hdr_'   
  !      if .nc file in separate folder in Linux, then include path in prefix. For example:
  !      CHARACTER(LEN=26), PARAMETER :: prefix='/share/lzhong/1995/y95hdr_'   