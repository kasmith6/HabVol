MODULE HYDRO_DATA_MOD
IMPLICIT NONE

  INTEGER :: iint, & !Keeps track of the input file, 0 = file 1, 1 = file 2, etc.
             step    !Keeps track of the forward time step

  INTEGER :: nCat    !Number of Categories of growth rates
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: cats

  INTEGER, ALLOCATABLE, DIMENSION(:)   :: rho_mask,rvr_mask,form,rvr_form
  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: mask_rho,mask_rvr,ij_form,rvr_ij_form,RE

  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: SC,CS

  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: depth,rx,ry,rlon,rlat
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: r_ele_x,r_ele_y

  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: zeta
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: salt,temp,disO

  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: bio

  INTEGER, ALLOCATABLE, DIMENSION(:) :: wetEles

  !The concatenated hydrodynamic input file name
  CHARACTER(len=100) :: filenm

END MODULE
