MODULE HYDRO_MOD
IMPLICIT NONE


CONTAINS


  SUBROUTINE init_Grid()
    USE HYDRO_DATA_MOD
    USE PARAM_MOD, ONLY: vi,uj,us,rho_nodes,rho_elements,NCgridfile,shallow,   &
      deep,sdepth,ddepth,numRiv,rivLocs,sqSize,vol_col_on,prefix,filenum,      &
      suffix,wet_elements,llconst,llConstFile,numConstPts
    USE PIP_MOD, ONLY: inpoly
    USE netcdf
    IMPLICIT NONE

    INCLUDE 'netcdf.inc'

    !NetCDF Variables
    INTEGER :: STATUS,IOSTAT,NCID,GF_ID,VID,dimid,dimcount

    !Iteration Variables
    INTEGER :: i,j,k,count,err

    REAL, ALLOCATABLE, DIMENSION(:,:) :: romdepth,x_rho,y_rho,lon_rho,lat_rho

    DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: llbnds

    err = 0

    ! *********************** GET GRID INFO ***********************

    WRITE(*,*) 'read-in grid information'

    ! OPEN NETCDF FILE - GET GF_ID VALUE

    STATUS = NF90_OPEN(NCgridfile,NF90_NOWRITE,GF_ID)
    if (STATUS .NE. NF90_NOERR) then
      write(*,*) 'Problem NF90_OPEN'
      err = 10
    endif

    ! GET VALUES FOR xi_rho,xi_u,xi_v,eta_rho,eta_u,eta_v

      STATUS = NF90_INQ_DIMID(GF_ID,'xi_rho',dimid)
      STATUS = NF90_INQUIRE_DIMENSION(GF_ID,dimid,len=dimcount)
      if (STATUS .NE. NF90_NOERR) then
        write(*,*) 'Problem dimid xi_rho'
        err = 20 
      endif
      vi = dimcount

      STATUS = NF90_INQ_DIMID(GF_ID,'eta_rho',dimid)
      STATUS = NF90_INQUIRE_DIMENSION(GF_ID,dimid,len=dimcount)
      if (STATUS .NE. NF90_NOERR) then
        write(*,*) 'Problem dimid eta_rho'
        err = 20 
      endif
      uj = dimcount

    ! CALCULATE RHO_NODES AND RHO_ELEMENTS

      !Calculate number of nodes in each grid
      rho_nodes = vi * uj

      !Calculate Maximum number of elements in each grid
      rho_elements = (vi-1)*(uj-1)


    ! ALLOCATE VARIABLE ARRAY DIMENSIONS

      ALLOCATE (romdepth(vi,uj),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating mask_rho'
        err = 30 
      endif

      ALLOCATE (x_rho(vi,uj),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating mask_rho'
        err = 30 
      endif

      ALLOCATE (y_rho(vi,uj),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating mask_rho'
        err = 30 
      endif

      ALLOCATE (lon_rho(vi,uj),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating mask_rho'
        err = 30 
      endif

      ALLOCATE (lat_rho(vi,uj),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating mask_rho'
        err = 30 
      endif

      ALLOCATE (mask_rho(vi,uj),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating mask_rho'
        err = 30 
      endif

      ALLOCATE (mask_rvr(vi,uj),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating mask_rvr'
        err = 30 
      endif

      ALLOCATE (rho_mask(rho_nodes),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating rho_mask'
        err = 30 
      endif

      ALLOCATE (rvr_mask(rho_nodes),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating rvr_mask'
        err = 30 
      endif

      ALLOCATE (ij_form(vi-1,uj-1),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating ij_form'
        err = 30 
      endif

      ALLOCATE (form(rho_elements),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating form'
        err = 30 
      endif

      ALLOCATE (rvr_ij_form(vi-1,uj-1),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating rvr_ij_form'
        err = 30 
      endif

      ALLOCATE (rvr_form(rho_elements),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating rvr_form'
        err = 30 
      endif

      ALLOCATE (RE(4,rho_elements),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating RE'
        err = 30 
      endif

      ALLOCATE (depth(rho_nodes),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating depth'
        err = 30 
      endif

      ALLOCATE (rx(rho_nodes),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating rx'
        err = 30 
      endif

      ALLOCATE (ry(rho_nodes),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating ry'
        err = 30 
      endif

      ALLOCATE (rlon(rho_nodes),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating rlon'
        err = 30 
      endif

      ALLOCATE (rlat(rho_nodes),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating rlat'
        err = 30 
      endif

      ALLOCATE (r_ele_x(4,rho_elements),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating r_ele_x'
        err = 30 
      endif

      ALLOCATE (r_ele_y(4,rho_elements),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating r_ele_y'
        err = 30 
      endif


    ! READ IN DATA FROM NETCDF FILE TO VARIABLES

      ! Depth (m)
      STATUS = NF90_INQ_VARID(GF_ID,'h',VID)
      STATUS = NF90_GET_VAR(GF_ID,VID,romdepth)
      if (STATUS .NE. NF90_NOERR) then
        write(*,*) 'Problem read depth'
        err = 40 
      endif

      ! x-coordinate at rho (m)
      STATUS = NF90_INQ_VARID(GF_ID,'x_rho',VID)
      STATUS = NF90_GET_VAR(GF_ID,VID,x_rho)
      if (STATUS .NE. NF90_NOERR) then
        write(*,*) 'Problem read x_rho'
        err = 40 
      endif

      ! y-coordinate at rho (m)
      STATUS = NF90_INQ_VARID(GF_ID,'y_rho',VID)
      STATUS = NF90_GET_VAR(GF_ID,VID,y_rho)
      if (STATUS .NE. NF90_NOERR) then
        write(*,*) 'Problem read y_rho'
        err = 40 
      endif

      ! x-coordinate at rho (m)
      STATUS = NF90_INQ_VARID(GF_ID,'lon_rho',VID)
      STATUS = NF90_GET_VAR(GF_ID,VID,lon_rho)
      if (STATUS .NE. NF90_NOERR) then
        write(*,*) 'Problem read lon_rho'
        err = 40 
      endif

      ! y-coordinate at rho (m)
      STATUS = NF90_INQ_VARID(GF_ID,'lat_rho',VID)
      STATUS = NF90_GET_VAR(GF_ID,VID,lat_rho)
      if (STATUS .NE. NF90_NOERR) then
        write(*,*) 'Problem read lat_rho'
        err = 40 
      endif

      ! rho grid mask
      STATUS = NF90_INQ_VARID(GF_ID,'mask_rho',VID)
      STATUS = NF90_GET_VAR(GF_ID,VID,mask_rho)
      if (STATUS .NE. NF90_NOERR) then
        write(*,*) 'Problem read mask_rho'
        err = 40 
      endif


    STATUS = NF90_CLOSE(GF_ID)
    if(STATUS /= NF90_NOERR) then
      write(*,*)'Problem closing GF_ID'
      err = 50
    endif

    if(err /= 0)then
      write(*,*) 'Error: Problem getting grid data. Error ID: ',err
      stop
    endif
    !  0=No Errors                 30=Error allocating arrays
    ! 10=Error Opening NCgridfile  40=Error getting variables
    ! 20=Error getting dimensions  50=Error Closing NCgridfile


    ! ************************** READ IN S LEVEL INFO *************************

    WRITE(filenm,'(A,I4.4,A)') prefix,filenum,suffix

!    write(*,*) filenm(1:LEN_TRIM(filenm))

    STATUS = NF90_OPEN(filenm, NF90_NOWRITE, NCID)
    if (STATUS .NE. NF90_NOERR) write(*,*) 'Problem NF90_OPEN'

    ! GET us DIMENSION

      STATUS = NF90_INQ_DIMID(NCID,'s_rho',dimid)
      STATUS = NF90_INQUIRE_DIMENSION(NCID,dimid,len=dimcount)
      if (STATUS .NE. NF90_NOERR) then
        write(*,*) 'Problem dimid us'
        err = 20 
      endif
      us = dimcount

    ! ALLOCATE VARIABLE ARRAY DIMENSIONS

      ALLOCATE (SC(us),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating SC'
        err = 30 
      endif

      ALLOCATE (CS(us),STAT=STATUS)
      if(STATUS /= 0) then
        write(*,*) 'Problem allocating CS'
        err = 30 
      endif

    ! READ IN DATA FROM NETCDF FILE TO VARIABLES

      ! s-coordinate on rho grid (sc_r)
      STATUS = NF90_INQ_VARID(NCID,'s_rho',VID)
      STATUS = NF90_GET_VAR(NCID,VID,SC)
      if (STATUS .NE. NF90_NOERR) write(*,*) 'Problem read SC'

      ! Cs value on rho grid (Cs_r)
      STATUS = NF90_INQ_VARID(NCID,'Cs_r',VID)
      STATUS = NF90_GET_VAR(NCID,VID,CS)
      if (STATUS .NE. NF90_NOERR) write(*,*) 'Problem read CS'

    !close the dataset and reassign the NCID
    STATUS = NF90_CLOSE(NCID)


    ! ****************************** ADJUST MASK ******************************

    if(llconst)then
      allocate(llbnds(numConstPts,2))

      open(1,FILE=TRIM(llConstFile))
        do i=1,numConstPts
          read(1,*)llbnds(i,1),llbnds(i,2)
        enddo
      close(1)

      do j=1,uj
        do i=1,vi
          if(mask_rho(i,j) == 1)then
            if(.NOT. inpoly(DBLE(lon_rho(i,j)),DBLE(lat_rho(i,j)),   &
               numConstPts,llbnds)) mask_rho(i,j) = 0
          endif
        enddo
      enddo

      deallocate(llbnds)
    endif


    !Make sure shallow and deep are not both set to TRUE
    if(shallow .AND. deep)then
      write(*,*) 'Error: shallow and deep cannot both be set .TRUE.'
      stop
    endif

    !If shallow or deep have been specified, edit the mask to remove all nodes
    !  that are not within the specified depth
    if(shallow .OR. deep)then
      do j=1,uj
        do i=1,vi
          if( (shallow .AND. abs(romdepth(i,j)) > sdepth) .OR. &
              (deep    .AND. abs(romdepth(i,j)) < ddepth)  ) mask_rho(i,j) = 0
        enddo
      enddo
    endif

    !Make a second mask for Salt,Temp,etc. without problematic river ends
    mask_rvr = mask_rho
    do k=1,numRiv*2-1,2
      do i=rivLocs(k)-((sqSize-1)/2),rivLocs(k)+((sqSize-1)/2)
        do j=rivLocs(k+1)-((sqSize-1)/2),rivLocs(k+1)+((sqSize-1)/2)
          mask_rvr(i,j) = 0
        enddo
      enddo
    enddo

    !If outputting volumes at each rho element with water, determine
    !  which elements contain water and store their id in wetEles
    if(vol_col_on)then
      !iterate through the rho elements and sum up the
      !  number of elements with at least 1 wet node
      wet_elements = 0
      do i=1,vi-1
        do j=1,uj-1
          if(mask_rho(i+1,j+1)==1 .OR. mask_rho(i,j+1)==1 .OR.       &
             mask_rho(i+1,j)==1 .OR. mask_rho(i,j)==1)               &
               wet_elements = wet_elements + 1
        enddo
      enddo

      ALLOCATE(wetEles(wet_elements))

      k = 0
      count = 0
      do i=1,vi-1
        do j=1,uj-1
          count = count + 1
          if(mask_rho(i+1,j+1)==1 .OR. mask_rho(i,j+1)==1 .OR.       &
             mask_rho(i+1,j)==1 .OR. mask_rho(i,j)==1) then
             k = k + 1
             wetEles(k) = count
          endif
        enddo
        count = count + 1
      enddo
    endif

    ! Assign mask values to rho nodes 
    count = 0
    do j=1,uj
      do i=1,vi
        count = count + 1    !move to next node number
            !cycles through each variable replacing the vi,uj part with count
            !  essentially giving it node numbers
        rho_mask(count) = mask_rho(i,j)
        rvr_mask(count) = mask_rvr(i,j)
      enddo
    enddo

    !Get Forms of all Elements
    call getForms()
    call getRvrForms()

    ! **************************** CREATE ELEMENTS ****************************

    write(*,*) 'create elements'

    ! Create matrix that contains the node numbers for each rho element
    count = 0
    do j=1,uj-1
      do i=1,vi-1
        count = count + 1
        RE(1,count) = i + (j-1)*vi
        RE(2,count) = i + 1 + (j-1)*vi
        RE(3,count) = i + 1 + j*vi
        RE(4,count) = i + j*vi
        form(count) = ij_form(i,j)
        rvr_form(count) = rvr_ij_form(i,j)
      enddo
    enddo

    ! Create matrices of  x/y for rho nodes and depth values in rho node number format 
    count = 0
    do j=1,uj
      do i=1,vi
        count = count + 1
        !cycles through each variable replacing the vi,uj part with count
        !  essentially giving it node numbers
        rx(count) = x_rho(i,j)
        ry(count) = y_rho(i,j)
        rlon(count) = lon_rho(i,j)
        rlat(count) = lat_rho(i,j)
        depth(count) = (-1.0)*romdepth(i,j)
      enddo
    enddo

    ! Create matrices of x/y node values for each rho element
    do j=1,rho_elements
      do i=1,4
        r_ele_x(i,j) = rx(RE(i,j))
        r_ele_y(i,j) = ry(RE(i,j))
      enddo
    enddo

    ALLOCATE(zeta(rho_nodes))
    ALLOCATE(salt(rho_nodes,us))
    ALLOCATE(temp(rho_nodes,us))
    ALLOCATE(disO(rho_nodes,us))

  END SUBROUTINE init_Grid

  SUBROUTINE fin_Grid()
  USE HYDRO_DATA_MOD
  IMPLICIT NONE

    !Deallocate everything
    DEALLOCATE(rho_mask,rvr_mask,form,rvr_form)
    DEALLOCATE(mask_rho,mask_rvr,ij_form,rvr_ij_form,RE)
    DEALLOCATE(depth,rx,ry,rlon,rlat)
    DEALLOCATE(r_ele_x,r_ele_y)

    DEALLOCATE(zeta)
    DEALLOCATE(salt)
    DEALLOCATE(temp)
    DEALLOCATE(disO)

    DEALLOCATE(SC,CS)

    IF(ALLOCATED(cats))DEALLOCATE(cats)

  END SUBROUTINE fin_Grid


  SUBROUTINE init_Hydro()
  !This Subroutine reads in the hydrodynamic information for the first iteration
  USE HYDRO_DATA_MOD
  USE PARAM_MOD, ONLY: prefix,suffix,filenum,son,ton,don,vi,uj,us
  USE netcdf
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

    INTEGER :: STATUS,NCID,VID

    !Used for reading in NetCDF variables one time step at a time
    INTEGER :: STARTr(4),COUNTr(4),STARTz(3),COUNTz(3)

    INTEGER :: i,j,k,count,counter
    REAL, ALLOCATABLE, DIMENSION(:,:,:)   :: romZ
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: romS,romT,romD

    ALLOCATE(romZ(vi,uj,1))
    ALLOCATE(romS(vi,uj,us,1))
    ALLOCATE(romT(vi,uj,us,1))
    ALLOCATE(romD(vi,uj,us,1))

    !Open netCDF file
    iint = 0
    counter=iint+filenum     ! 177 --> June 26,1995
    WRITE(filenm,'(A,I4.4,A)') prefix,counter,suffix
    write(*,*) TRIM(filenm)

    step=1    !Time step is 1st time step of file

    ! Read in data for first three external time steps
    STATUS = NF90_OPEN(TRIM(filenm), NF90_NOWRITE, NCID)
    if (STATUS .NE. NF90_NOERR) write(*,*) 'Problem NF90_OPEN'
    !write(*,*) 'ncid = ', ncid

    startz(1)=1
    startz(2)=1
    startz(3)=step

    countz(1)=vi
    countz(2)=uj
    countz(3)=1

    ! **** Zeta ****
    STATUS = NF90_INQ_VARID(NCID,'zeta',VID)
    STATUS = NF90_GET_VAR(NCID,VID,romZ,STARTz,COUNTz)
    if (STATUS .NE. NF90_NOERR) write(*,*) 'Problem read initial zeta array'

    startr(1)=1
    startr(2)=1
    startr(3)=1
    startr(4)=step

    countr(1)=vi
    countr(2)=uj
    countr(3)=us
    countr(4)=1

    !Ensure Salt, Temp, and DO are zero if not read in.
    romS = 0.0
    romT = 0.0
    romD = 0.0

    ! **** Salt ****
    if(son) then
      STATUS = NF90_INQ_VARID(NCID,'salt',VID)
      STATUS = NF90_GET_VAR(NCID,VID,romS,STARTr,COUNTr)
      if (STATUS .NE. NF90_NOERR) write(*,*) 'Problem read initial salt array'
    endif

    ! **** Temp ****
    if(ton) then
      STATUS = NF90_INQ_VARID(NCID,'temp',VID)
      STATUS = NF90_GET_VAR(NCID,VID,romT,STARTr,COUNTr)
      if (STATUS .NE. NF90_NOERR) write(*,*) 'Problem read initial temp array'
    endif

    ! **** Dissolved Oxygen ****
    if(don)then
      STATUS = NF90_INQ_VARID(NCID,'oxygen',VID)
      STATUS = NF90_GET_VAR(NCID,VID,romD,STARTr,COUNTr)
      if (STATUS .NE. NF90_NOERR) write(*,*) 'Problem read initial DO array'
    endif

  !close the dataset and reassign the NCID
  STATUS = NF90_CLOSE(NCID)

  !Reshape input to fit node numbers assigned to elements
  salt = 0.0
  temp = 0.0
  disO = 0.0
  count = 0
  do j=1,uj
    do i=1,vi
      count = count + 1
      do k=1,us
        if(son) salt(count,k) = romS(i,j,k,1) * DBLE(mask_rvr(i,j))
        if(ton) temp(count,k) = romT(i,j,k,1) * DBLE(mask_rvr(i,j))
        if(don) disO(count,k) = romD(i,j,k,1) * DBLE(mask_rvr(i,j))
      enddo
      zeta(count) = (-1.0)*romZ(i,j,1) * DBLE(mask_rho(i,j))
    enddo
  enddo

  DEALLOCATE(romZ,romS,romT,romD)

  END SUBROUTINE init_Hydro


  SUBROUTINE update_Hydro()
  USE HYDRO_DATA_MOD
  USE PARAM_MOD, ONLY: prefix,suffix,filenum,son,ton,don,vi,uj,us,tdim
  USE netcdf
  IMPLICIT NONE

  INCLUDE 'netcdf.inc'

    INTEGER :: STATUS,NCID,VID

    !Used for reading in NetCDF variables one time step at a time
    INTEGER :: STARTr(4),COUNTr(4),STARTz(3),COUNTz(3)

    INTEGER :: i,j,k,count,counter
    REAL, ALLOCATABLE, DIMENSION(:,:,:)   :: romZ
    REAL, ALLOCATABLE, DIMENSION(:,:,:,:) :: romS,romT,romD

    ALLOCATE(romZ(vi,uj,1))
    ALLOCATE(romS(vi,uj,us,1))
    ALLOCATE(romT(vi,uj,us,1))
    ALLOCATE(romD(vi,uj,us,1))

    !if the current input file is not yet finished, just increment stepf to the next time step
    IF (step .LT. tdim) THEN

      step=step+1

    ELSE
    !if the current input file is finished, update filnm to next input file, and reset stepf to 1

      !Open netCDF file
      iint = iint+1
      counter=iint+filenum   !176 + 1 = 177 --> June 26,1995
      WRITE(filenm,'(A,I4.4,A)') prefix,counter,suffix

      write(*,*) TRIM(filenm)

      step = 1

    ENDIF

    !Ensure Salt, Temp, and DO are zero if not read in.
    romS = 0.0
    romT = 0.0
    romD = 0.0

    ! Read in forward time step data 
    STATUS = NF90_OPEN(TRIM(filenm), NF90_NOWRITE, NCID)
    if (STATUS .NE. NF90_NOERR) write(*,*) 'Problem NF90_OPEN'
    !write(*,*) 'ncid = ', ncid

    startz(1)=1
    startz(2)=1
    startz(3)=step

    countz(1)=vi
    countz(2)=uj
    countz(3)=1

    ! **** Zeta ****
    STATUS = NF90_INQ_VARID(NCID,'zeta',VID)
    STATUS = NF90_GET_VAR(NCID,VID,romZ,STARTz,COUNTz)
    if (STATUS .NE. NF90_NOERR) write(*,*) 'Problem read zeta array'

    startr(1)=1
    startr(2)=1
    startr(3)=1
    startr(4)=step

    countr(1)=vi
    countr(2)=uj
    countr(3)=us
    countr(4)=1

    if(son)then
      ! **** Salt ****
      STATUS = NF90_INQ_VARID(NCID,'salt',VID)
      STATUS = NF90_GET_VAR(NCID,VID,romS,STARTr,COUNTr)
      if (STATUS .NE. NF90_NOERR) write(*,*) 'Problem read salt array'
    endif

    if(ton)then
      ! **** Temp ****
      STATUS = NF90_INQ_VARID(NCID,'temp',VID)
      STATUS = NF90_GET_VAR(NCID,VID,romT,STARTr,COUNTr)
      if (STATUS .NE. NF90_NOERR) write(*,*) 'Problem read temp array'
    endif

    if(don)then
      ! **** Dissolved Oxygen ****
      STATUS = NF90_INQ_VARID(NCID,'oxygen',VID)
      STATUS = NF90_GET_VAR(NCID,VID,romD,STARTr,COUNTr)
      if (STATUS .NE. NF90_NOERR) write(*,*) 'Problem read DO array'
    endif

    !close the dataset and reassign the NCID
    STATUS = NF90_CLOSE(NCID)


    !Reshape input to fit node numbers assigned to elements
    count = 0
    do j=1,uj
      do i=1,vi
        count = count + 1
        do k=1,us    
          if(son) salt(count,k) = romS(i,j,k,1) * DBLE(mask_rvr(i,j))
          if(ton) temp(count,k) = romT(i,j,k,1) * DBLE(mask_rvr(i,j))
          if(don) disO(count,k) = romD(i,j,k,1) * DBLE(mask_rvr(i,j))
        enddo
        zeta(count) = (-1.0) * romZ(i,j,1) * DBLE(mask_rvr(i,j))
      enddo
    enddo

  DEALLOCATE(romZ,romS,romT,romD)

  write(*,*) 'existing matrix, time step=',step

  END SUBROUTINE update_Hydro

!Based on code found in createBounds from Boundary Module of LTRANS
SUBROUTINE getForms()
  USE HYDRO_DATA_MOD, ONLY: ij_form,mask_rho
  USE PARAM_MOD, ONLY:ui,uj,vi,vj

!The subroutine does the following things:
!  1) Determines the form of each rho element
!    a) Elements consist of the rho nodes:  (i+1,j) --- (i+1,j+1)
!                                              |            |
!                                            (i,j)  ---  (i,j+1)
!
!    b) Element form is based on how the four nodes in the element are masked
!
!    c) If any elements exist where their form contains water and land crossing
!         diagonally, which are referred to as 'crosses' then these forms must
!         be solved as to which direction the boundarys are going through them
!

  !The following variables are the same as the similarly named variables
  INTEGER :: ipos1,jpos1,ipos2,jpos2,dir1,dir2  ! above, only these are
  LOGICAL :: wf1, wf2                           ! used for solving crosses

  INTEGER :: i,j,STATUS,m,crossnum,oldcrossnum
  LOGICAL :: found, deadend1, deadend2

    !*****************************************
    !*              GET FORMS                *
    !*****************************************

  !This section determines the form of each rho element
  !
  !Remember that:
  !  a) Elements consist of the rho nodes:  (i+1,j) --- (i+1,j+1)
  !                                            |            |
  !                                          (i,j)  ---  (i,j+1)
  !
  !  b) Element form is based on how the four nodes in the element are masked
  !
  !  c) If any elements exist where their form contains water and land crossing
  !       diagonally, which are referred to as 'crosses' then these forms must
  !       be solved as to which direction the boundarys are going through them

!  write(*,*) 'Getting Element Forms'
!  write(*,*) ' '

  crossnum = 0     !Initialize 'cross' element counter to 0

  do i=1,vi-1
    do j=1,uj-1

      !Determine the form of the current element
      ij_form(i,j) = 0
      if(mask_rho(i  ,j+1) == 0) ij_form(i,j) = ij_form(i,j) + 1
      if(mask_rho(i  ,j  ) == 0) ij_form(i,j) = ij_form(i,j) + 2
      if(mask_rho(i+1,j+1) == 0) ij_form(i,j) = ij_form(i,j) + 4
      if(mask_rho(i+1,j  ) == 0) ij_form(i,j) = ij_form(i,j) + 8

      SELECT CASE(ij_form(i,j))

        CASE(6,9) !ud
          !Elements with forms 6 or 9 are 'crosses', increment the cross counter
          crossnum = crossnum + 1

      END SELECT

    enddo
  enddo

        !*********************************
        !*         SOLVE CROSSES         *
        !*********************************

  oldcrossnum = 0  !Initialize oldcrossnum to 0
                   !This variable is to know if an endless loop has occurred

  if(crossnum > 0) then
!    write(*,*) '  Crosses Exist... Now Solving'
!    write(*,*) ' '
  endif

  do 
    if(crossnum == 0) exit             !If no crosses exist unsolved, exit
    if(oldcrossnum == crossnum)then    !If crosses still exist, but none were
!      write(*,*)'  Cross loop - using defaults' !  solved in prior loop
!      write(*,*)' '                             !  use defaults of 16 & 19
      do i=1,vi-1
        do j=1,uj-1
          if(ij_form(i,j) == 6) ij_form(i,j) = 16
          if(ij_form(i,j) == 9) ij_form(i,j) = 19
        enddo
      enddo
    endif
    oldcrossnum = crossnum

    do i=1,vi-1
      if(crossnum == 0)exit            !If no crosses exist unsolved, exit

      do j=1,uj-1
        if(crossnum == 0)exit          !If no crosses exist unsolved, exit


        if(ij_form(i,j) == 6) then
          !Form 6 has water nodes in the top left and bottom right.
          !  Boundary edges travelling clockwise around water therefore
          !  enter the element from top or bottom and exit left or right


          if(i==1 .OR. j==uj-1 .OR. i==vi-1 .OR. j==1)then
            !if the cross is in an element on the edge of the rho grid,
            !  then the boundary will either enter the top and exit left,
            !  or enter the bottom and exit right
            ij_form(i,j) = 17 
            crossnum = crossnum - 1
          else


            !if its not on the edge of the grid it will have to be solved
            !  the hard way.  This is done by exiting the two possible
            !  exits (left & right) and following the boundaries until
            !  either, one returns to this cross or, both hit another
            !  unsolved cross.  If one returns to this cross then this 
            !  cross is solved.  If neither direction returns to this 
            !  cross, skip this cross.
            !  As other crosses are solved this one becomes more likely
            !  to find a path back to itself.

            ipos1 = i        !initialize two paths to the cross location
            jpos1 = j
            ipos2 = i
            jpos2 = j
            wf1 = .false.    !initialize wf & deadend variables to false
            wf2 = .false.
            deadend1 = .false.
            deadend2 = .false.

            ! Initialized ipos1,jpos1 to the element Left of the cross
            if(jpos1 == 2)then         !If element to left is on edge
              wf1 = .true.             !  switch wf to TRUE and move up
              jpos1 = jpos1 - 1
              ipos1 = ipos1 + 1
              dir1 = 8
              if(ipos1 == vi-1)then    !If element to left and up from
                jpos1 = jpos1 + 1      !  cross is the corner, move right
                dir1 = 6               !  from corner
              endif
            else
              jpos1 = jpos1 - 1        !Else just move left from cross
              dir1 = 4
            endif
      
            ! Initialized ipos2,jpos2 to the element Right of the cross
            if(jpos2 == uj-2)then      !If element to right is on edge
              wf2 = .true.             !  switch wf to TRUE and move down
              jpos2 = jpos2 + 1
              ipos2 = ipos2 - 1
              dir2 = 2
              if(ipos2 == 1)then       !If element to right and down from
                jpos2 = jpos2 - 1      !  cross is corner, move left from
                dir2 = 4               !  corner
              endif
            else
              jpos2 = jpos2 + 1        !Else just move right from cross
              dir2 = 6
            endif

            do
              if(ipos1 == i .AND. jpos1 == j)then     !If left path returned to
                if(dir1 == 2)then                     !  cross, set new form
                  ij_form(i,j) = 16                   !  based on the direction
                elseif(dir1 == 8)then                 !  the path returns from
                  ij_form(i,j) = 17
                else
                  write(*,*)'Problem Form 6 Left Solution'
                endif
                crossnum = crossnum - 1               !  and decrement crossnum
                exit

              elseif(ipos2 == i .AND. jpos2 == j)then !If right path
                if(dir2 == 2)then                     !  returned to cross, set
                  ij_form(i,j) = 17                   !  new form based on the
                elseif(dir2 == 8)then                 !  direction the path
                  ij_form(i,j) = 16                   !  returns from
                else
                  write(*,*)'Problem Form 6 Right Solution'
                endif
                crossnum = crossnum - 1               !  and decrement crossnum
                exit
              endif

              !if the left path has hit a dead end, switch deadend1 to TRUE
              if(ij_form(ipos1,jpos1)==6 .OR. ij_form(ipos1,jpos1)==9) deadend1 = .true.

              !if the right path has hit a dead end, switch deadend2 to TRUE
              if(ij_form(ipos2,jpos2)==6 .OR. ij_form(ipos2,jpos2)==9) deadend2 = .true.

              !if both paths have hit a dead end, this cross cannot be
              !  solved yet, so move on and come back to it later
              if(deadend1 .AND. deadend2) exit
        
              !if left path has not hit dead end, move to next point on path
              if(.NOT. deadend1) CALL getNext(ipos1,jpos1,wf1,dir1,ij_form(ipos1,jpos1))

              !if right path has not hit dead end, move to next point on path
              if(.NOT. deadend2) CALL getNext(ipos2,jpos2,wf2,dir2,ij_form(ipos2,jpos2))

            enddo

          endif




        elseif(ij_form(i,j) == 9)then
          !Form 9 has water nodes in the bottom left and top right.
          !  Boundary edges travelling clockwise around water therefore
          !  enter the element from left or right and exit top or bottom


          if(i==1 .OR. j==1 .OR. i==vi-1 .OR. j==uj-1)then
            !if the cross is in an element on the edge of the rho grid,
            !  then the boundary will either enter right and exit the top,
            !  or enter left and exit the bottom
            ij_form(i,j) = 18
            crossnum = crossnum - 1


            !if its not on the edge of the grid it will have to be solved
            !  the hard way.  This is done by exiting the two possible
            !  exits (up & down) and following the boundaries until
            !  either, one returns to this cross or, both hit another
            !  unsolved cross.  If one returns to this cross then this 
            !  cross is solved.  If neither direction returns to this 
            !  cross, skip this cross.
            !  As other crosses are solved this one becomes more likely
            !  to find a path back to itself.
          else
            ipos1 = i        !initialize two paths to the cross location
            jpos1 = j
            ipos2 = i
            jpos2 = j
            wf1 = .false.    !initialize wf & deadend variables to false
            wf2 = .false.
            deadend1 = .false.
            deadend2 = .false.


            ! Initialized ipos1,jpos1 to the element Above the cross
            if(ipos1 == vi-2)then      !If element above is on edge
              wf1 = .true.             !  switch wf to TRUE & move right
              ipos1 = ipos1 + 1
              jpos1 = jpos1 + 1
              dir1 = 6
              if(jpos1 == uj-1)then    !If element above and to right
                ipos1 = ipos1 - 1      !  of cross is the corner, move
                dir1 = 2               !  down from corner
              endif
            else
              ipos1 = ipos1 + 1        !Else just move above the cross
              dir1 = 8
            endif


            ! Initialized ipos1,jpos1 to the element Below the cross
            if(ipos2 == 2)then         !If element below is on edge
              wf2 = .true.             !  switch wf to TRUE & move left
              ipos2 = ipos2 - 1
              jpos2 = jpos2 - 1
              if(jpos2 == 1)then       !If element below and left of
                ipos2 = ipos2 + 1      !  cross is the corner, move
                dir2 = 8               !  up from corner
              else
                dir2 = 4
              endif
            else
              ipos2 = ipos2 - 1        !Else just move below the cross
              dir2 = 2
            endif


            do
              if(ipos1 == i .AND. jpos1 == j)then     !If up path returned to
                if(dir1 == 4)then                     !  cross, set new form
                  ij_form(i,j) = 19                   !  based on the direction
                elseif(dir1 == 6)then                 !  the path returns from
                  ij_form(i,j) = 18
                else
                  write(*,*)'Problem Form 9 Up Solution'
                endif
                crossnum = crossnum - 1               !  and decrement crossnum
                exit

              elseif(ipos2 == i .AND. jpos2 == j)then !If down path returned
                if(dir2 == 4)then                     !  to cross, set new form
                  ij_form(i,j) = 18                   !  based on the direction
                elseif(dir2 == 6)then                 !  the path returns from
                  ij_form(i,j) = 19
                else
                  write(*,*)'Problem Form 9 Down Solution'
                endif
                crossnum = crossnum - 1               !  and decrement crossnum
                exit
              endif

              !if the up path has hit a dead end, switch deadend1 to TRUE
              if(ij_form(ipos1,jpos1)==6 .OR. ij_form(ipos1,jpos1)==9) deadend1 = .true.

              !if the down path has hit a dead end, switch deadend2 to TRUE
              if(ij_form(ipos2,jpos2)==6 .OR. ij_form(ipos2,jpos2)==9) deadend2 = .true.
 
              !if both paths have hit a dead end, this cross cannot be
              !  solved yet, so move on and come back to it later
              if(deadend1 .AND. deadend2) exit

              !if up path has not hit dead end, move to next point on path
              if(.NOT. deadend1) CALL getNext(ipos1,jpos1,wf1,dir1,ij_form(ipos1,jpos1))

              !if down path has not hit dead end, move to next point on path
              if(.NOT. deadend2) CALL getNext(ipos2,jpos2,wf2,dir2,ij_form(ipos2,jpos2))

            enddo

          endif
        endif
      enddo
    enddo

  enddo

  if(oldcrossnum > 0) then
!    write(*,*) '  Crosses Solved'
!    write(*,*) ' '
  endif

END SUBROUTINE getForms


!Basically Identical to getForms only uses mask_rvr instead of mask_rho and
!  creates rvr_ij_form instead of ij_form
SUBROUTINE getRvrForms()
  USE HYDRO_DATA_MOD, ONLY:rvr_ij_form,mask_rvr
  USE PARAM_MOD, ONLY:ui,uj,vi,vj

!The subroutine does the following things:
!  1) Determines the form of each rho element
!    a) Elements consist of the rho nodes:  (i+1,j) --- (i+1,j+1)
!                                              |            |
!                                            (i,j)  ---  (i,j+1)
!
!    b) Element form is based on how the four nodes in the element are masked
!
!    c) If any elements exist where their form contains water and land crossing
!         diagonally, which are referred to as 'crosses' then these forms must
!         be solved as to which direction the boundarys are going through them
!

  !The following variables are the same as the similarly named variables
  INTEGER :: ipos1,jpos1,ipos2,jpos2,dir1,dir2   ! above, only these are
  LOGICAL :: wf1, wf2                            ! used for solving crosses

  INTEGER :: i,j,STATUS,m,crossnum,oldcrossnum
  LOGICAL :: found, deadend1, deadend2

    !*****************************************
    !*              GET FORMS                *
    !*****************************************

  !This section determines the form of each rho element
  !
  !Remember that:
  !  a) Elements consist of the rho nodes:  (i+1,j) --- (i+1,j+1)
  !                                            |            |
  !                                          (i,j)  ---  (i,j+1)
  !
  !  b) Element form is based on how the four nodes in the element are masked
  !
  !  c) If any elements exist where their form contains water and land crossing
  !       diagonally, which are referred to as 'crosses' then these forms must
  !       be solved as to which direction the boundarys are going through them

!  write(*,*) 'Getting Element Forms'
!  write(*,*) ' '

  crossnum = 0     !Initialize 'cross' element counter to 0

  do i=1,vi-1
    do j=1,uj-1

      !Determine the form of the current element
      rvr_ij_form(i,j) = 0
      if(mask_rvr(i  ,j+1) == 0) rvr_ij_form(i,j) = rvr_ij_form(i,j) + 1
      if(mask_rvr(i  ,j  ) == 0) rvr_ij_form(i,j) = rvr_ij_form(i,j) + 2
      if(mask_rvr(i+1,j+1) == 0) rvr_ij_form(i,j) = rvr_ij_form(i,j) + 4
      if(mask_rvr(i+1,j  ) == 0) rvr_ij_form(i,j) = rvr_ij_form(i,j) + 8

      SELECT CASE(rvr_ij_form(i,j))

        CASE(6,9) !ud
          !Elements with forms 6 or 9 are 'crosses', increment the cross counter
          crossnum = crossnum + 1

      END SELECT

    enddo
  enddo

        !*********************************
        !*         SOLVE CROSSES         *
        !*********************************

  oldcrossnum = 0  !Initialize oldcrossnum to 0
                   !This variable is to know if an endless loop has occurred

  if(crossnum > 0) then
!    write(*,*) '  Crosses Exist... Now Solving'
!    write(*,*) ' '
  endif

  do 
    if(crossnum == 0) exit             !If no crosses exist unsolved, exit
    if(oldcrossnum == crossnum)then    !If crosses still exist, but none were
!      write(*,*)'  Cross loop - using defaults' !  solved in prior loop
!      write(*,*)' '                             !  use defaults of 16 & 19
      do i=1,vi-1
        do j=1,uj-1
          if(rvr_ij_form(i,j) == 6) rvr_ij_form(i,j) = 16
          if(rvr_ij_form(i,j) == 9) rvr_ij_form(i,j) = 19
        enddo
      enddo
    endif
    oldcrossnum = crossnum

    do i=1,vi-1
      if(crossnum == 0)exit            !If no crosses exist unsolved, exit

      do j=1,uj-1
        if(crossnum == 0)exit          !If no crosses exist unsolved, exit


        if(rvr_ij_form(i,j) == 6) then
          !Form 6 has water nodes in the top left and bottom right.
          !  Boundary edges travelling clockwise around water therefore
          !  enter the element from top or bottom and exit left or right


          if(i==1 .OR. j==uj-1 .OR. i==vi-1 .OR. j==1)then
            !if the cross is in an element on the edge of the rho grid,
            !  then the boundary will either enter the top and exit left,
            !  or enter the bottom and exit right
            rvr_ij_form(i,j) = 17 
            crossnum = crossnum - 1
          else


            !if its not on the edge of the grid it will have to be solved
            !  the hard way.  This is done by exiting the two possible
            !  exits (left & right) and following the boundaries until
            !  either, one returns to this cross or, both hit another
            !  unsolved cross.  If one returns to this cross then this 
            !  cross is solved.  If neither direction returns to this 
            !  cross, skip this cross.
            !  As other crosses are solved this one becomes more likely
            !  to find a path back to itself.

            ipos1 = i        !initialize two paths to the cross location
            jpos1 = j
            ipos2 = i
            jpos2 = j
            wf1 = .false.    !initialize wf & deadend variables to false
            wf2 = .false.
            deadend1 = .false.
            deadend2 = .false.

            ! Initialized ipos1,jpos1 to the element Left of the cross
            if(jpos1 == 2)then         !If element to left is on edge
              wf1 = .true.             !  switch wf to TRUE and move up
              jpos1 = jpos1 - 1
              ipos1 = ipos1 + 1
              dir1 = 8
              if(ipos1 == vi-1)then    !If element to left and up from
                jpos1 = jpos1 + 1      !  cross is the corner, move right
                dir1 = 6               !  from corner
              endif
            else
              jpos1 = jpos1 - 1        !Else just move left from cross
              dir1 = 4
            endif
      
            ! Initialized ipos2,jpos2 to the element Right of the cross
            if(jpos2 == uj-2)then      !If element to right is on edge
              wf2 = .true.             !  switch wf to TRUE and move down
              jpos2 = jpos2 + 1
              ipos2 = ipos2 - 1
              dir2 = 2
              if(ipos2 == 1)then       !If element to right and down from
                jpos2 = jpos2 - 1      !  cross is corner, move left from
                dir2 = 4               !  corner
              endif
            else
              jpos2 = jpos2 + 1        !Else just move right from cross
              dir2 = 6
            endif

            do
              if(ipos1 == i .AND. jpos1 == j)then     !If left path returned to
                if(dir1 == 2)then                     !  cross, set new form
                  rvr_ij_form(i,j) = 16               !  based on the direction
                elseif(dir1 == 8)then                 !  the path returns from
                  rvr_ij_form(i,j) = 17
                else
                  write(*,*)'Problem Form 6 Left Solution'
                endif
                crossnum = crossnum - 1               !  and decrement crossnum
                exit

              elseif(ipos2 == i .AND. jpos2 == j)then !If right path
                if(dir2 == 2)then                     !  returned to cross, set
                  rvr_ij_form(i,j) = 17               !  new form based on the
                elseif(dir2 == 8)then                 !  direction the path
                  rvr_ij_form(i,j) = 16               !  returns from
                else
                  write(*,*)'Problem Form 6 Right Solution'
                endif
                crossnum = crossnum - 1               !  and decrement crossnum
                exit
              endif

              !if the left path has hit a dead end, switch deadend1 to TRUE
              if(rvr_ij_form(ipos1,jpos1)==6 .OR. rvr_ij_form(ipos1,jpos1)==9) deadend1 = .true.

              !if the right path has hit a dead end, switch deadend2 to TRUE
              if(rvr_ij_form(ipos2,jpos2)==6 .OR. rvr_ij_form(ipos2,jpos2)==9) deadend2 = .true.

              !if both paths have hit a dead end, this cross cannot be
              !  solved yet, so move on and come back to it later
              if(deadend1 .AND. deadend2) exit
        
              !if left path has not hit dead end, move to next point on path
              if(.NOT. deadend1) CALL getNext(ipos1,jpos1,wf1,dir1,rvr_ij_form(ipos1,jpos1))

              !if right path has not hit dead end, move to next point on path
              if(.NOT. deadend2) CALL getNext(ipos2,jpos2,wf2,dir2,rvr_ij_form(ipos2,jpos2))

            enddo

          endif




        elseif(rvr_ij_form(i,j) == 9)then
          !Form 9 has water nodes in the bottom left and top right.
          !  Boundary edges travelling clockwise around water therefore
          !  enter the element from left or right and exit top or bottom


          if(i==1 .OR. j==1 .OR. i==vi-1 .OR. j==uj-1)then
            !if the cross is in an element on the edge of the rho grid,
            !  then the boundary will either enter right and exit the top,
            !  or enter left and exit the bottom
            rvr_ij_form(i,j) = 18
            crossnum = crossnum - 1


            !if its not on the edge of the grid it will have to be solved
            !  the hard way.  This is done by exiting the two possible
            !  exits (up & down) and following the boundaries until
            !  either, one returns to this cross or, both hit another
            !  unsolved cross.  If one returns to this cross then this 
            !  cross is solved.  If neither direction returns to this 
            !  cross, skip this cross.
            !  As other crosses are solved this one becomes more likely
            !  to find a path back to itself.
          else
            ipos1 = i        !initialize two paths to the cross location
            jpos1 = j
            ipos2 = i
            jpos2 = j
            wf1 = .false.    !initialize wf & deadend variables to false
            wf2 = .false.
            deadend1 = .false.
            deadend2 = .false.


            ! Initialized ipos1,jpos1 to the element Above the cross
            if(ipos1 == vi-2)then      !If element above is on edge
              wf1 = .true.             !  switch wf to TRUE & move right
              ipos1 = ipos1 + 1
              jpos1 = jpos1 + 1
              dir1 = 6
              if(jpos1 == uj-1)then    !If element above and to right
                ipos1 = ipos1 - 1      !  of cross is the corner, move
                dir1 = 2               !  down from corner
              endif
            else
              ipos1 = ipos1 + 1        !Else just move above the cross
              dir1 = 8
            endif


            ! Initialized ipos1,jpos1 to the element Below the cross
            if(ipos2 == 2)then         !If element below is on edge
              wf2 = .true.             !  switch wf to TRUE & move left
              ipos2 = ipos2 - 1
              jpos2 = jpos2 - 1
              if(jpos2 == 1)then       !If element below and left of
                ipos2 = ipos2 + 1      !  cross is the corner, move
                dir2 = 8               !  up from corner
              else
                dir2 = 4
              endif
            else
              ipos2 = ipos2 - 1        !Else just move below the cross
              dir2 = 2
            endif


            do
              if(ipos1 == i .AND. jpos1 == j)then     !If up path returned to
                if(dir1 == 4)then                     !  cross, set new form
                  rvr_ij_form(i,j) = 19               !  based on the direction
                elseif(dir1 == 6)then                 !  the path returns from
                  rvr_ij_form(i,j) = 18
                else
                  write(*,*)'Problem Form 9 Up Solution'
                endif
                crossnum = crossnum - 1               !  and decrement crossnum
                exit

              elseif(ipos2 == i .AND. jpos2 == j)then !If down path returned
                if(dir2 == 4)then                     !  to cross, set new form
                  rvr_ij_form(i,j) = 18               !  based on the direction
                elseif(dir2 == 6)then                 !  the path returns from
                  rvr_ij_form(i,j) = 19
                else
                  write(*,*)'Problem Form 9 Down Solution'
                endif
                crossnum = crossnum - 1               !  and decrement crossnum
                exit
              endif

              !if the up path has hit a dead end, switch deadend1 to TRUE
              if(rvr_ij_form(ipos1,jpos1)==6 .OR. rvr_ij_form(ipos1,jpos1)==9) deadend1 = .true.

              !if the down path has hit a dead end, switch deadend2 to TRUE
              if(rvr_ij_form(ipos2,jpos2)==6 .OR. rvr_ij_form(ipos2,jpos2)==9) deadend2 = .true.
 
              !if both paths have hit a dead end, this cross cannot be
              !  solved yet, so move on and come back to it later
              if(deadend1 .AND. deadend2) exit

              !if up path has not hit dead end, move to next point on path
              if(.NOT. deadend1) CALL getNext(ipos1,jpos1,wf1,dir1,rvr_ij_form(ipos1,jpos1))

              !if down path has not hit dead end, move to next point on path
              if(.NOT. deadend2) CALL getNext(ipos2,jpos2,wf2,dir2,rvr_ij_form(ipos2,jpos2))

            enddo

          endif
        endif
      enddo
    enddo

  enddo

  if(oldcrossnum > 0) then
!    write(*,*) '  Crosses Solved'
!    write(*,*) ' '
  endif

END SUBROUTINE getRvrForms

!Originally from Boundary Module
SUBROUTINE getNext(i,j,wf,dir,form)
  USE PARAM_MOD, ONLY: vi,uj
  !This subroutine is for finding the next element, following the bounds
  !  clockwise around water
  !This is all based on the element location, whether or not its on an 
  !  edge, the direction the path is coming from, and the element form
  IMPLICIT NONE

  INTEGER :: i,j,dir,form
  LOGICAL :: wf

  if(wf)then                 !If the element is on the edge
    if(dir == 2) then        !If path is going down,
      SELECT CASE(form)      !  then its on right edge

        CASE(0,1,4,5)        !If path stays on edge
          i = i - 1
          if(i == 2)then     !If path reaches bottom,
            dir = 4          !  then go left on bottom edge
            j = j - 1
          endif

        CASE DEFAULT         !Path exits edge
          wf = .false.

      END SELECT
    elseif(dir == 4) then    !If path is going left,
      SELECT CASE(form)      !  then its on bottom edge

        CASE(0,1,2,3)        !If path stays on edge
          j = j - 1
          if(j == 2)then     !If path reaches left edge,
            dir = 8          !  then go up on left edge
            i = i + 1
          endif

        CASE DEFAULT         !Path exits edge
          wf = .false.

      END SELECT
    elseif(dir == 6) then    !If path is going right,
      SELECT CASE(form)      !  then its on top edge

        CASE(0,4,8,12)       !If path stays on edge
          j = j + 1
          if(j == uj-2)then  !If path reaches right edge,
            dir = 2          !  then go down on right edge
            i = i - 1
          endif

        CASE DEFAULT         !Path exits edge
          wf = .false.

      END SELECT
    elseif(dir == 8) then    !If path is going up,
      SELECT CASE(form)      !  then its on left edge

        CASE(0,2,8,10)       !If path stays on edge
          i = i + 1
          if(i == vi-2)then  !If path reaches top edge,
            dir = 6          !  then go right on top edge
            j = j + 1
          endif

        CASE DEFAULT         !Path exits edge
          wf = .false.

      END SELECT
    else
      write(*,*) 'Error: wf direction not one of 2,4,6,8'
    endif





  else                       !If element is not on edge
    SELECT CASE(form)

    CASE(1,5,13)             !These forms exit element bottom
      if(i == 2)then         !if moving into bottom edge
        wf = .true.          !wf is true
        i = i - 1            !move down & left
        j = j - 1
        if(j == 1)then       !if moving to down left corner
          i = i + 1          !move up
          dir = 8
        else
          dir = 4
        endif
      else                   !if not moving to bottom edge
        i = i - 1            !just move down
        dir = 2
      endif

    CASE(2,3,7)              !These forms exit element left
      if(j == 2)then         !if moving into left edge
        wf = .true.          !wf is true
        j = j - 1            !move left and up
        i = i + 1
        if(i == vi-1)then    !if moving to top left corner
          j = j + 1          !move right
          dir = 6
        else
          dir = 8
        endif
      else                   !if not moving to left edge
        j = j - 1            !just move left
        dir = 4
      endif

    CASE(4,12,14)            !These forms exit element right
      if(j == uj-2)then      !if moving to right edge
        wf = .true.          !wf is true
        j = j + 1            !move right and down
        i = i - 1
        if(i == 1)then       !moving to down right corner
          j = j - 1          !move left
          dir = 4
        else
          dir = 2
        endif
      else                   !if not moving to right edge
        j = j + 1            !just move right
        dir = 6
      endif

    CASE(8,10,11)            !These forms exit element top
      if(i == vi-2)then      !if moving to top edge
        wf = .true.          !wf is true
        i = i + 1            !move up and right
        j = j + 1
        if(j == uj-1)then    !if moving to top right corner
          i = i - 1          !move down
          dir = 2
        else
          dir = 6
        endif
      else                   !if not moving to top edge
        i = i + 1            !just move up
        dir = 8
      endif

    CASE(16,17)              !These forms exit right or left
                             !16: u->r & d->l    17: d->r & u->l

      if((form == 16 .AND. dir == 2).OR. (form == 17 .AND. dir == 8))then !if exit right

        if(j == uj-2)then    !if moving to right edge
          wf = .true.        !wf is true
          j = j + 1          !move right and down
          i = i - 1
          if(i == 1)then     !moving to down right corner
            j = j - 1        !move left
            dir = 4
          else
            dir = 2
          endif
        else                 !if not moving to right edge
          j = j + 1          !just move right
          dir = 6
        endif
  
      else                   !if exit left

        if(j == 2)then       !if moving to left edge
          wf = .true.        !wf is true
          j = j - 1          !move left and up
          i = i + 1
          if(i == vi-1)then  !if moving to top left corner
            j = j + 1        !move right
            dir = 6
          else
            dir = 8
          endif
        else                 !if not moving to left edge
          j = j - 1          ! just move left
          dir = 4
        endif
  
      endif

    CASE(18,19)              !These forms exit up or down
                             !18: r->u & l->d    19: l->u & r->d

      if((form == 18 .AND. dir == 4).OR. (form == 19 .AND. dir == 6))then !if exit up

        if(i == vi-2)then    !if moving to top edge
          wf = .true.        !wf is true
          i = i + 1          !move up and right
          j = j + 1
          if(j == uj-1)then  !if moving to top right corner
            i = i - 1        !move down
            dir = 2
          else
            dir = 6
          endif
        else                 !if not moving to top edge
          i = i + 1          !just move up
          dir = 8
        endif

      else                   !if exit down

        if(i == 2)then       !if moving to bottom edge
          wf = .true.        !wf is true
          i = i - 1          !move down and left
          j = j - 1
          if(j == 1)then     !if moving to down left corner
            i = i + 1        !move up
            dir = 8
          else
            dir = 4
          endif
        else                 !if not moving to bottom edge
          i = i - 1          !just move down
          dir = 2
        endif

      endif

    END SELECT
  endif

END SUBROUTINE getNext

  DOUBLE PRECISION FUNCTION getSlevel(zeta,depth,i)
    !This function returns the depth of the current s-level
    USE HYDRO_DATA_MOD, ONLY: SC,CS
    USE PARAM_MOD, ONLY: hc,Vtransform
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: i
    DOUBLE PRECISION, INTENT(IN) :: zeta,depth

    DOUBLE PRECISION :: S,h

    ! convert negative depth to positive depth
    h = DBLE(-1.0) * depth


    SELECT CASE(Vtransform)

      CASE(1)  !Rutgers-ROMS formulation, eqn (1) of 
        !https://www.myroms.org/wiki/index.php/Vertical_S-coordinate

        S = hc*SC(i)+(h-hc)*CS(i)
        getSlevel = S+zeta*(DBLE(1.0)+S/h)

      CASE(2)  !UCLA-formulation, eqn(2) of 
        !https://www.myroms.org/wiki/index.php/Vertical_S-coordinate

        S = (hc*SC(i)+h*CS(i)) / (hc+h)
        getSlevel = zeta+(zeta+h)*S

      CASE(3)  !Song, Y. and D. B. Haidvogel, 1994: A semi-implicit
        !ocean circulation model using a generalized topography-following 
        !coordinate system, J. Comp. Phys., 115 (1), 228-244.

        getSlevel = zeta*(DBLE(1.0)+SC(i))+hc*SC(i)+(h-hc)*CS(i)

      CASE DEFAULT
        write(*,*) 'ERROR: Illegal Vtransform number'
        write(*,*) ' '
        write(*,*) 'The Program Cannot Continue and Will Terminate'
        stop

    END SELECT

  END FUNCTION getSlevel

END MODULE HYDRO_MOD