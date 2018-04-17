MODULE VOLUME_MOD 
IMPLICIT NONE 

  !  Counts each form scenario's occurence
  INTEGER ::  tCount(0:19,0:19)
  INTEGER ::  sCount(0:19,0:19)
  INTEGER ::  dCount(0:19,0:19)
  INTEGER :: stCount(0:19,0:19)
  INTEGER :: tdCount(0:19,0:19)
  INTEGER :: sdCount(0:19,0:19)
  INTEGER ::stdCount(0:19,0:19)
  !  Counts each occurence of multiple sections of ideal water at a node
  INTEGER :: tSecs(2:6),sSecs(2:6),dSecs(2:6)
  !  Counts the number of times two temp boundaries are found between two vertical nodes
  INTEGER :: vertdblcnt

  CHARACTER(LEN=40) :: fmt1,fmt2

CONTAINS

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  SUBROUTINE init_Output()
  USE PARAM_MOD, ONLY: s_vol_on,t_vol_on,d_vol_on,st_vol_on,sd_vol_on,         &
                       td_vol_on,std_vol_on,wet_elements,vol_col_on
  USE HYDRO_DATA_MOD, ONLY: rlon,rlat,RE,wetEles,nCat,cats
  USE CONSTRAINTS_MOD, ONLY: setGrowthCategories

    character(LEN=40) :: num,fname
    integer :: i,j

    !INITIALIZE COUNTING VARIABLES
    sCount   = 0
    tCount   = 0
    dCount   = 0
    stCount  = 0
    tdCount  = 0
    sdCount  = 0
    stdCount = 0
    tSecs    = 0
    sSecs    = 0
    dSecs    = 0
    vertdblcnt = 0

    write(num,*)wet_elements
    write(fmt1,*)"(A,',',",TRIM(adjustl(num)),"(',',F))"
    write(fmt2,*)"(I,',',",TRIM(adjustl(num)),"(',',F))"

    !      +-------------------------------+
    !      |       OPEN Output Files       |
    !      +-------------------------------+

    !Salinity Volume
    if(  s_vol_on)then
      OPEN(100,FILE='Salt_Volumes.csv',STATUS='REPLACE')
      write(100,"(A,',',A,',',A)")'time','vol','st vol'
      if(vol_col_on)then
        OPEN(300,FILE='Salt_Column_Volumes.csv',STATUS='REPLACE')
        write(300,TRIM(fmt1)) 'lon',(((rlon(RE(1,wetEles(i))) +      &
              rlon(RE(2,wetEles(i))) + rlon(RE(3,wetEles(i))) +      &
              rlon(RE(4,wetEles(i))) )/DBLE(4)), i=1,wet_elements)
        write(300,TRIM(fmt1)) 'lat',(((rlat(RE(1,wetEles(i))) +      &
              rlat(RE(2,wetEles(i))) + rlat(RE(3,wetEles(i))) +      &
              rlat(RE(4,wetEles(i))) )/DBLE(4)), i=1,wet_elements)
      endif
    endif
    !Temperature Volume
    if(  t_vol_on)then
      OPEN(101,FILE='Temp_Volumes.csv',STATUS='REPLACE')
      write(101,"(A,',',A,',',A)")'time','vol','tp vol'
      if(vol_col_on)then
        OPEN(301,FILE='Temp_Column_Volumes.csv',STATUS='REPLACE')
        write(301,TRIM(fmt1)) 'lon',(((rlon(RE(1,wetEles(i))) +      &
              rlon(RE(2,wetEles(i))) + rlon(RE(3,wetEles(i))) +      &
              rlon(RE(4,wetEles(i))) )/DBLE(4)), i=1,wet_elements)
        write(301,TRIM(fmt1)) 'lat',(((rlat(RE(1,wetEles(i))) +      &
              rlat(RE(2,wetEles(i))) + rlat(RE(3,wetEles(i))) +      &
              rlat(RE(4,wetEles(i))) )/DBLE(4)), i=1,wet_elements)
      endif
    endif
    !Dissolved Oxygen Volume
    if(  d_vol_on)then
      OPEN(102,FILE='DO_Volumes.csv',STATUS='REPLACE')
      write(102,"(A,',',A,',',A)")'time','vol','do vol'
      if(vol_col_on)then
        OPEN(302,FILE='DO_Column_Volumes.csv',STATUS='REPLACE')
        write(302,TRIM(fmt1)) 'lon',(((rlon(RE(1,wetEles(i))) +      &
              rlon(RE(2,wetEles(i))) + rlon(RE(3,wetEles(i))) +      &
              rlon(RE(4,wetEles(i))) )/DBLE(4)), i=1,wet_elements)
        write(302,TRIM(fmt1)) 'lat',(((rlat(RE(1,wetEles(i))) +      &
              rlat(RE(2,wetEles(i))) + rlat(RE(3,wetEles(i))) +      &
              rlat(RE(4,wetEles(i))) )/DBLE(4)), i=1,wet_elements)
      endif
    endif
    !Salinity & Temperature Combined Volume
    if( st_vol_on)then
      OPEN(103,FILE='StTp_Volumes.csv',STATUS='REPLACE')
      write(103,"(A,4(',',A))")'time','vol','st vol','tp vol','s&t vol'
      if(vol_col_on)then
        OPEN(303,FILE='StTp_Column_Volumes.csv',STATUS='REPLACE')
        write(303,TRIM(fmt1)) 'lon',(((rlon(RE(1,wetEles(i))) +      &
              rlon(RE(2,wetEles(i))) + rlon(RE(3,wetEles(i))) +      &
              rlon(RE(4,wetEles(i))) )/DBLE(4)), i=1,wet_elements)
        write(303,TRIM(fmt1)) 'lat',(((rlat(RE(1,wetEles(i))) +      &
              rlat(RE(2,wetEles(i))) + rlat(RE(3,wetEles(i))) +      &
              rlat(RE(4,wetEles(i))) )/DBLE(4)), i=1,wet_elements)
      endif
    endif
    !Salinity and Dissolved Oxygen Combined Volume
    if( sd_vol_on)then
      OPEN(104,FILE='StDo_Volumes.csv',STATUS='REPLACE')
      write(104,"(A,4(',',A))")'time','vol','st vol','do vol','s&d vol'
      if(vol_col_on)then
        OPEN(304,FILE='StDo_Column_Volumes.csv',STATUS='REPLACE')
        write(304,TRIM(fmt1)) 'lon',(((rlon(RE(1,wetEles(i))) +      &
              rlon(RE(2,wetEles(i))) + rlon(RE(3,wetEles(i))) +      &
              rlon(RE(4,wetEles(i))) )/DBLE(4)), i=1,wet_elements)
        write(304,TRIM(fmt1)) 'lat',(((rlat(RE(1,wetEles(i))) +      &
              rlat(RE(2,wetEles(i))) + rlat(RE(3,wetEles(i))) +      &
              rlat(RE(4,wetEles(i))) )/DBLE(4)), i=1,wet_elements)
      endif
    endif
    !Temperature and Dissolved Oxygen Combined Volume
    if( td_vol_on)then
      OPEN(105,FILE='TpDo_Volumes.csv',STATUS='REPLACE')
      write(105,"(A,4(',',A))")'time','vol','tp vol','do vol','t&d vol'
      if(vol_col_on)then
        OPEN(305,FILE='TpDo_Column_Volumes.csv',STATUS='REPLACE')
        write(305,TRIM(fmt1)) 'lon',(((rlon(RE(1,wetEles(i))) +      &
              rlon(RE(2,wetEles(i))) + rlon(RE(3,wetEles(i))) +      &
              rlon(RE(4,wetEles(i))) )/DBLE(4)), i=1,wet_elements)
        write(305,TRIM(fmt1)) 'lat',(((rlat(RE(1,wetEles(i))) +      &
              rlat(RE(2,wetEles(i))) + rlat(RE(3,wetEles(i))) +      &
              rlat(RE(4,wetEles(i))) )/DBLE(4)), i=1,wet_elements)
      endif
    endif
    !Salinity, Temperature, and Dissolved Oxygen Combined Volume
    if(std_vol_on)then
      OPEN(106,FILE='STD_Volumes.csv',STATUS='REPLACE')
      write(106,"(A,5(',',A))")'time','vol','st vol','tp vol','do vol','std vol'
      if(vol_col_on)then
        OPEN(306,FILE='STD_Column_Volumes.csv',STATUS='REPLACE')
        write(306,TRIM(fmt1)) 'lon',(((rlon(RE(1,wetEles(i))) +      &
              rlon(RE(2,wetEles(i))) + rlon(RE(3,wetEles(i))) +      &
              rlon(RE(4,wetEles(i))) )/DBLE(4)), i=1,wet_elements)
        write(306,TRIM(fmt1)) 'lat',(((rlat(RE(1,wetEles(i))) +      &
              rlat(RE(2,wetEles(i))) + rlat(RE(3,wetEles(i))) +      &
              rlat(RE(4,wetEles(i))) )/DBLE(4)), i=1,wet_elements)
      endif
    endif

    ! BioEnergetics Model Volumes
    call setGrowthCategories()
    if(nCat > 0)then
      do j=1,nCat
        write(num,*)j
        write(fname,*)"Category",TRIM(adjustl(num)),"_Volumes.csv"
        OPEN(1000+(2*j)-1,FILE=fname,STATUS='REPLACE')
        write(1000+(2*j)-1,"(A,',',A,',',A)")'time','vol','cat vol'
        if(vol_col_on)then
          write(num,*)j
          write(fname,*)"Category",TRIM(adjustl(num)),"_Column_Volumes.csv"
          OPEN(1000+(2*j),FILE='Salt_Column_Volumes.csv',STATUS='REPLACE')
          write(1000+(2*j),TRIM(fmt1)) 'lon',(((rlon(RE(1,wetEles(i))) +  &
                       rlon(RE(2,wetEles(i))) + rlon(RE(3,wetEles(i))) +  &
                       rlon(RE(4,wetEles(i))) )/DBLE(4)), i=1,wet_elements)
          write(1000+(2*j),TRIM(fmt1)) 'lat',(((rlat(RE(1,wetEles(i))) +  &
                       rlat(RE(2,wetEles(i))) + rlat(RE(3,wetEles(i))) +  &
                       rlat(RE(4,wetEles(i))) )/DBLE(4)), i=1,wet_elements)
        endif
      enddo
    endif

  END SUBROUTINE init_Output

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  SUBROUTINE fin_Output()
  USE PARAM_MOD, ONLY: s_vol_on,t_vol_on,d_vol_on,st_vol_on,sd_vol_on,         &
                       td_vol_on,std_vol_on,vol_col_on
  USE HYDRO_DATA_MOD, ONLY: nCat
  IMPLICIT NONE

  INTEGER :: i,j

    !OPEN(99,FILE='Sections.csv',STATUS='REPLACE')
    !WRITE(99,"( 5(A,','),A )")' ','Sec=2','Sec=3','Sec=4','Sec=5','Sec>5'

    !      +--------------------------------+
    !      |       CLOSE Output Files       |
    !      +--------------------------------+

    !Salinity Volume
    if(s_vol_on)then
      CLOSE(100)
      if(vol_col_on)CLOSE(300)

      !WRITE(99,"( A,5(',',I) )")'Salt',sSecs(2),sSecs(3), &
      !                        sSecs(4),sSecs(5),sSecs(6)

      OPEN(100,FILE='Salt_Matrix.csv',STATUS='REPLACE')
        do j=0,19
          WRITE(101,"(19(I,','),I)") (sCount(i,j), i=0,19)
        enddo
      CLOSE(100)
    endif

    !Temperature Volume
    if(t_vol_on)then
      CLOSE(101)
      if(vol_col_on)CLOSE(301)

      !WRITE(99,"( A,5(',',I) )")'Temp',tSecs(2),tSecs(3), &
      !                        tSecs(4),tSecs(5),tSecs(6)

      OPEN(101,FILE='Temp_Matrix.csv',STATUS='REPLACE')
        do j=0,19
          WRITE(100,"(19(I,','),I)") (tCount(i,j), i=0,19)
        enddo
      CLOSE(101)
    endif

    !Dissolved Oxygen Volume
    if(d_vol_on)then
      CLOSE(102)
      if(vol_col_on)CLOSE(302)

      !WRITE(99,"( A,5(',',I) )")'DO',dSecs(2),dSecs(3),   &
      !                      dSecs(4),dSecs(5),dSecs(6)

      OPEN(102,FILE='DO_Matrix.csv',STATUS='REPLACE')
        do j=0,19
          WRITE(102,"(19(I,','),I)") (dCount(i,j), i=0,19)
        enddo
      CLOSE(102)
    endif

    !Salinity and Temperature Combined Volume
    if(st_vol_on)then
      CLOSE(103)
      if(vol_col_on)CLOSE(303)

      OPEN(103,FILE='StTp_Matrix.csv',STATUS='REPLACE')
        do j=0,19
          WRITE(103,"(19(I,','),I)") (stCount(i,j), i=0,19)
        enddo
      CLOSE(103)
    endif

    !Salinity and Dissolved Oxygen Combined Volume
    if(sd_vol_on)then
      CLOSE(104)
      if(vol_col_on)CLOSE(304)

      OPEN(104,FILE='StDO_Matrix.csv',STATUS='REPLACE')
        do j=0,19
          WRITE(104,"(19(I,','),I)") (sdCount(i,j), i=0,19)
        enddo
      CLOSE(104)
    endif

    !Temperature and Dissolved Oxygen Combined Volume
    if(td_vol_on)then
      CLOSE(105)
      if(vol_col_on)CLOSE(305)

      OPEN(105,FILE='TpDO_Matrix.csv',STATUS='REPLACE')
        do j=0,19
          WRITE(105,"(19(I,','),I)") (tdCount(i,j), i=0,19)
        enddo
      CLOSE(105)
    endif

    !Salinity, Temperature, and Dissolved Oxygen Combined Volume
    if(std_vol_on)then
      CLOSE(106)
      if(vol_col_on)CLOSE(306)

      OPEN(106,FILE='stpd_Matrix.csv',STATUS='REPLACE')
        do j=0,19
          WRITE(106,"(19(I,','),I)") (stdCount(i,j), i=0,19)
        enddo
      CLOSE(106)
    endif

    !CLOSE(99)      !Sections Output

    !Bioenergetics Volumes
    if(nCat > 0)then
      do j=1,nCat
        CLOSE(1000+(2*j)-1)
        if(vol_col_on)then
          CLOSE(1000+(2*j))
        endif
      enddo
    endif

  END SUBROUTINE fin_Output

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  SUBROUTINE getVolumes(tStep)
  USE PARAM_MOD, ONLY: rho_elements,rho_nodes,us,t_vol_on,s_vol_on,d_vol_on,   &
    st_vol_on,sd_vol_on,td_vol_on,std_vol_on,t_surf_on,s_surf_on,d_surf_on,    &
    st_surf_on,sd_surf_on,td_surf_on,std_surf_on,vol_col_on,wet_elements,      &
    bio_on,b_vol_on,b_surf_on
  USE HYDRO_DATA_MOD, ONLY: zeta,depth,salt,temp,disO,rvr_mask,rvr_form,RE,    &
    rx,ry,rlon,rlat,form,nCat,cats,bio
  USE CONSTRAINTS_MOD, ONLY: getConstraints,getGrowth
  USE HYDRO_MOD, ONLY: getSlevel
  IMPLICIT NONE

  INTEGER, INTENT(IN) :: tStep !current model time step

  INTEGER :: i,j,k       !Iteration Variables
  INTEGER :: n1,n2,n3,n4 !Rho Nodes that make up an Element

                         !Min and Max z-coordinates of ideal 
                         !  salt, temp, & DO at each rho node
  DOUBLE PRECISION :: max_temp(rho_nodes)   ,min_temp(rho_nodes)   ,      &
                      max_salt(rho_nodes)   ,min_salt(rho_nodes)   ,      &
                      max_DO(rho_nodes)     ,min_DO(rho_nodes)     ,      &
                      max_saltemp(rho_nodes),min_saltemp(rho_nodes),      &
                      max_saltDO(rho_nodes) ,min_saltDO(rho_nodes) ,      &
                      max_tempDO(rho_nodes) ,min_tempDO(rho_nodes) ,      &
                      max_saltempDO(rho_nodes),min_saltempDO(rho_nodes)

                         !masking variables for ideal temp, salt, DO
                         !1 if ideal conditions exist at this node,
                         !0 if they do not
  INTEGER :: tmask(rho_nodes) , smask(rho_nodes),    &
             dmask(rho_nodes) ,stmask(rho_nodes),    &
             sdmask(rho_nodes),tdmask(rho_nodes),    &
             stdmask(rho_nodes)

                         !x,y,z boundary locations sent to Volume function
  DOUBLE PRECISION :: x1,y1,x2,y2,x3,y3,x4,y4
  DOUBLE PRECISION :: z1t,z1b,z2t,z2b,z3t,z3b,z4t,z4b

                         !Volumes
  DOUBLE PRECISION :: totVol,sVol,tVol,dVol,stVol,tdVol,sdVol,stdVol,Vol
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: sVolCol,tVolCol,dVolCol, &
                                    stVolCol,tdVolCol,sdVolCol,stdVolCol
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION( : ) :: cVol
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: cVolCol

  INTEGER :: nSecs       !Number of sections of 'Ideal' water returned

  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:) :: z,v,smax,smin,tmax,tmin,      &
                                                     dmax,dmin,bmax,bmin

  INTEGER :: s_form,t_form,d_form,st_form,sd_form,td_form,std_form
  DOUBLE PRECISION :: s_vol,t_vol,d_vol,st_vol,sd_vol,td_vol,std_vol,c_vol

  INTEGER, ALLOCATABLE, DIMENSION(:,:) :: bmask
  DOUBLE PRECISION, ALLOCATABLE, DIMENSION(:,:) :: min_bio,max_bio

                         !File Name Construction Variables
  CHARACTER(LEN=100) :: buffer,tFile,sFile,dFile,stFile,sdFile,tdFile,stdFile
  CHARACTER(LEN=4  ) :: prefix,suffix
  INTEGER            :: counter
  CHARACTER(LEN=100) :: buffer2,vFile
  CHARACTER(LEN=6  ) :: prefix2
  CHARACTER(LEN=4  ) :: suffix2
  INTEGER            :: counter2

   !Allocate local variables
    ALLOCATE(z(us))
    ALLOCATE(v(us))
    ALLOCATE(smax(us))
    ALLOCATE(smin(us))
    ALLOCATE(tmax(us))
    ALLOCATE(tmin(us))
    ALLOCATE(dmax(us))
    ALLOCATE(dmin(us))
    !Columns
    if(vol_col_on)then
      ALLOCATE(  tVolCol(rho_elements))
      ALLOCATE(  sVolCol(rho_elements))
      ALLOCATE(  dVolCol(rho_elements))
      ALLOCATE( stVolCol(rho_elements))
      ALLOCATE( sdVolCol(rho_elements))
      ALLOCATE( tdVolCol(rho_elements))
      ALLOCATE(stdVolCol(rho_elements))
    endif
    !Bioenergetics
    if(bio_on) ALLOCATE(bio(rho_nodes,us))
    if(nCat > 0)then
      ALLOCATE(cVol(nCat))
      if(b_surf_on)then
        ALLOCATE(bmax(us))
        ALLOCATE(bmin(us))
        ALLOCATE(bmask(nCat,rho_nodes))
        ALLOCATE(min_bio(nCat,rho_nodes))
        ALLOCATE(max_bio(nCat,rho_nodes))
      endif
      if(vol_col_on) ALLOCATE(cVolCol(nCat,rho_elements))
    endif


    !Initialize volume variables
    totVol = 0.0
      tVol = 0.0
      sVol = 0.0
      dVol = 0.0
     stVol = 0.0
     sdVol = 0.0
     tdVol = 0.0
    stdVol = 0.0
    if(vol_col_on)then
        tVolCol = 0.0
        sVolCol = 0.0
        dVolCol = 0.0
       stVolCol = 0.0
       sdVolCol = 0.0
       tdVolCol = 0.0
      stdVolCol = 0.0
    endif

    !Initialize min max variables
    min_temp = 0.0
    max_temp = 0.0
    min_salt = 0.0
    max_salt = 0.0
    min_DO   = 0.0
    max_DO   = 0.0
    min_saltemp = 0.0
    max_saltemp = 0.0
    min_saltDO  = 0.0
    max_saltDO  = 0.0
    min_tempDO  = 0.0
    max_tempDO  = 0.0
    min_saltempDO = 0.0
    max_saltempDO = 0.0

    !   +------------------------------------------------------------+
    !   |+----------------------------------------------------------+|
    !   ||            Generate Surfaces of 'ideal' Water            ||
    !   |+----------------------------------------------------------+|
    !   +------------------------------------------------------------+

    !iterate through nodes finding bounds of 'ideal' water
    do i=1,rho_nodes

      do j=1,us
        z(j) = DBLE(-1.0)*getSlevel(DBLE(-1.0)*zeta(i),DBLE(-1.0)*depth(i),j)
        call getConstraints(salt(i,j),temp(i,j),disO(i,j),smax(j),smin(j),     &
                                          tmax(j),tmin(j),dmax(j),dmin(j))
        if(bio_on) bio(i,j) = getGrowth(salt(i,j),temp(i,j),disO(i,j))
      enddo

      !            +--------------------------------------+
      !            |        Bioenergetics Surfaces        |
      !            +--------------------------------------+
      if(b_surf_on)then
        do j=1,us
          v(j) = bio(i,j)
        enddo
        if(rvr_mask(i) == 1)then

          do k=1,nCat
            bmin = cats(k,1)
            bmax = cats(k,2)
            call findBounds(z,depth(i),zeta(i),v,bmin,bmax,min_bio(k,i),  &
                            max_bio(k,i),bmask(k,i),nSecs)
            if(nSecs>1)then
              if(nSecs>5)then
                sSecs(6) = sSecs(nSecs)+1
              else
                sSecs(nSecs) = sSecs(nSecs)+1          
              endif
            endif
          enddo

        else
          bmask(:,i) = -1
        endif
      endif

      !            +--------------------------------------+
      !            |           Salinity Surfaces          |
      !            +--------------------------------------+
      if(s_surf_on .OR. st_surf_on .OR. sd_surf_on .OR. std_surf_on)then
        do j=1,us
          v(j) = salt(i,j)
        enddo
        if(rvr_mask(i) == 1)then
          call findBounds(z,depth(i),zeta(i),v,smin,smax,min_salt(i),     &
                          max_salt(i),smask(i),nSecs)
          if(nSecs>1)then
            if(nSecs>5)then
              sSecs(6) = sSecs(nSecs)+1
            else
              sSecs(nSecs) = sSecs(nSecs)+1          
            endif
          endif
        else
          smask(i) = -1
        endif
      endif

      !            +--------------------------------------+
      !            |         Temperature Surfaces         |
      !            +--------------------------------------+
      if(t_surf_on .OR. st_surf_on .OR. td_surf_on .OR. std_surf_on)then
        do j=1,us
          v(j) = temp(i,j)
        enddo
        if(rvr_mask(i) == 1)then
          call findBounds(z,depth(i),zeta(i),v,tmin,tmax,min_temp(i),     &
                          max_temp(i),tmask(i),nSecs)
          if(nSecs>1)then
            if(nSecs>5)then
              tSecs(6) = tSecs(nSecs)+1
            else
              tSecs(nSecs) = tSecs(nSecs)+1          
            endif
          endif
        else
          tmask(i) = -1
        endif
      endif

      !            +--------------------------------------+
      !            |       Dissolved Oxygen Surfaces      |
      !            +--------------------------------------+
      if(d_surf_on .OR. sd_surf_on .OR. td_surf_on .OR. std_surf_on)then
        do j=1,us
          v(j) = disO(i,j)
        enddo
        if(rvr_mask(i) == 1)then
          call findBounds(z,depth(i),zeta(i),v,dmin,dmax,min_DO(i),max_DO(i),  &
                          dmask(i),nSecs)
          if(nSecs>1)then
            if(nSecs>5)then
              dSecs(6) = dSecs(nSecs)+1
            else
              dSecs(nSecs) = dSecs(nSecs)+1          
            endif
          endif
        else
          dmask(i) = -1
        endif
      endif

      !            +--------------------------------------+
      !            |      Salt/Temp Combined Surfaces     |
      !            +--------------------------------------+
      if(st_surf_on)then
        if(tmask(i) == -1 .OR. smask(i) == -1)then
          stmask(i) = -1
        elseif(tmask(i) == 0 .OR. smask(i) == 0)then
          stmask(i) = 0
        else
          stmask(i) = 1
          if(min_salt(i) < max_salt(i))then
            min_saltemp(i) = max(min_salt(i),min_temp(i))
            max_saltemp(i) = min(max_salt(i),max_temp(i))
            if(min_saltemp(i) > max_saltemp(i))then
              stmask(i) = 0
              min_saltemp(i) = 0
              max_saltemp(i) = 0
            endif
          else
            min_saltemp(i) = min(min_salt(i),min_temp(i))
            max_saltemp(i) = max(max_salt(i),max_temp(i))
            if(max_saltemp(i) > min_saltemp(i))then
              stmask(i) = 0
              min_saltemp(i) = 0
              max_saltemp(i) = 0
            endif
          endif
        endif
      endif

      !            +--------------------------------------+
      !            |       Salt/DO Combined Surfaces      |
      !            +--------------------------------------+
      if(sd_surf_on)then
        if(dmask(i) == -1 .OR. smask(i) == -1)then
          sdmask(i) = -1
        elseif(dmask(i) == 0 .OR. smask(i) == 0)then
          sdmask(i) = 0
        else
          sdmask(i) = 1
          if(min_salt(i) < max_salt(i))then
            min_saltDO(i) = max(min_salt(i),min_DO(i))
            max_saltDO(i) = min(max_salt(i),max_DO(i))
            if(min_saltDO(i) > max_saltDO(i))then
              sdmask(i) = 0
              min_saltDO(i) = 0
              max_saltDO(i) = 0
            endif
          else
            min_saltDO(i) = min(min_salt(i),min_DO(i))
            max_saltDO(i) = max(max_salt(i),max_DO(i))
            if(max_saltDO(i) > min_saltDO(i))then
              sdmask(i) = 0
              min_saltDO(i) = 0
              max_saltDO(i) = 0
            endif
          endif
        endif
      endif

      !            +--------------------------------------+
      !            |       Temp/DO Combined Surfaces      |
      !            +--------------------------------------+
      if(td_surf_on)then
        if(tmask(i) == -1 .OR. dmask(i) == -1)then
          tdmask(i) = -1
        elseif(tmask(i) == 0 .OR. dmask(i) == 0)then
          tdmask(i) = 0
        else
          tdmask(i) = 1
          if(min_DO(i) < max_DO(i))then
            min_tempDO(i) = max(min_DO(i),min_temp(i))
            max_tempDO(i) = min(max_DO(i),max_temp(i))
            if(min_tempDO(i) > max_tempDO(i))then
              tdmask(i) = 0
              min_tempDO(i) = 0
              max_tempDO(i) = 0
            endif
          else
            min_tempDO(i) = min(min_DO(i),min_temp(i))
            max_tempDO(i) = max(max_DO(i),max_temp(i))
            if(max_tempDO(i) > min_tempDO(i))then
              tdmask(i) = 0
              min_tempDO(i) = 0
              max_tempDO(i) = 0
            endif
          endif
        endif
      endif

      !            +--------------------------------------+
      !            |    Salt/Temp/DO Combined Surfaces    |
      !            +--------------------------------------+
      if(std_surf_on)then
        if(smask(i) == -1 .OR. tmask(i) == -1 .OR. dmask(i) == -1)then
          stdmask(i) = -1
        elseif(smask(i) == 0 .OR. tmask(i) == 0 .OR. dmask(i) == 0)then
          stdmask(i) = 0
        else
          stdmask(i) = 1
          if(min_salt(i) < max_salt(i))then
            min_saltempDO(i) = max(min_salt(i),min_temp(i),min_DO(i))
            max_saltempDO(i) = min(max_salt(i),max_temp(i),max_DO(i))
            if(min_saltempDO(i) > max_saltempDO(i))then
              stdmask(i) = 0
              min_saltempDO(i) = 0
              max_saltempDO(i) = 0
            endif
          else
            min_saltempDO(i) = min(min_salt(i),min_temp(i),min_DO(i))
            max_saltempDO(i) = max(max_salt(i),max_temp(i),max_DO(i))
            if(max_saltempDO(i) > min_saltempDO(i))then
              stdmask(i) = 0
              min_saltempDO(i) = 0
              max_saltempDO(i) = 0
            endif
          endif
        endif
      endif

    enddo

    !   +------------------------------------------------------------+
    !   |+----------------------------------------------------------+|
    !   ||            Calculate Volumes of 'ideal' Water            ||
    !   |+----------------------------------------------------------+|
    !   +------------------------------------------------------------+

    counter2=tStep+10000000
    prefix2='volume'
    suffix2='.csv'
    write(buffer2,'(A,I8,A)') prefix2,counter2,suffix2
    read(buffer2,'(A)') vFile
    OPEN(250,FILE=vFile(1:LEN_TRIM(vFILE)),STATUS='REPLACE')

    !iterate through elements finding volume of:
    !  1) All Water
    !  2) 'Ideal' Water
    do i=1,rho_elements

      !Initialize variables for current element
      n1 = RE(1,i)
      n2 = RE(2,i)
      n3 = RE(3,i)
      n4 = RE(4,i)

      x1 = rx(n1)
      x2 = rx(n2)
      x3 = rx(n3)
      x4 = rx(n4)
      y1 = ry(n1)
      y2 = ry(n2)
      y3 = ry(n3)
      y4 = ry(n4)

      do j=0,us

        !Initialize variables for current depth level
        if(j==0)then
          z1b = depth(n1)
          z2b = depth(n2)
          z3b = depth(n3)
          z4b = depth(n4)
        else
          z1b = getSlevel(zeta(n1),depth(n1),j)
          z2b = getSlevel(zeta(n2),depth(n2),j)
          z3b = getSlevel(zeta(n3),depth(n3),j)
          z4b = getSlevel(zeta(n4),depth(n4),j)
        endif

        if(j==us)then
          z1t = zeta(n1)
          z2t = zeta(n2)
          z3t = zeta(n3)
          z4t = zeta(n4)
        else
          z1t = getSlevel(zeta(n1),depth(n1),j+1)
          z2t = getSlevel(zeta(n2),depth(n2),j+1)
          z3t = getSlevel(zeta(n3),depth(n3),j+1)
          z4t = getSlevel(zeta(n4),depth(n4),j+1)
        endif

        !          +--------------------------------------+
        !          |    Calculate Total Volume of Water   |
        !          +--------------------------------------+
        Vol = totalVol(x1,y1,z1t,z1b,x2,y2,z2t,z2b,x3,y3,z3t,z3b,    &
                       x4,y4,z4t,z4b,form(i))
        totVol = totVol + Vol
        write(250,"(F,2(',',F30.15))")tStep/DBLE(24)+DBLE(5),Vol,totVol


        !          +--------------------------------------+
        !          |  Calculate Volumes of 'Ideal' Water  |
        !          +--------------------------------------+
        !Calculate Volume of water for each s/t/d combination
        call combined_vol(i,j,s_form,t_form,d_form,st_form,sd_form,td_form,    &
                   std_form,s_vol,t_vol,d_vol,st_vol,sd_vol,td_vol,std_vol)


        !          +--------------------------------------+
        !          |  Update 'Ideal' Water Total Volumes  |
        !          +--------------------------------------+
        if(s_vol_on .OR. st_vol_on .OR. sd_vol_on .OR. std_vol_on)then
          sVol = sVol + s_vol
          sCount(rvr_form(i),s_form) = sCount(rvr_form(i),s_form) + 1
          if(vol_col_on)sVolCol(i) = sVolCol(i) + s_vol
        endif
        if(t_vol_on .OR. st_vol_on .OR. td_vol_on .OR. std_vol_on)then
          tVol = tVol + t_vol
          tCount(rvr_form(i),t_form) = tCount(rvr_form(i),t_form) + 1
          if(vol_col_on)tVolCol(i) = tVolCol(i) + t_vol
        endif
        if(d_vol_on .OR. sd_vol_on .OR. td_vol_on .OR. std_vol_on)then
          dVol = dVol + d_vol
          dCount(rvr_form(i),d_form) = dCount(rvr_form(i),d_form) + 1
          if(vol_col_on)dVolCol(i) = dVolCol(i) + d_vol
        endif
        if(st_vol_on)then
          stVol = stVol + st_vol
          stCount(rvr_form(i),st_form) = stCount(rvr_form(i),st_form) + 1
          if(vol_col_on)stVolCol(i) = stVolCol(i) + st_vol
        endif
        if(sd_vol_on)then
          sdVol = sdVol + sd_vol
          sdCount(rvr_form(i),sd_form) = sdCount(rvr_form(i),sd_form) + 1
          if(vol_col_on)sdVolCol(i) = sdVolCol(i) + sd_vol
        endif
        if(td_vol_on)then
          tdVol = tdVol + td_vol
          tdCount(rvr_form(i),td_form) = tdCount(rvr_form(i),td_form) + 1
          if(vol_col_on)tdVolCol(i) = tdVolCol(i) + td_vol
        endif
        if(std_vol_on)then
          stdVol = stdVol + std_vol
          stdCount(rvr_form(i),std_form) = stdCount(rvr_form(i),std_form) + 1
          if(vol_col_on)stdVolCol(i) = stdVolCol(i) + std_vol
        endif

        !          +--------------------------------------+
        !          |     Calculate Volume of Water for    |
        !          |      each Bioenergetics Category     |
        !          +--------------------------------------+
        if(b_vol_on)then
          do k=1,nCat
            call category_vol(i,j,k,c_vol)
            cVol(k) = cVol(k) + c_vol
            if(vol_col_on)cVolCol(k,i) = cVolCol(k,i) + c_vol
          enddo
        endif

      enddo !us loop

    enddo !element loop

    CLOSE(250)

    !   +------------------------------------------------------------+
    !   |+----------------------------------------------------------+|
    !   ||           Output Total Volumes of 'ideal' Water          ||
    !   |+----------------------------------------------------------+|
    !   +------------------------------------------------------------+
    102 format (F,2(',',F30.10))
    104 format (F,4(',',F30.10))
    105 format (F,5(',',F30.10))
    200 format (2(F,','),I,4(',',F))
    if(  s_vol_on) write(100,102)tStep/DBLE(24)+DBLE(5),totVol,sVol
    if(  t_vol_on) write(101,102)tStep/DBLE(24)+DBLE(5),totVol,tVol
    if(  d_vol_on) write(102,102)tStep/DBLE(24)+DBLE(5),totVol,dVol
    if( st_vol_on) write(103,104)tStep/DBLE(24)+DBLE(5),totVol,sVol,tVol,stVol
    if( sd_vol_on) write(104,104)tStep/DBLE(24)+DBLE(5),totVol,sVol,dVol,sdVol
    if( td_vol_on) write(105,104)tStep/DBLE(24)+DBLE(5),totVol,tVol,dVol,tdVol
    if(std_vol_on) write(106,105)tStep/DBLE(24)+DBLE(5),totVol,sVol,tVol,dVol, &
                                 stdVol

    if(vol_col_on)then
      if(  s_vol_on) write(300,fmt2)tStep/DBLE(24)+DBLE(5),     &
                          (  sVolCol(i), i=1,wet_elements)
      if(  t_vol_on) write(301,fmt2)tStep/DBLE(24)+DBLE(5),     &
                          (  tVolCol(i), i=1,wet_elements)
      if(  d_vol_on) write(302,fmt2)tStep/DBLE(24)+DBLE(5),     &
                          (  dVolCol(i), i=1,wet_elements)
      if( st_vol_on) write(303,fmt2)tStep/DBLE(24)+DBLE(5),     &
                          ( stVolCol(i), i=1,wet_elements)
      if( sd_vol_on) write(304,fmt2)tStep/DBLE(24)+DBLE(5),     &
                          ( sdVolCol(i), i=1,wet_elements)
      if( td_vol_on) write(305,fmt2)tStep/DBLE(24)+DBLE(5),     &
                          ( tdVolCol(i), i=1,wet_elements)
      if(std_vol_on) write(306,fmt2)tStep/DBLE(24)+DBLE(5),     &
                          (stdVolCol(i), i=1,wet_elements)
    endif

    !              +--------------------------------------+
    !              |  Output Volume of Water and Surface  |
    !              |    for each Bioenergetics Category   |
    !              +--------------------------------------+
    if(nCat > 0)then
      do j=1,nCat
        write(1000+(2*j)-1,102)tStep/DBLE(24)+DBLE(5),totVol,cVol(j)
        if(vol_col_on) write(1000+(2*j),fmt2)tStep/DBLE(24)+DBLE(5),   &
                                   (cVolCol(j,i), i=1,wet_elements)
        if(b_surf_on)then
          counter=tStep+10000000
          prefix='cat'
          suffix='.csv'
          write(buffer,'(A,I1,A,I8,A)') prefix,j,'_',counter,suffix
          read(buffer,'(A)') sFile
          OPEN(199,FILE=sFile(1:LEN_TRIM(sFILE)),STATUS='REPLACE')
            do i=1,rho_nodes
              if(bmask(j,i) == 1)then
                write(199,200)rlon(i),rlat(i),bmask(j,i),min_bio(j,i),         &
                              max_bio(j,i),depth(i),zeta(i)
              elseif(bmask(j,i) == -1)then
                write(199,200)rlon(i),rlat(i),bmask(j,i),-34.5,-34.5,0.0,zeta(i)
              else
                write(199,200)rlon(i),rlat(i),bmask(j,i),-35.0,-35.0,depth(i), &
                              zeta(i)
              endif
            enddo
          CLOSE(199)
        endif
      enddo
    endif

    !   +------------------------------------------------------------+
    !   |+----------------------------------------------------------+|
    !   ||              Output Surfaces of 'ideal' Water            ||
    !   |+----------------------------------------------------------+|
    !   +------------------------------------------------------------+

    !              +--------------------------------------+
    !              |           Salinity Surfaces          |
    !              +--------------------------------------+
    if(s_surf_on)then
      counter=tStep+10000000
      prefix='salt'
      suffix='.csv'
      write(buffer,'(A,I8,A)') prefix,counter,suffix
      read(buffer,'(A)') sFile
      OPEN(200,FILE=sFile(1:LEN_TRIM(sFILE)),STATUS='REPLACE')
        do i=1,rho_nodes
          if(smask(i) == 1)then
            write(200,200)rlon(i),rlat(i),smask(i),min_salt(i),max_salt(i),    &
                          depth(i),zeta(i)
          elseif(smask(i) == -1)then
            write(200,200)rlon(i),rlat(i),smask(i),-34.5,-34.5,0.0,zeta(i)
          else
            write(200,200)rlon(i),rlat(i),smask(i),-35.0,-35.0,depth(i),zeta(i)
          endif
        enddo
      CLOSE(200)
    endif

    !              +--------------------------------------+
    !              |          Temperature Surfaces        |
    !              +--------------------------------------+
    if(t_surf_on)then
      counter=tStep+10000000
      prefix='temp'
      suffix='.csv'
      write(buffer,'(A,I8,A)') prefix,counter,suffix
      read(buffer,'(A)') tFile
      OPEN(201,FILE=tFile(1:LEN_TRIM(tFILE)),STATUS='REPLACE')
        do i=1,rho_nodes
          if(tmask(i) == 1)then
            write(201,200)rlon(i),rlat(i),tmask(i),min_temp(i),max_temp(i),    &
                          depth(i),zeta(i)
          elseif(tmask(i) == -1)then
            write(201,200)rlon(i),rlat(i),tmask(i),-34.5,-34.5,0.0,zeta(i)
          else
            write(201,200)rlon(i),rlat(i),tmask(i),-35.0,-35.0,depth(i),zeta(i)
          endif
        enddo
      CLOSE(201)
    endif

    !              +--------------------------------------+
    !              |       Dissolved Oxygen Surfaces      |
    !              +--------------------------------------+
    if(d_surf_on)then
      counter=tStep+10000000
      prefix='disO'
      suffix='.csv'
      write(buffer,'(A,I8,A)') prefix,counter,suffix
      read(buffer,'(A)') dFile
      OPEN(202,FILE=TRIM(dFile),STATUS='REPLACE')
        do i=1,rho_nodes
          if(dmask(i) == 1)then
            write(202,200)rlon(i),rlat(i),dmask(i),min_DO(i),max_DO(i),        &
                          depth(i),zeta(i)
          elseif(dmask(i) == -1)then
            write(202,200)rlon(i),rlat(i),dmask(i),-34.5,-34.5,0.0,zeta(i)
          else
            write(202,200)rlon(i),rlat(i),dmask(i),-35.0,-35.0,depth(i),zeta(i)
          endif
        enddo
      CLOSE(202)
    endif

    !              +--------------------------------------+
    !              |      Salt/Temp Combined Surfaces     |
    !              +--------------------------------------+
    if(st_surf_on)then
      counter=tStep+10000000
      prefix='sttp'
      suffix='.csv'
      write(buffer,'(A,I8,A)') prefix,counter,suffix
      read(buffer,'(A)') stFile
      OPEN(203,FILE=stFile(1:LEN_TRIM(stFILE)),STATUS='REPLACE')
        do i=1,rho_nodes
          if(stmask(i) == 1) then
            write(203,200)rlon(i),rlat(i),stmask(i),min_saltemp(i),            &
                          max_saltemp(i),depth(i),zeta(i)
          elseif(stmask(i) == -1)then
            write(203,200)rlon(i),rlat(i),stmask(i),-34.5,-34.5,0.0,zeta(i)
          else
            write(203,200)rlon(i),rlat(i),stmask(i),-35.0,-35.0,depth(i),zeta(i)
          endif
        enddo
      CLOSE(203)
    endif

    !              +--------------------------------------+
    !              |       Salt/DO Combined Surfaces      |
    !              +--------------------------------------+
    if(sd_surf_on)then
      counter=tStep+10000000
      prefix='stdo'
      suffix='.csv'
      write(buffer,'(A,I8,A)') prefix,counter,suffix
      read(buffer,'(A)') sdFile
      OPEN(204,FILE=TRIM(sdFILE),STATUS='REPLACE')
        do i=1,rho_nodes
          if(sdmask(i) == 1) then
            write(204,200)rlon(i),rlat(i),sdmask(i),min_saltDO(i),             &
                          max_saltDO(i),depth(i),zeta(i)
          elseif(sdmask(i) == -1)then
            write(204,200)rlon(i),rlat(i),sdmask(i),-34.5,-34.5,0.0,zeta(i)
          else
            write(204,200)rlon(i),rlat(i),sdmask(i),-35.0,-35.0,depth(i),zeta(i)
          endif
        enddo
      CLOSE(204)
    endif

    !              +--------------------------------------+
    !              |       Temp/DO Combined Surfaces      |
    !              +--------------------------------------+
    if(td_surf_on)then
      counter=tStep+10000000
      prefix='tpdo'
      suffix='.csv'
      write(buffer,'(A,I8,A)') prefix,counter,suffix
      read(buffer,'(A)') tdFile
      OPEN(205,FILE=TRIM(tdFILE),STATUS='REPLACE')
        do i=1,rho_nodes
          if(tdmask(i) == 1) then
            write(205,200)rlon(i),rlat(i),tdmask(i),min_tempDO(i),             &
                          max_tempDO(i),depth(i),zeta(i)
          elseif(tdmask(i) == -1)then
            write(205,200)rlon(i),rlat(i),tdmask(i),-34.5,-34.5,0.0,zeta(i)
          else
            write(205,200)rlon(i),rlat(i),tdmask(i),-35.0,-35.0,depth(i),zeta(i)
          endif
        enddo
      CLOSE(205)
    endif

    !              +--------------------------------------+
    !              |    Salt/Temp/DO Combined Surfaces    |
    !              +--------------------------------------+
    if(std_surf_on)then
      counter=tStep+10000000
      prefix='std_'
      suffix='.csv'
      write(buffer,'(A,I8,A)') prefix,counter,suffix
      read(buffer,'(A)') stdFile
      OPEN(206,FILE=TRIM(stdFILE),STATUS='REPLACE')
        do i=1,rho_nodes
          if(stdmask(i) == 1) then
            write(206,200)rlon(i),rlat(i),stdmask(i),min_saltempDO(i),         &
                          max_saltempDO(i),depth(i),zeta(i)
          elseif(stdmask(i) == -1)then
            write(206,200)rlon(i),rlat(i),stdmask(i),-34.5,-34.5,0.0,zeta(i)
          else
            write(206,200)rlon(i),rlat(i),stdmask(i),-35.0,-35.0,depth(i),     &
                          zeta(i)
          endif
        enddo
      CLOSE(206)
    endif


    !Deallocate Local Variables
    DEALLOCATE(z,v,smax,smin,tmax,tmin,dmax,dmin)

    if(vol_col_on)then
      DEALLOCATE(sVolCol,tVolCol,dVolCol,stdVolCol)
      DEALLOCATE(stVolCol,tdVolCol,sdVolCol)
    endif

    if(nCat > 0)then
      DEALLOCATE(cVol)
      if(b_surf_on) DEALLOCATE(bmax,bmin,bmask,min_bio,max_bio)
      if(vol_col_on) DEALLOCATE(cVolCol)
    endif

    if(bio_on) DEALLOCATE(bio)

  END SUBROUTINE getVolumes

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  SUBROUTINE findBounds(z,zmin,zmax,v,vmin,vmax,min,max,mask,nSecs)
  USE PARAM_MOD, ONLY: us
  IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: z(us),zmin,zmax,v(us),vmin(us),vmax(us)
    DOUBLE PRECISION, INTENT(OUT) :: min,max
    INTEGER, INTENT(OUT) :: mask,nSecs  !mask : 0 no ideal water
                                        !mask : 1 ideal water found
                                        !nSecs: # of sections of ideal water

    DOUBLE PRECISION :: mn,mx,mv,m1,m2,bv,b1,b2
    INTEGER :: prev,i, &
               bc   !counter of boundaries found
    LOGICAL :: gmin,gmax

    bc = 0

    max = -999.
    min =  999.

    if(v(1) < vmin(1))then        !Determine if bottom node is below,
      prev = -1                   !  above, or within 'ideal' bounds
    elseif(v(1) > vmax(1))then
      prev = 1
    else
      min = zmin                  !if within, set lower bound to
      prev = 0                    !  bottom depth
      bc = 1
    endif

    do i=2,us                     !Iterate through nodes from bottom to top
      if(prev==-1)then            !If previous node was less than ideal:
        if(vmin(i) > v(i))cycle   !  If current node is less than ideal: cycle
        if(v(i) > vmax(i))then    !  If current node is more than ideal:

          mv = (   v(i)-   v(i-1)) / (z(i)-z(i-1))
          m1 = (vmin(i)-vmin(i-1)) / (z(i)-z(i-1))
          m2 = (vmax(i)-vmax(i-1)) / (z(i)-z(i-1))
          bv =    v(i) - mv*z(i)
          b1 = vmin(i) - m1*z(i)
          b2 = vmax(i) - m2*z(i)

          mn = (b1 - bv) / (mv - m1)   !Find depths where 'ideal' water is
          mx = (b2 - bv) / (mv - m2)   !  entered and exited

          if(mn<min)min=mn             !If these points are above previous max
          if(max<mn)max=mn             !  or below previous min, update with
          if(max<mx)max=mx             !  new max or min
          if(mx<min)min=mx

          vertdblcnt = vertdblcnt + 1

          bc = bc + 2
          prev = 1
            
        else                      !  If current node is within 'ideal' bounds:

          mv = (   v(i)-   v(i-1)) / (z(i)-z(i-1))
          m1 = (vmin(i)-vmin(i-1)) / (z(i)-z(i-1))
          bv =    v(i) - mv*z(i)
          b1 = vmin(i) - m1*z(i)

          mn = (b1 - bv) / (mv - m1)   !Find depth that 'ideal' water is entered

          if(mn<min)min=mn             !If above previous max or below previous
          if(max<mn)max=mn             !  min, update with new max or min

          bc = bc + 1
          prev = 0
        endif
      elseif(prev==0)then         !If previous node was within 'ideal' bounds:
                                  !  If current node is within 'ideal' bounds:
        if(vmin(i) <= v(i) .AND. v(i) <= vmax(i))cycle     !  then cycle
        if(v(i) > vmax(i))then    !  If current node is more than ideal:

          mv = (   v(i)-   v(i-1)) / (z(i)-z(i-1))
          m2 = (vmax(i)-vmax(i-1)) / (z(i)-z(i-1))
          bv =    v(i) - mv*z(i)
          b2 = vmax(i) - m2*z(i)

          mx = (b2 - bv) / (mv - m2)   !Find depth that 'ideal' water is exited

          if(max<mx)max=mx             !If above previous max or below previous
          if(mx<min)min=mx             !  min, update with new max or min

          bc = bc + 1
          prev = 1
            
        else                      !  If current node is less than ideal:

          mv = (   v(i)-   v(i-1)) / (z(i)-z(i-1))
          m1 = (vmin(i)-vmin(i-1)) / (z(i)-z(i-1))
          bv =    v(i) - mv*z(i)
          b1 = vmin(i) - m1*z(i)

          mn = (b1 - bv) / (mv - m1)   !Find depth that 'ideal' water is exited

          if(mn<min)min=mn             !If above previous max or below previous
          if(max<mn)max=mn             !  min, update with new max or min

          bc = bc + 1
          prev = -1
        endif
      else                        !If previous node was more than ideal:
        if(v(i) > vmax(i))cycle   !  If current node is more than ideal: cycle
        if(vmin(i) > v(i))then    !  If current node is less than ideal:

          mv = (   v(i)-   v(i-1)) / (z(i)-z(i-1))
          m1 = (vmin(i)-vmin(i-1)) / (z(i)-z(i-1))
          m2 = (vmax(i)-vmax(i-1)) / (z(i)-z(i-1))
          bv =    v(i) - mv*z(i)
          b1 = vmin(i) - m1*z(i)
          b2 = vmax(i) - m2*z(i)

          mn = (b1 - bv) / (mv - m1)   !Find depths where 'ideal' water is
          mx = (b2 - bv) / (mv - m2)   !  entered and exited

          if(mn<min)min=mn             !If these points are above previous max
          if(max<mn)max=mn             !  or below previous min, update with
          if(max<mx)max=mx             !  new max or min
          if(mx<min)min=mx

          vertdblcnt = vertdblcnt + 1

          bc = bc + 2
          prev = -1
            
        else                      !  If current node is within 'ideal' bounds:

          mv = (   v(i)-   v(i-1)) / (z(i)-z(i-1))
          m2 = (vmax(i)-vmax(i-1)) / (z(i)-z(i-1))
          bv =    v(i) - mv*z(i)
          b2 = vmax(i) - m2*z(i)

          mx = (b2 - bv) / (mv - m2)   !Find depth that 'ideal' water is entered

          if(max<mx)max=mx             !If above previous max or below previous
          if(mx<min)min=mx             !  min, update with new max or min

          bc = bc + 1
          prev = 0
        endif
      endif
    enddo

    if(vmin(us) <= v(us) .AND. v(us) <= vmax(us))then
      max = zmax                  !If the top node is within 'ideal' bounds:
      bc = bc + 1                 !  set upper bound to surface height
    endif

    if (mod(bc,2)/=0) then
      write(*,*)'ERROR: Bounds Mismatch'
      stop
    endif

    mask = 1
    if (bc == 0) then
      mask = 0
      min = 0
      max = 0
    endif
    nSecs = bc/2

  END SUBROUTINE findBounds

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  SUBROUTINE combined_vol(e,d,v1_form,v2_form,v3_form,v12_form,v13_form,       &
      v23_form,v123_form,v1_vol,v2_vol,v3_vol,v12_vol,v13_vol,v23_vol,v123_vol)
  USE HYDRO_DATA_MOD, ONLY: salt,temp,disO,RE,rx,ry,rvr_form,zeta,depth
  USE CONSTRAINTS_MOD, ONLY: getConstraints
  USE PARAM_MOD, ONLY: us,atBottom,offBott
  USE HYDRO_MOD, ONLY: getSlevel
  IMPLICIT NONE


    INTEGER, INTENT(IN) :: e,d  !e = element #, d = depth level

    DOUBLE PRECISION, INTENT(OUT) :: v1_vol,v2_vol,v3_vol,v12_vol, &
                                     v13_vol,v23_vol,v123_vol
    INTEGER, INTENT(OUT) :: v1_form,v2_form,v3_form,v12_form, &
                            v13_form,v23_form,v123_form

    INTEGER :: n1,n2,n3,n4,t,b
    DOUBLE PRECISION :: x1,y1,z1t,z1b,x2,y2,z2t,z2b,x3,y3,z3t,z3b,x4,y4,z4t,z4b

    !midpoints of the polygons making up the top and bottom of the volume
    DOUBLE PRECISION :: cx,cy,ct,cb,       & 
            zmax,zmin,mv,bv,m1,b1,m2,b2,tmp

    INTEGER :: form,tmp_form_1,tmp_form_2
    DOUBLE PRECISION :: v1_z1t,v1_z1b,v1_z2t,v1_z2b,v1_z3t,v1_z3b,v1_z4t, &
          v1_z4b,v2_z1t,v2_z1b,v2_z2t,v2_z2b,v2_z3t,v2_z3b,v2_z4t,v2_z4b, &
                 v3_z1t,v3_z1b,v3_z2t,v3_z2b,v3_z3t,v3_z3b,v3_z4t,v3_z4b, &
                 vv_z1t,vv_z1b,vv_z2t,vv_z2b,vv_z3t,vv_z3b,vv_z4t,vv_z4b

    DOUBLE PRECISION :: salt1b,salt2b,salt3b,salt4b,salt1t,salt2t,salt3t, &
          salt4t,temp1b,temp2b,temp3b,temp4b,temp1t,temp2t,temp3t,temp4t, &
                 disO1b,disO2b,disO3b,disO4b,disO1t,disO2t,disO3t,disO4t, &
                 smin1b,smin2b,smin3b,smin4b,smin1t,smin2t,smin3t,smin4t, &
                 smax1b,smax2b,smax3b,smax4b,smax1t,smax2t,smax3t,smax4t, &
                 tmin1b,tmin2b,tmin3b,tmin4b,tmin1t,tmin2t,tmin3t,tmin4t, &
                 tmax1b,tmax2b,tmax3b,tmax4b,tmax1t,tmax2t,tmax3t,tmax4t, &
                 dmin1b,dmin2b,dmin3b,dmin4b,dmin1t,dmin2t,dmin3t,dmin4t, &
                 dmax1b,dmax2b,dmax3b,dmax4b,dmax1t,dmax2t,dmax3t,dmax4t

    !   +------------------------------------------------------------+
    !   |+----------------------------------------------------------+|
    !   ||            Prepare Values at the Eight Corners           ||
    !   |+----------------------------------------------------------+|
    !   +------------------------------------------------------------+

    n1 = RE(1,e)
    n2 = RE(2,e)
    n3 = RE(3,e)
    n4 = RE(4,e)

    x1 = rx(n1)
    x2 = rx(n2)
    x3 = rx(n3)
    x4 = rx(n4)
    y1 = ry(n1)
    y2 = ry(n2)
    y3 = ry(n3)
    y4 = ry(n4)

    if(d==0)then
      b = 1
      t = 1
    elseif(d==us)then
      b = us
      t = us
    else
      b = d
      t = d+1
    endif

    salt1b = salt(n1,b)
    salt2b = salt(n2,b)
    salt3b = salt(n3,b)
    salt4b = salt(n4,b)
    salt1t = salt(n1,t)
    salt2t = salt(n2,t)
    salt3t = salt(n3,t)
    salt4t = salt(n4,t)
    temp1b = temp(n1,b)
    temp2b = temp(n2,b)
    temp3b = temp(n3,b)
    temp4b = temp(n4,b)
    temp1t = temp(n1,t)
    temp2t = temp(n2,t)
    temp3t = temp(n3,t)
    temp4t = temp(n4,t)
    disO1b = disO(n1,b)
    disO2b = disO(n2,b)
    disO3b = disO(n3,b)
    disO4b = disO(n4,b)
    disO1t = disO(n1,t)
    disO2t = disO(n2,t)
    disO3t = disO(n3,t)
    disO4t = disO(n4,t)

    if(d==0)then
      z1b = depth(n1)
      z2b = depth(n2)
      z3b = depth(n3)
      z4b = depth(n4)
    else
      z1b = getSlevel(zeta(n1),depth(n1),d)
      z2b = getSlevel(zeta(n2),depth(n2),d)
      z3b = getSlevel(zeta(n3),depth(n3),d)
      z4b = getSlevel(zeta(n4),depth(n4),d)
    endif

    if(d==us)then
      z1t = zeta(n1)
      z2t = zeta(n2)
      z3t = zeta(n3)
      z4t = zeta(n4)
    else
      z1t = getSlevel(zeta(n1),depth(n1),d+1)
      z2t = getSlevel(zeta(n2),depth(n2),d+1)
      z3t = getSlevel(zeta(n3),depth(n3),d+1)
      z4t = getSlevel(zeta(n4),depth(n4),d+1)
    endif

    form = rvr_form(e)

    !              +--------------------------------------+
    !              |   If Bottom Oriented Species check   |
    !              |    the depth of the eight corners    |
    !              +--------------------------------------+

    if(atBottom)then

      if(z1b-depth(n1) > offBott)then  !If bottom node at node 1 is above cutoff
        SELECT CASE(form)              !  change form to exclude this node
          CASE(0)
            form = 2
          CASE(1)
            form = 3
          CASE(4)
            form = 16
          CASE(5)
            form = 7
          CASE(8)
            form = 10
          CASE(12)
            form = 14
          CASE(13)
            form = 15
          CASE(18,19)
            form = 11
        END SELECT
      elseif(z1t-depth(n1) > offBott)then   !If just top node is above cutoff
        tmp    = depth(n1) + offBott        !  Move top node down to cutoff
        mv     = (salt1t-salt1b)/(z1t-z1b)  !  and use linear interpolation
        bv     = salt1t - mv*z1t            !  to determine values at new
        salt1t = mv*tmp + bv                !  top location
        mv     = (temp1t-temp1b)/(z1t-z1b)
        bv     = temp1t - mv*z1t
        temp1t = mv*tmp + bv
        mv     = (disO1t-disO1b)/(z1t-z1b)
        bv     = disO1t - mv*z1t
        disO1t = mv*tmp + bv
        z1t    = depth(n1) + offBott
      endif

      if(z2b-depth(n2) > offBott)then  !If bottom node at node 2 is above cutoff
        SELECT CASE(form)              !  change form to exclude this node
          CASE(0)
            form = 8
          CASE(1)
            form = 19
          CASE(2)
            form = 10
          CASE(3)
            form = 11
          CASE(4)
            form = 12
          CASE(5)
            form = 13
          CASE(7)
            form = 15
          CASE(16,17)
            form = 14
        END SELECT
      elseif(z2t-depth(n2) > offBott)then   !If just top node is above cutoff
        tmp    = depth(n2) + offBott        !  Move top node down to cutoff
        mv     = (salt2t-salt2b)/(z2t-z2b)  !  and use linear interpolation
        bv     = salt2t - mv*z2t            !  to determine values at new
        salt2t = mv*tmp + bv                !  top location
        mv     = (temp2t-temp2b)/(z2t-z2b)
        bv     = temp2t - mv*z2t
        temp2t = mv*tmp + bv
        mv     = (disO2t-disO2b)/(z2t-z2b)
        bv     = disO2t - mv*z2t
        disO2t = mv*tmp + bv
        z2t    = depth(n2) + offBott
      endif

      if(z3b-depth(n3) > offBott)then  !If bottom node at node 3 is above cutoff
        SELECT CASE(form)              !  change form to exclude this node
          CASE(0)
            form = 4
          CASE(1)
            form = 5
          CASE(2)
            form = 16
          CASE(3)
            form = 7
          CASE(8)
            form = 12
          CASE(10)
            form = 14
          CASE(11)
            form = 15
          CASE(18,19)
            form = 13
        END SELECT
      elseif(z3t-depth(n3) > offBott)then   !If just top node is above cutoff
        tmp    = depth(n3) + offBott        !  Move top node down to cutoff
        mv     = (salt3t-salt3b)/(z3t-z3b)  !  and use linear interpolation
        bv     = salt3t - mv*z3t            !  to determine values at new
        salt3t = mv*tmp + bv                !  top location
        mv     = (temp3t-temp3b)/(z3t-z3b)
        bv     = temp3t - mv*z3t
        temp3t = mv*tmp + bv
        mv     = (disO3t-disO3b)/(z3t-z3b)
        bv     = disO3t - mv*z3t
        disO3t = mv*tmp + bv
        z3t    = depth(n3) + offBott
      endif

      if(z4b-depth(n4) > offBott)then  !If bottom node at node 4 is above cutoff
        SELECT CASE(form)              !  change form to exclude this node
          CASE(0)
            form = 1
          CASE(2)
            form = 3
          CASE(4)
            form = 5
          CASE(8)
            form = 19
          CASE(10)
            form = 11
          CASE(12)
            form = 13
          CASE(14)
            form = 15
          CASE(16,17)
            form = 7
        END SELECT
      elseif(z4t-depth(n4) > offBott)then   !If just top node is above cutoff
        tmp    = depth(n4) + offBott        !  Move top node to cutoff
        mv     = (salt4t-salt4b)/(z4t-z4b)  !  and use linear interpolation
        bv     = salt4t - mv*z4t            !  to determine values at new
        salt4t = mv*tmp + bv                !  top location
        mv     = (temp4t-temp4b)/(z4t-z4b)
        bv     = temp4t - mv*z4t
        temp4t = mv*tmp + bv
        mv     = (disO4t-disO4b)/(z4t-z4b)
        bv     = disO4t - mv*z4t
        disO4t = mv*tmp + bv
        z4t    = depth(n4) + offBott
      endif

    endif !atBottom

    !              +--------------------------------------+
    !              |      Determine the constraints       |
    !              |        at the eight corners          |
    !              +--------------------------------------+
    call getConstraints(salt1t,temp1t,disO1t,smax1t,smin1t,tmax1t,tmin1t,      &
                        dmax1t,dmin1t)
    call getConstraints(salt1b,temp1b,disO1b,smax1b,smin1b,tmax1b,tmin1b,      &
                        dmax1b,dmin1b)
    call getConstraints(salt2t,temp2t,disO2t,smax2t,smin2t,tmax2t,tmin2t,      &
                        dmax2t,dmin2t)
    call getConstraints(salt2b,temp2b,disO2b,smax2b,smin2b,tmax2b,tmin2b,      &
                        dmax2b,dmin2b)
    call getConstraints(salt3t,temp3t,disO3t,smax3t,smin3t,tmax3t,tmin3t,      &
                        dmax3t,dmin3t)
    call getConstraints(salt3b,temp3b,disO3b,smax3b,smin3b,tmax3b,tmin3b,      &
                        dmax3b,dmin3b)
    call getConstraints(salt4t,temp4t,disO4t,smax4t,smin4t,tmax4t,tmin4t,      &
                        dmax4t,dmin4t)
    call getConstraints(salt4b,temp4b,disO4b,smax4b,smin4b,tmax4b,tmin4b,      &
                        dmax4b,dmin4b)

    v1_z1t = z1t
    v1_z1b = z1b
    v1_z2t = z2t
    v1_z2b = z2b
    v1_z3t = z3t
    v1_z3b = z3b
    v1_z4t = z4t
    v1_z4b = z4b
    v2_z1t = z1t
    v2_z1b = z1b
    v2_z2t = z2t
    v2_z2b = z2b
    v2_z3t = z3t
    v2_z3b = z3b
    v2_z4t = z4t
    v2_z4b = z4b
    v3_z1t = z1t
    v3_z1b = z1b
    v3_z2t = z2t
    v3_z2b = z2b
    v3_z3t = z3t
    v3_z3b = z3b
    v3_z4t = z4t
    v3_z4b = z4b

    cx = (x1 +x2 +x3 +x4 )/DBLE(4)
    cy = (y1 +y2 +y3 +y4 )/DBLE(4)

    v1_form = form
    v2_form = form
    v3_form = form
    v12_form = form
    v13_form = form
    v23_form = form
    v123_form = form

    !   +------------------------------------------------------------+
    !   |+----------------------------------------------------------+|
    !   ||          Determine Boundaries of Each Constraint         ||
    !   |+----------------------------------------------------------+|
    !   +------------------------------------------------------------+

    !Node 1 (bottom left)
    if(form == 0  .OR. form == 1  .OR. form == 4  .OR. &
       form == 5  .OR. form == 8  .OR. form == 12 .OR. &
       form == 13 .OR. form == 18 .OR. form == 19) then

      !Constraint #1 - Salinity
      if( (salt1t > smax1t .AND. salt1b > smax1b)  .OR. &
          (salt1t < smin1t .AND. salt1b < smin1b)) then
        !Both nodes out, Change form
        SELECT CASE(v1_form)
          CASE(0)
            v1_form = 2
          CASE(1)
            v1_form = 3
          CASE(4)
            v1_form = 16
          CASE(5)
            v1_form = 7
          CASE(8)
            v1_form = 10
          CASE(12)
            v1_form = 14
          CASE(13)
            v1_form = 15
          CASE(18,19)
            v1_form = 11
        END SELECT

      else

        v1_z1t = z1t
        v1_z1b = z1b

        !If either node is outside salinity constraints:
        if(salt1t > smax1t .OR. salt1t < smin1t .OR. &
           salt1b > smax1b .OR. salt1b < smin1b)then
          !Find Linear Equation through (z1t,salt1t) and (z1b,salt1b)
          mv = (salt1t-salt1b) / (z1t-z1b)
          bv =  salt1t - mv*z1t

          if(salt1t<smin1t .OR. salt1b<smin1b)then    !If either node is below
            if(smin1t==smin1b)then                    ! minimum salinity, find
              m1 = (z1t-z1b)/(salt1t-salt1b)          ! the location where the
              b1 = z1t - (m1*salt1t)                  ! lines for min salinity
              zmin = m1*smin1t + b1                   ! and actual salinity
            else                                      ! intersect and store the
              m1 = (smin1t-smin1b) / (z1t-z1b)        ! depth in zmin
              b1 = smin1t - m1*z1t
              zmin = (b1 - bv) / (mv - m1)
            endif
          endif
          if(salt1t>smax1t .OR. salt1b>smax1b)then    !If either node is above
            if(smax1t==smax1b)then                    ! maximum salinity, find
              m1 = (z1t-z1b)/(salt1t-salt1b)          ! the location where the
              b1 = z1t - (m1*salt1t)                  ! lines for max salinity
              zmax = m1*smax1t + b1                   ! and actual salinity
            else                                      ! intersect and store the
              m2 = (smax1t-smax1b) / (z1t-z1b)        ! depth in zmax
              b2 = smax1t - m2*z1t
              zmax = (b2 - bv) / (mv - m2)
            endif
          endif
          if( (salt1t > smax1t .AND. salt1b < smin1b) .OR. &
              (salt1t < smin1t .AND. salt1b > smax1b))then
            if(z1t>z1b)then
              v1_z1t = max(zmin,zmax)
              v1_z1b = min(zmin,zmax)
            else
              v1_z1t = min(zmin,zmax)
              v1_z1b = max(zmin,zmax)
            endif
          elseif(salt1t>smax1t)then
            v1_z1t = zmax
          elseif(salt1t<smin1t)then
            v1_z1t = zmin
          elseif(salt1b>smax1b)then
            v1_z1b = zmax
          else
            v1_z1b = zmin
          endif
        endif

      endif


      !Constraint #2 - Temperature
      if( (temp1t > tmax1t .AND. temp1b > tmax1b)  .OR. &
          (temp1t < tmin1t .AND. temp1b < tmin1b)) then
        !Both nodes out, Change form
        SELECT CASE(v2_form)
          CASE(0)
            v2_form = 2
          CASE(1)
            v2_form = 3
          CASE(4)
            v2_form = 16
          CASE(5)
            v2_form = 7
          CASE(8)
            v2_form = 10
          CASE(12)
            v2_form = 14
          CASE(13)
            v2_form = 15
          CASE(18,19)
            v2_form = 11
        END SELECT

      else

        v2_z1t = z1t
        v2_z1b = z1b

        if(temp1t > tmax1t .OR. temp1t < tmin1t .OR. &
           temp1b > tmax1b .OR. temp1b < tmin1b)then
          !Find Linear Equation through (z1t,temp1t) and (z1b,temp1b)
          mv = (temp1t-temp1b) / (z1t-z1b)
          bv =  temp1t - mv*z1t

          if(temp1t<tmin1t .OR. temp1b<tmin1b)then
            if(tmin1t==tmin1b)then
              m1 = (z1t-z1b)/(temp1t-temp1b)
              b1 = z1t - (m1*temp1t)
              zmin = m1*tmin1t + b1
            else
              m1 = (tmin1t-tmin1b) / (z1t-z1b)
              b1 = tmin1t - m1*z1t
              zmin = (b1 - bv) / (mv - m1)
            endif
          endif
          if(temp1t>tmax1t .OR. temp1b>tmax1b)then
            if(tmax1t==tmax1b)then
              m1 = (z1t-z1b)/(temp1t-temp1b)
              b1 = z1t - (m1*temp1t)
              zmax = m1*tmax1t + b1
            else
              m2 = (tmax1t-tmax1b) / (z1t-z1b)
              b2 = tmax1t - m2*z1t
              zmax = (b2 - bv) / (mv - m2)
            endif
          endif
          if( (temp1t > tmax1t .AND. temp1b < tmin1b) .OR. &
              (temp1t < tmin1t .AND. temp1b > tmax1b))then
            if(z1t>z1b)then
              v2_z1t = max(zmin,zmax)
              v2_z1b = min(zmin,zmax)
            else
              v2_z1t = min(zmin,zmax)
              v2_z1b = max(zmin,zmax)
            endif
          elseif(temp1t>tmax1t)then
            v2_z1t = zmax
          elseif(temp1t<tmin1t)then
            v2_z1t = zmin
          elseif(temp1b>tmax1b)then
            v2_z1b = zmax
          else
            v2_z1b = zmin
          endif
        endif
      endif


      !Constraint #3 - Dissolved Oxygen
      if( (disO1t > dmax1t .AND. disO1b > dmax1b)  .OR. &
          (disO1t < dmin1t .AND. disO1b < dmin1b)) then
        !Both nodes out, Change form
        SELECT CASE(v3_form)
          CASE(0)
            v3_form = 2
          CASE(1)
            v3_form = 3
          CASE(4)
            v3_form = 16
          CASE(5)
            v3_form = 7
          CASE(8)
            v3_form = 10
          CASE(12)
            v3_form = 14
          CASE(13)
            v3_form = 15
          CASE(18,19)
            v3_form = 11
        END SELECT

      else

        v3_z1t = z1t
        v3_z1b = z1b

        if(disO1t > dmax1t .OR. disO1t < dmin1t .OR. &
           disO1b > dmax1b .OR. disO1b < dmin1b)then
          !Find Linear Equation through (z1t,disO1t) and (z1b,disO1b)
          mv = (disO1t-disO1b) / (z1t-z1b)
          bv =  disO1t - mv*z1t

          if(disO1t<dmin1t .OR. disO1b<dmin1b)then
            if(dmin1t==dmin1b)then
              m1 = (z1t-z1b)/(disO1t-disO1b)
              b1 = z1t - (m1*disO1t)
              zmin = m1*dmin1t + b1
            else
              m1 = (dmin1t-dmin1b) / (z1t-z1b)
              b1 = dmin1t - m1*z1t
              zmin = (b1 - bv) / (mv - m1)
            endif
          endif
          if(disO1t>dmax1t .OR. disO1b>dmax1b)then
            if(dmax1t==dmax1b)then
              m1 = (z1t-z1b)/(disO1t-disO1b)
              b1 = z1t - (m1*disO1t)
              zmax = m1*dmax1t + b1
            else
              m2 = (dmax1t-dmax1b) / (z1t-z1b)
              b2 = dmax1t - m2*z1t
              zmax = (b2 - bv) / (mv - m2)
            endif
          endif
          if( (disO1t > dmax1t .AND. disO1b < dmin1b) .OR. &
              (disO1t < dmin1t .AND. disO1b > dmax1b))then
            if(z1t>z1b)then
              v3_z1t = max(zmin,zmax)
              v3_z1b = min(zmin,zmax)
            else
              v3_z1t = min(zmin,zmax)
              v3_z1b = max(zmin,zmax)
            endif
          elseif(disO1t>dmax1t)then
            v3_z1t = zmax
          elseif(disO1t<dmin1t)then
            v3_z1t = zmin
          elseif(disO1b>dmax1b)then
            v3_z1b = zmax
          else
            v3_z1b = zmin
          endif
        endif
      endif

    endif


    !Node 2 (top left)
    if(form == 0 .OR. form == 1  .OR. form == 2 .OR. &
       form == 3 .OR. form == 4  .OR. form == 5 .OR. &
       form == 7 .OR. form == 16 .OR. form == 17) then

      !Constraint #1 - Salinity
      if( (salt2t > smax2t .AND. salt2b > smax2b)  .OR. &
          (salt2t < smin2t .AND. salt2b < smin2b)) then
        !Both nodes out, Change form
        SELECT CASE(v1_form)
          CASE(0)
            v1_form = 8
          CASE(1)
            v1_form = 19
          CASE(2)
            v1_form = 10
          CASE(3)
            v1_form = 11
          CASE(4)
            v1_form = 12
          CASE(5)
            v1_form = 13
          CASE(7)
            v1_form = 15
          CASE(16,17)
            v1_form = 14
        END SELECT

      else

        v1_z2t = z2t
        v1_z2b = z2b

        if(salt2t > smax2t .OR. salt2t < smin2t .OR. &
           salt2b > smax2b .OR. salt2b < smin2b)then
          !Find Linear Equation through (z2t,v1_1t) and (z2b,v1_1b)
          mv = (salt2t-salt2b) / (z2t-z2b)
          bv =  salt2t - mv*z2t

          if(salt2t<smin2t .OR. salt2b<smin2b)then
            if(smin2t==smin2b)then
              m1 = (z2t-z2b)/(salt2t-salt2b)
              b1 = z2t - (m1*salt2t)
              zmin = m1*smin2t + b1
            else
              m1 = (smin2t-smin2b) / (z2t-z2b)
              b1 = smin2t - m1*z2t
              zmin = (b1 - bv) / (mv - m1)
            endif
          endif
          if(salt2t>smax2t .OR. salt2b>smax2b)then
            if(smax2t==smax2b)then
              m1 = (z2t-z2b)/(salt2t-salt2b)
              b1 = z2t - (m1*salt2t)
              zmax = m1*smax2t + b1
            else
              m2 = (smax2t-smax2b) / (z2t-z2b)
              b2 = smax2t - m2*z2t
              zmax = (b2 - bv) / (mv - m2)
            endif
          endif
          if( (salt2t > smax2t .AND. salt2b < smin2b) .OR. &
              (salt2t < smin2t .AND. salt2b > smax2b))then
            if(z2t>z2b)then
              v1_z2t = max(zmin,zmax)
              v1_z2b = min(zmin,zmax)
            else
              v1_z2t = min(zmin,zmax)
              v1_z2b = max(zmin,zmax)
            endif
          elseif(salt2t>smax2t)then
            v1_z2t = zmax
          elseif(salt2t<smin2t)then
            v1_z2t = zmin
          elseif(salt2b>smax2b)then
            v1_z2b = zmax
          else
            v1_z2b = zmin
          endif
        endif
      endif


      !Constraint #2 - Temperature
      if( (temp2t > tmax2t .AND. temp2b > tmax2b)  .OR. &
          (temp2t < tmin2t .AND. temp2b < tmin2b)) then
        !Both nodes out, Change form
        SELECT CASE(v2_form)
          CASE(0)
            v2_form = 8
          CASE(1)
            v2_form = 19
          CASE(2)
            v2_form = 10
          CASE(3)
            v2_form = 11
          CASE(4)
            v2_form = 12
          CASE(5)
            v2_form = 13
          CASE(7)
            v2_form = 15
          CASE(16,17)
            v2_form = 14
        END SELECT

      else

        v2_z2t = z2t
        v2_z2b = z2b

        if(temp2t > tmax2t .OR. temp2t < tmin2t .OR. &
           temp2b > tmax2b .OR. temp2b < tmin2b)then
          !Find Linear Equation through (z2t,v2_1t) and (z2b,v2_1b)
          mv = (temp2t-temp2b) / (z2t-z2b)
          bv =  temp2t - mv*z2t

          if(temp2t<tmin2t .OR. temp2b<tmin2b)then
            if(tmin2t==tmin2b)then
              m1 = (z2t-z2b)/(temp2t-temp2b)
              b1 = z2t - (m1*temp2t)
              zmin = m1*tmin2t + b1
            else
              m1 = (tmin2t-tmin2b) / (z2t-z2b)
              b1 = tmin2t - m1*z2t
              zmin = (b1 - bv) / (mv - m1)
            endif
          endif
          if(temp2t>tmax2t .OR. temp2b>tmax2b)then
            if(tmax2t==tmax2b)then
              m1 = (z2t-z2b)/(temp2t-temp2b)
              b1 = z2t - (m1*temp2t)
              zmax = m1*tmax2t + b1
            else
              m2 = (tmax2t-tmax2b) / (z2t-z2b)
              b2 = tmax2t - m2*z2t
              zmax = (b2 - bv) / (mv - m2)
            endif
          endif
          if( (temp2t > tmax2t .AND. temp2b < tmin2b) .OR. &
              (temp2t < tmin2t .AND. temp2b > tmax2b))then
            if(z2t>z2b)then
              v2_z2t = max(zmin,zmax)
              v2_z2b = min(zmin,zmax)
            else
              v2_z2t = min(zmin,zmax)
              v2_z2b = max(zmin,zmax)
            endif
          elseif(temp2t>tmax2t)then
            v2_z2t = zmax
          elseif(temp2t<tmin2t)then
            v2_z2t = zmin
          elseif(temp2b>tmax2b)then
            v2_z2b = zmax
          else
            v2_z2b = zmin
          endif
        endif
      endif


      !Constraint #3 - Dissolved Oxygen
      if( (disO2t > dmax2t .AND. disO2b > dmax2b)  .OR. &
          (disO2t < dmin2t .AND. disO2b < dmin2b)) then
        !Both nodes out, Change form
        SELECT CASE(v3_form)
          CASE(0)
            v3_form = 8
          CASE(1)
            v3_form = 19
          CASE(2)
            v3_form = 10
          CASE(3)
            v3_form = 11
          CASE(4)
            v3_form = 12
          CASE(5)
            v3_form = 13
          CASE(7)
            v3_form = 15
          CASE(16,17)
            v3_form = 14
        END SELECT

      else

        v3_z2t = z2t
        v3_z2b = z2b

        if(disO2t > dmax2t .OR. disO2t < dmin2t .OR. &
           disO2b > dmax2b .OR. disO2b < dmin2b)then
          !Find Linear Equation through (z2t,v3_1t) and (z2b,v3_1b)
          mv = (disO2t-disO2b) / (z2t-z2b)
          bv =  disO2t - mv*z2t

          if(disO2t<dmin2t .OR. disO2b<dmin2b)then
            if(dmin2t==dmin2b)then
              m1 = (z2t-z2b)/(disO2t-disO2b)
              b1 = z2t - (m1*disO2t)
              zmin = m1*dmin2t + b1
            else
              m1 = (dmin2t-dmin2b) / (z2t-z2b)
              b1 = dmin2t - m1*z2t
              zmin = (b1 - bv) / (mv - m1)
            endif
          endif
          if(disO2t>dmax2t .OR. disO2b>dmax2b)then
            if(dmax2t==dmax2b)then
              m1 = (z2t-z2b)/(disO2t-disO2b)
              b1 = z2t - (m1*disO2t)
              zmax = m1*dmax2t + b1
            else
              m2 = (dmax2t-dmax2b) / (z2t-z2b)
              b2 = dmax2t - m2*z2t
              zmax = (b2 - bv) / (mv - m2)
            endif
          endif
          if( (disO2t > dmax2t .AND. disO2b < dmin2b) .OR. &
              (disO2t < dmin2t .AND. disO2b > dmax2b))then
            if(z2t>z2b)then
              v3_z2t = max(zmin,zmax)
              v3_z2b = min(zmin,zmax)
            else
              v3_z2t = min(zmin,zmax)
              v3_z2b = max(zmin,zmax)
            endif
          elseif(disO2t>dmax2t)then
            v3_z2t = zmax
          elseif(disO2t<dmin2t)then
            v3_z2t = zmin
          elseif(disO2b>dmax2b)then
            v3_z2b = zmax
          else
            v3_z2b = zmin
          endif
        endif
      endif


    endif


    !Node 3 (top right)
    if(form == 0  .OR. form == 1  .OR. form == 2  .OR. &
       form == 3  .OR. form == 8  .OR. form == 10 .OR. &
       form == 11 .OR. form == 18 .OR. form == 19) then

      !Constraint #1 - Salinity
      if( (salt3t > smax3t .AND. salt3b > smax3b)  .OR. &
          (salt3t < smin3t .AND. salt3b < smin3b)) then
        !Both nodes out, Change form
        SELECT CASE(v1_form)
          CASE(0)
            v1_form = 4
          CASE(1)
            v1_form = 5
          CASE(2)
            v1_form = 16
          CASE(3)
            v1_form = 7
          CASE(8)
            v1_form = 12
          CASE(10)
            v1_form = 14
          CASE(11)
            v1_form = 15
          CASE(18,19)
            v1_form = 13
        END SELECT

      else

        v1_z3t = z3t
        v1_z3b = z3b

        if(salt3t > smax3t .OR. salt3t < smin3t .OR. &
           salt3b > smax3b .OR. salt3b < smin3b)then
          !Find Linear Equation through (z3t,v1_1t) and (z3b,v1_1b)
          mv = (salt3t-salt3b) / (z3t-z3b)
          bv =  salt3t - mv*z3t

          if(salt3t<smin3t .OR. salt3b<smin3b)then
            if(smin3t==smin3b)then
              m1 = (z3t-z3b)/(salt3t-salt3b)
              b1 = z3t - (m1*salt3t)
              zmin = m1*smin3t + b1
            else
              m1 = (smin3t-smin3b) / (z3t-z3b)
              b1 = smin3t - m1*z3t
              zmin = (b1 - bv) / (mv - m1)
            endif
          endif
          if(salt3t>smax3t .OR. salt3b>smax3b)then
            if(smax3t==smax3b)then
              m1 = (z3t-z3b)/(salt3t-salt3b)
              b1 = z3t - (m1*salt3t)
              zmax = m1*smax3t + b1
            else
              m2 = (smax3t-smax3b) / (z3t-z3b)
              b2 = smax3t - m2*z3t
              zmax = (b2 - bv) / (mv - m2)
            endif
          endif
          if( (salt3t > smax3t .AND. salt3b < smin3b) .OR. &
              (salt3t < smin3t .AND. salt3b > smax3b))then
            if(z3t>z3b)then
              v1_z3t = max(zmin,zmax)
              v1_z3b = min(zmin,zmax)
            else
              v1_z3t = min(zmin,zmax)
              v1_z3b = max(zmin,zmax)
            endif
          elseif(salt3t>smax3t)then
            v1_z3t = zmax
          elseif(salt3t<smin3t)then
            v1_z3t = zmin
          elseif(salt3b>smax3b)then
            v1_z3b = zmax
          else
            v1_z3b = zmin
          endif
        endif
      endif


      !Constraint #2 - Temperature
      if( (temp3t > tmax3t .AND. temp3b > tmax3b)  .OR. &
          (temp3t < tmin3t .AND. temp3b < tmin3b)) then
        !Both nodes out, Change form
        SELECT CASE(v2_form)
          CASE(0)
            v2_form = 4
          CASE(1)
            v2_form = 5
          CASE(2)
            v2_form = 16
          CASE(3)
            v2_form = 7
          CASE(8)
            v2_form = 12
          CASE(10)
            v2_form = 14
          CASE(11)
            v2_form = 15
          CASE(18,19)
            v2_form = 13
        END SELECT

      else

        v2_z3t = z3t
        v2_z3b = z3b

        if(temp3t > tmax3t .OR. temp3t < tmin3t .OR. &
           temp3b > tmax3b .OR. temp3b < tmin3b)then
          !Find Linear Equation through (z3t,v2_1t) and (z3b,v2_1b)
          mv = (temp3t-temp3b) / (z3t-z3b)
          bv =  temp3t - mv*z3t

          if(temp3t<tmin3t .OR. temp3b<tmin3b)then
            if(tmin3t==tmin3b)then
              m1 = (z3t-z3b)/(temp3t-temp3b)
              b1 = z3t - (m1*temp3t)
              zmin = m1*tmin3t + b1
            else
              m1 = (tmin3t-tmin3b) / (z3t-z3b)
              b1 = tmin3t - m1*z3t
              zmin = (b1 - bv) / (mv - m1)
            endif
          endif
          if(temp3t>tmax3t .OR. temp3b>tmax3b)then
            if(tmax3t==tmax3b)then
              m1 = (z3t-z3b)/(temp3t-temp3b)
              b1 = z3t - (m1*temp3t)
              zmax = m1*tmax3t + b1
            else
              m2 = (tmax3t-tmax3b) / (z3t-z3b)
              b2 = tmax3t - m2*z3t
              zmax = (b2 - bv) / (mv - m2)
            endif
          endif
          if( (temp3t > tmax3t .AND. temp3b < tmin3b) .OR. &
              (temp3t < tmin3t .AND. temp3b > tmax3b))then
            if(z3t>z3b)then
              v2_z3t = max(zmin,zmax)
              v2_z3b = min(zmin,zmax)
            else
              v2_z3t = min(zmin,zmax)
              v2_z3b = max(zmin,zmax)
            endif
          elseif(temp3t>tmax3t)then
            v2_z3t = zmax
          elseif(temp3t<tmin3t)then
            v2_z3t = zmin
          elseif(temp3b>tmax3b)then
            v2_z3b = zmax
          else
            v2_z3b = zmin
          endif
        endif
      endif


      !Constraint #3 - Dissolved Oxygen
      if( (disO3t > dmax3t .AND. disO3b > dmax3b)  .OR. &
          (disO3t < dmin3t .AND. disO3b < dmin3b)) then
        !Both nodes out, Change form
        SELECT CASE(v3_form)
          CASE(0)
            v3_form = 4
          CASE(1)
            v3_form = 5
          CASE(2)
            v3_form = 16
          CASE(3)
            v3_form = 7
          CASE(8)
            v3_form = 12
          CASE(10)
            v3_form = 14
          CASE(11)
            v3_form = 15
          CASE(18,19)
            v3_form = 13
        END SELECT

      else

        v3_z3t = z3t
        v3_z3b = z3b

        if(disO3t > dmax3t .OR. disO3t < dmin3t .OR. &
           disO3b > dmax3b .OR. disO3b < dmin3b)then
          !Find Linear Equation through (z3t,v3_1t) and (z3b,v3_1b)
          mv = (disO3t-disO3b) / (z3t-z3b)
          bv =  disO3t - mv*z3t

          if(disO3t<dmin3t .OR. disO3b<dmin3b)then
            if(dmin3t==dmin3b)then
              m1 = (z3t-z3b)/(disO3t-disO3b)
              b1 = z3t - (m1*disO3t)
              zmin = m1*dmin3t + b1
            else
              m1 = (dmin3t-dmin3b) / (z3t-z3b)
              b1 = dmin3t - m1*z3t
              zmin = (b1 - bv) / (mv - m1)
            endif
          endif
          if(disO3t>dmax3t .OR. disO3b>dmax3b)then
            if(dmax3t==dmax3b)then
              m1 = (z3t-z3b)/(disO3t-disO3b)
              b1 = z3t - (m1*disO3t)
              zmax = m1*dmax3t + b1
            else
              m2 = (dmax3t-dmax3b) / (z3t-z3b)
              b2 = dmax3t - m2*z3t
              zmax = (b2 - bv) / (mv - m2)
            endif
          endif
          if( (disO3t > dmax3t .AND. disO3b < dmin3b) .OR. &
              (disO3t < dmin3t .AND. disO3b > dmax3b))then
            if(z3t>z3b)then
              v3_z3t = max(zmin,zmax)
              v3_z3b = min(zmin,zmax)
            else
              v3_z3t = min(zmin,zmax)
              v3_z3b = max(zmin,zmax)
            endif
          elseif(disO3t>dmax3t)then
            v3_z3t = zmax
          elseif(disO3t<dmin3t)then
            v3_z3t = zmin
          elseif(disO3b>dmax3b)then
            v3_z3b = zmax
          else
            v3_z3b = zmin
          endif
        endif
      endif

    endif


    !Node 4 (bottom right)
    if(form == 0  .OR. form == 2  .OR. form == 4  .OR. &
       form == 8  .OR. form == 10 .OR. form == 12 .OR. &
       form == 14 .OR. form == 16 .OR. form == 17) then

      !Constraint #1 - Salinity
      if( (salt4t > smax4t .AND. salt4b > smax4b)  .OR. &
          (salt4t < smin4t .AND. salt4b < smin4b)) then
        !Both nodes out, Change form
        SELECT CASE(v1_form)
          CASE(0)
            v1_form = 1
          CASE(2)
            v1_form = 3
          CASE(4)
            v1_form = 5
          CASE(8)
            v1_form = 19
          CASE(10)
            v1_form = 11
          CASE(12)
            v1_form = 13
          CASE(14)
            v1_form = 15
          CASE(16,17)
            v1_form = 7
        END SELECT

      else

        v1_z4t = z4t
        v1_z4b = z4b

        if(salt4t > smax4t .OR. salt4t < smin4t .OR. &
           salt4b > smax4b .OR. salt4b < smin4b)then
          !Find Linear Equation through (z4t,v1_1t) and (z4b,v1_1b)
          mv = (salt4t-salt4b) / (z4t-z4b)
          bv =  salt4t - mv*z4t

          if(salt4t<smin4t .OR. salt4b<smin4b)then
            if(smin4t==smin4b)then
              m1 = (z4t-z4b)/(salt4t-salt4b)
              b1 = z4t - (m1*salt4t)
              zmin = m1*smin4t + b1
            else
              m1 = (smin4t-smin4b) / (z4t-z4b)
              b1 = smin4t - m1*z4t
              zmin = (b1 - bv) / (mv - m1)
            endif
          endif
          if(salt4t>smax4t .OR. salt4b>smax4b)then
            if(smax4t==smax4b)then
              m1 = (z4t-z4b)/(salt4t-salt4b)
              b1 = z4t - (m1*salt4t)
              zmax = m1*smax4t + b1
            else
              m2 = (smax4t-smax4b) / (z4t-z4b)
              b2 = smax4t - m2*z4t
              zmax = (b2 - bv) / (mv - m2)
            endif
          endif
          if( (salt4t > smax4t .AND. salt4b < smin4b) .OR. &
              (salt4t < smin4t .AND. salt4b > smax4b))then
            if(z4t>z4b)then
              v1_z4t = max(zmin,zmax)
              v1_z4b = min(zmin,zmax)
            else
              v1_z4t = min(zmin,zmax)
              v1_z4b = max(zmin,zmax)
            endif
          elseif(salt4t>smax4t)then
            v1_z4t = zmax
          elseif(salt4t<smin4t)then
            v1_z4t = zmin
          elseif(salt4b>smax4b)then
            v1_z4b = zmax
          else
            v1_z4b = zmin
          endif
        endif
      endif


      !Constraint #2 - Temperature
      if( (temp4t > tmax4t .AND. temp4b > tmax4b)  .OR. &
          (temp4t < tmin4t .AND. temp4b < tmin4b)) then
        !Both nodes out, Change form
        SELECT CASE(v2_form)
          CASE(0)
            v2_form = 1
          CASE(2)
            v2_form = 3
          CASE(4)
            v2_form = 5
          CASE(8)
            v2_form = 19
          CASE(10)
            v2_form = 11
          CASE(12)
            v2_form = 13
          CASE(14)
            v2_form = 15
          CASE(16,17)
            v2_form = 7
        END SELECT

      else

        v2_z4t = z4t
        v2_z4b = z4b

        if(temp4t > tmax4t .OR. temp4t < tmin4t .OR. &
           temp4b > tmax4b .OR. temp4b < tmin4b)then
          !Find Linear Equation through (z4t,v2_1t) and (z4b,v2_1b)
          mv = (temp4t-temp4b) / (z4t-z4b)
          bv =  temp4t - mv*z4t

          if(temp4t<tmin4t .OR. temp4b<tmin4b)then
            if(tmin4t==tmin4b)then
              m1 = (z4t-z4b)/(temp4t-temp4b)
              b1 = z4t - (m1*temp4t)
              zmin = m1*tmin4t + b1
            else
              m1 = (tmin4t-tmin4b) / (z4t-z4b)
              b1 = tmin4t - m1*z4t
              zmin = (b1 - bv) / (mv - m1)
            endif
          endif
          if(temp4t>tmax4t .OR. temp4b>tmax4b)then
            if(tmax4t==tmax4b)then
              m1 = (z4t-z4b)/(temp4t-temp4b)
              b1 = z4t - (m1*temp4t)
              zmax = m1*tmax4t + b1
            else
              m2 = (tmax4t-tmax4b) / (z4t-z4b)
              b2 = tmax4t - m2*z4t
              zmax = (b2 - bv) / (mv - m2)
            endif
          endif
          if( (temp4t > tmax4t .AND. temp4b < tmin4b) .OR. &
              (temp4t < tmin4t .AND. temp4b > tmax4b))then
            if(z4t>z4b)then
              v2_z4t = max(zmin,zmax)
              v2_z4b = min(zmin,zmax)
            else
              v2_z4t = min(zmin,zmax)
              v2_z4b = max(zmin,zmax)
            endif
          elseif(temp4t>tmax4t)then
            v2_z4t = zmax
          elseif(temp4t<tmin4t)then
            v2_z4t = zmin
          elseif(temp4b>tmax4b)then
            v2_z4b = zmax
          else
            v2_z4b = zmin
          endif
        endif
      endif


      !Constraint #3 - Dissolved Oxygen
      if( (disO4t > dmax4t .AND. disO4b > dmax4b)  .OR. &
          (disO4t < dmin4t .AND. disO4b < dmin4b)) then
        !Both nodes out, Change form
        SELECT CASE(v3_form)
          CASE(0)
            v3_form = 1
          CASE(2)
            v3_form = 3
          CASE(4)
            v3_form = 5
          CASE(8)
            v3_form = 19
          CASE(10)
            v3_form = 11
          CASE(12)
            v3_form = 13
          CASE(14)
            v3_form = 15
          CASE(16,17)
            v3_form = 7
        END SELECT

      else

        v3_z4t = z4t
        v3_z4b = z4b

        if(disO4t > dmax4t .OR. disO4t < dmin4t .OR. &
           disO4b > dmax4b .OR. disO4b < dmin4b)then
          !Find Linear Equation through (z4t,v3_1t) and (z4b,v3_1b)
          mv = (disO4t-disO4b) / (z4t-z4b)
          bv =  disO4t - mv*z4t

          if(disO4t<dmin4t .OR. disO4b<dmin4b)then
            if(dmin4t==dmin4b)then
              m1 = (z4t-z4b)/(disO4t-disO4b)
              b1 = z4t - (m1*disO4t)
              zmin = m1*dmin4t + b1
            else
              m1 = (dmin4t-dmin4b) / (z4t-z4b)
              b1 = dmin4t - m1*z4t
              zmin = (b1 - bv) / (mv - m1)
            endif
          endif
          if(disO4t>dmax4t .OR. disO4b>dmax4b)then
            if(dmax4t==dmax4b)then
              m1 = (z4t-z4b)/(disO4t-disO4b)
              b1 = z4t - (m1*disO4t)
              zmax = m1*dmax4t + b1
            else
              m2 = (dmax4t-dmax4b) / (z4t-z4b)
              b2 = dmax4t - m2*z4t
              zmax = (b2 - bv) / (mv - m2)
            endif
          endif
          if( (disO4t > dmax4t .AND. disO4b < dmin4b) .OR. &
              (disO4t < dmin4t .AND. disO4b > dmax4b))then
            if(z4t>z4b)then
              v3_z4t = max(zmin,zmax)
              v3_z4b = min(zmin,zmax)
            else
              v3_z4t = min(zmin,zmax)
              v3_z4b = max(zmin,zmax)
            endif
          elseif(disO4t>dmax4t)then
            v3_z4t = zmax
          elseif(disO4t<dmin4t)then
            v3_z4t = zmin
          elseif(disO4b>dmax4b)then
            v3_z4b = zmax
          else
            v3_z4b = zmin
          endif
        endif
      endif


    endif

    !   +------------------------------------------------------------+
    !   |+----------------------------------------------------------+|
    !   ||            Calculate Volumes of 'ideal' Water            ||
    !   |+----------------------------------------------------------+|
    !   +------------------------------------------------------------+

!Calculate v1
    call getCTandCB(v1_form,v1_z1t,v1_z2t,v1_z3t,v1_z4t,  &
                    v1_z1b,v1_z2b,v1_z3b,v1_z4b,ct,cb)
    v1_vol = vol(x1,y1,v1_z1t,v1_z1b,x2,y2,v1_z2t,v1_z2b, &
                 x3,y3,v1_z3t,v1_z3b,x4,y4,v1_z4t,v1_z4b, &
                 cx,cy,ct,cb,v1_form)


!Calculate v2
    call getCTandCB(v2_form,v2_z1t,v2_z2t,v2_z3t,v2_z4t,  &
                    v2_z1b,v2_z2b,v2_z3b,v2_z4b,ct,cb)
    v2_vol = vol(x1,y1,v2_z1t,v2_z1b,x2,y2,v2_z2t,v2_z2b, &
                 x3,y3,v2_z3t,v2_z3b,x4,y4,v2_z4t,v2_z4b, &
                 cx,cy,ct,cb,v2_form)


!Calculate v3
    call getCTandCB(v3_form,v3_z1t,v3_z2t,v3_z3t,v3_z4t,  &
                    v3_z1b,v3_z2b,v3_z3b,v3_z4b,ct,cb)
    v3_vol = vol(x1,y1,v3_z1t,v3_z1b,x2,y2,v3_z2t,v3_z2b, &
                 x3,y3,v3_z3t,v3_z3b,x4,y4,v3_z4t,v3_z4b, &
                 cx,cy,ct,cb,v3_form)


!Calculate v1 & v2 combined
    if(z1t > z1b)then

      vv_z1t = min(v1_z1t,v2_z1t)
      vv_z1b = max(v1_z1b,v2_z1b)
      vv_z2t = min(v1_z2t,v2_z2t)
      vv_z2b = max(v1_z2b,v2_z2b)
      vv_z3t = min(v1_z3t,v2_z3t)
      vv_z3b = max(v1_z3b,v2_z3b)
      vv_z4t = min(v1_z4t,v2_z4t)
      vv_z4b = max(v1_z4b,v2_z4b)

      tmp_form_1 = 0
      if(vv_z1b > vv_z1t)tmp_form_1 = tmp_form_1 + 2
      if(vv_z2b > vv_z2t)tmp_form_1 = tmp_form_1 + 8
      if(vv_z3b > vv_z3t)tmp_form_1 = tmp_form_1 + 4
      if(vv_z4b > vv_z4t)tmp_form_1 = tmp_form_1 + 1

    else

      vv_z1t = max(v1_z1t,v2_z1t)
      vv_z1b = min(v1_z1b,v2_z1b)
      vv_z2t = max(v1_z2t,v2_z2t)
      vv_z2b = min(v1_z2b,v2_z2b)
      vv_z3t = max(v1_z3t,v2_z3t)
      vv_z3b = min(v1_z3b,v2_z3b)
      vv_z4t = max(v1_z4t,v2_z4t)
      vv_z4b = min(v1_z4b,v2_z4b)

      tmp_form_1 = 0
      if(vv_z1b < vv_z1t)tmp_form_1 = tmp_form_1 + 2
      if(vv_z2b < vv_z2t)tmp_form_1 = tmp_form_1 + 8
      if(vv_z3b < vv_z3t)tmp_form_1 = tmp_form_1 + 4
      if(vv_z4b < vv_z4t)tmp_form_1 = tmp_form_1 + 1

    endif

    if(tmp_form_1 == 6)tmp_form_1 = 16
    if(tmp_form_1 == 9)tmp_form_1 = 19
    tmp_form_2 = combineForms(v1_form,v2_form)
    v12_form = combineForms(tmp_form_1,tmp_form_2)

    if(v12_form == 15)then
      v12_vol = 0.0
    else
      call getCTandCB(v12_form,vv_z1t,vv_z2t,vv_z3t,vv_z4t,  &
                      vv_z1b,vv_z2b,vv_z3b,vv_z4b,ct,cb)
      v12_vol = vol(x1,y1,vv_z1t,vv_z1b,x2,y2,vv_z2t,vv_z2b, &
                    x3,y3,vv_z3t,vv_z3b,x4,y4,vv_z4t,vv_z4b, &
                    cx,cy,ct,cb,v12_form)
    endif


!Calculate v1 & v3 combined
    if(z1t > z1b)then

      vv_z1t = min(v1_z1t,v3_z1t)
      vv_z1b = max(v1_z1b,v3_z1b)
      vv_z2t = min(v1_z2t,v3_z2t)
      vv_z2b = max(v1_z2b,v3_z2b)
      vv_z3t = min(v1_z3t,v3_z3t)
      vv_z3b = max(v1_z3b,v3_z3b)
      vv_z4t = min(v1_z4t,v3_z4t)
      vv_z4b = max(v1_z4b,v3_z4b)

      tmp_form_1 = 0
      if(vv_z1b > vv_z1t)tmp_form_1 = tmp_form_1 + 2
      if(vv_z2b > vv_z2t)tmp_form_1 = tmp_form_1 + 8
      if(vv_z3b > vv_z3t)tmp_form_1 = tmp_form_1 + 4
      if(vv_z4b > vv_z4t)tmp_form_1 = tmp_form_1 + 1

    else

      vv_z1t = max(v1_z1t,v3_z1t)
      vv_z1b = min(v1_z1b,v3_z1b)
      vv_z2t = max(v1_z2t,v3_z2t)
      vv_z2b = min(v1_z2b,v3_z2b)
      vv_z3t = max(v1_z3t,v3_z3t)
      vv_z3b = min(v1_z3b,v3_z3b)
      vv_z4t = max(v1_z4t,v3_z4t)
      vv_z4b = min(v1_z4b,v3_z4b)

      tmp_form_1 = 0
      if(vv_z1b < vv_z1t)tmp_form_1 = tmp_form_1 + 2
      if(vv_z2b < vv_z2t)tmp_form_1 = tmp_form_1 + 8
      if(vv_z3b < vv_z3t)tmp_form_1 = tmp_form_1 + 4
      if(vv_z4b < vv_z4t)tmp_form_1 = tmp_form_1 + 1

    endif

    if(tmp_form_1 == 6)tmp_form_1 = 16
    if(tmp_form_1 == 9)tmp_form_1 = 19
    tmp_form_2 = combineForms(v1_form,v3_form)
    v13_form = combineForms(tmp_form_1,tmp_form_2)

    if(v13_form == 15)then
      v13_vol = 0.0
    else
      call getCTandCB(v13_form,vv_z1t,vv_z2t,vv_z3t,vv_z4t,  &
                      vv_z1b,vv_z2b,vv_z3b,vv_z4b,ct,cb)
      v13_vol = vol(x1,y1,vv_z1t,vv_z1b,x2,y2,vv_z2t,vv_z2b, &
                    x3,y3,vv_z3t,vv_z3b,x4,y4,vv_z4t,vv_z4b, &
                    cx,cy,ct,cb,v13_form)
    endif

!Calculate v2 & v3 combined
    if(z1t > z1b)then

      vv_z1t = min(v2_z1t,v3_z1t)
      vv_z1b = max(v2_z1b,v3_z1b)
      vv_z2t = min(v2_z2t,v3_z2t)
      vv_z2b = max(v2_z2b,v3_z2b)
      vv_z3t = min(v2_z3t,v3_z3t)
      vv_z3b = max(v2_z3b,v3_z3b)
      vv_z4t = min(v2_z4t,v3_z4t)
      vv_z4b = max(v2_z4b,v3_z4b)

      tmp_form_1 = 0
      if(vv_z1b > vv_z1t)tmp_form_1 = tmp_form_1 + 2
      if(vv_z2b > vv_z2t)tmp_form_1 = tmp_form_1 + 8
      if(vv_z3b > vv_z3t)tmp_form_1 = tmp_form_1 + 4
      if(vv_z4b > vv_z4t)tmp_form_1 = tmp_form_1 + 1

    else

      vv_z1t = max(v2_z1t,v3_z1t)
      vv_z1b = min(v2_z1b,v3_z1b)
      vv_z2t = max(v2_z2t,v3_z2t)
      vv_z2b = min(v2_z2b,v3_z2b)
      vv_z3t = max(v2_z3t,v3_z3t)
      vv_z3b = min(v2_z3b,v3_z3b)
      vv_z4t = max(v2_z4t,v3_z4t)
      vv_z4b = min(v2_z4b,v3_z4b)

      tmp_form_1 = 0
      if(vv_z1b < vv_z1t)tmp_form_1 = tmp_form_1 + 2
      if(vv_z2b < vv_z2t)tmp_form_1 = tmp_form_1 + 8
      if(vv_z3b < vv_z3t)tmp_form_1 = tmp_form_1 + 4
      if(vv_z4b < vv_z4t)tmp_form_1 = tmp_form_1 + 1

    endif

    if(tmp_form_1 == 6)tmp_form_1 = 16
    if(tmp_form_1 == 9)tmp_form_1 = 19
    tmp_form_2 = combineForms(v2_form,v3_form)
    v23_form = combineForms(tmp_form_1,tmp_form_2)

    if(v23_form == 15)then
      v23_vol = 0.0
    else
      call getCTandCB(v23_form,vv_z1t,vv_z2t,vv_z3t,vv_z4t,  &
                      vv_z1b,vv_z2b,vv_z3b,vv_z4b,ct,cb)
      v23_vol = vol(x1,y1,vv_z1t,vv_z1b,x2,y2,vv_z2t,vv_z2b, &
                    x3,y3,vv_z3t,vv_z3b,x4,y4,vv_z4t,vv_z4b, &
                    cx,cy,ct,cb,v23_form)
    endif

!Calculate v1 , v2 & v3 combined
    if(z1t > z1b)then

      vv_z1t = min(v1_z1t,v2_z1t,v3_z1t)
      vv_z1b = max(v1_z1b,v2_z1b,v3_z1b)
      vv_z2t = min(v1_z2t,v2_z2t,v3_z2t)
      vv_z2b = max(v1_z2b,v2_z2b,v3_z2b)
      vv_z3t = min(v1_z3t,v2_z3t,v3_z3t)
      vv_z3b = max(v1_z3b,v2_z3b,v3_z3b)
      vv_z4t = min(v1_z4t,v2_z4t,v3_z4t)
      vv_z4b = max(v1_z4b,v2_z4b,v3_z4b)

      tmp_form_1 = 0
      if(vv_z1b > vv_z1t)tmp_form_1 = tmp_form_1 + 2
      if(vv_z2b > vv_z2t)tmp_form_1 = tmp_form_1 + 8
      if(vv_z3b > vv_z3t)tmp_form_1 = tmp_form_1 + 4
      if(vv_z4b > vv_z4t)tmp_form_1 = tmp_form_1 + 1

    else

      vv_z1t = max(v1_z1t,v2_z1t,v3_z1t)
      vv_z1b = min(v1_z1b,v2_z1b,v3_z1b)
      vv_z2t = max(v1_z2t,v2_z2t,v3_z2t)
      vv_z2b = min(v1_z2b,v2_z2b,v3_z2b)
      vv_z3t = max(v1_z3t,v2_z3t,v3_z3t)
      vv_z3b = min(v1_z3b,v2_z3b,v3_z3b)
      vv_z4t = max(v1_z4t,v2_z4t,v3_z4t)
      vv_z4b = min(v1_z4b,v2_z4b,v3_z4b)

      tmp_form_1 = 0
      if(vv_z1b < vv_z1t)tmp_form_1 = tmp_form_1 + 2
      if(vv_z2b < vv_z2t)tmp_form_1 = tmp_form_1 + 8
      if(vv_z3b < vv_z3t)tmp_form_1 = tmp_form_1 + 4
      if(vv_z4b < vv_z4t)tmp_form_1 = tmp_form_1 + 1

    endif

    if(tmp_form_1 == 6)tmp_form_1 = 16
    if(tmp_form_1 == 9)tmp_form_1 = 19
    tmp_form_2 = combineForms(v12_form,v3_form)
    v123_form = combineForms(tmp_form_1,tmp_form_2)

    if(v123_form == 15)then
      v123_vol = 0.0
    else
      call getCTandCB(v123_form,vv_z1t,vv_z2t,vv_z3t,vv_z4t,  &
                      vv_z1b,vv_z2b,vv_z3b,vv_z4b,ct,cb)
      v123_vol = vol(x1,y1,vv_z1t,vv_z1b,x2,y2,vv_z2t,vv_z2b, &
                     x3,y3,vv_z3t,vv_z3b,x4,y4,vv_z4t,vv_z4b, &
                     cx,cy,ct,cb,v123_form)
    endif

  END SUBROUTINE combined_vol

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  SUBROUTINE category_vol(e,d,c,c_vol)
  USE HYDRO_DATA_MOD, ONLY: salt,temp,disO,bio,RE,rx,ry,rvr_form,zeta,depth,cats
  USE CONSTRAINTS_MOD, ONLY: getGrowth
  USE PARAM_MOD, ONLY: us,atBottom,offBott
  USE HYDRO_MOD, ONLY: getSlevel
  IMPLICIT NONE

    INTEGER, INTENT(IN) :: e,d,c  !e = element #, d = depth level, c = category
    DOUBLE PRECISION, INTENT(OUT) :: c_vol

    INTEGER :: n1,n2,n3,n4,t,b
    DOUBLE PRECISION :: x1,y1,z1t,z1b,x2,y2,z2t,z2b,x3,y3,z3t,z3b,x4,y4,z4t,z4b

    !midpoints of the polygons making up the top and bottom of the volume
    DOUBLE PRECISION :: cx,cy,ct,cb,mv,bv,tmp

    INTEGER :: form
    DOUBLE PRECISION :: c_z1t,c_z1b,c_z2t,c_z2b,c_z3t,c_z3b,c_z4t,c_z4b

    DOUBLE PRECISION :: bio1b,bio2b,bio3b,bio4b,bio1t,bio2t,bio3t,bio4t,  &
                salt1b,salt2b,salt3b,salt4b,salt1t,salt2t,salt3t,salt4t,  &
                temp1b,temp2b,temp3b,temp4b,temp1t,temp2t,temp3t,temp4t,  &
                disO1b,disO2b,disO3b,disO4b,disO1t,disO2t,disO3t,disO4t,  &
                bmax,bmin

    !   +------------------------------------------------------------+
    !   |+----------------------------------------------------------+|
    !   ||            Prepare Values at the Eight Corners           ||
    !   |+----------------------------------------------------------+|
    !   +------------------------------------------------------------+

    bmax = cats(c,2)
    bmin = cats(c,1)

    n1 = RE(1,e)
    n2 = RE(2,e)
    n3 = RE(3,e)
    n4 = RE(4,e)

    x1 = rx(n1)
    x2 = rx(n2)
    x3 = rx(n3)
    x4 = rx(n4)
    y1 = ry(n1)
    y2 = ry(n2)
    y3 = ry(n3)
    y4 = ry(n4)

    if(d==0)then
      b = 1
      t = 1
    elseif(d==us)then
      b = us
      t = us
    else
      b = d
      t = d+1
    endif

    salt1b = salt(n1,b)
    salt2b = salt(n2,b)
    salt3b = salt(n3,b)
    salt4b = salt(n4,b)
    salt1t = salt(n1,t)
    salt2t = salt(n2,t)
    salt3t = salt(n3,t)
    salt4t = salt(n4,t)
    temp1b = temp(n1,b)
    temp2b = temp(n2,b)
    temp3b = temp(n3,b)
    temp4b = temp(n4,b)
    temp1t = temp(n1,t)
    temp2t = temp(n2,t)
    temp3t = temp(n3,t)
    temp4t = temp(n4,t)
    disO1b = disO(n1,b)
    disO2b = disO(n2,b)
    disO3b = disO(n3,b)
    disO4b = disO(n4,b)
    disO1t = disO(n1,t)
    disO2t = disO(n2,t)
    disO3t = disO(n3,t)
    disO4t = disO(n4,t)

    bio1b  = bio(n1,b)
    bio2b  = bio(n2,b)
    bio3b  = bio(n3,b)
    bio4b  = bio(n4,b)
    bio1t  = bio(n1,t)
    bio2t  = bio(n2,t)
    bio3t  = bio(n3,t)
    bio4t  = bio(n4,t)


    if(d==0)then
      z1b = depth(n1)
      z2b = depth(n2)
      z3b = depth(n3)
      z4b = depth(n4)
    else
      z1b = getSlevel(zeta(n1),depth(n1),d)
      z2b = getSlevel(zeta(n2),depth(n2),d)
      z3b = getSlevel(zeta(n3),depth(n3),d)
      z4b = getSlevel(zeta(n4),depth(n4),d)
    endif

    if(d==us)then
      z1t = zeta(n1)
      z2t = zeta(n2)
      z3t = zeta(n3)
      z4t = zeta(n4)
    else
      z1t = getSlevel(zeta(n1),depth(n1),d+1)
      z2t = getSlevel(zeta(n2),depth(n2),d+1)
      z3t = getSlevel(zeta(n3),depth(n3),d+1)
      z4t = getSlevel(zeta(n4),depth(n4),d+1)
    endif

    form = rvr_form(e)

    !              +--------------------------------------+
    !              |   If Bottom Oriented Species check   |
    !              |    the depth of the eight corners    |
    !              +--------------------------------------+
    if(atBottom)then

      if(z1b-depth(n1) > offBott)then  !If bottom node at node 1 is above cutoff
        SELECT CASE(form)              !  change form to exclude this node
          CASE(0)
            form = 2
          CASE(1)
            form = 3
          CASE(4)
            form = 16
          CASE(5)
            form = 7
          CASE(8)
            form = 10
          CASE(12)
            form = 14
          CASE(13)
            form = 15
          CASE(18,19)
            form = 11
        END SELECT
      elseif(z1t-depth(n1) > offBott)then   !If just top node is above cutoff
        tmp    = depth(n1) + offBott        !  Move top node down to cutoff
        mv     = (salt1t-salt1b)/(z1t-z1b)  !  and use linear interpolation
        bv     = salt1t - mv*z1t            !  to determine values at new
        salt1t = mv*tmp + bv                !  top location
        mv     = (temp1t-temp1b)/(z1t-z1b)
        bv     = temp1t - mv*z1t
        temp1t = mv*tmp + bv
        mv     = (disO1t-disO1b)/(z1t-z1b)
        bv     = disO1t - mv*z1t
        disO1t = mv*tmp + bv
        z1t    = depth(n1) + offBott
        bio1t  = getGrowth(salt1t,temp1t,disO1t)
      endif

      if(z2b-depth(n2) > offBott)then  !If bottom node at node 1 is above cutoff
        SELECT CASE(form)              !  change form to exclude this node
          CASE(0)
            form = 8
          CASE(1)
            form = 19
          CASE(2)
            form = 10
          CASE(3)
            form = 11
          CASE(4)
            form = 12
          CASE(5)
            form = 13
          CASE(7)
            form = 15
          CASE(16,17)
            form = 14
        END SELECT
      elseif(z2t-depth(n2) > offBott)then   !If just top node is above cutoff
        tmp    = depth(n2) + offBott        !  Move top node down to cutoff
        mv     = (salt2t-salt2b)/(z2t-z2b)  !  and use linear interpolation
        bv     = salt2t - mv*z2t            !  to determine values at new
        salt2t = mv*tmp + bv                !  top location
        mv     = (temp2t-temp2b)/(z2t-z2b)
        bv     = temp2t - mv*z2t
        temp2t = mv*tmp + bv
        mv     = (disO2t-disO2b)/(z2t-z2b)
        bv     = disO2t - mv*z2t
        disO2t = mv*tmp + bv
        z2t    = depth(n2) + offBott
        bio2t  = getGrowth(salt2t,temp2t,disO2t)
      endif

      if(z3b-depth(n3) > offBott)then  !If bottom node at node 1 is above cutoff
        SELECT CASE(form)              !  change form to exclude this node
          CASE(0)
            form = 4
          CASE(1)
            form = 5
          CASE(2)
            form = 16
          CASE(3)
            form = 7
          CASE(8)
            form = 12
          CASE(10)
            form = 14
          CASE(11)
            form = 15
          CASE(18,19)
            form = 13
        END SELECT
      elseif(z3t-depth(n3) > offBott)then   !If just top node is above cutoff
        tmp    = depth(n3) + offBott        !  Move top node down to cutoff
        mv     = (salt3t-salt3b)/(z3t-z3b)  !  and use linear interpolation
        bv     = salt3t - mv*z3t            !  to determine values at new
        salt3t = mv*tmp + bv                !  top location
        mv     = (temp3t-temp3b)/(z3t-z3b)
        bv     = temp3t - mv*z3t
        temp3t = mv*tmp + bv
        mv     = (disO3t-disO3b)/(z3t-z3b)
        bv     = disO3t - mv*z3t
        disO3t = mv*tmp + bv
        z3t    = depth(n3) + offBott
        bio3t  = getGrowth(salt3t,temp3t,disO3t)
      endif

      if(z4b-depth(n4) > offBott)then  !If bottom node at node 1 is above cutoff
        SELECT CASE(form)              !  change form to exclude this node
          CASE(0)
            form = 1
          CASE(2)
            form = 3
          CASE(4)
            form = 5
          CASE(8)
            form = 19
          CASE(10)
            form = 11
          CASE(12)
            form = 13
          CASE(14)
            form = 15
          CASE(16,17)
            form = 7
        END SELECT
      elseif(z4t-depth(n4) > offBott)then   !If just top node is above cutoff
        tmp    = depth(n4) + offBott        !  Move top node down to cutoff
        mv     = (salt4t-salt4b)/(z4t-z4b)  !  and use linear interpolation
        bv     = salt4t - mv*z4t            !  to determine values at new
        salt4t = mv*tmp + bv                !  top location
        mv     = (temp4t-temp4b)/(z4t-z4b)
        bv     = temp4t - mv*z4t
        temp4t = mv*tmp + bv
        mv     = (disO4t-disO4b)/(z4t-z4b)
        bv     = disO4t - mv*z4t
        disO4t = mv*tmp + bv
        z4t    = depth(n4) + offBott
        bio4t  = getGrowth(salt4t,temp4t,disO4t)
      endif

    endif !atBottom

    c_z1t = z1t
    c_z1b = z1b
    c_z2t = z2t
    c_z2b = z2b
    c_z3t = z3t
    c_z3b = z3b
    c_z4t = z4t
    c_z4b = z4b

    cx = (x1 +x2 +x3 +x4 )/DBLE(4)
    cy = (y1 +y2 +y3 +y4 )/DBLE(4)

    !   +------------------------------------------------------------+
    !   |+----------------------------------------------------------+|
    !   ||          Determine Boundaries of Each Constraint         ||
    !   |+----------------------------------------------------------+|
    !   +------------------------------------------------------------+

    !Node 1 (bottom left)
    if(form == 0  .OR. form == 1  .OR. form == 4  .OR. &
       form == 5  .OR. form == 8  .OR. form == 12 .OR. &
       form == 13 .OR. form == 18 .OR. form == 19) then

      !Constraint #1 - Salinity
      if( (bio1t > bmax .AND. bio1b > bmax)  .OR. &
          (bio1t < bmin .AND. bio1b < bmin)) then
        !Both nodes out, Change form
        SELECT CASE(form)
          CASE(0)
            form = 2
          CASE(1)
            form = 3
          CASE(4)
            form = 16
          CASE(5)
            form = 7
          CASE(8)
            form = 10
          CASE(12)
            form = 14
          CASE(13)
            form = 15
          CASE(18,19)
            form = 11
        END SELECT
      else
        if(bio1t /= bio1b)then
          mv = (z1t-z1b)/(bio1t-bio1b)           !Find linear equation through
          bv = z1t - (mv*bio1t)                  !  (bio1t,z1t) and (bio1b,z1b)
          if(bio1t < bmin) c_z1t = mv*bmin + bv  !If either point is above the
          if(bio1b < bmin) c_z1b = mv*bmin + bv  !  max or below the min, solve
          if(bio1t > bmax) c_z1t = mv*bmax + bv  !  for the max or min to get
          if(bio1b > bmax) c_z1b = mv*bmax + bv  !  its new z location
        endif
      endif

    endif


    !Node 2 (top left)
    if(form == 0 .OR. form == 1  .OR. form == 2 .OR. &
       form == 3 .OR. form == 4  .OR. form == 5 .OR. &
       form == 7 .OR. form == 16 .OR. form == 17) then

      !Constraint #1 - Salinity
      if( (bio2t > bmax .AND. bio2b > bmax)  .OR. &
          (bio2t < bmin .AND. bio2b < bmin)) then
        !Both nodes out, Change form
        SELECT CASE(form)
          CASE(0)
            form = 8
          CASE(1)
            form = 19
          CASE(2)
            form = 10
          CASE(3)
            form = 11
          CASE(4)
            form = 12
          CASE(5)
            form = 13
          CASE(7)
            form = 15
          CASE(16,17)
            form = 14
        END SELECT
      else
        if(bio2t /= bio2b)then
          mv = (z2t-z2b)/(bio2t-bio2b)           !Find linear equation through
          bv = z2t - (mv*bio2t)                  !  (bio2t,z2t) and (bio2b,z2b)
          if(bio2t < bmin) c_z2t = mv*bmin + bv  !If either point is above the
          if(bio2b < bmin) c_z2b = mv*bmin + bv  !  max or below the min, solve
          if(bio2t > bmax) c_z2t = mv*bmax + bv  !  for the max or min to get
          if(bio2b > bmax) c_z2b = mv*bmax + bv  !  its new z location
        endif
      endif

    endif


    !Node 3 (top right)
    if(form == 0  .OR. form == 1  .OR. form == 2  .OR. &
       form == 3  .OR. form == 8  .OR. form == 10 .OR. &
       form == 11 .OR. form == 18 .OR. form == 19) then

      !Constraint #1 - Salinity
      if( (bio3t > bmax .AND. bio3b > bmax)  .OR. &
          (bio3t < bmin .AND. bio3b < bmin)) then
        !Both nodes out, Change form
        SELECT CASE(form)
          CASE(0)
            form = 4
          CASE(1)
            form = 5
          CASE(2)
            form = 16
          CASE(3)
            form = 7
          CASE(8)
            form = 12
          CASE(10)
            form = 14
          CASE(11)
            form = 15
          CASE(18,19)
            form = 13
        END SELECT
      else
        if(bio3t /= bio3b)then
          mv = (z3t-z3b)/(bio3t-bio3b)           !Find linear equation through
          bv = z3t - (mv*bio3t)                  !  (bio3t,z3t) and (bio3b,z3b)
          if(bio3t < bmin) c_z3t = mv*bmin + bv  !If either point is above the
          if(bio3b < bmin) c_z3b = mv*bmin + bv  !  max or below the min, solve
          if(bio3t > bmax) c_z3t = mv*bmax + bv  !  for the max or min to get
          if(bio3b > bmax) c_z3b = mv*bmax + bv  !  its new z location
        endif
      endif
    endif


    !Node 4 (bottom right)
    if(form == 0  .OR. form == 2  .OR. form == 4  .OR. &
       form == 8  .OR. form == 10 .OR. form == 12 .OR. &
       form == 14 .OR. form == 16 .OR. form == 17) then

      !Constraint #1 - Salinity
      if( (bio4t > bmax .AND. bio4b > bmax)  .OR. &
          (bio4t < bmin .AND. bio4b < bmin)) then
        !Both nodes out, Change form
        SELECT CASE(form)
          CASE(0)
            form = 1
          CASE(2)
            form = 3
          CASE(4)
            form = 5
          CASE(8)
            form = 19
          CASE(10)
            form = 11
          CASE(12)
            form = 13
          CASE(14)
            form = 15
          CASE(16,17)
            form = 7
        END SELECT
      else
        if(bio4t /= bio4b)then
          mv = (z4t-z4b)/(bio4t-bio4b)           !Find linear equation through
          bv = z4t - (mv*bio4t)                  !  (bio4t,z4t) and (bio4b,z4b)
          if(bio4t < bmin) c_z4t = mv*bmin + bv  !If either point is above the
          if(bio4b < bmin) c_z4b = mv*bmin + bv  !  max or below the min, solve
          if(bio4t > bmax) c_z4t = mv*bmax + bv  !  for the max or min to get
          if(bio4b > bmax) c_z4b = mv*bmax + bv  !  its new z location
        endif
      endif

    endif

    !   +------------------------------------------------------------+
    !   |+----------------------------------------------------------+|
    !   ||                 Calculate Volume of Water                ||
    !   |+----------------------------------------------------------+|
    !   +------------------------------------------------------------+

    call getCTandCB(form,c_z1t,c_z2t,c_z3t,c_z4t,     &
                    c_z1b,c_z2b,c_z3b,c_z4b,ct,cb)
    c_vol = vol(x1,y1,c_z1t,c_z1b,x2,y2,c_z2t,c_z2b,  &
                x3,y3,c_z3t,c_z3b,x4,y4,c_z4t,c_z4b,  &
                cx,cy,ct,cb,form)


  END SUBROUTINE category_vol

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  DOUBLE PRECISION FUNCTION vol(x1,y1,z1t,z1b,x2,y2,z2t,z2b,x3,y3,z3t,z3b,     &
                                x4,y4,z4t,z4b,cx,cy,ct,cb,form)
  IMPLICIT NONE

    INTEGER, INTENT(IN) :: form
    DOUBLE PRECISION, INTENT(IN) :: x1,y1,z1t,z1b,x2,y2,z2t,z2b,x3,y3,z3t,z3b, &
                                    x4,y4,z4t,z4b,cx,cy,ct,cb

    DOUBLE PRECISION :: h1,h2,h3,h4,h5,h6, & !height of the triangular prisms
                        t1,t2,t3,t4,t5,t6, & !area of the triangle used in prism
                        v1,v2,v3,v4,v5,v6, & !volume of each triangular prism
            xx1,xx2,xx3,xx4,yy1,yy2,yy3,yy4,         &
            zz1t,zz1b,zz2t,zz2b,zz3t,zz3b,zz4t,zz4b, &
            zmax,zmin,m,b,tmpx,tmpy,tmpt,tmpb,       &
            tx1,ty1,tz1t,tz1b,tx2,ty2,tz2t,tz2b,     &
            tx3,ty3,tz3t,tz3b,tx4,ty4,tz4t,tz4b

    xx1 = x1
    yy1 = y1
    xx2 = x2
    yy2 = y2
    xx3 = x3
    yy3 = y3
    xx4 = x4
    yy4 = y4
    zz1t = z1t
    zz1b = z1b
    zz2t = z2t
    zz2b = z2b
    zz3t = z3t
    zz3b = z3b
    zz4t = z4t
    zz4b = z4b

    SELECT CASE(form)
      CASE(0)

        !Find the heights of the triangular prisms
        h1 = abs(zz1b+zz2b+cb -zz1t-zz2t-ct)/DBLE(3)
        h2 = abs(zz2b+zz3b+cb -zz2t-zz3t-ct)/DBLE(3)
        h3 = abs(zz3b+zz4b+cb -zz3t-zz4t-ct)/DBLE(3)
        h4 = abs(zz4b+zz1b+cb -zz4t-zz1t-ct)/DBLE(3)

        !Find the area of each triangle                              !Triangle:
        t1 = DBLE(0.5)*abs(xx1*(yy2-cy) +xx2*(cy-yy1) +cx*(yy1-yy2)) !1 2 C
        t2 = DBLE(0.5)*abs(xx2*(yy3-cy) +xx3*(cy-yy2) +cx*(yy2-yy3)) !2 3 C
        t3 = DBLE(0.5)*abs(xx3*(yy4-cy) +xx4*(cy-yy3) +cx*(yy3-yy4)) !3 4 C
        t4 = DBLE(0.5)*abs(xx4*(yy1-cy) +xx1*(cy-yy4) +cx*(yy4-yy1)) !4 1 C

        !Find the volume of each triangular prism
        v1 = h1*t1
        v2 = h2*t2
        v3 = h3*t3
        v4 = h4*t4

        !Find total volume
        vol = v1 + v2 + v3 + v4

      CASE(1,2,4,8)

        !If a form other than 1, rotate the element nodes to match form 1
        if(form == 2)then
          tmpx = xx1
          tmpy = yy1
          tmpt = zz1t
          tmpb = zz1b
          xx1  = xx2
          yy1  = yy2
          zz1t = zz2t
          zz1b = zz2b
          xx2  = xx3
          yy2  = yy3
          zz2t = zz3t
          zz2b = zz3b
          xx3  = xx4
          yy3  = yy4
          zz3t = zz4t
          zz3b = zz4b
          xx4  = tmpx
          yy4  = tmpy
          zz4t = tmpt
          zz4b = tmpb
        elseif(form == 4)then
          tmpx = xx1
          tmpy = yy1
          tmpt = zz1t
          tmpb = zz1b
          xx1  = xx4
          yy1  = yy4
          zz1t = zz4t
          zz1b = zz4b
          xx4  = xx3
          yy4  = yy3
          zz4t = zz3t
          zz4b = zz3b
          xx3  = xx2
          yy3  = yy2
          zz3t = zz2t
          zz3b = zz2b
          xx2  = tmpx
          yy2  = tmpy
          zz2t = tmpt
          zz2b = tmpb
        elseif(form == 8)then
          tmpx = xx1
          tmpy = yy1
          tmpt = zz1t
          tmpb = zz1b
          xx1  = xx3
          yy1  = yy3
          zz1t = zz3t
          zz1b = zz3b
          xx3  = tmpx
          yy3  = tmpy
          zz3t = tmpt
          zz3b = tmpb
          tmpx = xx2
          tmpy = yy2
          tmpt = zz2t
          tmpb = zz2b
          xx2  = xx4
          yy2  = yy4
          zz2t = zz4t
          zz2b = zz4b
          xx4  = tmpx
          yy4  = tmpy
          zz4t = tmpt
          zz4b = tmpb
        endif

        !Find the midpoint between nodes 1 and 4 (referred to as 1/4 below)
        tx1  = (xx1  + xx4 ) / DBLE(2)
        ty1  = (yy1  + yy4 ) / DBLE(2)
        tz1t = (zz1t + zz4t) / DBLE(2)
        tz1b = (zz1b + zz4b) / DBLE(2)

        !Find the midpoint between nodes 3 and 4 (referred to as 3/4 below)
        tx2  = (xx3  + xx4 ) / DBLE(2)
        ty2  = (yy3  + yy4 ) / DBLE(2)
        tz2t = (zz3t + zz4t) / DBLE(2)
        tz2b = (zz3b + zz4b) / DBLE(2)

        !Find the heights of the triangular prisms
        h1 = abs(zz1b+zz2b+cb -zz1t-zz2t-ct)/DBLE(3)
        h2 = abs(zz2b+zz3b+cb -zz2t-zz3t-ct)/DBLE(3)
        h3 = abs(zz3b+tz2b+cb -zz3t-tz2t-ct)/DBLE(3)
        h4 = abs(tz1b+zz1b+cb -tz1t-zz1t-ct)/DBLE(3)

        !Find the area of each triangle                              !Triangle:
        t1 = DBLE(0.5)*abs(xx1*(yy2-cy) +xx2*(cy-yy1) +cx*(yy1-yy2)) !1  2  C
        t2 = DBLE(0.5)*abs(xx2*(yy3-cy) +xx3*(cy-yy2) +cx*(yy2-yy3)) !2  3  C
        t3 = DBLE(0.5)*abs(xx3*(ty2-cy) +tx2*(cy-yy3) +cx*(yy3-ty2)) !3 3/4 C
        t4 = DBLE(0.5)*abs(tx1*(yy1-cy) +xx1*(cy-ty1) +cx*(ty1-yy1)) !1 1/4 C

        !Find the volume of each triangular prism
        v1 = h1*t1
        v2 = h2*t2
        v3 = h3*t3
        v4 = h4*t4

        !Find total volume
        vol = v1 + v2 + v3 + v4

      CASE(3,5,10,12)

        !If a form other than 3, rotate the element nodes to match form 3
        if(form == 5)then
          tmpx = xx1
          tmpy = yy1
          tmpt = zz1t
          tmpb = zz1b
          xx1  = xx4
          yy1  = yy4
          zz1t = zz4t
          zz1b = zz4b
          xx4  = xx3
          yy4  = yy3
          zz4t = zz3t
          zz4b = zz3b
          xx3  = xx2
          yy3  = yy2
          zz3t = zz2t
          zz3b = zz2b
          xx2  = tmpx
          yy2  = tmpy
          zz2t = tmpt
          zz2b = tmpb
        elseif(form == 10)then
          tmpx = xx1
          tmpy = yy1
          tmpt = zz1t
          tmpb = zz1b
          xx1  = xx2
          yy1  = yy2
          zz1t = zz2t
          zz1b = zz2b
          xx2  = xx3
          yy2  = yy3
          zz2t = zz3t
          zz2b = zz3b
          xx3  = xx4
          yy3  = yy4
          zz3t = zz4t
          zz3b = zz4b
          xx4  = tmpx
          yy4  = tmpy
          zz4t = tmpt
          zz4b = tmpb
        elseif(form == 12)then
          tmpx = xx1
          tmpy = yy1
          tmpt = zz1t
          tmpb = zz1b
          xx1  = xx3
          yy1  = yy3
          zz1t = zz3t
          zz1b = zz3b
          xx3  = tmpx
          yy3  = tmpy
          zz3t = tmpt
          zz3b = tmpb
          tmpx = xx2
          tmpy = yy2
          tmpt = zz2t
          tmpb = zz2b
          xx2  = xx4
          yy2  = yy4
          zz2t = zz4t
          zz2b = zz4b
          xx4  = tmpx
          yy4  = tmpy
          zz4t = tmpt
          zz4b = tmpb
        endif

        !Find the midpoint between nodes 1 and 2 (referred to as 1/2 below)
        tx1  = (xx1  + xx2 ) / DBLE(2)
        ty1  = (yy1  + yy2 ) / DBLE(2)
        tz1t = (zz1t + zz2t) / DBLE(2)
        tz1b = (zz1b + zz2b) / DBLE(2)

        !Find the midpoint between nodes 3 and 4 (referred to as 3/4 below)
        tx2  = (xx3  + xx4 ) / DBLE(2)
        ty2  = (yy3  + yy4 ) / DBLE(2)
        tz2t = (zz3t + zz4t) / DBLE(2)
        tz2b = (zz3b + zz4b) / DBLE(2)

        !Find the heights of the triangular prisms
        h1 = abs(tz1b+zz2b+cb -tz1t-zz2t-ct)/DBLE(3)
        h2 = abs(zz2b+zz3b+cb -zz2t-zz3t-ct)/DBLE(3)
        h3 = abs(zz3b+tz2b+cb -zz3t-tz2t-ct)/DBLE(3)

        !Find the area of each triangle                              !Triangle:
        t1 = DBLE(0.5)*abs(tx1*(yy2-cy) +xx2*(cy-ty1) +cx*(ty1-yy2)) !2 1/2 C
        t2 = DBLE(0.5)*abs(xx2*(yy3-cy) +xx3*(cy-yy2) +cx*(yy2-yy3)) !2  3  C
        t3 = DBLE(0.5)*abs(xx3*(ty2-cy) +tx2*(cy-yy3) +cx*(yy3-ty2)) !3 3/4 C

        !Find the volume of each triangular prism
        v1 = h1*t1
        v2 = h2*t2
        v3 = h3*t3

        !Find total volume
        vol = v1 + v2 + v3

      CASE(7,11,13,14)

        !If a form other than 7, rotate the element nodes to match form 7
        if(form == 11)then
          tmpx = xx1
          tmpy = yy1
          tmpt = zz1t
          tmpb = zz1b
          xx1  = xx2
          yy1  = yy2
          zz1t = zz2t
          zz1b = zz2b
          xx2  = xx3
          yy2  = yy3
          zz2t = zz3t
          zz2b = zz3b
          xx3  = xx4
          yy3  = yy4
          zz3t = zz4t
          zz3b = zz4b
          xx4  = tmpx
          yy4  = tmpy
          zz4t = tmpt
          zz4b = tmpb
        elseif(form == 13)then
          tmpx = xx1
          tmpy = yy1
          tmpt = zz1t
          tmpb = zz1b
          xx1  = xx4
          yy1  = yy4
          zz1t = zz4t
          zz1b = zz4b
          xx4  = xx3
          yy4  = yy3
          zz4t = zz3t
          zz4b = zz3b
          xx3  = xx2
          yy3  = yy2
          zz3t = zz2t
          zz3b = zz2b
          xx2  = tmpx
          yy2  = tmpy
          zz2t = tmpt
          zz2b = tmpb
        elseif(form == 14)then
          tmpx = xx1
          tmpy = yy1
          tmpt = zz1t
          tmpb = zz1b
          xx1  = xx3
          yy1  = yy3
          zz1t = zz3t
          zz1b = zz3b
          xx3  = tmpx
          yy3  = tmpy
          zz3t = tmpt
          zz3b = tmpb
          tmpx = xx2
          tmpy = yy2
          tmpt = zz2t
          tmpb = zz2b
          xx2  = xx4
          yy2  = yy4
          zz2t = zz4t
          zz2b = zz4b
          xx4  = tmpx
          yy4  = tmpy
          zz4t = tmpt
          zz4b = tmpb
        endif

        !Find the midpoint between nodes 1 and 2 (referred to as 1/2 below)
        tx1  = (xx1  + xx2 ) / DBLE(2)
        ty1  = (yy1  + yy2 ) / DBLE(2)
        tz1t = (zz1t + zz2t) / DBLE(2)
        tz1b = (zz1b + zz2b) / DBLE(2)

        !Find the midpoint between nodes 2 and 3 (referred to as 2/3 below)
        tx2  = (xx2  + xx3 ) / DBLE(2)
        ty2  = (yy2  + yy3 ) / DBLE(2)
        tz2t = (zz2t + zz3t) / DBLE(2)
        tz2b = (zz2b + zz3b) / DBLE(2)

        !Find the heights of the triangular prisms
        h1 = abs(tz1b+zz2b+cb -tz1t-zz2t-ct)/DBLE(3)
        h2 = abs(zz2b+tz2b+cb -zz2t-tz2t-ct)/DBLE(3)

        !Find the area of each triangle                              !Triangle:
        t1 = DBLE(0.5)*abs(tx1*(yy2-cy) +xx2*(cy-ty1) +cx*(ty1-yy2)) !2 1/2 C
        t2 = DBLE(0.5)*abs(xx2*(ty2-cy) +tx2*(cy-yy2) +cx*(yy2-ty2)) !2 2/3 C

        !Find the volume of each triangular prism
        v1 = h1*t1
        v2 = h2*t2

        !Find total volume
        vol = v1 + v2

      !All land or non-ideal water, volume is 0.0
      CASE(15)
        vol = 0.0

      CASE(16,19)

        !If a form 19, rotate the element nodes to match form 16
        if(form == 19)then
          tmpx = xx1
          tmpy = yy1
          tmpt = zz1t
          tmpb = zz1b
          xx1  = xx2
          yy1  = yy2
          zz1t = zz2t
          zz1b = zz2b
          xx2  = xx3
          yy2  = yy3
          zz2t = zz3t
          zz2b = zz3b
          xx3  = xx4
          yy3  = yy4
          zz3t = zz4t
          zz3b = zz4b
          xx4  = tmpx
          yy4  = tmpy
          zz4t = tmpt
          zz4b = tmpb
        endif

        !Find the midpoint between nodes 1 and 2 (referred to as 1/2 below)
        tx1  = (xx1  + xx2 ) / DBLE(2)
        ty1  = (yy1  + yy2 ) / DBLE(2)
        tz1t = (zz1t + zz2t) / DBLE(2)
        tz1b = (zz1b + zz2b) / DBLE(2)

        !Find the midpoint between nodes 2 and 3 (referred to as 2/3 below)
        tx2  = (xx2  + xx3 ) / DBLE(2)
        ty2  = (yy2  + yy3 ) / DBLE(2)
        tz2t = (zz2t + zz3t) / DBLE(2)
        tz2b = (zz2b + zz3b) / DBLE(2)

        !Find the midpoint between nodes 3 and 4 (referred to as 3/4 below)
        tx3  = (xx3  + xx4 ) / DBLE(2)
        ty3  = (yy3  + yy4 ) / DBLE(2)
        tz3t = (zz3t + zz4t) / DBLE(2)
        tz3b = (zz3b + zz4b) / DBLE(2)

        !Find the midpoint between nodes 1 and 4 (referred to as 1/4 below)
        tx4  = (xx1  + xx4 ) / DBLE(2)
        ty4  = (yy1  + yy4 ) / DBLE(2)
        tz4t = (zz1t + zz4t) / DBLE(2)
        tz4b = (zz1b + zz4b) / DBLE(2)

        !Find the heights of the triangular prisms
        h1 = abs(tz1b+zz2b+cb -tz1t-zz2t-ct)/DBLE(3)
        h2 = abs(zz2b+tz2b+cb -zz2t-tz2t-ct)/DBLE(3)
        h3 = abs(tz3b+zz4b+cb -tz3t-zz4t-ct)/DBLE(3)
        h4 = abs(zz4b+tz4b+cb -zz4t-tz4t-ct)/DBLE(3)
        h5 = abs(tz1b+tz4b+cb -tz1t-tz4t-ct)/DBLE(3)
        h6 = abs(tz2b+tz3b+cb -tz2t-tz3t-ct)/DBLE(3)

        !Find the area of each triangle                              !Triangle:
        t1 = DBLE(0.5)*abs(tx1*(yy2-cy) +xx2*(cy-ty1) +cx*(ty1-yy2)) ! 2  1/2 C
        t2 = DBLE(0.5)*abs(xx2*(ty2-cy) +tx2*(cy-yy2) +cx*(yy2-ty2)) ! 2  2/3 C
        t3 = DBLE(0.5)*abs(tx3*(yy4-cy) +xx4*(cy-ty3) +cx*(ty3-yy4)) ! 4  3/4 C
        t4 = DBLE(0.5)*abs(xx4*(ty4-cy) +tx4*(cy-yy4) +cx*(yy4-ty4)) ! 4  1/4 C
        t5 = DBLE(0.5)*abs(tx1*(ty4-cy) +tx4*(cy-ty1) +cx*(ty1-ty4)) !1/2 1/4 C
        t6 = DBLE(0.5)*abs(tx2*(ty3-cy) +tx3*(cy-ty2) +cx*(ty2-ty3)) !2/3 3/4 C

        !Find the volume of each triangular prism
        v1 = h1*t1
        v2 = h2*t2
        v3 = h3*t3
        v4 = h4*t4
        v5 = h5*t5
        v6 = h6*t6

        !Find total volume
        vol = v1 + v2 + v3 + v4 + v5 + v6

      CASE(17,18)

        !If a form 18, rotate the element nodes to match form 17
        if(form == 18)then
          tmpx = xx1
          tmpy = yy1
          tmpt = zz1t
          tmpb = zz1b
          xx1  = xx2
          yy1  = yy2
          zz1t = zz2t
          zz1b = zz2b
          xx2  = xx3
          yy2  = yy3
          zz2t = zz3t
          zz2b = zz3b
          xx3  = xx4
          yy3  = yy4
          zz3t = zz4t
          zz3b = zz4b
          xx4  = tmpx
          yy4  = tmpy
          zz4t = tmpt
          zz4b = tmpb
        endif

        !Find the midpoint between nodes 1 and 2 (referred to as 1/2 below)
        tx1  = (xx1  + xx2 ) / DBLE(2)
        ty1  = (yy1  + yy2 ) / DBLE(2)
        tz1t = (zz1t + zz2t) / DBLE(2)
        tz1b = (zz1b + zz2b) / DBLE(2)

        !Find the midpoint between nodes 2 and 3 (referred to as 2/3 below)
        tx2  = (xx2  + xx3 ) / DBLE(2)
        ty2  = (yy2  + yy3 ) / DBLE(2)
        tz2t = (zz2t + zz3t) / DBLE(2)
        tz2b = (zz2b + zz3b) / DBLE(2)

        !Find the midpoint between nodes 3 and 4 (referred to as 3/4 below)
        tx3  = (xx3  + xx4 ) / DBLE(2)
        ty3  = (yy3  + yy4 ) / DBLE(2)
        tz3t = (zz3t + zz4t) / DBLE(2)
        tz3b = (zz3b + zz4b) / DBLE(2)

        !Find the midpoint between nodes 1 and 4 (referred to as 1/4 below)
        tx4  = (xx1  + xx4 ) / DBLE(2)
        ty4  = (yy1  + yy4 ) / DBLE(2)
        tz4t = (zz1t + zz4t) / DBLE(2)
        tz4b = (zz1b + zz4b) / DBLE(2)

        !Find the heights of the triangular prisms
        h1 = abs(tz1b+tz2b+zz2b -tz1t-tz2t-zz2t)/DBLE(3)
        h2 = abs(tz3b+tz4b+zz4b -tz3t-tz4t-zz4t)/DBLE(3)

        !Find the area of each triangle                              !Triangle:
        t1 =DBLE(0.5)*abs(tx1*(ty2-yy2)+tx2*(yy2-ty1)+xx2*(ty1-ty2)) !1/2 2 2/3
        t2 =DBLE(0.5)*abs(tx3*(ty4-yy4)+tx4*(yy4-ty3)+xx4*(ty3-ty4)) !3/4 4 1/4

        !Find the volume of each triangular prism
        v1 = h1*t1
        v2 = h2*t2

        !Find total volume
        vol = v1 + v2

      CASE DEFAULT
        write(*,*) "Error, Impossible Case: ",form
        pause

    END SELECT

  END FUNCTION vol

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  DOUBLE PRECISION FUNCTION totalVol(x1,y1,z1t,z1b,x2,y2,z2t,z2b,x3,y3,z3t,    &
                                 z3b,x4,y4,z4t,z4b,form)
  IMPLICIT NONE

    INTEGER, INTENT(IN) :: form
    DOUBLE PRECISION, INTENT(IN) :: x1,y1,z1t,z1b,x2,y2,z2t,z2b,x3,y3,z3t,z3b, &
                                    x4,y4,z4t,z4b

    DOUBLE PRECISION :: cx,cy,ct,cb  !midpoints of the polygons making up the 
                                     !  top and bottom of the volume

    cx = (x1 +x2 +x3 +x4 )/DBLE(4)
    cy = (y1 +y2 +y3 +y4 )/DBLE(4)

    call getCTandCB(form,z1t,z2t,z3t,z4t,z1b,z2b,z3b,z4b,ct,cb)

    totalVol = vol(x1,y1,z1t,z1b,x2,y2,z2t,z2b,x3,y3,z3t,z3b,x4,y4,z4t,z4b,    &
                   cx,cy,ct,cb,form)

  END FUNCTION totalVol

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  SUBROUTINE getCTandCB(form,zz1t,zz2t,zz3t,zz4t,zz1b,zz2b,zz3b,zz4b,ct,cb)
    INTEGER, INTENT(IN) :: form
    DOUBLE PRECISION, INTENT(IN) :: zz1t,zz2t,zz3t,zz4t,zz1b,zz2b,zz3b,zz4b
    DOUBLE PRECISION, INTENT(OUT) :: ct,cb

    ct = 0.0
    cb = 0.0

    !Calculate based on Form
    SELECT CASE(form)
    CASE(0,15)
      ct = (zz1t+zz2t+zz3t+zz4t) / DBLE(4)
      cb = (zz1b+zz2b+zz3b+zz4b) / DBLE(4)
    CASE(1)
      ct = (zz1t+zz2t+zz3t) / DBLE(3)
      cb = (zz1b+zz2b+zz3b) / DBLE(3)
    CASE(2)
      ct = (zz2t+zz3t+zz4t) / DBLE(3)
      cb = (zz2b+zz3b+zz4b) / DBLE(3)
    CASE(3)
      ct = (zz2t+zz3t) / DBLE(2)
      cb = (zz2b+zz3b) / DBLE(2)
    CASE(4)
      ct = (zz1t+zz2t+zz4t) / DBLE(3)
      cb = (zz1b+zz2b+zz4b) / DBLE(3)
    CASE(5)
      ct = (zz1t+zz2t) / DBLE(2)
      cb = (zz1b+zz2b) / DBLE(2)
    CASE(7)
      ct = (zz2t)
      cb = (zz2b)
    CASE(8)
      ct = (zz1t+zz3t+zz4t) / DBLE(3)
      cb = (zz1b+zz3b+zz4b) / DBLE(3)
    CASE(10)
      ct = (zz3t+zz4t) / DBLE(2)
      cb = (zz3b+zz4b) / DBLE(2)
    CASE(11)
      ct = (zz3t)
      cb = (zz3b)
    CASE(12)
      ct = (zz1t+zz4t) / DBLE(2)
      cb = (zz1b+zz4b) / DBLE(2)
    CASE(13)
      ct = (zz1t)
      cb = (zz1b)
    CASE(14)
      ct = (zz4t)
      cb = (zz4b)
    CASE(16,17)
      ct = (zz2t+zz4t) / DBLE(2)
      cb = (zz2b+zz4b) / DBLE(2)
    CASE(18,19)
      ct = (zz1t+zz3t) / DBLE(2)
      cb = (zz1b+zz3b) / DBLE(2)
    CASE DEFAULT
      write(*,*)'Error: Undefined Case'
      stop
    END SELECT

  END SUBROUTINE getCTandCB

! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  INTEGER FUNCTION combineForms(form1,form2)
    INTEGER, INTENT(IN) :: form1,form2
    INTEGER :: form(0:19,0:19)

    form( 0,:) = (/ 0, 1, 2, 3, 4, 5,-1, 7, 8,-1,10,11,12,13,14,15,16,17,18,19/)
    form( 1,:) = (/ 1, 1, 3, 3, 5, 5,-1, 7,19,-1,11,11,13,13,15,15, 7, 7,18,19/)
    form( 2,:) = (/ 2, 3, 2, 3,16, 7,-1, 7,10,-1,10,11,14,15,14,15,16,17,11,11/)
    form( 3,:) = (/ 3, 3, 3, 3, 7, 7,-1, 7,11,-1,11,11,15,15,15,15, 7, 7,11,11/)
    form( 4,:) = (/ 4, 5,16, 7, 4, 5,-1, 7,12,-1,14,15,12,13,14,15,16,17,13,13/)
    form( 5,:) = (/ 5, 5, 7, 7, 5, 5,-1, 7,13,-1,15,15,13,13,15,15, 7, 7,13,13/)
    form( 6,:) = (/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/)
    form( 7,:) = (/ 7, 7, 7, 7, 7, 7,-1, 7,15,-1,15,15,15,15,15,15, 7, 7,15,15/)
    form( 8,:) = (/ 8,19,10,11,12,13,-1,15, 8,-1,10,11,12,13,14,15,14,14,18,19/)
    form( 9,:) = (/-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1/)
    form(10,:) = (/10,11,10,11,14,15,-1,15,10,-1,10,11,14,15,14,15,14,14,11,11/)
    form(11,:) = (/11,11,11,11,15,15,-1,15,11,-1,11,11,15,15,15,15,15,15,11,11/)
    form(12,:) = (/12,13,14,15,12,13,-1,15,12,-1,14,15,12,13,14,15,14,14,13,13/)
    form(13,:) = (/13,13,15,15,13,13,-1,15,13,-1,15,15,13,13,15,15,15,15,13,13/)
    form(14,:) = (/14,15,14,15,14,15,-1,15,14,-1,14,15,14,15,14,15,14,14,15,15/)
    form(15,:) = (/15,15,15,15,15,15,-1,15,15,-1,15,15,15,15,15,15,15,15,15,15/)
    form(16,:) = (/16, 7,16, 7,16, 7,-1, 7,14,-1,14,15,14,15,14,15,16,17,15,15/)
    form(17,:) = (/17, 7,17, 7,17, 7,-1, 7,14,-1,14,15,14,15,14,15,17,17,15,15/)
    form(18,:) = (/18,18,11,11,13,13,-1,15,18,-1,11,11,13,13,15,15,15,15,18,18/)
    form(19,:) = (/19,19,11,11,13,13,-1,15,19,-1,11,11,13,13,15,15,15,15,18,19/)

    combineForms = form(form1,form2)

  END FUNCTION combineForms

END MODULE VOLUME_MOD