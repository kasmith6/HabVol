MODULE CONSTRAINTS_MOD

!  The Constraint Module 
!
!  Created by:        Zachary Schlag
!  Created on:        14 Jul 2011
!  Last Modified on:  14 Jul 2011

CONTAINS

  SUBROUTINE getConstraints(s,t,o,maxsalt,minsalt,maxtemp,mintemp,maxdo,mindo)
    IMPLICIT NONE

    DOUBLE PRECISION, INTENT(IN) :: s,t,o
    DOUBLE PRECISION, INTENT(OUT):: maxsalt,minsalt,maxtemp,mintemp,maxdo,mindo

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    !  Atlantic Sturgeon Dissolved Oxygen Code
    !    -if not using, please comment out

   ! DOUBLE PRECISION :: DOtps,TK,DOml,DOmg,DOtmg,DOtmmol

    !Atlantic Sturgeon DO Calculation
    !Calculate DO threshold in percent sat using mortality equation with set
    !mortality of .0625 (.5 per 6 hr)
    !DOtps =  ( (log(log(.0625)/log(.99)) - (DBLE(.4)*(s-DBLE(10.3))))/DBLE(.8) &
      !         -t+DBLE(18.9) )/DBLE(-.3)+DBLE(72.3)
    !Calculate 100% DO saturation at T(Kelvin) and S
    !TK = DBLE(273.15)+t
    !DOml = exp(DBLE(-173.4292)+(DBLE(249.6339)*(DBLE(100)/TK))                 &
     !      +(DBLE(143.3483)*log(TK/DBLE(100)))+(DBLE(-21.8492)*(TK/DBLE(100))) &
      !     +S*(DBLE(-0.033096)+DBLE(0.014259)*(TK/DBLE(100))                   &
       !    +(DBLE(-0.0017)*(TK/DBLE(100))**2)))
    !Convert DO in ml to mg using standard pressure
    !DOmg = ((DBLE(760)/TK)*DBLE(0.513))*DOml
    !Convert DO threshold in percent sat to mg
    !DOtmg = (DOtps*DOmg)/DBLE(100)
    !Convert DO threshold in mg to mmole/m^3
    !DOtmmol = (DOtmg/DBLE(32))*DBLE(1000)

    !~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    !For each of the following you may either:
    !  Set a specific value such as:
    !    maxsalt = 22
    !    minsalt = 14
    !  OR specify an equation to calculate the value based on other factors,
    !  such as:
    !    maxtemp = DBLE(0.0735)*s + DBLE(36.5)
    !    mintemp = DBLE(0.0484)*s + DBLE(5.1548)
    !      where s = salinity
    !
    !  Notes:
    !    putting DBLE() around numbers (as demonstrated above) ensures that 
    !      calculations are made in double precision
    !
    !    if no constraint is desired for a particular value below, use a
    !      a really large value such as 9999.999 (positive or negative, 
    !      dependent on max or min) to ensure nothing is constrained
    !
    !    when writing an equation below based on other factors:
    !      s = salinity, t = temperature, o = dissolved oxygen

    maxsalt =  36

    minsalt =   5

    maxtemp =  35

    mintemp =  8

    maxdo   =  100000.0

    mindo   =  159.375


  END SUBROUTINE getConstraints


  DOUBLE PRECISION FUNCTION getGrowth(s,t,o)
    USE PARAM_MOD, ONLY: b_species
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: s,t,o
    !  s = salinity, t = temperature, o = dissolved oxygen

    !Variables
    !  Routine Metabolism
    DOUBLE PRECISION :: arm,brm,tk1rm,tk4rm,crm,drm,grm,hrm,irm,smin
    !  Food Consumption
    DOUBLE PRECISION :: afc,bfc,tk1fc,tk4fc,tl98fc,cfc,dfc,gfc,jfc,kfc
    !  Egestion
    DOUBLE PRECISION :: aeg,ceg,deg,geg,E,EG
    !  Excretion
    DOUBLE PRECISION :: aex,bex,cex,EX
    !  SDA
    DOUBLE PRECISION :: asda,SDA
    !  AM
    DOUBLE PRECISION :: aact,AM
    !  Other
    DOUBLE PRECISION :: rt1,rt4,s4,s1,do1,B1,OX,CT1,CT4,GR,CJ_OBS,RATION
    !  Modelo RM
    DOUBLE PRECISION :: y1,ey1,FTrm,FSArm,FSBrm,FSrm,DOCrm,KO1rm,dorel,SLrm,   &
                        FOrm,KRM,RM
    !  Modelo FC
    DOUBLE PRECISION :: cy1,ecy1,cka,cy2,ecy2,ckb,FTfc,CKS1,CKS4,YA,EYA,KSA,   &
                        YB,EYB,KSB,FSfc,DOCfc,KO1fc,SLfc,FOfc,CJG_MAX,CJ_MAX,  &
                        CJG_PRED,CJ_PRED,CJ
    !  Energy Content
    DOUBLE PRECISION :: TL,LNL,WS,ENEC
    !  Growth
    DOUBLE PRECISION :: GKJ_PRED,G_PRED
	!  Set Lower Boundary for Temperature
	DOUBLE PRECISION :: temp,oxy


    !********************** STRIPED BASS ***********************
    DOUBLE PRECISION :: Kprey,Kpred,CA,CB,CK1,CTO,CQ,CK4,CTL,CTM,RA,RB,RQ,     &
      RTO,FA,UA,G1,G2,L1,L2,Ka,Kb,FCT,FCDO,C,ACT,FRT,R,F,SS,U,M
    !The following variable is used by striped bass but shares a name with a
    !  variable in the atlantic sturgeon section above: SDA


    !*********************** CONVERT DO ************************
    ! This section converts dissolved oxygen from millimoles to
    !   percent saturation.
    DOUBLE PRECISION :: domg,do100ml,do100mg,dops,k

    temp = t
     if (t.le.0.1) then
       temp = DBLE(0.1)
	endif

    k = temp+DBLE(273.15)

    domg = o * DBLE(32) / DBLE(1000)
    do100ml = exp( DBLE(-173.4292) + (DBLE(249.6339) * (DBLE(100)/k)) +        &
      (DBLE(143.3489) * log(k/DBLE(100))) + (DBLE(-21.8492) * (k/DBLE(100))) + &
      s*( DBLE(-0.033096) + DBLE(0.014259)*(k/DBLE(100)) +                     &
      (DBLE(-0.0017)*(k/DBLE(100))**2) ) )
    do100mg = (DBLE(760)/k*DBLE(0.513))*do100ml
    dops = domg * DBLE(100) / do100mg
	if (dops.GT.100.0) dops = DBLE(100)
	if (dops.LT.25.0) dops = DBLE(25.0)

    GR     = 200 !*REPLACE THIS BY THE ACTUAL FISH WEIGHT IF NEEDED
    CJ_OBS = 0  !*IF YOU DECLARE THIS VARIABLE EQUAL TO 0, THE MODEL ASSUMES MAXIMUM CONSUMPTION RATE (CMAX)
    RATION = 1  !*IF YOU DECLARE THIS VARIABLE EQUAL TO 1, THE MODEL ASSUMES p-value=1

    !********* DETERMINE SPECIES SPECIFIC BIOENERGETICS ********

    SELECT CASE(b_species)

    !+---------------------------------------------------------+
    !+              Atlantic Sturgeon Mortality                |
    !+---------------------------------------------------------+
	CASE(1)

      getGrowth = DBLE(.99)**exp(DBLE(.4)*(s-DBLE(10.3))+DBLE(.8)*             &
                  (t-DBLE(18.9)-DBLE(.3)*(dops-DBLE(72.3))))


    !+---------------------------------------------------------+
    !+              Striped Bass Bioenergetics                 |
    !+---------------------------------------------------------+
    CASE(2)

    !Adult Striped Bass Bioenergetics Model
    !Variables
    !T = Water Temperature (C)
    !M = Mass of fish
    !Oxygen calculated in %saturation

      M    = 1179.334 !*REPLACE THIS BY THE ACTUAL FISH WEIGHT IF NEEDED

      !Constants
      Kprey = 6488
      Kpred = 6488
      CA = .3021
      CB = -.2523
      CK1 = .255
      CTO = 18
      CQ = 6.6
      CK4 = 0.9
      CTL = 32
      CTM = 29
      RA = 0.0028
      RB = -0.218
      RQ = 0.076
      RTO = 0.5002
      SDA = 0.172
      FA = 0.104
      UA = 0.068

      !Equations
      !Consumption
      G1 = (DBLE(1)/(CTO-CQ))*log((DBLE(0.98)*(DBLE(1)-CK1))/(CK1*DBLE(0.02)))
      G2 = (DBLE(1)/(CTL-CTM))*log((DBLE(0.98)*(DBLE(1)-CK4))/(CK4*DBLE(0.02)))
      L1 = exp(G1*(T-CQ))
      L2 = exp(G2*(CTL-T))
      Ka = (CK1*L1)/(DBLE(1)+CK1*(L1-DBLE(1)))
      Kb = (CK4*L2)/(DBLE(1)+CK4*(L2-DBLE(1)))
      FCT = Ka*Kb
      FCDO = DBLE(-.288)+DBLE(0.233)*(dops)-(DBLE(0.000105)*(dops))**2
      C = CA*M**(CB)*FCT*FCDO

      !Respiration
      ACT = exp(RTO)
      FRT = exp(RQ*T)
      R = RA*M**(RB)*FRT*ACT

      !Egestion
      F = FA*C

      !Specific Dynamic Action
      SS = SDA*(C-F)

      !Excretion
      U = UA*(C-F)

      !Growth Rate Potential
      getGrowth = (Kprey/Kpred)*(C-(R+SS+F+U))


    !+---------------------------------------------------------+
    !+            Atlantic Sturgeon Bioenergetics              |
    !+---------------------------------------------------------+
	CASE(3)

    !******************* READING PARAMETERS*********************
	
	oxy = dops
	if (dops.le.0) then
	oxy = DBLE(0.1)
	endif 

    !** ROUTINE METABOLISM
    arm   =  0.522
    brm   = -0.17
    tk1rm =  0.141
    tk4rm =  0.796
    crm   =  1.0
    drm   =  1.048
    grm   =  0.748
    hrm   =  0.268
    irm   =  0.352
    smin  =  9.166

    !** FOOD CONSUMPTION
    afc    =  1.028
    bfc    = -0.197
    tk1fc  =  0.195
    tk4fc  =  0.556
    tl98fc = 26.09
    cfc    =  1
    dfc    =  2.516
    gfc    =  0.733
    jfc    =  0.359
    kfc    =  0.247

    !** EGESTION
    aeg =  0.335
    ceg = -0.75
    deg = -0.62
    geg =  0

    !** EXCRETION
    aex =  0.055703
    bex = -0.29
    cex =  0.0392

    !** SDA
    asda = 0.1657

    !** AM
    aact = 0.29
 

    !************************ MODELO RM ************************
    !* Constants
    rt1 =  6
    rt4 = 28
    s4  = 29
    s1  =  1
    do1 = 25
    B1  =  -.158
    OX  = 13.55

    !* FT
    y1   = (DBLE(1)/(rt4-rt1)) *                                &
           log(tk4rm*(DBLE(1)-tk1rm)/(tk1rm*(DBLE(1)-tk4rm)))
    ey1  = exp(y1*(temp-rt1))
    FTrm = tk1rm*ey1/(DBLE(1)+tk1rm*(ey1-DBLE(1)))

    !* FS
    FSArm = DBLE(1)+DBLE(0.01)*EXP(hrm*GR**B1*(s-SMIN))
    FSBrm = DBLE(1)+DBLE(0.01)*EXP(irm*GR**B1*(SMIN-s))
    FSrm  = FSArm*FSBrm/DBLE(1.0201)

    !* FO
    DOCrm = DBLE(100)*(DBLE(1)-crm*exp(-FTrm*FSrm))
    KO1rm = DBLE(1)-drm*EXP(FTrm*FSrm-DBLE(1))
    dorel = (DOCrm-oxy)/DBLE(100)
    SLrm  = (DBLE(.98)-KO1rm)/((DBLE(.02)*(DOCrm-do1))**crm)
    if (dorel>0) then
      FOrm = (DBLE(0.98)-SLrm*dorel**crm)/DBLE(0.98)
    else
      FOrm = 1
    endif

    KRM = FOrm*FSrm*FTrm
    RM  = GR*(ARM*GR**BRM)*KRM*DBLE(24)*OX/DBLE(1000)

    !************************ MODELO FC ************************
    !* Constants
    CT1  =  6
    CT4  = 28
    DO1  = 25
    S1   =  1
    S4   = 29

    !* FT
    cy1  = (DBLE(1)/(tl98fc-ct1)) *                             &
           log(DBLE(0.98)*(DBLE(1)-tk1fc)/(tk1fc*DBLE(0.02)))
    ecy1 = exp(cy1*(temp-ct1))
    cka  = tk1fc*ecy1/(DBLE(1)+tk1fc*(ecy1-DBLE(1)))

    cy2  = (DBLE(1)/(ct4-tl98fc)) *                             &
           log(DBLE(0.98)*(DBLE(1)-tk4fc)/(tk4fc*DBLE(0.02)))
    ecy2 = exp(cy2*(ct4-temp))
    ckb  = tk4fc*ecy2/(DBLE(1)+tk4fc*(ecy2-DBLE(1)))

    FTfc = cka*ckb

    !* FS
    CKS1 = jfc*GR**-B1
    CKS4 = kfc*GR**-B1

    YA   = (DBLE(1)/(smin-S1))*log(DBLE(0.98)*(DBLE(1)-CKS1)/(CKS1*DBLE(0.02)))
    EYA  = exp(YA*(s-S1))
    KSA  = CKS1*EYA/(DBLE(1)+CKS1*(EYA-DBLE(1)))

    YB   = (DBLE(1)/(S4-smin))*log(DBLE(0.98)*(DBLE(1)-CKS4)/(CKS4*DBLE(0.02)))
    EYB  = exp(YB*(S4-s))
    KSB  = CKS4*EYB/(DBLE(1)+CKS4*(EYB-DBLE(1)))
    FSfc = (KSA*KSB)/DBLE(1)

    !* FO
    DOCfc = DBLE(100)*(DBLE(1)-gfc*exp(-KRM))
    KO1fc = DBLE(1)-dfc*EXP(KRM-DBLE(1))
    dorel = (DOCfc-oxy)/DBLE(100)
    SLfc  = (DBLE(.98)-KO1fc)/((DBLE(.02)*(DOCfc-do1))**cfc)
    if (dorel>0) then
      FOfc = (DBLE(0.98)-SLfc*dorel**cfc)/DBLE(0.98)
    else
      FOfc = 1
    endif

    CJG_MAX  = (afc*GR**bfc)*FTfc*FSfc*FOfc
    CJ_MAX   = GR*CJG_MAX
    CJG_PRED = RATION*CJG_MAX
    CJ_PRED  = CJG_PRED*GR

    if (CJ_OBS==0) then
      CJ = CJ_PRED
    else
      CJ = CJ_OBS
    endif

    !********************** EGESTION MODEL *********************
    E  = aeg*(temp/DBLE(6))**ceg*(oxy/DOCrm)**deg*RATION**geg
    EG = E*CJ

    !************************ EXCRETION ************************
    EX = aex*GR**bex*RM+cex*CJ

    !*************************** SDA ***************************
    SDA = (CJ-EG)*asda

    !******************** ACTIVE METABOLISM ********************
    AM=CJ_MAX*aact

    !********************* ENERGY CONTENT **********************
    TL   = EXP((log(gr)+DBLE(6.1488))/DBLE(3.113))
    LNL  = LOG(TL)
    WS   = TL/EXP((log(gr)+DBLE(6.1488))/DBLE(3.113))
    ENEC = exp(DBLE(-1.0065)+DBLE(1.0336)*log(WS)+DBLE(0.6807)*LNL)

    !************************* GROWTH **************************
    GKJ_PRED = CJ-RM-EG-SDA-EX-AM
    G_PRED   = log((gr+GKJ_PRED/ENEC)/gr)

	if (temp.LT.4.0) G_PRED = DBLE(0.0)

    getGrowth = G_PRED
	!if (G_PRED <= 0.0 .OR. G_PRED >0.0) then 
	  !write(*,*) ' '
	!else 
	 !if (t .EQ. 0.0 .AND. s .EQ. 0.0 .AND. o .EQ. 0.0) then
	   !write(*,*) ' '
	 !else
	   !write(*,*)'s,temp,dops,o', s,temp,dops,o
	   !write(*,*)'RM,EG,SDA,EX,AM,KRM,GKJ_PRED,G_PRED', RM,EG,SDA,EX,AM,GKJ_PRED,G_PRED
	  !end if
	!end if

    CASE DEFAULT
      write(*,*) "Error: Bioenergetics turned on with bad b_species value"
      stop

    END SELECT

  END FUNCTION getGrowth

  SUBROUTINE setGrowthCategories()
    USE HYDRO_DATA_MOD, only: nCat,cats
    IMPLICIT NONE


   !SET the number of categories:
    nCat = 2


    if(nCat > 0) ALLOCATE(cats(nCat,2))
    !Add or Remove (or comment out) categories below so exactly nCat
    !  categories are being specified; no more, no less.

    cats(1,1) =  0.0  !Category 1 Min
    cats(1,2) =  1.0  !Category 1 Max

    cats(2,1) =  0.02007  !Category 4 Min
    cats(2,2) =  1.0  !Category 4 Max

  END SUBROUTINE setGrowthCategories


END MODULE CONSTRAINTS_MOD