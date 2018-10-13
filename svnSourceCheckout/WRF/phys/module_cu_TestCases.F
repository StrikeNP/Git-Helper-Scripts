!-----------------------------------------------------------------------
! $Id: module_cu_TestCases.F 193 2010-07-13 21:50:35Z ldgrant@uwm.edu $
!===============================================================================
#ifdef TESTCASES
MODULE module_cu_TestCases

! Created by Brian Griffin.

! This module holds the forcing and geostrophic wind codes for the following
! test cases:
! BOMEX
!         -- marine cumulus case
!         -- large-scale subsidence
!         -- large-scale radiative cooling
!         -- large-scale low-level drying due to horizontal advection
!         -- geostrophic winds
! ARM
!         -- diurnal cumulus over land case
!         -- large-scale radiative cooling
!         -- large-scale temperature and moisture advection
!         -- geostrophic winds
! FIRE
!         -- nocturnal stratocumulus case
!         -- large-scale subsidence
!         -- radiative cooling (use either RRTM scheme or ATEX analytic scheme)
!         -- geostrophic winds
! ATEX
!         -- trade-wind cumulus case
!         -- large-scale subsidence
!         -- large-scale temperature and moisture advection
!         -- radiative cooling (use ATEX analytic scheme)
!         -- geostrophic winds
! Wangara
!         -- clear convective boundary layer case
!         -- geostrophic winds only

! This module holds the following subroutines:
!
! BOMEX_tend
! BOMEX_forcings
! BOMEX_init
! ARM_tend
! ARM_forcings
! ARM_init
! FIRE_tend
! FIRE_forcings
! FIRE_init
! ATEX_tend
! ATEX_forcings
! ATEX_analytic_radiation
! find_zi
! ATEX_init
! Wangara_tend

IMPLICIT NONE

PUBLIC :: BOMEX_tend, &
          BOMEX_init, &
          ARM_tend, &
          ARM_init, &
          FIRE_tend, &
          FIRE_init, &
          ATEX_tend, &
          ATEX_init, &
          Wangara_tend

PRIVATE :: BOMEX_forcings, &
           ARM_forcings, &
           FIRE_forcings, &
           ATEX_forcings, &
           ATEX_analytic_radiation, &
           find_zi

PRIVATE ! default scope

CONTAINS

!===============================================================================
!###############################################################################
!     GCSS BOMEX CASE
!###############################################################################
!===============================================================================

SUBROUTINE BOMEX_tend( RTHCUTEN, RUCUTEN, RVCUTEN, RQVCUTEN, &
                       RQCCUTEN, CU_ACT_FLAG, th_phy,        &
                       u_phy, v_phy, qv_curr, qc_curr, z,    &
                       num_force_layers, z_force, w_subs,    &
                       u_g, v_g, z_base, u_base, v_base,     &
                       ids, ide, jds, jde, kds, kde,         &
                       ims, ime, jms, jme, kms, kme,         &
                       its, ite, jts, jte, kts, kte          )

USE module_quadratic, ONLY: quadratic_derivative
USE module_init_utilities, ONLY: interp_0

IMPLICIT NONE

! Input variables.
INTEGER, INTENT(IN):: ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte

REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)::  &
      th_phy,  & ! Potential Temperature              [K]
      u_phy,   & ! u-wind at half-level of x          [m/s]
      v_phy,   & ! v-wind at half-level of y          [m/s]
      qv_curr, & ! Water Vapor Mixing Ratio           [kg/kg]
      qc_curr, & ! Cloud Water Mixing Ratio           [kg/kg]
      z          ! Altitude at thermodynamic levels   [m]

REAL, DIMENSION(kms:kme), INTENT(IN)::  &
      z_base    ! Initial altitudes of thermodynamic grid levels   [m]

INTEGER, INTENT(IN):: num_force_layers

REAL, DIMENSION(num_force_layers), INTENT(IN)::  &
      z_force    ! Altitudes of subsidence profile    [m]

REAL, DIMENSION(num_force_layers), INTENT(IN)::  &
      u_g,   & ! U-component of geostrophic wind    [m/s]
      v_g,   & ! V-component of geostrophic wind    [m/s]
      w_subs   ! Imposed large-scale subsidence     [m/s]

! Output variables.
REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(OUT)::  &
      RTHCUTEN, & ! Potential temperature tendency       [K/s]
      RUCUTEN,  & ! U-wind component tendency            [m/s]
      RVCUTEN,  & ! V-wind component tendency            [m/s]
      RQVCUTEN, & ! Water vapor mixing ratio tendency    [(kg/kg)/s]
      RQCCUTEN    ! Cloud water mixing ratio tendency    [(kg/kg)/s]

REAL, DIMENSION(kms:kme), INTENT(OUT)::  &
      u_base, & ! U-component of geostrophic winds at z_base altitudes  [m/s]
      v_base    ! V-component of geostrophic winds at z_base altitudes  [m/s]

LOGICAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT)::  CU_ACT_FLAG

! Local variables
REAL, DIMENSION(ims:ime, kms:kme, jms:jme)::  &
      dTHdZ, & ! d(theta)/dz; for use with large-scale subsidence  [K/m]
      dUdZ,  & ! du/dz; for use with large-scale subsidence        [(m/s)/m]
      dVdZ,  & ! dv/dz; for use with large-scale subsidence        [(m/s)/m]
      dQVdZ, & ! d(q_v)/dz; for use with large-scale subsidence    [(kg/kg)/m]
      dQCdZ    ! d(q_c)/dz; for use with large-scale subsidence    [(kg/kg)/m]

REAL::  &
      w_ls_sub  ! Imposed large-scale subsidence (thermodynamic levels)  [m/s]

REAL, DIMENSION(ims:ime, kms:kme, jms:jme)::  &
      th_tend, & ! Potential temperature tendency   [K/s]
      qt_tend    ! Total water tendency             [(kg/kg)/s]

INTEGER:: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k

!-------------------------------------------------------------------------------

i_start = its
i_end   = MIN(ide-1, ite)
j_start = jts
j_end   = MIN(jde-1, jte)
k_start = kts
k_end   = MIN(kde-1, kte)

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      CU_ACT_FLAG(i,j) = .true.
   ENDDO
ENDDO

! Get the values for radiative cooling and large-scale horizontal advection.

CALL BOMEX_forcings( z, qv_curr, qc_curr,          &
                     th_tend, qt_tend,             &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     its, ite, jts, jte, kts, kte  )

! Tendencies due to vertical advection by imposed large-scale subsidence.

! Get the derivative according to a parabolic fit between three points
! on three consecutive levels.

CALL quadratic_derivative( th_phy, z, dTHdZ,             &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           its, ite, jts, jte, kts, kte  )

CALL quadratic_derivative( u_phy, z, dUdZ,               &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           its, ite, jts, jte, kts, kte  )

CALL quadratic_derivative( v_phy, z, dVdZ,               &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           its, ite, jts, jte, kts, kte  )

CALL quadratic_derivative( qv_curr, z, dQVdZ,              &
                           ids, ide, jds, jde, kds, kde,   &
                           ims, ime, jms, jme, kms, kme,   &
                           its, ite, jts, jte, kts, kte    )

CALL quadratic_derivative( qc_curr, z, dQCdZ,              &
                           ids, ide, jds, jde, kds, kde,   &
                           ims, ime, jms, jme, kms, kme,   &
                           its, ite, jts, jte, kts, kte    )

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      DO k = k_start, k_end, 1

         ! Value of imposed large-scale subsidence
         ! WRF's large-scale subsidence is input from an external netCDF file.
         ! If there is input for large-scale subsidence, z_force will be an
         ! array of various reference altitudes.  Otherwise, z_force will be an
         ! array of zeros.
         IF ( ANY( z_force(:) > 0.0 ) .AND. num_force_layers > 0 ) THEN
            ! Large-scale subsidence is computed at thermodynamic grid levels.
            w_ls_sub = interp_0(w_subs,z_force,z(i,k,j),num_force_layers)
         ELSE
            w_ls_sub = 0.0
         ENDIF

         RTHCUTEN(i,k,j) = - w_ls_sub * dTHdZ(i,k,j)
         RUCUTEN(i,k,j)  = - w_ls_sub * dUdZ(i,k,j)
         RVCUTEN(i,k,j)  = - w_ls_sub * dVdZ(i,k,j)
         RQVCUTEN(i,k,j) = - w_ls_sub * dQVdZ(i,k,j)
         RQCCUTEN(i,k,j) = - w_ls_sub * dQCdZ(i,k,j)

      ENDDO
   ENDDO
ENDDO

! Change in d(Something)/dt due to radiation or advection.

DO i = i_start, i_end, 1
   DO k = k_start, k_end, 1
      DO j = j_start, j_end, 1
         RTHCUTEN(i,k,j) = RTHCUTEN(i,k,j) + th_tend(i,k,j)
         RQVCUTEN(i,k,j) = RQVCUTEN(i,k,j) + qt_tend(i,k,j)
      ENDDO
   ENDDO
ENDDO

! Set the values of u_base and v_base to the values of the geostrophic winds.
!
! The values of u_base, v_base, t_base, qv_base, and z_base are all set in the
! appropriate module_initialize file.  However, u_base and v_base are reset at
! this point so that they will contain the u and v geostrophic wind profiles,
! rather than simply the initial values of the u and v winds.  The values of
! u_base and v_base are reset based on the altitude profile z_base.  That way,
! u_base and v_base are reset at the altitudes at which they were originally
! set, and at which the t_base and qv_base profiles are set.  This is important
! for the use of these variables in the WRF model code.  For example, subroutine
! perturbation_coriolis (in module_big_step_utilities_em) uses u_base and v_base
! at altitude profile z_base, and then makes a weighted adjustment based on the
! current altitude profile, z, for each column.
!
! In an idealized case such as this one, WRF initially sets the geopotential
! profile, and thus the altitude profiles, to be the same in every column.
! Thus, the various base arrays only need to be set in the vertical.
!
! This code sets the u_base and v_base profiles to the geostrophic wind profiles
! in the same manner that is found in WRF's subroutine force_scm.
!
! In order to use this code in determining the coriolis effect on the wind
! profiles, set the flag pert_coriolis to .true. in namelist.input.

DO k = k_start, k_end, 1

   ! Values of the geostrophic winds.
   ! WRF's geostrophic winds are input from an external netCDF file.  If there
   ! is input for the geostrophic winds, z_force will be an array of various
   ! reference altitudes.  Otherwise, z_force will be an array of zeros.
   IF ( ANY( z_force(:) > 0.0 ) .AND. num_force_layers > 0 ) THEN
      ! Geostrophic winds are computed at thermodynamic grid levels.
      u_base(k) = interp_0(u_g,z_force,z_base(k),num_force_layers)
      v_base(k) = interp_0(v_g,z_force,z_base(k),num_force_layers)
   ELSE
      ! If there is not netCDF input for the geostrophic winds, then keep u_base
      ! and v_base at the same values, which would be the initial wind profile.
      u_base(k) = u_base(k)
      v_base(k) = v_base(k)
   ENDIF

ENDDO

END SUBROUTINE BOMEX_tend

!===============================================================================

SUBROUTINE BOMEX_forcings( z, qv_curr, qc_curr,          &
                           th_tend, qt_tend,             &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           its, ite, jts, jte, kts, kte  )

IMPLICIT NONE

! Input variables
INTEGER, INTENT (IN):: ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte

REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)::  &
      qv_curr, & ! Water Vapor Mixing Ratio           [kg/kg]
      qc_curr, & ! Cloud Water Mixing Ratio           [kg/kg]
      z          ! Altitude at thermodynamic levels   [m]

! Output variables
REAL, DIMENSION(ims:ime, kms:kme, jms:jme)::  &
      th_tend, & ! Potential temperature tendency   [K/s]
      qt_tend    ! Total water tendency             [(kg/kg)/s]

! Local variables
REAL::  &
      th_rad, &  ! Theta tendency from radiative cooling           [K/s]
      qv_adv     ! Total Water tendency from large-scale advection [(kg/kg)/s]

INTEGER:: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k

!-------------------------------------------------------------------------------

i_start = its
i_end   = MIN(ide-1, ite)
j_start = jts
j_end   = MIN(jde-1, jte)
k_start = kts
k_end   = MIN(kde-1, kte)

! Change in d(Theta)/dt due to Radiative Cooling (K/s)
DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      DO k = k_start, k_end, 1

         IF ( z(i,k,j) >= 0.0 .AND. z(i,k,j) <= 1500.0 ) THEN
            th_rad = -0.00002315
         ELSEIF ( z(i,k,j) > 1500.0 .AND. z(i,k,j) <= 2500.0 ) THEN
            th_rad = -0.00002315  &
                     + 0.00002315/(2500.0-1500.0) * ( z(i,k,j) - 1500.0 )
         ELSEIF ( z(i,k,j) > 2500.0 ) THEN
            th_rad = 0.0
         ENDIF

         th_tend(i,k,j) = th_rad

      ENDDO
   ENDDO
ENDDO

! Change in d(q_t)/dt due to Large Scale Horizontal Advection (s^-1)
! The BOMEX specifications give large-scale advective moisture tendency in
! terms of total water specific humidity.
DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      DO k = k_start, k_end, 1

         IF ( z(i,k,j) >= 0.0 .AND. z(i,k,j) <= 300.0 ) THEN
            qv_adv = -1.2e-8
         ELSEIF ( z(i,k,j) > 300.0 .AND. z(i,k,j) <= 500.0 ) THEN
            qv_adv = - ( 1.2e-8  &
                        - 1.2e-8 * ( z(i,k,j) - 300.0 ) / ( 500.0 - 300.0 ) )
         ELSEIF ( z(i,k,j) > 500.0 ) THEN
            qv_adv = 0.0
         ENDIF

         ! Convert forcings from terms of total water specific humidity to terms
         ! of total water mixing ratio.  The relationship is given by the
         ! equation:  d(r_tm)/dt|_f = ( 1 + r_tm )^2 * d(q_tm)/dt|_f; where r_tm
         ! is the mean total water mixing ratio, d(q_tm)/dt|_f is the forcing in
         ! terms of specific humidity, and d(r_tm)/dt|_f is the forcing in terms
         ! of mixing ratio.
         qt_tend(i,k,j) = ( 1.0 + qv_curr(i,k,j) + qc_curr(i,k,j) )**2 * qv_adv

      ENDDO
   ENDDO
ENDDO

END SUBROUTINE BOMEX_forcings

!===============================================================================

SUBROUTINE BOMEX_init( RTHCUTEN, RUCUTEN, RVCUTEN,           &
                       RQVCUTEN, RQCCUTEN, restart,          &
                       ids, ide, jds, jde, kds, kde,         &
                       ims, ime, jms, jme, kms, kme,         &
                       its, ite, jts, jte, kts, kte          )

IMPLICIT NONE

! Input variables
INTEGER, INTENT(IN):: ids, ide, jds, jde, kds, kde,    &
                      ims, ime, jms, jme, kms, kme,    &
                      its, ite, jts, jte, kts, kte

LOGICAL, INTENT(IN):: restart

! Output variables
REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(OUT)::  &
      RTHCUTEN, & ! Potential temperature tendency       [K]
      RUCUTEN,  & ! U-wind component tendency            [m/s]
      RVCUTEN,  & ! V-wind component tendency            [m/s]
      RQVCUTEN, & ! Water vapor mixing ratio tendency    [(kg/kg)/s]
      RQCCUTEN    ! Cloud water mixing ratio tendency    [(kg/kg)/s]

! Local Variables
INTEGER:: i, j, k, itf, jtf, ktf

!-------------------------------------------------------------------------------

jtf = min0(jte, jde-1)
ktf = min0(kte, kde-1)
itf = min0(ite, ide-1)

! Initialize the tendency variables to 0, as long as it is not a restart run.
! All the tendency variables are located at thermodynamic levels.
IF (.not. restart) THEN
   DO i = its, itf, 1 
      DO j = jts, jtf, 1
         DO k = kts, ktf, 1
            RTHCUTEN(i,k,j) = 0.0
            RUCUTEN(i,k,j)  = 0.0
            RVCUTEN(i,k,j)  = 0.0
            RQVCUTEN(i,k,j) = 0.0
            RQCCUTEN(i,k,j) = 0.0
         ENDDO
      ENDDO
   ENDDO
ENDIF

END SUBROUTINE BOMEX_init

!===============================================================================
!###############################################################################
!     GCSS ARM CASE
!###############################################################################
!===============================================================================

SUBROUTINE ARM_tend( RTHCUTEN, RQVCUTEN,                &
                     z, dt, itimestep, CU_ACT_FLAG,     &
                     num_force_layers, z_force,         &
                     u_g, v_g, z_base, u_base, v_base,  &
                     ids, ide, jds, jde, kds, kde,      &
                     ims, ime, jms, jme, kms, kme,      &
                     its, ite, jts, jte, kts, kte       )

USE module_init_utilities, ONLY: interp_0

IMPLICIT NONE

! Input variables
INTEGER, INTENT(IN):: ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte

INTEGER, INTENT(IN):: itimestep  ! Model timestep number

REAL, INTENT(IN):: dt  ! Model timestep duration     [s]

REAL, DIMENSION (ims:ime, kms:kme, jms:jme), INTENT(IN)::  &
      z         ! Altitude at thermodynamic levels   [m]

REAL, DIMENSION(kms:kme), INTENT(IN)::  &
      z_base    ! Initial altitudes of thermodynamic grid levels   [m]

INTEGER, INTENT(IN):: num_force_layers

REAL, DIMENSION(num_force_layers), INTENT(IN)::  &
      z_force    ! Altitudes of subsidence profile    [m]

REAL, DIMENSION(num_force_layers), INTENT(IN)::  &
      u_g,   & ! U-component of geostrophic wind    [m/s]
      v_g      ! V-component of geostrophic wind    [m/s]

! Output variables
REAL, DIMENSION (ims:ime, kms:kme, jms:jme), INTENT(INOUT)::  &
      RTHCUTEN, & ! Potential temperature tendency       [K/s]
      RQVCUTEN    ! Water vapor mixing ratio tendency    [(kg/kg)/s]

REAL, DIMENSION(kms:kme), INTENT(OUT)::  &
      u_base, & ! U-component of geostrophic winds at z_base altitudes  [m/s]
      v_base    ! V-component of geostrophic winds at z_base altitudes  [m/s]

LOGICAL, DIMENSION (ims:ime, jms:jme), INTENT (INOUT):: CU_ACT_FLAG

! Local variables
REAL, DIMENSION(ims:ime, kms:kme, jms:jme)::  &
      th_tend, & ! Potential temperature tendency   [K/s]
      qt_tend    ! Total water tendency             [(kg/kg)/s]

INTEGER:: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k

!------------------------------------------------------------------------------

i_start = its
i_end   = MIN(ide-1, ite)
j_start = jts
j_end   = MIN(jde-1, jte)
k_start = kts
k_end   = MIN(kde-1, kte)

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      CU_ACT_FLAG(i,j) = .true.
   ENDDO
ENDDO

CALL ARM_forcings( z, dt, itimestep, th_tend, qt_tend, &
                   ids, ide, jds, jde, kds, kde,       &
                   ims, ime, jms, jme, kms, kme,       &
                   its, ite, jts, jte, kts, kte        )

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      DO k = k_start, k_end, 1
         RTHCUTEN(i,k,j) = th_tend(i,k,j)
         RQVCUTEN(i,k,j) = qt_tend(i,k,j)
      ENDDO
   ENDDO
ENDDO

! Set the values of u_base and v_base to the values of the geostrophic winds.
!
! The values of u_base, v_base, t_base, qv_base, and z_base are all set in the
! appropriate module_initialize file.  However, u_base and v_base are reset at
! this point so that they will contain the u and v geostrophic wind profiles,
! rather than simply the initial values of the u and v winds.  The values of
! u_base and v_base are reset based on the altitude profile z_base.  That way,
! u_base and v_base are reset at the altitudes at which they were originally
! set, and at which the t_base and qv_base profiles are set.  This is important
! for the use of these variables in the WRF model code.  For example, subroutine
! perturbation_coriolis (in module_big_step_utilities_em) uses u_base and v_base
! at altitude profile z_base, and then makes a weighted adjustment based on the
! current altitude profile, z, for each column.
!
! In an idealized case such as this one, WRF initially sets the geopotential
! profile, and thus the altitude profiles, to be the same in every column.
! Thus, the various base arrays only need to be set in the vertical.
!
! This code sets the u_base and v_base profiles to the geostrophic wind profiles
! in the same manner that is found in WRF's subroutine force_scm.
!
! In order to use this code in determining the coriolis effect on the wind
! profiles, set the flag pert_coriolis to .true. in namelist.input.

DO k = k_start, k_end, 1

   ! Values of the geostrophic winds.
   ! WRF's geostrophic winds are input from an external netCDF file.  If there
   ! is input for the geostrophic winds, z_force will be an array of various
   ! reference altitudes.  Otherwise, z_force will be an array of zeros.
   IF ( ANY( z_force(:) > 0.0 ) .AND. num_force_layers > 0 ) THEN
      ! Geostrophic winds are computed at thermodynamic grid levels.
      u_base(k) = interp_0(u_g,z_force,z_base(k),num_force_layers)
      v_base(k) = interp_0(v_g,z_force,z_base(k),num_force_layers)
   ELSE
      ! If there is not netCDF input for the geostrophic winds, then keep u_base
      ! and v_base at the same values, which would be the initial wind profile.
      u_base(k) = u_base(k)
      v_base(k) = v_base(k)
   ENDIF

ENDDO

END SUBROUTINE ARM_tend

!===============================================================================

SUBROUTINE ARM_forcings( z, dt, itimestep, th_tend, qt_tend, &
                         ids, ide, jds, jde, kds, kde,       &
                         ims, ime, jms, jme, kms, kme,       &
                         its, ite, jts, jte, kts, kte        )

IMPLICIT NONE

! Input Variables
INTEGER, INTENT(IN):: ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte

INTEGER, INTENT(IN):: itimestep  ! Model timestep number

REAL, INTENT(IN):: dt  ! Model timestep duration     [s]

REAL, DIMENSION (ims:ime, kms:kme, jms:jme), INTENT(IN)::  &
      z         ! Altitude at thermodynamic levels   [m]

! Output variables
REAL, DIMENSION(ims:ime, kms:kme, jms:jme)::  &
      th_tend, & ! Potential temperature tendency   [K/s]
      qt_tend    ! Total water tendency             [(kg/kg)/s]

! Local variables
REAL::  &
      th_rad, &  ! Theta tendency from radiative cooling           [K/s]
      th_adv, &  ! Theta tendency from large-scale advection       [K/s]
      qt_adv     ! Total Water tendency from large-scale advection [(kg/kg)/s]

REAL:: total_time  ! Elapsed time since midnight on start date     [s]

INTEGER:: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k

!-------------------------------------------------------------------------------

i_start = its
i_end   = MIN(ide-1, ite)
j_start = jts
j_end   = MIN(jde-1, jte)
k_start = kts
k_end   = MIN(kde-1, kte)

total_time = 41400.0  ! Number of seconds into day at 11:30 a.m., when model starts

total_time = total_time + dt*itimestep

! The values for advth and radth are in K/hour
! The values for advqt is in g/kg/hour
IF (total_time >= 41400.0 .AND. total_time < 52200.0) THEN
   th_adv = ( 0.000-0.000)/(52200.0-41400.0)*(total_time-41400.0) + 0.000
   th_rad = ( 0.000+0.125)/(52200.0-41400.0)*(total_time-41400.0) - 0.125
   qt_adv = ( 0.020-0.080)/(52200.0-41400.0)*(total_time-41400.0) + 0.080
ELSEIF (total_time >= 52200.0 .AND. total_time < 63000.0) THEN
   th_adv = ( 0.000-0.000)/(63000.0-52200.0)*(total_time-52200.0) + 0.000
   th_rad = ( 0.000-0.000)/(63000.0-52200.0)*(total_time-52200.0) + 0.000
   qt_adv = (-0.040-0.020)/(63000.0-52200.0)*(total_time-52200.0) + 0.020
ELSEIF (total_time >= 63000.0 .AND. total_time < 73800.0) THEN
   th_adv = (-0.080-0.000)/(73800.0-63000.0)*(total_time-63000.0) + 0.000
   th_rad = ( 0.000-0.000)/(73800.0-63000.0)*(total_time-63000.0) + 0.000
   qt_adv = (-0.100+0.040)/(73800.0-63000.0)*(total_time-63000.0) - 0.040
ELSEIF (total_time >= 73800.0 .AND. total_time < 84600.0) THEN
   th_adv = (-0.160+0.080)/(84600.0-73800.0)*(total_time-73800.0) - 0.080
   th_rad = ( 0.000-0.000)/(84600.0-73800.0)*(total_time-73800.0) + 0.000
   qt_adv = (-0.160+0.100)/(84600.0-73800.0)*(total_time-73800.0) - 0.100
ELSEIF (total_time >= 84600.0 .AND. total_time <= 93600.0) THEN
   th_adv = (-0.160+0.160)/(93600.0-84600.0)*(total_time-84600.0) - 0.160
   th_rad = (-0.100-0.000)/(93600.0-84600.0)*(total_time-84600.0) + 0.000
   qt_adv = (-0.300+0.160)/(93600.0-84600.0)*(total_time-84600.0) - 0.160
ENDIF

! Convert the values of advth and radth to K/s
th_adv = th_adv/3600.0
th_rad = th_rad/3600.0

! Convert the value of advqt to kg/kg/s
qt_adv = (qt_adv/1000.0)/3600.0

! The overall time and height tendencies for theta and qt.
DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      DO k = k_start, k_end, 1

         IF (z(i,k,j) >= 0.0 .AND. z(i,k,j) < 1000.0) THEN
            th_tend(i,k,j) = th_adv + th_rad
            qt_tend(i,k,j) = qt_adv
         ELSEIF (z(i,k,j) >= 1000.0 .AND. z(i,k,j) < 3000.0) THEN
            th_tend(i,k,j) = (0.0-(th_adv+th_rad))/(3000.0-1000.0)     &
                                          *(z(i,k,j)-1000.0) + (th_adv+th_rad)
            qt_tend(i,k,j) = (0.0-qt_adv)/(3000.0-1000.0)              &
                                          *(z(i,k,j)-1000.0) + qt_adv
         ELSEIF (z(i,k,j) >= 3000.0) THEN
            th_tend(i,k,j) = 0.0
            qt_tend(i,k,j) = 0.0
         ENDIF

      ENDDO
   ENDDO
ENDDO

END SUBROUTINE ARM_forcings

!===============================================================================

SUBROUTINE ARM_init( RTHCUTEN, RQVCUTEN, restart,       &
                     ids, ide, jds, jde, kds, kde,      &
                     ims, ime, jms, jme, kms, kme,      &
                     its, ite, jts, jte, kts, kte       )

IMPLICIT NONE

! Input Variables
INTEGER, INTENT(IN):: ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte

LOGICAL, INTENT(IN):: restart

! Output variables
REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(OUT)::  &
      RTHCUTEN, & ! Potential temperature tendency       [K]
      RQVCUTEN    ! Water vapor mixing ratio tendency    [(kg/kg)/s]

! Local Variables
INTEGER:: i, j, k, itf, jtf, ktf

!-------------------------------------------------------------------------------

jtf = min0(jte, jde-1)
ktf = min0(kte, kde-1)
itf = min0(ite, ide-1)

! Initialize the tendency variables to 0, as long as it is not a restart run.
! All the tendency variables are located at thermodynamic levels.
IF (.not. restart) THEN
   DO j = jts, jtf, 1
      DO k = kts, ktf, 1
         DO i = its, itf, 1
            RTHCUTEN(i,k,j) = 0.0
            RQVCUTEN(i,k,j) = 0.0
         ENDDO
      ENDDO
   ENDDO
ENDIF

END SUBROUTINE ARM_init

!===============================================================================
!###############################################################################
!     GCSS FIRE CASE
!###############################################################################
!===============================================================================

SUBROUTINE FIRE_tend( RTHCUTEN, RUCUTEN, RVCUTEN,                 &
                      RQVCUTEN, RQCCUTEN, CU_ACT_FLAG,            &
                      th_phy, u_phy, v_phy, qv_curr, qc_curr,     &
                      dz8w, rho, pi_phy, p_phy, z, ra_lw_physics, &
                      num_force_layers, z_force, w_subs,          &
                      u_g, v_g, z_base, u_base, v_base,           &
#ifdef CLUBB
                      ! variables added for CLUBB PBL parameterization
                      bl_pbl_physics, rcm_CLUBB,                  &
                      ! CLUBB PBL changes end
#endif /*CLUBB*/
                      ids, ide, jds, jde, kds, kde,               &
                      ims, ime, jms, jme, kms, kme,               &
                      its, ite, jts, jte, kts, kte                )

USE module_quadratic, ONLY: quadratic_derivative
USE module_init_utilities, ONLY: interp_0

IMPLICIT NONE

! Input variables

!Michael Falk addition
INTEGER, INTENT(IN):: ra_lw_physics
!end addition

#ifdef CLUBB
! Variable added for CLUBB PBL parameterization
INTEGER, INTENT(IN):: bl_pbl_physics
! CLUBB PBL changes end
#endif /*CLUBB*/

INTEGER, INTENT(IN):: ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte

REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)::  &
      th_phy,  & ! Potential Temperature              [K]
      u_phy,   & ! u-wind at half-level of x          [m/s]
      v_phy,   & ! v-wind at half-level of y          [m/s]
      qv_curr, & ! Water Vapor Mixing Ratio           [kg/kg]
      qc_curr, & ! Cloud Water Mixing Ratio           [kg/kg]
      dz8w,    & ! Distance between momentum levels   [m]
      rho,     & ! Density of air                     [kg/m^3]
      pi_phy,  & ! Exner Function                     [-]
      p_phy,   & ! Pressure                           [Pa]
      z          ! Altitude of thermodynamic levels   [m]

REAL, DIMENSION(kms:kme), INTENT(IN)::  &
      z_base    ! Initial altitudes of thermodynamic grid levels   [m]

INTEGER, INTENT(IN):: num_force_layers

REAL, DIMENSION(num_force_layers), INTENT(IN)::  &
      z_force    ! Altitudes of subsidence profile    [m]

REAL, DIMENSION(num_force_layers), INTENT(IN)::  &
      u_g,   & ! U-component of geostrophic wind    [m/s]
      v_g,   & ! V-component of geostrophic wind    [m/s]
      w_subs   ! Imposed large-scale subsidence     [m/s]

#ifdef CLUBB
! Variable added for CLUBB PBL parameterization
REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)::  &
      rcm_CLUBB  ! Cloud Water Mixing Ratio from CLUBB PBL scheme  [kg/kg]
! CLUBB PBL changes end
#endif /*CLUBB*/

! Output variables
REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(OUT)::  &
      RTHCUTEN, & ! Potential temperature tendency       [K]
      RUCUTEN,  & ! U-wind component tendency            [m/s]
      RVCUTEN,  & ! V-wind component tendency            [m/s]
      RQVCUTEN, & ! Water vapor mixing ratio tendency    [(kg/kg)/s]
      RQCCUTEN    ! Cloud water mixing ratio tendency    [(kg/kg)/s]

REAL, DIMENSION(kms:kme), INTENT(OUT)::  &
      u_base, & ! U-component of geostrophic winds at z_base altitudes  [m/s]
      v_base    ! V-component of geostrophic winds at z_base altitudes  [m/s]

LOGICAL, DIMENSION(ims:ime, jms:jme), INTENT(INOUT)::  CU_ACT_FLAG

! Local variables
REAL, DIMENSION(ims:ime, kms:kme, jms:jme)::  &
      dTHdZ, & ! d(theta)/dz; for use with large-scale subsidence  [K/m]
      dUdZ,  & ! du/dz; for use with large-scale subsidence        [(m/s)/m]
      dVdZ,  & ! dv/dz; for use with large-scale subsidence        [(m/s)/m]
      dQVdZ, & ! d(q_v)/dz; for use with large-scale subsidence    [(kg/kg)/m]
      dQCdZ    ! d(q_c)/dz; for use with large-scale subsidence    [(kg/kg)/m]

REAL::  &
      w_ls_sub  ! Imposed large-scale subsidence (thermodynamic levels)  [m/s]

REAL, DIMENSION(ims:ime, kms:kme, jms:jme)::  &
      th_tend   ! Potential temperature tendency   [K/s]

INTEGER:: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k

!------------------------------------------------------------------------------

i_start = its
i_end   = MIN(ide-1, ite)
j_start = jts
j_end   = MIN(jde-1, jte)
k_start = kts
k_end   = MIN(kde-1, kte)

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      CU_ACT_FLAG(i,j) = .true.
   ENDDO
ENDDO

! Get the values for radiative cooling.

CALL FIRE_forcings( th_phy, qv_curr, qc_curr, dz8w, rho,   &
                    pi_phy, p_phy, ra_lw_physics, th_tend, &
#ifdef CLUBB
                    ! variables added for CLUBB PBL parameterization
                    bl_pbl_physics, rcm_CLUBB,             &
                    ! CLUBB PBL changes end
#endif /*CLUBB*/
                    ids, ide, jds, jde, kds, kde,          &
                    ims, ime, jms, jme, kms, kme,          &
                    its, ite, jts, jte, kts, kte           )

! Tendencies due to vertical advection by imposed large-scale subsidence.

! Get the derivative according to a parabolic fit between three points
! on three consecutive levels.
   
CALL quadratic_derivative( th_phy, z, dTHdZ,             &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           its, ite, jts, jte, kts, kte  )
   
CALL quadratic_derivative( u_phy, z, dUdZ,               &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           its, ite, jts, jte, kts, kte  )

CALL quadratic_derivative( v_phy, z, dVdZ,               &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           its, ite, jts, jte, kts, kte  )

CALL quadratic_derivative( qv_curr, z, dQVdZ,              &
                           ids, ide, jds, jde, kds, kde,   &
                           ims, ime, jms, jme, kms, kme,   &
                           its, ite, jts, jte, kts, kte    )

CALL quadratic_derivative( qc_curr, z, dQCdZ,              &
                           ids, ide, jds, jde, kds, kde,   &
                           ims, ime, jms, jme, kms, kme,   &
                           its, ite, jts, jte, kts, kte    )

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      DO k = k_start, k_end, 1

         ! Value of imposed large-scale subsidence
         ! WRF's large-scale subsidence is input from an external netCDF file.
         ! If there is input for large-scale subsidence, z_force will be an
         ! array of various reference altitudes.  Otherwise, z_force will be an
         ! array of zeros.
         IF ( ANY( z_force(:) > 0.0 ) .AND. num_force_layers > 0 ) THEN
            ! Large-scale subsidence is computed at thermodynamic grid levels.
            w_ls_sub = interp_0(w_subs,z_force,z(i,k,j),num_force_layers)
         ELSE
            w_ls_sub = 0.0
         ENDIF

         RTHCUTEN(i,k,j) = - w_ls_sub * dTHdZ(i,k,j)
         RUCUTEN(i,k,j)  = - w_ls_sub * dUdZ(i,k,j)
         RVCUTEN(i,k,j)  = - w_ls_sub * dVdZ(i,k,j)
         RQVCUTEN(i,k,j) = - w_ls_sub * dQVdZ(i,k,j)
         RQCCUTEN(i,k,j) = - w_ls_sub * dQCdZ(i,k,j)

      ENDDO
   ENDDO
ENDDO

! Change in d(Theta)/dt due to radiation.

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      DO k = k_start, k_end, 1
         RTHCUTEN(i,k,j) = RTHCUTEN(i,k,j) + th_tend(i,k,j)
      ENDDO
   ENDDO
ENDDO

! Set the values of u_base and v_base to the values of the geostrophic winds.
!
! The values of u_base, v_base, t_base, qv_base, and z_base are all set in the
! appropriate module_initialize file.  However, u_base and v_base are reset at
! this point so that they will contain the u and v geostrophic wind profiles,
! rather than simply the initial values of the u and v winds.  The values of
! u_base and v_base are reset based on the altitude profile z_base.  That way,
! u_base and v_base are reset at the altitudes at which they were originally
! set, and at which the t_base and qv_base profiles are set.  This is important
! for the use of these variables in the WRF model code.  For example, subroutine
! perturbation_coriolis (in module_big_step_utilities_em) uses u_base and v_base
! at altitude profile z_base, and then makes a weighted adjustment based on the
! current altitude profile, z, for each column.
!
! In an idealized case such as this one, WRF initially sets the geopotential
! profile, and thus the altitude profiles, to be the same in every column.
! Thus, the various base arrays only need to be set in the vertical.
!
! This code sets the u_base and v_base profiles to the geostrophic wind profiles
! in the same manner that is found in WRF's subroutine force_scm.
!
! In order to use this code in determining the coriolis effect on the wind
! profiles, set the flag pert_coriolis to .true. in namelist.input.

DO k = k_start, k_end, 1

   ! Values of the geostrophic winds.
   ! WRF's geostrophic winds are input from an external netCDF file.  If there
   ! is input for the geostrophic winds, z_force will be an array of various
   ! reference altitudes.  Otherwise, z_force will be an array of zeros.
   IF ( ANY( z_force(:) > 0.0 ) .AND. num_force_layers > 0 ) THEN
      ! Geostrophic winds are computed at thermodynamic grid levels.
      u_base(k) = interp_0(u_g,z_force,z_base(k),num_force_layers)
      v_base(k) = interp_0(v_g,z_force,z_base(k),num_force_layers)
   ELSE
      ! If there is not netCDF input for the geostrophic winds, then keep u_base
      ! and v_base at the same values, which would be the initial wind profile.
      u_base(k) = u_base(k)
      v_base(k) = v_base(k)
   ENDIF

ENDDO

END SUBROUTINE FIRE_tend

!==============================================================================

SUBROUTINE FIRE_forcings( th_phy, qv_curr, qc_curr, dz8w, rho,   &
                          pi_phy, p_phy, ra_lw_physics, th_tend, &
#ifdef CLUBB
                          ! variables added for CLUBB PBL parameterization
                          bl_pbl_physics, rcm_CLUBB,             &
                          ! CLUBB PBL changes end
#endif /*CLUBB*/
                          ids, ide, jds, jde, kds, kde,          &
                          ims, ime, jms, jme, kms, kme,          &
                          its, ite, jts, jte, kts, kte           )

#ifdef CLUBB
! Added for CLUBB PBL parameterization
USE module_state_description, ONLY:  &
    CLUBBSCHEME
! CLUBB PBL changes end
#endif /*CLUBB*/

IMPLICIT NONE

! Input variables

!Michael Falk addition
INTEGER, INTENT(IN):: ra_lw_physics  ! Long-wave radiation option
!end addition

#ifdef CLUBB
! Variable added for CLUBB PBL parameterization
INTEGER, INTENT(IN):: bl_pbl_physics
! CLUBB PBL changes end
#endif /*CLUBB*/

INTEGER, INTENT (IN):: ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte

REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)::  &
      th_phy,  & ! Potential Temperature              [K]
      qv_curr, & ! Water Vapor Mixing Ratio           [kg/kg]
      qc_curr, & ! Cloud Water Mixing Ratio           [kg/kg]
      dz8w,    & ! Distance between momentum levels   [m]
      rho,     & ! Density of air                     [kg/m^3]
      pi_phy,  & ! Exner Function                     [-]
      p_phy      ! Pressure                           [Pa]

#ifdef CLUBB
! Variable added for CLUBB PBL parameterization
REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)::  &
      rcm_CLUBB  ! Cloud Water Mixing Ratio from CLUBB PBL scheme  [kg/kg]
! CLUBB PBL changes end
#endif /*CLUBB*/

! Output variables
REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(OUT)::  &
      th_tend       ! Overall theta tendency       [K/s]

! Local variables
REAL, DIMENSION(ims:ime, kms:kme, jms:jme)::  &
      th_rad        ! Theta tendency from radiative cooling   [K/s]

INTEGER:: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k

!-------------------------------------------------------------------------------

i_start = its
i_end   = MIN(ide-1, ite)
j_start = jts
j_end   = MIN(jde-1, jte)
k_start = kts
k_end   = MIN(kde-1, kte)

! For FIRE case we can either use RRTM radiation or ATEX radiation scheme.
! Using ATEX code for the FIRE case.

!Michael Falk added this selector here, so that if RRTM is on, this is off.
IF ( ra_lw_physics == 0 ) THEN

#ifdef CLUBB
   ! Added for CLUBB PBL parameterization
   IF ( bl_pbl_physics == CLUBBSCHEME ) THEN

      ! Use CLUBB's cloud water mixing ratio, rcm_CLUBB, in the ATEX analytic
      ! radiation scheme.

      CALL ATEX_analytic_radiation( th_phy, qv_curr, rcm_CLUBB, dz8w, &
                                    rho, pi_phy, p_phy, th_rad,       &
                                    ids, ide, jds, jde, kds, kde,     &
                                    ims, ime, jms, jme, kms, kme,     &
                                    its, ite, jts, jte, kts, kte      )

   ELSE

      ! Use WRF's cloud water mixing ratio, qc_curr, in the ATEX analytic
      ! radiation scheme.

      CALL ATEX_analytic_radiation( th_phy, qv_curr, qc_curr, dz8w,   &
                                    rho, pi_phy, p_phy, th_rad,       &
                                    ids, ide, jds, jde, kds, kde,     &
                                    ims, ime, jms, jme, kms, kme,     &
                                    its, ite, jts, jte, kts, kte      )

   ENDIF

#else

   CALL ATEX_analytic_radiation( th_phy, qv_curr, qc_curr, dz8w,   &
                                 rho, pi_phy, p_phy, th_rad,       &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 its, ite, jts, jte, kts, kte      )

#endif /*CLUBB*/

ELSE

   DO i = i_start, i_end, 1
      DO j = j_start, j_end, 1
         DO k = k_start, k_end, 1
            th_rad(i,k,j) = 0.0
         ENDDO
      ENDDO
   ENDDO

ENDIF

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      DO k = k_start, k_end, 1
         th_tend(i,k,j)  = th_rad(i,k,j)
      ENDDO
   ENDDO
ENDDO

END SUBROUTINE FIRE_forcings

!===============================================================================

SUBROUTINE FIRE_init( RTHCUTEN, RUCUTEN, RVCUTEN,           &
                      RQVCUTEN, RQCCUTEN, restart,          &
                      ids, ide, jds, jde, kds, kde,         &
                      ims, ime, jms, jme, kms, kme,         &
                      its, ite, jts, jte, kts, kte          )

IMPLICIT NONE

! Input Variables
INTEGER, INTENT (IN):: ids, ide, jds, jde, kds, kde,  &
                       ims, ime, jms, jme, kms, kme,  &
                       its, ite, jts, jte, kts, kte

LOGICAL, INTENT (IN):: restart

! Output Variables
REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT (OUT)::  &
      RTHCUTEN, & ! Potential temperature tendency       [K]
      RUCUTEN,  & ! U-wind component tendency            [m/s]
      RVCUTEN,  & ! V-wind component tendency            [m/s]
      RQVCUTEN, & ! Water vapor mixing ratio tendency    [(kg/kg)/s]
      RQCCUTEN    ! Cloud water mixing ratio tendency    [(kg/kg)/s]

! Local Variables
INTEGER:: i, j, k, itf, jtf, ktf

!-------------------------------------------------------------------------------

itf = min0(ite, ide-1)
jtf = min0(jte, jde-1)
ktf = min0(kte, kde-1)

! Initialize the tendency variables to 0, as long as it is not a restart run.
! All the tendency variables are located at thermodynamic levels.
IF (.not. restart) THEN
   DO i = its, itf
      DO j = jts, jtf
         DO k = kts, ktf
            RTHCUTEN(i,k,j) = 0.0
            RUCUTEN(i,k,j)  = 0.0
            RVCUTEN(i,k,j)  = 0.0
            RQVCUTEN(i,k,j) = 0.0
            RQCCUTEN(i,k,j) = 0.0
         ENDDO
      ENDDO
   ENDDO
ENDIF

END SUBROUTINE FIRE_init

!===============================================================================
!###############################################################################
!     GCSS ATEX CASE
!###############################################################################
!===============================================================================

SUBROUTINE ATEX_tend( RTHCUTEN, RUCUTEN, RVCUTEN,            &
                      RQVCUTEN, RQCCUTEN, CU_ACT_FLAG,       &
                      th_phy, u_phy, v_phy, qv_curr,         &
                      qc_curr, dz8w, rho, pi_phy, p_phy,     &
                      z, num_force_layers, z_force, w_subs,  &
                      u_g, v_g, z_base, u_base, v_base,      &
                      dt, itimestep,                         &
#ifdef CLUBB
                      ! variables added for CLUBB PBL parameterization
                      bl_pbl_physics, rcm_CLUBB,             &
                      ! CLUBB PBL changes end
#endif /*CLUBB*/
                      ids, ide, jds, jde, kds, kde,          &
                      ims, ime, jms, jme, kms, kme,          &
                      its, ite, jts, jte, kts, kte           )

USE module_quadratic, ONLY: quadratic_derivative
USE module_init_utilities, ONLY: interp_0

IMPLICIT NONE

! Input variables
INTEGER, INTENT (IN):: ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte

#ifdef CLUBB
! Variable added for CLUBB PBL parameterization
INTEGER, INTENT(IN):: bl_pbl_physics
! CLUBB PBL changes end
#endif /*CLUBB*/

INTEGER, INTENT(IN):: itimestep  ! Model timestep number

REAL, INTENT(IN):: dt  ! Model timestep duration      [s]

REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)::  &
      th_phy,  & ! Potential Temperature              [K]
      u_phy,   & ! u-wind at half-level of x          [m/s]
      v_phy,   & ! v-wind at half-level of y          [m/s]
      qv_curr, & ! Water Vapor Mixing Ratio           [kg/kg]
      qc_curr, & ! Cloud Water Mixing Ratio           [kg/kg]
      dz8w,    & ! Distance between momentum levels   [m]
      rho,     & ! Density of air                     [kg/m^3]
      pi_phy,  & ! Exner Function                     [-]
      p_phy,   & ! Pressure                           [Pa]
      z          ! Altitude of thermodynamic levels   [m]

#ifdef CLUBB
! Variable added for CLUBB PBL parameterization
REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)::  &
      rcm_CLUBB  ! Cloud Water Mixing Ratio from CLUBB PBL scheme  [kg/kg]
! CLUBB PBL changes end
#endif /*CLUBB*/

REAL, DIMENSION(kms:kme), INTENT(IN)::  &
      z_base    ! Initial altitudes of thermodynamic grid levels   [m]

INTEGER, INTENT(IN):: num_force_layers

! Input/Output variables
REAL, DIMENSION(num_force_layers), INTENT(INOUT)::  &
      z_force    ! Altitudes of subsidence profile    [m]

REAL, DIMENSION(num_force_layers), INTENT(INOUT)::  &
      u_g,   & ! U-component of geostrophic wind    [m/s]
      v_g,   & ! V-component of geostrophic wind    [m/s]
      w_subs   ! Imposed large-scale subsidence     [m/s]

! Output variables
REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT (OUT)::  &
      RTHCUTEN, & ! Potential temperature tendency       [K]
      RUCUTEN,  & ! U-wind component tendency            [m/s]
      RVCUTEN,  & ! V-wind component tendency            [m/s]
      RQVCUTEN, & ! Water vapor mixing ratio tendency    [(kg/kg)/s]
      RQCCUTEN    ! Cloud water mixing ratio tendency    [(kg/kg)/s]

REAL, DIMENSION(kms:kme), INTENT(OUT)::  &
      u_base, & ! U-component of geostrophic winds at z_base altitudes  [m/s]
      v_base    ! V-component of geostrophic winds at z_base altitudes  [m/s]

LOGICAL, DIMENSION (ims:ime, jms:jme), INTENT (INOUT)::  CU_ACT_FLAG

! Local variables
REAL, DIMENSION(ims:ime, kms:kme, jms:jme)::  &
      dTHdZ, & ! d(theta)/dz; for use with large-scale subsidence  [K/m]
      dUdZ,  & ! du/dz; for use with large-scale subsidence        [(m/s)/m]
      dVdZ,  & ! dv/dz; for use with large-scale subsidence        [(m/s)/m]
      dQVdZ, & ! d(q_v)/dz; for use with large-scale subsidence    [(kg/kg)/m]
      dQCdZ    ! d(q_c)/dz; for use with large-scale subsidence    [(kg/kg)/m]

REAL::  &
      w_ls_sub  ! Imposed large-scale subsidence (thermodynamic levels)  [m/s]

REAL, DIMENSION(ims:ime, kms:kme, jms:jme)::  &
      th_tend, & ! Potential temperature tendency   [K/s]
      qt_tend    ! Total water tendency             [(kg/kg)/s]

INTEGER:: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k

!-------------------------------------------------------------------------------

i_start = its
i_end   = MIN(ide-1, ite)
j_start = jts
j_end   = MIN(jde-1, jte)
k_start = kts
k_end   = MIN(kde-1, kte)

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      CU_ACT_FLAG(i,j) = .true.
   ENDDO
ENDDO

! Get the values for radiative cooling and large-scale horizontal advection.

CALL ATEX_forcings( th_phy, qv_curr, qc_curr, dz8w,        &
                    rho, pi_phy, p_phy, z, dt,             &
                    itimestep, th_tend, qt_tend,           &
                    num_force_layers, z_force,             &
                    w_subs, u_g, v_g,                      &
#ifdef CLUBB
                    ! variables added for CLUBB PBL parameterization
                    bl_pbl_physics, rcm_CLUBB,             &
                    ! CLUBB PBL changes end
#endif /*CLUBB*/
                    ids, ide, jds, jde, kds, kde,          &
                    ims, ime, jms, jme, kms, kme,          &
                    its, ite, jts, jte, kts, kte           )

! Tendencies due to vertical advection by imposed large-scale subsidence.

! Get the derivative according to a parabolic fit between three points
! on three consecutive levels.
   
CALL quadratic_derivative( th_phy, z, dTHdZ,             &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           its, ite, jts, jte, kts, kte  )
   
CALL quadratic_derivative( u_phy, z, dUdZ,               &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           its, ite, jts, jte, kts, kte  )

CALL quadratic_derivative( v_phy, z, dVdZ,               &
                           ids, ide, jds, jde, kds, kde, &
                           ims, ime, jms, jme, kms, kme, &
                           its, ite, jts, jte, kts, kte  )

CALL quadratic_derivative( qv_curr, z, dQVdZ,              &
                           ids, ide, jds, jde, kds, kde,   &
                           ims, ime, jms, jme, kms, kme,   &
                           its, ite, jts, jte, kts, kte    )

CALL quadratic_derivative( qc_curr, z, dQCdZ,              &
                           ids, ide, jds, jde, kds, kde,   &
                           ims, ime, jms, jme, kms, kme,   &
                           its, ite, jts, jte, kts, kte    )

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      DO k = k_start, k_end, 1

         ! Value of imposed large-scale subsidence
         ! WRF's large-scale subsidence is input from an external netCDF file.
         ! If there is input for large-scale subsidence, z_force will be an
         ! array of various reference altitudes.  Otherwise, z_force will be an
         ! array of zeros.
         ! The ATEX case applies imposed large-scale subsidence 90 minutes
         ! (5400 seconds) into the model run.
         IF ( ANY( z_force(:) > 0.0 ) .AND. num_force_layers > 0 ) THEN
            ! Large-scale subsidence is computed at thermodynamic grid levels.
            w_ls_sub = interp_0(w_subs,z_force,z(i,k,j),num_force_layers)
         ELSE
            w_ls_sub = 0.0
         ENDIF

         RTHCUTEN(i,k,j) = - w_ls_sub * dTHdZ(i,k,j)
         RUCUTEN(i,k,j)  = - w_ls_sub * dUdZ(i,k,j)
         RVCUTEN(i,k,j)  = - w_ls_sub * dVdZ(i,k,j)
         RQVCUTEN(i,k,j) = - w_ls_sub * dQVdZ(i,k,j)
         RQCCUTEN(i,k,j) = - w_ls_sub * dQCdZ(i,k,j)

      ENDDO
   ENDDO
ENDDO

! Change in d(Something)/dt due to radiation and advection.

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      DO k = k_start, k_end, 1
         RTHCUTEN(i,k,j) = RTHCUTEN(i,k,j) + th_tend(i,k,j)
         RQVCUTEN(i,k,j) = RQVCUTEN(i,k,j) + qt_tend(i,k,j)
      ENDDO
   ENDDO
ENDDO

! Set the values of u_base and v_base to the values of the geostrophic winds.
!
! The values of u_base, v_base, t_base, qv_base, and z_base are all set in the
! appropriate module_initialize file.  However, u_base and v_base are reset at
! this point so that they will contain the u and v geostrophic wind profiles,
! rather than simply the initial values of the u and v winds.  The values of
! u_base and v_base are reset based on the altitude profile z_base.  That way,
! u_base and v_base are reset at the altitudes at which they were originally
! set, and at which the t_base and qv_base profiles are set.  This is important
! for the use of these variables in the WRF model code.  For example, subroutine
! perturbation_coriolis (in module_big_step_utilities_em) uses u_base and v_base
! at altitude profile z_base, and then makes a weighted adjustment based on the
! current altitude profile, z, for each column.
!
! In an idealized case such as this one, WRF initially sets the geopotential
! profile, and thus the altitude profiles, to be the same in every column.
! Thus, the various base arrays only need to be set in the vertical.
!
! This code sets the u_base and v_base profiles to the geostrophic wind profiles
! in the same manner that is found in WRF's subroutine force_scm.
!
! In order to use this code in determining the coriolis effect on the wind
! profiles, set the flag pert_coriolis to .true. in namelist.input.

DO k = k_start, k_end, 1

   ! Values of the geostrophic winds.
   ! WRF's geostrophic winds are input from an external netCDF file.  If there
   ! is input for the geostrophic winds, z_force will be an array of various
   ! reference altitudes.  Otherwise, z_force will be an array of zeros.
   IF ( ANY( z_force(:) > 0.0 ) .AND. num_force_layers > 0 ) THEN
      ! Geostrophic winds are computed at thermodynamic grid levels.
      u_base(k) = interp_0(u_g,z_force,z_base(k),num_force_layers)
      v_base(k) = interp_0(v_g,z_force,z_base(k),num_force_layers)
   ELSE
      ! If there is not netCDF input for the geostrophic winds, then keep u_base
      ! and v_base at the same values, which would be the initial wind profile.
      u_base(k) = u_base(k)
      v_base(k) = v_base(k)
   ENDIF

ENDDO

END SUBROUTINE ATEX_tend

!===============================================================================

SUBROUTINE ATEX_forcings( th_phy, qv_curr, qc_curr, dz8w,        &
                          rho, pi_phy, p_phy, z, dt,             &
                          itimestep, th_tend, rt_tend,           &
                          num_force_layers, z_force,             &
                          w_subs, u_g, v_g,                      &
#ifdef CLUBB
                          ! variables added for CLUBB PBL parameterization
                          bl_pbl_physics, rcm_CLUBB,             &
                          ! CLUBB PBL changes end
#endif /*CLUBB*/
                          ids, ide, jds, jde, kds, kde,          &
                          ims, ime, jms, jme, kms, kme,          &
                          its, ite, jts, jte, kts, kte           )

#ifdef CLUBB
! Added for CLUBB PBL parameterization
USE module_state_description, ONLY:  &
    CLUBBSCHEME
! CLUBB PBL changes end
#endif /*CLUBB*/

IMPLICIT NONE

! Input variables
INTEGER, INTENT (IN):: ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte

#ifdef CLUBB
! Variable added for CLUBB PBL parameterization
INTEGER, INTENT(IN):: bl_pbl_physics
! CLUBB PBL changes endi
#endif /*CLUBB*/

INTEGER, INTENT(IN):: itimestep  ! Model timestep number

REAL, INTENT(IN):: dt  ! Model timestep duration      [s]

REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)::  &
      th_phy,  & ! Potential Temperature              [K]
      qv_curr, & ! Water Vapor Mixing Ratio           [kg/kg]
      qc_curr, & ! Cloud Water Mixing Ratio           [kg/kg]
      dz8w,    & ! Distance between momentum levels   [m]
      rho,     & ! Density of air                     [kg/m^3]
      pi_phy,  & ! Exner Function                     [-]
      p_phy,   & ! Pressure                           [Pa]
      z          ! Altitude of thermodynamic levels   [m]

#ifdef CLUBB
! Variable added for CLUBB PBL parameterization
REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)::  &
      rcm_CLUBB  ! Cloud Water Mixing Ratio from CLUBB PBL scheme  [kg/kg]
! CLUBB PBL changes end
#endif /*CLUBB*/

INTEGER, INTENT(IN):: num_force_layers

! Input/Output variables
REAL, DIMENSION(num_force_layers), INTENT(INOUT)::  &
      z_force    ! Altitudes of subsidence profile    [m]

REAL, DIMENSION(num_force_layers), INTENT(INOUT)::  &
      u_g,   & ! U-component of geostrophic wind    [m/s]
      v_g,   & ! V-component of geostrophic wind    [m/s]
      w_subs   ! Imposed large-scale subsidence     [m/s]

! Output variables
REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(OUT)::  &
      th_tend, & ! Overall theta tendency       [K/s]
      rt_tend    ! Overall total water tendency [(kg/kg)/s]

! Local Variables
REAL, DIMENSION(ims:ime, kms:kme, jms:jme)::  &
      th_rad, &  ! Theta tendency from radiative cooling           [K/s]
      th_adv, &  ! Theta tendency from large-scale advection       [K/s]
      rt_adv     ! Total Water tendency from large-scale advection [(kg/kg)/s]

REAL::  &
      total_time, & ! Elapsed time since midnight on start date             [s]
      z_i           ! Altitude of 6.5 g/kg total water mixing ratio contour [m]

INTEGER:: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k, lev

!------------------------------------------------------------------------------

i_start = its
i_end   = MIN(ide-1, ite)
j_start = jts
j_end   = MIN(jde-1, jte)
k_start = kts
k_end   = MIN(kde-1, kte)

#ifdef CLUBB
! Added for CLUBB PBL parameterization
IF ( bl_pbl_physics == CLUBBSCHEME ) THEN

   ! Use CLUBB's cloud water mixing ratio, rcm_CLUBB, in the ATEX analytic
   ! radiation scheme.

   ! Find the change in theta with respect to time due to radiation.
   CALL ATEX_analytic_radiation( th_phy, qv_curr, rcm_CLUBB, dz8w, &
                                 rho, pi_phy, p_phy, th_rad,       &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 its, ite, jts, jte, kts, kte      )

ELSE

   ! Use WRF's cloud water mixing ratio, qc_curr, in the ATEX analytic
   ! radiation scheme.

   ! Find the change in theta with respect to time due to radiation.
   CALL ATEX_analytic_radiation( th_phy, qv_curr, qc_curr, dz8w,   &
                                 rho, pi_phy, p_phy, th_rad,       &
                                 ids, ide, jds, jde, kds, kde,     &
                                 ims, ime, jms, jme, kms, kme,     &
                                 its, ite, jts, jte, kts, kte      )

ENDIF

#else

! Find the change in theta with respect to time due to radiation.
CALL ATEX_analytic_radiation( th_phy, qv_curr, qc_curr, dz8w,   &
                              rho, pi_phy, p_phy, th_rad,       &
                              ids, ide, jds, jde, kds, kde,     &
                              ims, ime, jms, jme, kms, kme,     &
                              its, ite, jts, jte, kts, kte      )

#endif /*CLUBB*/

! Find the inversion height, z_i.
CALL find_zi( qv_curr, qc_curr, z, z_i,     &
              ids, ide, jds, jde, kds, kde, &
              ims, ime, jms, jme, kms, kme, &
              its, ite, jts, jte, kts, kte  )

! The ATEX case applies forcings 90 minutes (5400 seconds) into the model run.
total_time = dt*itimestep

! The ATEX case specifies that the value of imposed large-scale subsidence
! varies linearly between 0 m/s at the surface and -0.0065 m/s at the inversion
! height, z_i.  Above the inversion, the value of imposed large-scale subsidence
! tapers off linearly to 0 m/s at an altitude of z_i + 300 m.
!
! Profile for elapsed time < 5400.0 seconds:
!
! lev z_force    u_g       v_g     w_subs
! --- -------  -------   -------  -------
!  1     0.0   -11.00     -2.00     0.0
!  2   150.0   -10.55     -1.90     0.0
!  3   700.0    -8.90     -1.10     0.0
!  4   750.0    -8.75     -1.00     0.0
!  5  1400.0    -6.80     -0.14     0.0
!  6  any z_1   linint    linint    0.0
!  7  1650.0    -5.75      0.18     0.0
!  8  any z_2   linint    linint    0.0
!  9  4000.0     1.00      2.75     0.0
!
! where linint stands for a value that must be found by linear interpolation
! based on the specified bookend values.  For the profile that is applied before
! 5400.0 elapsed seconds, "any z_1" can be any value that satisfies:
! 1400.0 m. < "any z_1" < 1650.0 m.; and "any z_2" can be any value that
! satisfies:  1650.0 m. < "any z_2" < 4000.0.  This is due to the fact that
! these levels are simply "placeholders" for the levels occupied by z_i and
! z_i + 300.0 m., respectively, in the profile that is applied after 5400.0
! elapsed seconds.  Therefore, in the ATEX forcing file (ATEX_forcing.txt),
! "any z_1" and "any z_2" were chosen so that u_g and v_g at those levels came
! out evenly to three decimal places, for ease of entering those values.  Thus,
! for level 6, z_force = 1575.0, u_g = -6.065, and v_g = 0.084.  For level 8,
! z_force = 2825.0, u_g = -2.375, and v_g = 1.465.
!
! Profile for elapsed time >= 5400.0 seconds:
!
! lev z_force    u_g       v_g     w_subs
! --- -------  -------   -------  -------
!  1     0.0   -11.00     -2.00     0.0
!  2   150.0   -10.55     -1.90    linint
!  3   700.0    -8.90     -1.10    linint
!  4   750.0    -8.75     -1.00    linint
!  5  1400.0    -6.80     -0.14    linint
!  6     z_i    linint    linint  -0.0065
!  7  1650.0    -5.75      0.18    linint
!  8  z_i+300   linint    linint    0.0
!  9  4000.0     1.00      2.75     0.0
!
! where linint stands for a value that must be found by linear interpolation
! based on the specified bookend values.  For an ATEX run that is working
! properly, z_i usually ranges between about 1560 m. and 1610 m.  Thus, it
! should not exceed 1650.0 m., and there should not be any problem with it
! staying at level 6.  Likewise, z_i + 300.0 m. should stay comfortably at
! level 8.

IF ( total_time >= 5400.0 ) THEN

   ! Set z_force at level 6 to z_i.
   z_force(6) = z_i
   ! Set z_force at level 8 to z_i + 300.0 m.
   z_force(8) = z_i + 300.0

   ! Reset the geostrophic wind values at altitude z_i, which changes from
   ! timestep-to-timestep.  Use linear interpolation for level 6, based on the
   ! specified values at levels 5 and 7.
   u_g(6) = ( ( u_g(7) - u_g(5) ) / ( z_force(7) - z_force(5) ) )  &
            * ( z_force(6) - z_force(5) ) + u_g(5)
   v_g(6) = ( ( v_g(7) - v_g(5) ) / ( z_force(7) - z_force(5) ) )  &
            * ( z_force(6) - z_force(5) ) + v_g(5)

   ! Reset the geostrophic wind values at altitude z_i + 300.0 m., which changes
   ! from timestep-to-timestep.  Use linear interpolation for level 8, based on
   ! the specified values at levels 7 and 9.
   u_g(8) = ( ( u_g(9) - u_g(7) ) / ( z_force(9) - z_force(7) ) )  &
            * ( z_force(8) - z_force(7) ) + u_g(7)
   v_g(8) = ( ( v_g(9) - v_g(7) ) / ( z_force(9) - z_force(7) ) )  &
            * ( z_force(8) - z_force(7) ) + v_g(7)

   ! The value of imposed large-scale subsidence will be set to -0.0065 m/s at
   ! altitude z_i, which is at level 6.
   w_subs(6) = -0.0065

   ! Reset the imposed large-scale subsidence profile so that it varies linearly
   ! between the surface at altitude z_i.  Therefore, use linear interpolation
   ! for levels 2 through 5, based on the specified values at levels 1 and 6.
   DO lev = 2, 5, 1
      w_subs(lev) = ( w_subs(6) / z_force(6) ) * z_force(lev)
   ENDDO
   ! Reset the imposed large-scale subsidence profile so that it varies linearly
   ! between altitude z_i and altitude z_i + 300 m.  Therefore, use linear
   ! interpolation at level 7, based on the specified values at levels 6 and 8.
   w_subs(7) = ( ( w_subs(8) - w_subs(6) ) / ( z_force(8) - z_force(6) ) )  &
               * ( z_force(7) - z_force(6) ) + w_subs(6)

ENDIF

DO i = i_start, i_end, 1
   DO k = k_start, k_end, 1
      DO j = j_start, j_end, 1

         IF ( total_time >= 5400.0 ) THEN
            IF ( z(i,k,j) >= 0.0 .AND. z(i,k,j) <= z_i ) THEN
               th_adv(i,k,j)   = -0.00002315 - (0.00002315/2.0)*(1.0 - (z(i,k,j)/z_i))
               rt_adv(i,k,j)   = -0.0000000158 * (1.0 - (z(i,k,j)/z_i))
            ELSEIF ( z(i,k,j) > z_i .AND. z(i,k,j) <= z_i+300.0 ) THEN
               th_adv(i,k,j)   = -0.00002315 * (1.0 - (z(i,k,j)-z_i)/300.0)
               rt_adv(i,k,j)   = 0.0
            ELSEIF ( z(i,k,j) > z_i+300.0 ) THEN
               th_adv(i,k,j)   = 0.0
               rt_adv(i,k,j)   = 0.0
            ENDIF
         ELSE
            th_adv(i,k,j)   = 0.0
            rt_adv(i,k,j)   = 0.0
         ENDIF

         ! Send back th_tend and qt_tend; th_tend sums radiative and advective tends.
         th_tend(i,k,j) = th_rad(i,k,j) + th_adv(i,k,j)
         rt_tend(i,k,j) = rt_adv(i,k,j)

      ENDDO
   ENDDO
ENDDO

END SUBROUTINE ATEX_forcings

!==============================================================================

SUBROUTINE ATEX_analytic_radiation( th_phy, qv_curr, qc_curr, dz8w,   &
                                    rho, pi_phy, p_phy, th_rad,       &
                                    ids, ide, jds, jde, kds, kde,     &
                                    ims, ime, jms, jme, kms, kme,     &
                                    its, ite, jts, jte, kts, kte      )

USE module_model_constants

IMPLICIT NONE

! Input variables
INTEGER, INTENT (IN):: ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte

REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)::  &
      th_phy,  & ! Potential Temperature              [K]
      qv_curr, & ! Water Vapor Mixing Ratio           [kg/kg]
      qc_curr, & ! Cloud Water Mixing Ratio           [kg/kg]
      dz8w,    & ! Distance between momentum levels   [m]
      rho,     & ! Density of air                     [kg/m^3]
      pi_phy,  & ! Exner Function                     [-]
      p_phy      ! Pressure                           [Pa]

! Output variable
REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(OUT)::  &
      th_rad   ! Potential Temperature tendency due to long-wave radiation [K/s]

! Local variables
REAL, DIMENSION(kms:kme)::  &
      rho_d, & ! Density of dry air   [kg/m^3]
      LWP,   & ! Liquid Water Path    [kg/m^2]
      F_rad    ! Radiative Flux       [W/m^2]

REAL, PARAMETER:: F0 = 74.0      ! [W/m^2]
REAL, PARAMETER:: kappa = 130.0  ! [m/kg]

INTEGER:: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k

!-------------------------------------------------------------------------------
! DESCRIPTION -- Brian Griffin.
!
! Liquid Water Path is defined as the total amount of liquid water present
! in an air column ( kg. liquid water / m^2 air ).
! Here, the only species of liquid water to be considered is cloud water.
! Cloud water mixing ratio is the mass cloud water / mass dry air.
! Dry-air density is mass dry air / volume air.
! Multiplying the two results in mass cloud water / volume air.
! Integrating that result over a vertical column from a starting height
! to an ending height gives Liquid Water Path.  In our case, the top of the
! model serves as the ending point.  The value of Liquid Water Path is
! computed at each momentum-level grid height with that height as the 
! starting point and then integrating over every level all the way up.
!
! dz8w is the difference in height between momentum levels.  It is located
! at the centered thermodynamic level.
!
! The formula for radiative forcing at a specific level is as follows:
!
! F_rad = F0 * EXP[ -kappa * LWP ]
!
! kappa is in m / kg. liquid water.
! F0 is in W / m^2; as is F_rad.
!
! The formula for finding the change in theta with respect to time
! based upon radiative cooling is as follows:
!
! dTH/dt = [ -1.0 / ( cp * exner * rho ) ] * dF_rad/dz
!
! Here, rho is the overall rho.
!
! One finds the dry-air density given the total density by the following method:
!
! The Equation of State for Dry Air (Note that dry-air pressure and dry-air
! density are often called "base state" pressure and density):
! p_d = rho_d * Rd * T
!
! The Equation of State for Water Vapor:
! p_v = rho_v * Rv * T
!
! Law of Partial Pressures:
! p = p_d + p_v
!
! Thus:
! p = ( rho_d * Rd * T ) + ( rho_v * Rv * T )
! p = ( rho_d * Rd + rho_v * Rv ) T
!
! Definition of Water Vapor Mixing Ratio:
! r_v = rho_v / rho_d
!
! Thus:
! rho_v = rho_d * r_v
!
! Therefore:
! p = ( rho_d * Rd + rho_d * r_v * Rv ) T
! p = rho_d ( Rd + r_v * Rv ) T
!
! Definition of Potential Temperature:
! T = Theta * (p/p0)^(Rd/cp) = Theta * exner
!
! Therefore:
! p = rho_d ( Rd + r_v * Rv ) * Theta * exner
!
! The density of dry air, rho_d, is:
! rho_d = p / [ ( Rd + r_v * Rv ) * Theta * exner ]
!
!-------------------------------------------------------------------------------

i_start = its
i_end   = MIN(ide-1, ite)
j_start = jts
j_end   = MIN(jde-1, jte)
k_start = kts
k_end   = MIN(kde-1, kte)

! Note on the WRF grid indices
!
! Throughout much of the WRF code, the grid index kte stands for the number of
! WRF momentum levels.  This can be found in subroutine phy_prep in
! module_big_step_utilities.F.  This can also be found in subroutine phy_init
! in module_physics_init.F.  However, in WRF's call to it's various drivers,
! such as cumulus_driver, the value of kte that is sent in is reduced by 1.
! Thus, the value of kte sent into this subroutine is 1 less than the number of
! WRF momentum levels.  Thus, in any loop that is related to the number of
! momentum levels, the upper index must be kte+1.  Other cumulus schemes also
! make use of grid index kte+1.
!
! The number of WRF thermodynamic levels is one less than the number of WRF
! momentum levels.  The index of the uppermost WRF thermodynamic level is
! usually kte-1.  However, in this subroutine, the index of the uppermost WRF
! thermodynamic level is kte.  Also, in this subroutine, k_end = kte.  Thus, the
! index of the uppermost WRF thermodynamic level is k_end

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1

      ! Find dry-air density, rho_d, at thermodynamic levels.
      DO k = k_start, k_end, 1
         rho_d(k)  &
         = p_phy(i,k,j) /                                         &
           ( ( r_d + qv_curr(i,k,j)*r_v ) * th_phy(i,k,j) * pi_phy(i,k,j) )
      ENDDO

      ! Find the Liquid Water Path at each momentum grid level in the column.
      LWP(kte+1) = 0.0
      DO k = kte, k_start, -1
         LWP(k) = LWP(k+1) + ( rho_d(k) * qc_curr(i,k,j) * dz8w(i,k,j) )
      ENDDO

      ! Find the radiative forcing based upon the liquid water path at each
      ! momentum level.
      DO k = k_start, kte+1, 1
         F_rad(k) = F0 * EXP( -kappa * LWP(k) )
      ENDDO

      ! Find the change in theta with respect to time.  This is centered at
      ! thermodynamic levels.
      DO k = k_start, k_end, 1
         th_rad(i,k,j) = ( -1.0 / ( cp * pi_phy(i,k,j) * rho(i,k,j) ) )   &
                         * ( F_rad(k+1) - F_rad(k) ) / dz8w(i,k,j)
      ENDDO

   ENDDO
ENDDO

END SUBROUTINE ATEX_analytic_radiation

!===============================================================================
! In this subroutine we find z_i.
! z_i is the horizontally-averaged height
! of the 6.5 g/kg total water mixing ratio contour at a fixed time.

SUBROUTINE find_zi( qv_curr, qc_curr, z, z_i,     &
                    ids, ide, jds, jde, kds, kde, &
                    ims, ime, jms, jme, kms, kme, &
                    its, ite, jts, jte, kts, kte  )

IMPLICIT NONE

! Input variables
INTEGER, INTENT(IN):: ids, ide, jds, jde, kds, kde, &
                      ims, ime, jms, jme, kms, kme, &
                      its, ite, jts, jte, kts, kte

REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT(IN)::  &
      qv_curr, & ! Water Vapor Mixing Ratio           [kg/kg]
      qc_curr, & ! Cloud Water Mixing Ratio           [kg/kg]
      z          ! Altitude of thermodynamic levels   [m]

! Output variable
REAL, INTENT(OUT)::  &
      z_i        ! Average Height of 6.5 g/kg contour [m]

! Local variables
INTEGER:: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k

REAL, DIMENSION(kms:kme):: mean_rt, mean_z

REAL:: sum_rt, sum_z, deriv

!-------------------------------------------------------------------------------

i_start = its
i_end   = MIN(ide-1,ite)
j_start = jts
j_end   = MIN(jde-1,jte)
k_start = kts
k_end   = MIN(kde-1,kte)

! Find the average total water mixing ratio for each horizontal grid level.
DO k = k_start, k_end, 1

   sum_rt = 0.0

   DO i = i_start, i_end, 1
      DO j = j_start, j_end, 1
         sum_rt = sum_rt + qv_curr(i,k,j) + qc_curr(i,k,j)
      ENDDO
   ENDDO

   mean_rt(k) = sum_rt / ( (i_end-i_start+1)*(j_end-j_start+1) )

ENDDO

! Find the average thermodynamic grid height for each horizontal grid level.
DO k = k_start, k_end, 1

   sum_z = 0.0

   DO i = i_start, i_end, 1
      DO j = j_start, j_end, 1
         sum_z = sum_z + z(i,k,j)
      ENDDO
   ENDDO

   mean_z(k) = sum_z / ( (i_end-i_start+1)*(j_end-j_start+1) )

ENDDO

! Find z_i, the average height of the 6.5 g/kg total water mixing ratio contour.
DO k = k_start, k_end, 1

   IF ( mean_rt(k) <= 0.0065 ) THEN
      deriv = ( mean_rt(k) - mean_rt(k-1) ) / ( mean_z(k) - mean_z(k-1) )
      z_i = mean_z(k-1) + ( 0.0065 - mean_rt(k-1) ) / deriv
      EXIT
   ENDIF

ENDDO

END SUBROUTINE find_zi

!===============================================================================

SUBROUTINE ATEX_init( RTHCUTEN, RUCUTEN, RVCUTEN,           &
                      RQVCUTEN, RQCCUTEN, restart,          &
                      ids, ide, jds, jde, kds, kde,         &
                      ims, ime, jms, jme, kms, kme,         &
                      its, ite, jts, jte, kts, kte          )

IMPLICIT NONE

! Input Variables
INTEGER, INTENT (IN):: ids, ide, jds, jde, kds, kde,                  &
                       ims, ime, jms, jme, kms, kme,                  &
                       its, ite, jts, jte, kts, kte

LOGICAL, INTENT (IN):: restart

! Output Variables
REAL, DIMENSION(ims:ime, kms:kme, jms:jme), INTENT (OUT)::  &
      RTHCUTEN, & ! Potential temperature tendency       [K]
      RUCUTEN,  & ! U-wind component tendency            [m/s]
      RVCUTEN,  & ! V-wind component tendency            [m/s]
      RQVCUTEN, & ! Water vapor mixing ratio tendency    [(kg/kg)/s]
      RQCCUTEN    ! Cloud water mixing ratio tendency    [(kg/kg)/s]

! Local Variables
INTEGER:: i, j, k, itf, jtf, ktf

!-------------------------------------------------------------------------------

itf = min0(ite, ide-1)
jtf = min0(jte, jde-1)
ktf = min0(kte, kde-1)

! Initialize the tendency variables to 0, as long as it is not a restart run.
! All the tendency variables are located at thermodynamic levels.
IF (.not. restart) THEN
   DO i = its, itf
      DO j = jts, jtf
         DO k = kts, ktf
            RTHCUTEN(i,k,j) = 0.0
            RUCUTEN(i,k,j)  = 0.0
            RVCUTEN(i,k,j)  = 0.0
            RQVCUTEN(i,k,j) = 0.0
            RQCCUTEN(i,k,j) = 0.0
         ENDDO
      ENDDO
   ENDDO
ENDIF

END SUBROUTINE ATEX_init

!===============================================================================
!###############################################################################
!     WANGARA CASE
!###############################################################################
!===============================================================================

SUBROUTINE Wangara_tend( num_force_layers, z_force, u_g,       &
                         v_g, z_base, u_base, v_base,          &
                         ids, ide, jds, jde, kds, kde,         &
                         ims, ime, jms, jme, kms, kme,         &
                         its, ite, jts, jte, kts, kte          )

USE module_init_utilities, ONLY: interp_0

IMPLICIT NONE

! Input Variables
INTEGER, INTENT (IN):: ids, ide, jds, jde, kds, kde,                  &
                       ims, ime, jms, jme, kms, kme,                  &
                       its, ite, jts, jte, kts, kte

INTEGER, INTENT(IN):: num_force_layers

REAL, DIMENSION(num_force_layers), INTENT(IN)::  &
      z_force    ! Altitudes of geostrophic wind profiles    [m]

REAL, DIMENSION(num_force_layers), INTENT(IN)::  &
      u_g, & ! U-component of geostrophic wind    [m/s]
      v_g    ! V-component of geostrophic wind    [m/s]

REAL, DIMENSION(kms:kme), INTENT(IN)::  &
      z_base    ! Initial altitudes of thermodynamic grid levels   [m]

! Output Variables
REAL, DIMENSION(kms:kme), INTENT(OUT)::  &
      u_base, & ! U-component of geostrophic winds at z_base altitudes  [m/s]
      v_base    ! V-component of geostrophic winds at z_base altitudes  [m/s]

! Local Variables
INTEGER:: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k

!-------------------------------------------------------------------------------

i_start = its
i_end   = MIN(ide-1, ite)
j_start = jts
j_end   = MIN(jde-1, jte)
k_start = kts
k_end   = MIN(kde-1, kte)

! Set the values of u_base and v_base to the values of the geostrophic winds.
!
! The values of u_base, v_base, t_base, qv_base, and z_base are all set in the
! appropriate module_initialize file.  However, u_base and v_base are reset at
! this point so that they will contain the u and v geostrophic wind profiles,
! rather than simply the initial values of the u and v winds.  The values of
! u_base and v_base are reset based on the altitude profile z_base.  That way,
! u_base and v_base are reset at the altitudes at which they were originally
! set, and at which the t_base and qv_base profiles are set.  This is important
! for the use of these variables in the WRF model code.  For example, subroutine
! perturbation_coriolis (in module_big_step_utilities_em) uses u_base and v_base
! at altitude profile z_base, and then makes a weighted adjustment based on the
! current altitude profile, z, for each column.
!
! In an idealized case such as this one, WRF initially sets the geopotential
! profile, and thus the altitude profiles, to be the same in every column.
! Thus, the various base arrays only need to be set in the vertical.
!
! This code sets the u_base and v_base profiles to the geostrophic wind profiles
! in the same manner that is found in WRF's subroutine force_scm.
!
! In order to use this code in determining the coriolis effect on the wind
! profiles, set the flag pert_coriolis to .true. in namelist.input.

DO k = k_start, k_end, 1

   ! Values of the geostrophic winds.
   ! WRF's geostrophic winds are input from an external netCDF file.  If there
   ! is input for the geostrophic winds, z_force will be an array of various
   ! reference altitudes.  Otherwise, z_force will be an array of zeros.
   IF ( ANY( z_force(:) > 0.0 ) .AND. num_force_layers > 0 ) THEN
      ! Geostrophic winds are computed at thermodynamic grid levels.
      u_base(k) = interp_0(u_g,z_force,z_base(k),num_force_layers)
      v_base(k) = interp_0(v_g,z_force,z_base(k),num_force_layers)
   ELSE
      ! If there is not netCDF input for the geostrophic winds, then keep u_base
      ! and v_base at the same values, which would be the initial wind profile.
      u_base(k) = u_base(k)
      v_base(k) = v_base(k)
   ENDIF

ENDDO

END SUBROUTINE Wangara_tend

!===============================================================================

END MODULE module_cu_TestCases
#endif /*TESTCASES*/
