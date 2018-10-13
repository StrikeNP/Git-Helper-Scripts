! $Id: module_mp_feingold2m.F 337 2012-03-26 18:00:00Z dschanen@uwm.edu $
!WRF:MODEL_LAYER:PHYSICS
!
#ifdef ESRL
MODULE module_mp_feingold2m
 
!	USE module_wrf_error

	IMPLICIT NONE
	
	! 2012/01 Tak Yamaguchi
	! - Cleaned & organized code upto the end of mp_2m_feingold so that the code is eaier
	!   for "me" to work with.
	! - All previous comments, etc. are kept.
	! - Add "SAVE" attribute for variables declared before "CONTAINS."
	! - Use VOCALS_RF06 values for lognormal distribution without -DWRFLES
	! - Add option to use a bimodal lognormal aerosol distribution with -DWRFLES
	
	PRIVATE ! BE CAREFULE WHEN REMOVE THIS LINE
	PUBLIC :: mp_2m_feingold
	
#ifdef WRFLES
	! Input from module_wrfles.F
	REAL, SAVE :: nccn   ! CCN number concentration (#/mg)
	REAL, SAVE :: dg1    ! Geometric mean of size distribution (cm)
	REAL, SAVE :: sg1    ! Geometric stddev of size distribution
	REAL, SAVE :: sig1   ! Sigma of cloud distribution
	REAL, SAVE :: sig2   ! Sigma of rain distribution
	REAL, SAVE :: drzst  ! # of seconds to initiate drizzle process after model starts
	INTEGER, SAVE :: drzflg   ! Bin sedimentation(drzflg=1) or bulk sedimentation (drzflg=2)
	LOGICAL, SAVE :: colflg   ! Flag for collection
	LOGICAL, SAVE :: bimodal  ! Bimodal lognormal distribution
	REAL, SAVE :: nccn2  ! If bimodal=TRUE, then nccn2, dg2, & sg2 have to be set. They represent
	REAL, SAVE :: dg2    ! the distribution for the larger geometric mean. nccn, dg1, & sg1
	REAL, SAVE :: sg2    ! initially represent the distribution for smaller geometric mean.
	REAL, SAVE :: nccn1_weight ! nccn1/(nccn1+nccn2) is used for partitioning nccn to nccn1 & nccn2
#else
	! VOCALS_RF06
	! Maximum CCN, cloud droplets, rain drops spectral info
	REAL, PARAMETER :: nccn = 150.
	REAL, PARAMETER :: dg1  = 0.2E-4
	REAL, PARAMETER :: sg1  = 1.5
	REAL, PARAMETER :: sig1 = 1.2
	REAL, PARAMETER :: sig2 = 1.2
	! # of seconds to initiate drizzle process after model starts
	REAL, PARAMETER :: drzst = 3600.
	! Flags for collection and drizzle
	! Bin sedimentation(drzflg=1) vs. Bulk sedimentation (drzflg=2)
	INTEGER, PARAMETER :: drzflg = 1
	LOGICAL, PARAMETER :: colflg = .TRUE.
#endif  /*WRFLES*/
	
	!
	REAL, SAVE :: pval, qval, expval, exqval, expvali, exqvali, exp1h, exq1h, exq15
	
	! Bin dimensions
	INTEGER, PARAMETER :: ln   = 2
	INTEGER, PARAMETER :: lpx  = 30
	INTEGER, PARAMETER :: lpa  = 36
	INTEGER, PARAMETER :: lpa1 = lpa + 1
	INTEGER, PARAMETER :: ithresh = 12
	
	! Look up table dimensions
	INTEGER, PARAMETER :: tbd = 40
	INTEGER, PARAMETER :: ir  = 2000
	
	!	
	REAL, PARAMETER :: rpi = 3.141592654
	REAL, PARAMETER :: rhowtr = 1000.   ! density of water = 1000 kg/m3 (comment by TY)
	REAL, PARAMETER :: xi  = 0.9840654
	REAL, PARAMETER :: ex1 = 1./3.
	REAL, PARAMETER :: ex2 = 2./3.
	
	!
	REAL, DIMENSION(1:lpa1), SAVE :: x, rad
	REAL, DIMENSION(1:lpx), SAVE  :: alp, bet, reyn
	
	! Hold collection tables
	REAL, DIMENSION(1:tbd,1:tbd), SAVE :: yym1, yyn1, yyn2
	REAL, DIMENSION(1:tbd,1:tbd,1:tbd,1:tbd), SAVE :: ym1, yn1, yn2
	
	! Hold sedimentation tables
	REAL, DIMENSION(1:ir,1:lpx), SAVE :: sc0, sc3, sd0, sd3
	
	! Constants to retrieve indices for lookup tables
	REAL, SAVE :: wgtcm, wgtcn, wgtrm1, wgtrm2, wgtrn1, wgtrn2
	
	! Lower bounds of mass and number (cm.g.s units) to consider collection
	REAL, SAVE :: mincm, mincn, minrn, minrm
	
CONTAINS

	!=====================================================================================
	! * NEW level 3.5 2-moment bulk scheme with LOOK-UP TABLE for collection
	!-------------------------------------------------------------------------------------
	! My collection table
	! My sedim table working
	! 
	! Used for 75/cc strongly drizzling GCSS case Atmos. Res. (Feingold et al. 1998)
	! Functions well but gives precip too early and then not enough.
	!-------------------------------------------------------------------------------------
	! * subroutine moms also changed to prevent killing drizzle before it has a chance to
	! * develop.
	! * 
	! * Includes average ventilation effects (April, 1996)
	! * Check interpolation of collection table
	!-------------------------------------------------------------------------------------
	! * 4 scalars for N, M in cloud and rain
	! * 1 scalar for supersat
	!=====================================================================================
	
	SUBROUTINE mp_2m_feingold( &
		! IN
		dt_in, itimestep, z, p, pii, rho, dz8w, &
		! INOUT
		qv, qc, qr, nca, ncc, ncr, ssr, eta0, th, rainnc, rainncv, &
		! OUT
		opdt, rainrtk, &
		! INDICES
		ids, ide, jds, jde, kds, kde, &   ! domain dims
		ims, ime, jms, jme, kms, kme, &   ! memory dims
		its, ite, jts, jte, kts, kte )    ! tile dims
	
#ifdef WRFLES
		! Tak Yamaguchi
		USE module_wrfles, ONLY: &
			nccn_feingold2m, dg_ccn_feingold2m, sg_ccn_feingold2m, bimodal_feingold2m, &
			nccn2_feingold2m, dg2_ccn_feingold2m, sg2_ccn_feingold2m, sg_cloud_feingold2m, &
			sg_rain_feingold2m, drzst_feingold2m, drzflg_feingold2m, colflg_feingold2m, &
			srcccn_add_feingold2m, srcccn_startstep_feingold2m, srcccn_starttime_feingold2m, &
			srcccn_feingold2m, srcccn_qvmin_feingold2m
#endif /*WRFLES*/

#ifdef WRFSTAT
		! Tak Yamaguchi 2012/02
		USE module_statistics, ONLY: docollect_data, doavgprecflux, precipl_eta, cond_rate_eta, &
		                             evap_rate_eta
#endif /*WRFSTAT*/

		IMPLICIT NONE
		
		! Subroutine arguments
		REAL, INTENT(IN) :: dt_in
		INTEGER, INTENT(IN) :: itimestep
		INTEGER, INTENT(IN) :: ids, ide, jds, jde, kds, kde
		INTEGER, INTENT(IN) :: ims, ime, jms, jme, kms, kme
		INTEGER, INTENT(IN) :: its, ite, jts, jte, kts, kte
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(IN) :: z, p, pii, rho, dz8w
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: qv, qc, qr
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: nca, ncc, ncr
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme), INTENT(INOUT) :: eta0, ssr, th
		REAL, DIMENSION(ims:ime,jms:jme), INTENT(INOUT) :: rainnc, rainncv
		REAL, DIMENSION(ims:ime,jms:jme), INTENT(OUT) :: opdt
		REAL, DIMENSION(ims:ime, kms:kme,jms:jme), INTENT(OUT) :: rainrtk
		
		! Local variables
	!	REAL, DIMENSION(1:lpa1) :: x, rad
	!	REAL, DIMENSION(1:lpx)  :: reyn, alp, bet, cm, cn
		REAL, DIMENSION(1:lpx)  :: cm, cn
		REAL, DIMENSION(1:lpx,kts:kte) :: ank2, amk2   ! for drizzle scheme 1
		REAL, DIMENSION(1:ln,kts:kte)  :: ank3, amk3   ! for drizzle scheme 2
		REAL, DIMENSION(1:ln) :: ankd, amkd, ank, amk, rnk, rmk
	!	REAL, DIMENSION(its:ite,kts:kte,jts:jte) :: tk
		REAL, DIMENSION(kts:kte) :: vctr1, vctr2, vctr3, vctr4, vctr9, rrate, zradar, dzm, dzt, dnk
		REAL :: vctr6, opd1, opd2
		
		! For look up tables
	!	REAL, DIMENSION(1:tbd,1:tbd) :: yym1, yyn1, yyn2
	!	REAL, DIMENSION(1:tbd,1:tbd,1:tbd,1:tbd) :: ym1, yn1, yn2
	!	REAL, DIMENSION(1:ir,1:lpx) :: sc0, sc3, sd0, sd3
		REAL :: dn1dt, dm1dt, dn2dt, dm2dt
		
		!
		REAL :: dtlt, pptrain
		INTEGER :: i, j, k, l, icolflg, im1, in1, im2, in2, jmin, jmax
		REAL :: tau, eta_n, eta_d, eta_0, eta_a, rst, r0
		REAL :: an0, an1, an3, an4, cnfrac, aa, cn1, rcl, anc, rr, anr, rg1, rg2, ssat
		REAL :: est, pres, rv, dndt_col, wvel, tk
		REAL :: dqcr, dth, dtk, qvnew, thnew, dncr
		
		! For debugging
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme) :: tn0, tn1, tn2, tn3, tn4, tn5, tn6
		REAL :: xcp, ycp, ptb
	!	INTEGER :: ird, jrd
	!	REAL :: randn
	!	INTEGER, DIMENSION(ids:ide) :: irad
	!	INTEGER, DIMENSION(jds:jde) :: jrad
		
		! Used for initialization and restart to load lookup tables, etc.
		INTEGER, SAVE :: ncall = 0
		
#ifdef WRFSTAT
		! Tak Yamaguchi 2012/02 - Temporal array to hold output variables for module_statistics.F
		REAL :: qcr_old
		REAL, DIMENSION(ims:ime,kms:kme,jms:jme) :: cond3d, evap3d, prec3d
		! Initialize array
		cond3d(its:ite,kts:kte,jts:jte) = 0.   ! condensation rate
		evap3d(its:ite,kts:kte,jts:jte) = 0.   ! eaporation rate
		prec3d(its:ite,kts:kte,jts:jte) = 0.   ! precipitation rate
#endif /*WRFSTAT*/
		
#ifdef WRFLES
		! Initialization of parameters specified in /Path2WRF/frame/module_wrfles.F
		IF ( ncall == 0) THEN   ! ncall=0 at the intial start and restart run
			nccn = nccn_feingold2m
			dg1  = dg_ccn_feingold2m
			sg1  = sg_ccn_feingold2m
			sig1 = sg_cloud_feingold2m
			sig2 = sg_rain_feingold2m
			drzflg = drzflg_feingold2m
			colflg = colflg_feingold2m
			drzst  = drzst_feingold2m
			srcccn_startstep_feingold2m = INT(srcccn_starttime_feingold2m / dt_in)
			
			! Initialization of bimodal distribution
			bimodal = bimodal_feingold2m
			IF ( bimodal ) THEN
				nccn2 = nccn2_feingold2m
				dg2   = dg2_ccn_feingold2m
				sg2   = sg2_ccn_feingold2m
				nccn1_weight = nccn / ( nccn + nccn2 )
				nccn = nccn + nccn2
			ENDIF
		ENDIF
#endif /*WRFLES*/

		! Time step (s)
		dtlt = dt_in
	!	PRINT *,'--------dt, timestep: ', dtlt, itimestep
		
		! 2012/01 Tak Yamaguchi
		! The following variables are constant throughout simulation since sig1 and sig2 are constant.
		! Moved these in the initialization block below.
		! Also "ALOG" was changed to "LOG" since "ALOG" is obsolete.
	!	pval = ALOG( sig1 )
	!	qval = ALOG( sig2 )
	!	expval = EXP( 4.5 * pval * pval )
	!	exqval = EXP( 4.5 * qval * qval )
	!	expvali = 1. / expval
	!	exqvali = 1. / exqval
	!	exp1h = EXP( 0.5 * pval * pval )
	!	exq1h = EXP( 0.5 * qval * qval )
	!	exq15 = EXP( 1.5 * qval * qval )
		
		! Initialize variables on first call
		IF ( ncall == 0 ) THEN   ! for the intial start or restart run
			pval = LOG( sig1 )
			qval = LOG( sig2 )
			expval = EXP( 4.5 * pval * pval )
			exqval = EXP( 4.5 * qval * qval )
			expvali = 1. / expval
			exqvali = 1. / exqval
			exp1h = EXP( 0.5 * pval * pval )
			exq1h = EXP( 0.5 * qval * qval )
			exq15 = EXP( 1.5 * qval * qval )
			
			! Lookup table
			OPEN (12, FILE='newcoll_1', STATUS='OLD')
			OPEN (13, FILE='newcoll_2', STATUS='OLD')
			OPEN (30, FILE='sc12_0', STATUS='OLD')
			OPEN (31, FILE='sc12_3', STATUS='OLD')
			OPEN (32, FILE='sd12_0', STATUS='OLD')
			OPEN (33, FILE='sd12_3', STATUS='OLD')
		!	CALL wrf_debug(150, 'READING MICROPHYSICS LOOKUP TABLES ... ')
		!	CALL DATA35(alp, bet, reyn, ym1, yn1, yn2, yym1, yyn1, yyn2, sc0, sc3, sd0, sd3)
			CALL DATA35()
		ENDIF
		
		
		! 2012/01 Tak Yamaguchi
		! Move initialization of aerosol concentration from inner loop to reduce one IF statement
		! 2011/03 Tak Yamaguchi
		! ncall=0 at restart so using ncall=0 in if statement will cause reinitialization on restart!
	!	IF ( ncall == 0 .OR. itimestep <= 1 ) nca(its:ite,kts:kte,jts:jte) = nccn  ! #/mg
		! Temporally remove ncall=0. Need solution for nesting.
		IF ( itimestep <= 1 ) nca(its:ite,kts:kte,jts:jte) = nccn   ! #/mg
		
	!	CALL azero(nzp,vctr9)
	!	CALL azero(nzp*nxp*nyp,ssat)
		ssat = 0.   ! initial value of zero
		
		
	!!	! Generate a random column cluster in the innner 1/4 domain
	!!	IF (itimestep.EQ.2400) then
	!!		DO ird= ids, ide
	!!			CALL random_number(randn)
	!!			irad(ird)=nint(randn*(ide-ids)/2.)+(ide-ids)/4
	!!		ENDDO
	!!		DO jrd= jds, jde
	!!			CALL random_number(randn)
	!!			jrad(jrd)=nint(randn*(jde-jds)/2.)+(jde-jds)/4
	!!		ENDDO
	!!	ENDIF
		
		
		! Begin loop over array space
		microphysics_j_loop: DO j = jts, jte
			microphysics_i_loop: DO i = its, ite
			
				opdt(i,j) = 0.   ! initialize optical depth
				DO k = 1, kte-1
					dzt(k) = 1. / ( z(i,k+1,j) - z(i,k,j) )
				ENDDO
				dzt(kte) = 1. / ( z(i,kte,j) - z(i,kte-1,j) )
				
				microphysics_k_loop: DO k = kts, kte
				
					an1 = 0.
					rcl = 0.
					anc = 0.
					rr  = 0.
					anr = 0.
					dqcr = 0.
					dncr = 0.   ! adding ccn due to evaporation
					
					
			! 2012/01 Tak Yamaguhi - Moved outside of the loop (See above)
			!		! 2011/03 Tak Yamaguchi - Change IF statements, no nesting
			!		! Initialization
			!		IF (itimestep <= 1) nca(i,k,j) = nccn   ! #/mg
			!		
			!	!!	! The regular initialization of CCN
			!	!!	IF (itimestep < 1000) THEN ! to prevent re-initialization of ccn at the begining
			!	!!	                            ! of restart
			!	!!		IF ( ncall == 0 .OR. itimestep <= 1) then   ! for each nested domain
			!	!!			nca(i,k,j) = nccn   ! initialize the CCN, #/mg
			!	!!		!	nca(i,k,j)=nccn+i/ccndx*ccninc   ! linearly increase
			!	!!		ENDIF
			!	!!	ENDIF
					
					
#ifdef WRFLES
! Tak Yamaguchi 2011/04 Add CCN source. The unit of srcccn_feingold2m (srcccn_qvmin_feingold2m)
!                       has been converted to #/mg/second (kg/kg) in the subroutine read_prm in
!                       module_wrfles.F.
					IF ( srcccn_add_feingold2m ) THEN
						IF ( ( itimestep >= srcccn_startstep_feingold2m ) &
							.AND. ( qv(i,k,j) >= srcccn_qvmin_feingold2m ) ) THEN
							nca(i,k,j) = nca(i,k,j) + dtlt * srcccn_feingold2m
						ENDIF
					ENDIF
#else
					! Add a source to CCN
				!!	IF ((itimestep >= 3600).AND.(qv(i,k,j) > 3.E-3)) then   ! after 6 hours
				!!		nca(i,k,j) = nca(i,k,j) + 0.2E-3 * dtlt
				!!	ENDIF
#endif /*WRFLES*/

					!
					tn0(i,k,j) = 0.
					tn1(i,k,j) = 0.
					tn2(i,k,j) = 0.
					tn3(i,k,j) = 0.
					tn4(i,k,j) = 0.
					tn5(i,k,j) = 0.
					tn6(i,k,j) = 0.
					
					
					! If a negative value is present, zero both number and mass arrays.
					IF ( MIN( qc(i,k,j), ncc(i,k,j) ) <= 0. ) THEN
						qc(i,k,j)  = 0.
						ncc(i,k,j) = 0.
					ENDIF
					IF ( MIN( qr(i,k,j), ncr(i,k,j) ) <= 0. ) THEN
						qr(i,k,j)  = 0.
						ncr(i,k,j) = 0.
					ENDIF
					
					! 2012/01 Tak Yamaguchi
					! "Factor of 0.001 converts from #/kg to #/g" <== This is for the uncommented lines.
					! The correct comment is "Factor of 1E3 converts from #/mg to #/g"
					
					! 2012/01 Tak Yamaguchi: What is this comment for?
					! Solve for total numb and fill working arrays according to balanced dynamic arrays.
					
				!	ankd(1) = ncc(i,k,j) * 0.001   ! ncc in #/kg
					ankd(1) = ncc(i,k,j) * 1.E3   ! ncc in #/mg ==> ankd(1) in #/g
					amkd(1) = qc(i,k,j)
				!	ankd(2) = ncr(i,k,j) * 0.001
					ankd(2) = ncr(i,k,j) * 1.E3   ! ncr in #/mg ==> ankd(2) in #/g
					amkd(2) = qr(i,k,j)
					an1 = ankd(1) + ankd(2)
				!	cn1 = MIN( nca(i,k,j), nccn ) * 1.E3   ! CCN #, in #/g
					cn1 = nca(i,k,j) * 1.E3   ! CCN #, nca in #/mg ==> cn1 in #/g
					
					
					! Fill scalars with appropriate thermodynamic quantity at the given grid point...
					! Scratch arrays are filled in thermot (prior to the call to this current subroutine)
					tk = th(i,k,j) * pii(i,k,j)
					pres = p(i,k,j)
					rv = qv(i,k,j)
				!	wvel = w(i,k,j)
					r0 = rho(i,k,j) * 1.E-3   ! kg/m^3 to g/cm^3
					dnk(k) = rho(i,k,j)   ! rho in the vertical column, kg/m^3
					dzm(k) = 1. / dz8w(i,k,j)
					
					tn0(i,k,j) = ankd(1) * r0
					tn1(i,k,j) = an1 * r0
					
					est = es0(tk)   ! *10 for cgs; H. Wang got rid of 10
					rst = 0.62197 * est / ( pres - est )
					eta_d = rv - rst
					
					eta_0 = eta0(i,k,j)   ! eta_0 is eta_d advected with drops in last timestep,
					                      ! to compute the dynamical effect on ss
					
					
					! 2011/03 Tak Yamaguchi - A bug fixed for no-nesting case
					! ncall=0 at restart so using ncall=0 in if statement will cause reinitialization!
				!!	IF ( ncall == 0 .OR. itimestep <= 1 ) eta_0 = eta_d   ! true at restart ncall=0
					! Temporally remove ncall=0. Need a solution for nesting.
					IF ( itimestep <= 1 ) eta_0 = eta_d
					
					
					! Zero drop bins with fewer than 1E-2 drops/g = 1E-5 drops/cc.
					! Then call subroutine to get various measures of excess vapor (eta)
					anc = ankd(1)
					anr = ankd(2)
					rcl = amkd(1)
					rr  = amkd(2)
					rg1 = rg( anc, rcl, expvali )
					rg2 = rg( anr, rr, exqvali )
					
					
					! Calculate optical depth
					opd1 = opd( anc, rg1, pval, dz8w(i,k,j), r0 )
					opd2 = opd( anr, rg2, qval, dz8w(i,k,j), r0 )
					opdt(i,j) = opdt(i,j) + opd1 + opd2
					
					
					!
					CALL GET_FORCING( rst, tk, r0, eta_0, eta_d, dtlt, tau, eta_n, eta_a, rg1, rg2, &
					                  anc, anr, rr )
					
					
				!	ssat = ( eta_a / rst ) * 100.
				!	write(*,*) eta_a, rv, rst, ssat
					
					
					! Adjust cloud and rain
					IF ( rg1 > rad(ithresh+1) ) THEN
						anr = anr + anc
						rr  = rr + rcl
						anc = 0.
						rcl = 0.
					ENDIF
					IF ( rg2 < rad(ithresh+1) ) THEN
						anc = anr + anc
						rcl = rr + rcl
						anr = 0.
						rr  = 0.
					ENDIF
					
					
#ifdef WRFSTAT
					! Tak Yamaguchi 2012/02 - Store qc+qr value
					qcr_old = rcl + rr ! = amkd(1) + amkd(2) = qc(i,k,j) + qr(i,k,j)
#endif /*WRFSTAT*/
					
					
					! Do evaporation/condensation, followed by activation
				!	IF ( anr+anc > 1.E-6 .AND. itimestep >= drzst/dtlt ) THEN
					IF ( anr+anc > 1.E-6 ) THEN
						IF ( tau > 0. ) THEN
							CALL cond35( tau, rg1, anc, rcl, rg2, anr, rr, dqcr )
						ELSEIF ( tau < 0 ) THEN
							CALL evap35( tau, rg1, anc, rcl, rg2, anr, rr, dqcr, dncr )
						ENDIF
					ENDIF
					
					
					! Update water vapor and temperature after cond./evap.
					qv(i,k,j) = MAX( qv(i,k,j)-dqcr, 0. )
					CALL LHONTH( dqcr, rv, tk, dtk )
					dth = dtk / pii(i,k,j)
					th(i,k,j) = th(i,k,j) + dth
				!	PRINT*, 'dth: ', dth
					
					
					amk(1) = rcl
					amk(2) = rr
					ank(1) = anc
					ank(2) = anr
					
				!	tn2(i,k,j) = (anc+anr)*r0
					
				! H. Wang	IF (AN1.EQ.0) ETA_A=(ETA_0+ETA_N)*.5
					eta_a = ( eta_0 + eta_n ) * 0.5
					an0 = 0.
					
					ssat = ( eta_a / rst ) * 100.
					
					
					! Print out for debugging
					
					
					tn2(i,k,j) = eta_0
					tn3(i,k,j) = eta_d
					ssr(i,k,j) = ssat
					
					IF ( eta_a > 0. ) THEN
						! Effective addition of CCN in the amount equivalent to the depletion
						! due to collection
#ifdef WRFLES
						! 2012/01 Tak Yamaguchi: Option to use a bimodal lognomal distribution
						! Assume the ratio between nccn and nccn2 is time-independent.
						! This also implicitly assumes that the regeneration of CCN is partitioned into
						! these two distributions with the constant ratio.
						IF ( bimodal ) THEN
							! Activated CCN number for the distribution with smaller geometric mean
							an0 = MAX( 0., ACTIVATE( tk, ssat, dg1, sg1) * cn1 * nccn1_weight )
							! Add activated CCN number for the distribution with larger geometric mean
							an0 = an0 + MAX( 0., ACTIVATE( tk, ssat, dg2, sg2) * cn1 * (1.-nccn1_weight) )
						ELSE
							an0 = MAX( 0., ACTIVATE( tk, ssat, dg1, sg1) * cn1 )
						ENDIF
#else
						an0 = MAX( 0., ACTIVATE( tk, ssat, dg1, sg1) * cn1 )
#endif /*WRFLES*/
						IF ( an0 > 0. ) THEN
							cn1 = cn1 - an0   ! depletion of CCN by activation
							ank(1) = ank(1) + an0
							!========== CAUTION =============================================
							! 2012/02 Tak Yamaguchi: x(5) is too large for polluted case
						!	dqcr = an0 * x(5)
						!	amk(1) = amk(1) + dqcr
						!	amk(1) = MAX( ank(1) * x(5), amk(1) )
							dqcr = an0 * x(1)
							amk(1) = amk(1) + dqcr
							amk(1) = MAX( ank(1) * x(1), amk(1) )
							!================================================================
							qv(i,k,j) = MAX( qv(i,k,j) - dqcr, 0. )   ! update qv
							CALL LHONTH( dqcr, rv, tk, dtk )
							dth = dtk / pii(i,k,j)
							th(i,k,j) = th(i,k,j) + dth   ! update th
						ENDIF
					ENDIF
					
					
#ifdef WRFSTAT
					! Tak Yamaguchi 2012/02 - Store condensation and evaporation. Unit conversion later.
					dqcr = amk(1) + amk(2) - qcr_old   ! amk(1)+amk(2)=rcl+rr after cond/evap & activ
					IF ( dqcr > 0 ) THEN
						cond3d(i,k,j) = dqcr   ! condensation over dt
					ELSE
						evap3d(i,k,j) = dqcr   ! evaporation over dt
					ENDIF
#endif /*WRFSTAT*/
					
					
					! Call to stochastic collection, if appropriate... also count number of collected
					! drops and store in an3
					an3 = 0.
					dndt_col = 0.
				!	IF ( an1 > 0.01 .AND. colflg ) THEN
					IF ( an1 > 0.01 .AND. colflg .AND. itimestep >= drzst/dtlt ) THEN
					
						! Use look-up table
						dn1dt = 0.
						dm1dt = 0.
						dn2dt = 0.
						dm2dt = 0.
						im1 = 0
						in1 = 0
						im2 = 0
						in2 = 0
						
						ank(1) = MAX( ank(1), 0. )
						amk(1) = MAX( amk(1), 0. )
						ank(2) = MAX( ank(2), 0. )
						amk(2) = MAX( amk(2), 0. )
						
						! Multiply by r0 to get the right indices
					!	write(*,*) 'nc,nr----before', ank(1)/1.E3, ank(2)/1.E3
						
						icolflg = 0
						IF ( ank(1) > mincn .AND. amk(1) > mincm &
						     .AND. ank(2) > minrn .AND. amk(2) > minrm ) THEN
							im1 = amk(1) * r0 / wgtcm   ! for lagr1nr_11
							in1 = ank(1) * r0 / wgtcn   ! for lagr1nr_11
							im2 = ( amk(2) * r0 - wgtrm1 ) / wgtrm2 + 1
							in2 = ( ank(2) * r0 - wgtrn1 ) / wgtrn2 + 1
							icolflg = 1
						ENDIF
						
						IF ( ank(1) > mincn .AND. amk(1) > mincm &
						     .AND. ank(2) <= minrn .AND. amk(2) <= minrm ) THEN
							im1 = amk(1) * r0 / wgtcm ! +1  should not plus 1???? H. Wang
							in1 = ank(1) * r0 / wgtcn
							icolflg = 2
						ENDIF
						
					!	write(*,*) 'indices: ',im1,in1,im2,in2
						
						! 2011/03 Tak Yamaguchi
						! Change the "GO TO" statement to "IF"&"CASE" since it is obsolete.
					!	IF ( icolflg == 0 ) GO TO 888
						IF ( icolflg /= 0 ) THEN
							!
							im1 = MIN( tbd, im1 )
							in1 = MIN( tbd, in1 )
							im2 = MIN( tbd, im2 )
							in2 = MIN( tbd, in2 )
							
							!
							im1 = MAX( 1, im1 )
							in1 = MAX( 1, in1 )
							im2 = MAX( 1, im2 )
							in2 = MAX( 1, in2 )
							
							SELECT CASE(icolflg)
							CASE(1)
								dm1dt = ym1( im1, in1, im2, in2 )
								dm1dt = MIN( dm1dt, 0. )
								dn1dt = yn1( im1, in1, im2, in2 )
								dn1dt = MIN( dn1dt, 0. )
								dm2dt = - dm1dt
								dn2dt = yn2( im1, in1, im2, in2 )
							CASE(2)
								dm1dt = yym1( im1, in1 )
								dm1dt = MIN( dm1dt, 0. )
								dn1dt = yyn1( im1, in1 )
								dn1dt = MIN( dn1dt, 0. )
								dm2dt = - dm1dt
								dn2dt = yyn2( im1, in1 )
								dn2dt = MAX( dn2dt, 0. )
							CASE DEFAULT
							
							END SELECT
							
							! tendencies are in units of g/cm3/sec, or #/cm^3/sec
							
							ank(1) = ank(1) + dn1dt / r0 * dtlt
							amk(1) = amk(1) + dm1dt / r0 * dtlt
							ank(2) = ank(2) + dn2dt / r0 * dtlt
							amk(2) = amk(2) + dm2dt / r0 * dtlt
							
							ank(1) = MAX( ank(1), 0. )
							amk(1) = MAX( amk(1), 0. )
							ank(2) = MAX( ank(2), 0. )
							amk(2) = MAX( amk(2), 0. )
						ENDIF
! 2011/03 Tak Yamaguchi - uncomment
! 888     continue
							
						DO l = 1, ln
							ank(l) = MAX( 0., ank(l) )
							amk(l) = MAX( 0., amk(l) )
							an3 = an3 + ank(l)
						ENDDO
						an3 = an1 + an0 - an3
						dndt_col = an3 * r0 / dtlt   ! #/cc/sec
					ENDIF   ! IF collection
					
					
					! Update microphysical arrays. when drizzle is to be called the update is temporary
					! and data is put in a column vector for drizzle calculations
					ncc(i,k,j) = ank(1) * 1.E-3   ! ncc in #/mg
					ncr(i,k,j) = ank(2) * 1.E-3
					qc(i,k,j)  = amk(1)
					qr(i,k,j)  = amk(2)
					nca(i,k,j) = ( cn1 + dncr ) * 1.E-3   ! update CCN, pass to solver, #/mg
					
					IF ( drzflg == 1 .AND. itimestep >= drzst/dtlt ) THEN
						rg1 = rg( ank(1), amk(1), expvali )
						rg2 = rg( ank(2), amk(2), exqvali )
						rg1 = MAX( 0., MIN( rg1, rad(ithresh+1) ) )
						rg2 = MAX( 0., MIN( rg2, rad(lpx) ) )
						
					!	CALL nnn( cn, cm, rg1, rg2, ank(1), amk(1), ank(2), amk(2), sc0, sc3, sd0, sd3 )
						CALL nnn( cn, cm, rg1, rg2, ank(1), amk(1), ank(2), amk(2) )
						
						DO l = 1, lpx
							ank2(l,k) = MAX( 0., cn(l) )
							amk2(l,k) = MAX( 0., cm(l) )
						!	tn5(i,k,j) = tn5(i,k,j) + ank2(l,k) * r0
						ENDDO
						vctr1(k) = 0.
					ENDIF
					
					IF ( drzflg == 2 ) THEN
						DO l = 1, ln
							ank3(l,k) = MAX( 0., ank(l) )
							amk3(l,k) = MAX( 0., amk(l) )
						!	tn5(i,k,j) = tn5(i,k,j) + ank2(l,k) * r0
						ENDDO
						vctr1(k)=0.
					ENDIF
					
					
					! Update eta0 to consider dynamical effect on ss in next timestep
					tk = th(i,k,j) * pii(i,k,j)
					est = es0(tk)
					rst = 0.62197 * est / ( pres - est )
					eta0(i,k,j) = qv(i,k,j) - rst
					tn5(i,k,j) = eta0(i,k,j)
					
				ENDDO microphysics_k_loop
				
				
				! ---- No drizzle for parcel model -----
				! Call sedim which only operates on columns.
			!	IF ( drzflg >= 1 ) THEN
				IF ( drzflg >= 1 .AND. itimestep >= drzst/dtlt ) THEN
					IF ( drzflg == 1 ) THEN
					!	CALL SEDIM4( kts, kte, vctr1, alp, bet, vctr2, dtlt, dzm, dzt, &
						CALL SEDIM4( kts, kte, vctr1, vctr2, dtlt, dzm, dzt, dnk, amk2, ank2, vctr3, &
						             vctr4, rrate, vctr6 )
					ELSE IF ( drzflg == 2 ) THEN
						CALL SEDIM2( kts, kte, vctr1, dtlt, dzm, dzt, dnk, amk3, ank3, rrate, vctr6 )
					ENDIF
					
					rainncv(i,j) = vctr6 * 1000.   ! precip, m to mm
					rainnc(i,j)  = rainnc(i,j) + vctr6 * 1000.
					
					!
					sedimentation_k_loop: DO k = kts, kte-1
						rainrtk(i,k,j) = rrate(k) * 1000. * 24. * 3600.   ! from m s-1 to mm day-1
#ifdef WRFSTAT
						! Tak Yamaguchi 2012/02 - Store rainrate. Unit conversion later.
						prec3d(i,k,j) = rrate(k)
#endif /*WRFSTAT*/
						IF ( drzflg == 1 ) THEN
							ank(1) = 0.
							amk(1) = 0.
							DO l = 1, ithresh   ! H. Wang changed ithresh to ithresh+1
								ank(1) = ank(1) + ank2(l,k)
								amk(1) = amk(1) + amk2(l,k)
							ENDDO
							ank(2) = 0.
							amk(2) = 0.
							DO l = ithresh+1, lpx
								ank(2) = ank(2) + ank2(l,k)
								amk(2) = amk(2) + amk2(l,k)
							ENDDO
						ELSE IF ( drzflg == 2 ) THEN
							DO l = 1, ln
								ank(L) = ank3(l,k)
								amk(L) = amk3(l,k)
							ENDDO
						ENDIF
						
						! Update cloud and rain mass and # conc.
						an4 = ncc(i,k,j) + ncr(i,k,j) - ( ank(1) + ank(2) ) * 1000.   ! total depleted #
						ncc(i,k,j) = ank(1) * 1.E-3   ! ncc in #/mg
						ncr(i,k,j) = ank(2) * 1.E-3
						qc(i,k,j) = amk(1)
						qr(i,k,j) = amk(2)
						
						tn6(i,k,j) = ( ank(1) + ank(2) ) * r0
					ENDDO sedimentation_k_loop
				ENDIF   ! sedimentation
				
			ENDDO microphysics_i_loop
		ENDDO microphysics_j_loop
		
		ncall = ncall + 1
		
#ifdef WRFSTAT
		! Tak Yamaguchi 2012/02 - Collect data for module_statistics.F
		IF ( doavgprecflux ) THEN
			DO j = jts, jte
				DO k = kts, kte
					DO i = its, ite
						precipl_eta(i,k,j) = precipl_eta(i,k,j) + prec3d(i,k,j) * 1000.   ! m/s ==> mm/s
					ENDDO
				ENDDO
			ENDDO
		ENDIF
		IF ( docollect_data ) THEN
			DO i = its, ite
				DO j = jts, jte
					DO k = kts, kte
						cond_rate_eta(i,k,j) =   cond3d(i,k,j) / dtlt   ! kg/kg ==> kg/kg/s
						evap_rate_eta(i,k,j) = - evap3d(i,k,j) / dtlt   ! positively defined
					ENDDO
				ENDDO
			ENDDO
			IF ( .NOT.doavgprecflux ) THEN
				DO i = its, ite
					DO j = jts, jte
						DO k = kts, kte
							precipl_eta(i,k,j) = prec3d(i,k,j) * 1000.   ! m/s ==> mm/s
						ENDDO
					ENDDO
				ENDDO
			ENDIF
		ENDIF
#endif /*WRFSTAT*/
		
	END SUBROUTINE mp_2m_feingold
	
	!=====================================================================================

!
!------------------------------------------------------------------------------
! GET_FORCING:   This routine gets the integrated forcing for cnd/evp 
! in accordance with the analytic/numerical method.  It also returns
! various measures of the excess vapor amount for use in nucleation 
! and in diagnostic tests.
! 
	SUBROUTINE GET_FORCING(RST,TEMP,R0,ETA_0,ETA_D,DT  &
	                      ,TAU,ETA_N,ETA_A              &
	                      ,rg1,rg2,an1,an2,rr)
	
		IMPLICIT NONE
		
		REAL ::   TEMP,RST,R0, DT,TAU,ETA_0,ETA_D,ETA_A,ETA_N,ETA_DT
		REAL ::   CHI,SDDM,ZZZ
		REAL ::   rg1,rg2,an1,an2,rr, &
		           fac1,fac2, cont1,cont2
		
		LOGICAL, PARAMETER :: vflag = .true.
		REAL, PARAMETER ::   EPS = 1.E-20
		REAL, PARAMETER ::   L0  = 6.5E-04
		REAL, PARAMETER ::   xmult  = 1.611991954       ! (4/3 pi)^0.333333
		
!		REAL, DIMENSION(1:LPA1) :: x,rad
!		EXTERNAL APT,CPT,FLHV,FPHI1
		
!		DATA XI,EX1,EX2/0.9840654,0.3333333,0.6666667/
		
		REAL :: etav,um,pres,dv,sc,c2,fbar
		REAL :: xmin,xmax,rlv,fsw1,fsw2,fsw
		
		IF (vflag.AND.rg2.GT.0..AND.rr.GT.1.e-06.AND.  &
			eta_0.LT.0..AND.eta_d.LT.0.) THEN
			etav=(1.718+0.0049*(temp-273.15))*1.e-04   !dynamic viscosity
			um=etav/r0    !kinematic viscosity
			pres=r0*2.8704e+06*temp
			dv=0.211*(1013250./pres)*(temp/273.15)**1.94    !Diffusion coeff (cgs)
			sc=um/(dv)   !Schmidt number
			c2=0.3430712*sc**ex1*sqrt(4.962e+03/um)
			fbar=0.78 + c2*xmult*rg2*exq15
			fbar=min(max(fbar,1.),3.86)
		ELSE
			fbar=1.
		ENDIF
		
! H. Wang changed the following to calculate fac1 and fac2 instead of setting to 1.
		fac1=0.
		fac2=0.
		cont1=0.
		cont2=0.
		SDDM=0.
		
!!		IF (rg1.GT.rad(1)) THEN
!			xmin=rad(1)
!			xmax=rad(ithresh)
!			CALL trunc(xmin,xmax,rg1,sig1,pval,fac1,1.)
			fac1=1.
			cont1=an1*rg1*exp1h*fac1*xmult
!!		ELSE
!!			cont1=0.
!!		ENDIF
		
!!		IF (rg2.GT.rad(ithresh)) THEN
!			xmin=rad(ithresh)
!			xmax=rad(25)
!			CALL trunc(xmin,xmax,rg2,sig2,qval,fac2,1.)
			fac2=1.
			cont2=an2*rg2*exq1h*fac2*xmult*fbar
!!		ELSE
!!			cont2=0.
!!		ENDIF
!		WRITE(*,*)'fac1,2:',fac1,fac2
		sddm=cont1+cont2
!		sddm=sddm*80./126.
		ZZZ=CPT(TEMP,RST)
!
		IF (SDDM.LT.1.E-30) THEN
			TAU=0.
			ETA_DT=0.
			ETA_N=ETA_D
			RETURN
		ENDIF
		
!--------------- H. Wang adopted the following from the bin code
!		rlv=FLHV(temp)
!		dyn=wvel*FPHI1(temp,rv/rst,rlv)
! dyn should be multiplied by 100 because wvel should be in cm/s if the cgs functions are used,
! It also needs to be multiplied by rst because this is d eta/dt = d/dt (rv-rst) and not ds/dt
!		dyn=dyn*100.*rst
!---------------
! Eta at the next timestep and the integral of eta are estimated
!
!		WRITE(*,*) 'APT, CPT: ',APT(temp,rst),zzz
		CHI   = -XI*ZZZ*SDDM*APT(TEMP,RST)
		ETA_N = ETA_NEW(ETA_0,ETA_D,CHI,DT)
		ETA_DT= ETA_INT(ETA_0,ETA_D,CHI,DT)
!		WRITE(*,*) (eta_d-eta_0)/dt,dyn,chi,sddm
!
! Here the integral for eta over the timestep is bounded by its values
! at the endpoints, erroneous values of eta_dt sometimes are computed 
! for vanishingly small amounts of mass in the presence of evaporation,
! here the limiting eliminates erroneous values
!
		FSW1 = MIN(1.,MAX((MIN(ETA_0,ETA_N)*DT-ETA_DT)*1.E35,0.))
		FSW2 = MIN(1.,MAX((ETA_DT-MAX(ETA_0,ETA_N)*DT)*1.E35,0.))
		FSW = MAX(FSW1,FSW2)
		ETA_DT=ETA_DT*(1.-FSW) + FSW*(ETA_0+ETA_N)*DT*0.5
!
! Average value of eta over the timestep is computed for diagnostics
! as is the estimated change in mass
!
		ETA_A = ETA_DT/DT
!		IF (eta_a.GT.0..AND.eta_d.LT.0.) PRINT*,'eta_a,d',100*eta_a/rst,100*eta_0/rst
		TAU   = EX2*ZZZ*ETA_DT
!		WRITE(*,*) 'TAU', TAU
		RETURN
	END SUBROUTINE GET_FORCING
!
!------------------------------------------------------------------------------
! ETA_INT is the integral of the difference between the vapor mixing
! ratio and the saturation vapor mixing ratio over the timestep.  It
! is calculated in a manner consistent with the dynamical and micro-
! physical forcings via the analytical/numerical method.
!
! X ......generic variable for designating difference between the
!         vapor mixing ratio and the saturation vapor mixing ratio
! X0 .....value of X at the end of the previous timestep
! XD .....value of X after dynamics
! C ......function related to dynamic forcing for an excess vapor
!         equation of the form dX/dt = D + CX
! DT .... length of the timestep
!
	REAL FUNCTION ETA_INT(X0,XD,C,DT)
		IMPLICIT NONE
		REAL ::  X0,XD,DT,C,CI,D
		D=(XD-X0)/DT  ! H.Wang
		CI=1./C
		ETA_INT=CI*((D*CI+X0)*(EXP(C*DT)-1.) - D*DT)
!		ETA_INT=MIN(ETA_INT,MAX(0.,XD*DT))
		RETURN
	END FUNCTION ETA_INT
!
!------------------------------------------------------------------------------
! ETA_NEW is the estimated value of ETA at the end of the timestep 
! given the forcings.  It is calculated in a manner consistent with 
! the dynamical and micro-physical forcings via the 
! analytical/numerical method.
!
! X ......generic variable for designating difference between the
!         vapor mixing ratio and the saturation vapor mixing ratio
! X0 .....value of X at the end of the previous timestep
! XD .....value of X after dynamics
! C ......function related to dynamic forcing for an excess vapor
!         equation of the form dX/dt = D + CX
! DT .... length of the timestep
!
	REAL FUNCTION ETA_NEW(X0,XD,C,DT)
		IMPLICIT NONE
		REAL :: X0,XD,DT,C,CI,D
		D=(XD-X0)/DT
		CI=1./C
		ETA_NEW=(D*CI+X0)*EXP(C*DT) - D*CI
		RETURN
	END FUNCTION ETA_NEW
!
!------------------------------------------------------------------------------
! APT is the thermodynamic function for evaluation of condensation and 
! evaporation in Tzivion et. al. JAS 46:3312 (appendix B), here it is
! reformulated to have a simple form using the values for
! latent heat, the gas constant for water vapor and the specific heat
! of water vapor consistent with rest of model.  
!
! RST ...... saturation vapor mixing ratio
! T ........ temperature in Kelvin
! C ........ constant = L^2/(Cp*Rv)
! ALVL ..... latent heat of vaporization (from rconstants.h)
! CP ....... specific heat at constant pressure (from rconstants.h)
!
	REAL FUNCTION APT(T,RST)
		IMPLICIT NONE
!		INCLUDE 'rconstants.h'
		REAL :: RST,T
		REAL, PARAMETER :: cp=1004.,alvl=2.5e6, rvm=461.5
		REAL, PARAMETER :: C=alvl*alvl/(CP*rvm)
		APT=1.+C*RST/(T*T)
		RETURN
	END FUNCTION APT
!
!------------------------------------------------------------------------------
! CPT is the normalized growth rate for a drop: 
!        dm/dt = [CPT*m^(1/3)]*(R_v - R_s)
! This is similar to what is defined by Soong but formulated for mixing 
! mixing ratios.  The trailing factor of 10 in the numerator puts CPT
! in CGS units.
!
! Here CPT  =              (4pi*(3/(4pi))**(1/3))*10.
!              ----------------------------------------------
!               1/(Dv*Rho) + Rst*(L/(Ka*T))*((L/(Rv*T)) - 1.)
!
! From Rogers and Yau we can write: Dv*Rho = 1.64*mu and Ka = 1404*mu
! where mu is the kinematic viscosity of air, which can be expressed
! in a temperature dependent form: mu=[6.76E-3/(T+120.)](T/273.)**(3/2)
! so that CPT may be expressed interms of mu:
!
! Here CPT  =            mu*(4pi*(3/(4pi))**(1/3))*10.
!              ----------------------------------------------
!               1/1.64 + Rst*(L/(1404.*T))*((L/(Rv*T)) - 1.)
!
! where Rv above is taken as 461.5: 1/Rv = 2.1669E-3
!
! RST ...... saturation vapor mixing ratio
! X0 ....... T/273. (used in temperature representation of mu)
! X1 ....... mu*(4pi*(3/(4pi))**(1/3))*10.
! X2 ....... L/T (L is latent heat of vaporization from rconstants.h)
!
	REAL FUNCTION CPT(T,RST)
		IMPLICIT NONE
!		INCLUDE 'rconstants.h'
		REAL, PARAMETER :: alvl=2.5e6, rvm=461.5
		REAL, PARAMETER :: C1=1./1.64, C2=1./1404.,C3=1./rvm
		REAL :: X0,X1,X2,T,RST
		X0=3.663E-3*T
		X1=5.2975E-01*X0*SQRT(X0)/(T+120.)
		X2=ALVL/T
		CPT=X1/(C1 + C2*RST*X2*(C3*X2-1.))
		RETURN
	END FUNCTION CPT
!
!------------------------------------------------------------------------------
! FUNCTION CUTLOG:  This function integrates a lognormal character-
! ized by mean diameter dg, and width parameger sg, between the limits
! dmn and dmx.  It returns the ord'th moment of the distribution
! according to eq (17) of Feingold and Levin, Journ of Climate and App.
! Meteor. 25:1346-1363.  
!
! This function is specifically defined to calculate the 
! moments associated with the limits xmn, and xmx.  If xmin is small
! enough to be considered zero or xmax is large enough to be taken as
! infinity, the functions BOTLOG, and TOPLOG should be used as they
! are specifically designed for the truncation of only the bottom or
! top ends of a log normal.  Note:  C0 is the factor (2)^(-1/2)
!
	REAL FUNCTION CUTLOG(DMN,DMX,DG,SG,ORD)
		IMPLICIT NONE
		REAL ::  DMX,DMN,DG,SG,ORD
		DOUBLE PRECISION :: DDMX,DDMN,DDG,DSG,DORD
		DOUBLE PRECISION :: YMX,YMN,EMX,EMN,LNS,C0
#if ( FC == PGI )
		DOUBLE PRECISION :: DERF
#endif
		DATA C0/0.7071067814D0/
		DDMX=DBLE(DMX)
		DDMN=DBLE(DMN)
		DORD=DBLE(ORD)
		DDG=DBLE(DG)
		DSG=DBLE(SG)
		
		LNS=LOG(DSG)
		YMX=(LOG(DDMX/DDG)/LNS - DORD*LNS)*C0
		EMX=DERF(YMX)
		YMN=(LOG(DDMN/DDG)/LNS - DORD*LNS)*C0
		EMN=DERF(YMN)
		CUTLOG=SNGL(DDG**DORD                   &
		           *EXP(.5D0*DORD*DORD*LNS*LNS) &
		           *(EMX-EMN)*.5D0)
		RETURN
	END FUNCTION CUTLOG
!
!------------------------------------------------------------------------------
! BOTLOG: Finds the moments associated with the lower portion of the
! lognormal function specified by the parameters dg and sg
!
	REAL FUNCTION BOTLOG(DMN,DG,SG,ORD)
		IMPLICIT NONE
		REAL :: DMN,DG,SG,ORD
		DOUBLE PRECISION :: C0,LNS,YMN,EMN
#if ( FC == PGI )
		DOUBLE PRECISION :: DERF
#endif
		DATA C0/0.7071067814D0/
		
		LNS = LOG(DBLE(SG))
		YMN = (LOG(DBLE(DMN)/DBLE(DG))/LNS - DBLE(ORD)*LNS)*C0
		EMN = DERF(YMN)
		BOTLOG = SNGL(DBLE(DG)**DBLE(ORD)             &
		             *EXP(.5D0*DBLE(ORD*ORD)*LNS*LNS) &
		             *(EMN+1.D0)*.5D0)
		RETURN
	END FUNCTION BOTLOG
!
!------------------------------------------------------------------------------
! TOPLOG: Finds the moments associated with the upper portion of the
! lognormal function specified by the parameters dg and sg
!
	REAL FUNCTION TOPLOG(DMX,DG,SG,ORD)
		IMPLICIT NONE
		REAL :: DMX,DG,SG,ORD
		DOUBLE PRECISION :: C0,LNS,YMX,EMX
#if ( FC == PGI )
		DOUBLE PRECISION :: DERF
#endif
		DATA C0/0.7071067814D0/
		
		LNS = LOG(DBLE(SG))
		YMX = (LOG(DBLE(DMX)/DBLE(DG))/LNS - DBLE(ORD)*LNS)*C0
		EMX = DERF(YMX)
		TOPLOG = SNGL(DBLE(DG)**DBLE(ORD)              &
		             *EXP(.5D0*DBLE(ORD*ORD)*LNS*LNS)  &
		             *(1.D0-EMX)*.5D0)
		RETURN
	END FUNCTION TOPLOG
!
!------------------------------------------------------------------------------
! ACTIVATE: Computes the fraction of aerosol in a log normal dist.
! activated at a given temperature (K) and supersaturation (%)
! 
	REAL FUNCTION ACTIVATE(TEMP,S,DG,SG)
		IMPLICIT NONE
		REAL, PARAMETER :: BETA = 0.651232
		REAL :: TEMP,S,DG,SG,DCRIT
		REAL :: GAMMA, A
!
! Calculations of critical dry ccn radius associated with equilibrium 
! nucleation.
!
		GAMMA = LOG(S/100.+1.)
		A = (3.298E-05 - 6.7172E-08*(TEMP-273.15))/TEMP
		IF (S.GT.0.) THEN
			DCRIT =2.*((4.*A*A*A)/(27.*GAMMA*GAMMA*BETA))**(0.3333333333)
		ELSE
			DCRIT = 1.e-15
		ENDIF
!
! Calculation of number larger than critical radius
!
		ACTIVATE=TOPLOG(DCRIT,DG,SG,0.)
	END FUNCTION ACTIVATE
!
!------------------------------------------------------------------------------
!  SEDIM: Smolarkiewicz's advection scheme applied to falling drops
!  this is the same as mpdata1 (written by smolarkiewicz) but
!  staggered velocities are implicit based on stagered grid and
!  boundary conditions implicit in specifications of loop limits.
!
!     v(k) .....is the fall velocity in cm/sec
!     vbar(k) ..is the fall velocity normalized by deltaz/deltat
!     amk(l,k) & ank(l,k) .....in units of [g/g] and [#/g], resp.
!     amn(k) ...the average mass in g
!     binpcp ...the precipitation associated with each bin at surf.
!     drtld ....the change in total water due to sedimentation
!
	SUBROUTINE SEDIM4(N0,N1,DRT1D,AMN,DT,DZM,DZT  &
!	SUBROUTINE SEDIM4(N0,N1,DRT1D,ALP,BET,AMN,DT,DZM,DZT  &
	                 ,DNK,AMK,ANK,FM,V,rainrate,ACCPR)
	
		IMPLICIT NONE
		INTEGER, INTENT(IN  ) :: N0,N1
		REAL, DIMENSION(1:LPX, N0:N1) :: ANK, AMK
!		REAL, DIMENSION(1:LPX) :: ALP, BET
		REAL, DIMENSION(N0:N1) :: DRT1D, AMN, DZM, DZT, FM, V, VBAR, FN, rainrate,DNK
		REAL :: BINPCP, DT, ACCPR
		INTEGER :: L, K
!
!  starting loop for sedimentation: initialization of fluxes
!  at top of boundary and specification of order of advection
!  scheme.
!
!		FM(N1-1)=0.
!		FN(N1-1)=0.

! zero initial rainrate
		ACCPR=0.
		DO K=N0,N1
			rainrate(k)=0.
		ENDDO
		
		DO L=1,LPX
			DO K=N0,N1
				IF ((AMK(L,K).GT.0.).AND.(ANK(L,K).GT.0.)) THEN
					AMN(K)=MAX(X(L),MIN(X(L+1),AMK(L,K)/ANK(L,K)))
					V(K)=-ALP(L)*AMN(K)**BET(L)
				ELSE
					AMN(K)=0.
					V(K)=0.
				ENDIF
				VBAR(K)=MAX(0.01*V(K)*DT*DZT(K),-1.)   ! Tak: terminal velocity never gets less than -1
				FM(K)=AMK(L,K)*VBAR(K)
				FN(K)=ANK(L,K)*VBAR(K)
				
				BINPCP=-1.*FM(k)*DNK(k)/DZT(k)  ! temp variable for rain rate
				rainrate(k)=rainrate(k)+BINPCP/(dt*rhowtr)  !grid scale precip at each level, m s-1
			ENDDO
!
			DO K=N0,N1-1
				AMK(L,K)=AMK(L,K)-(FM(K+1)*DNK(k+1)*dzt(k)/(dzt(k+1)*DNK(k))-FM(K))
				ANK(L,K)=ANK(L,K)-(FN(K+1)*DNK(k+1)*dzt(k)/(dzt(k+1)*DNK(k))-FN(K))
				DRT1D(K)=DRT1D(K)-(FM(K+1)*DNK(k+1)*dzt(k)/(dzt(k+1)*DNK(k))-FM(K))
			ENDDO
			AMK(L,N1)=AMK(L,N1)+FM(N1) 
			ANK(L,N1)=ANK(L,N1)+FN(N1) 
			DRT1D(N1)=DRT1D(N1)+FM(N1) 
!
			BINPCP=-1.*FM(1)*DNK(1)/DZT(1)
			ACCPR=ACCPR+BINPCP/rhowtr  !grid scale precup at surface in each timestep
!			PCPRR=PCPRR+BINPCP/Dt
		ENDDO
!
		RETURN
	END SUBROUTINE SEDIM4
!
!------------------------------------------------------------------------------
!--- A new sedimentation scheme that uses bulk terminal velocity instead of bin Vt --
!--  H. Wang 01/2008
	SUBROUTINE SEDIM2(N0,N1,DRT1D,DT,DZM,DZT,DNK,AMK,ANK,rainrate,ACCPR)
		IMPLICIT NONE
		INTEGER, INTENT(IN  ) :: N0,N1
		REAL, DIMENSION(1:LN, N0:N1) :: ANK, AMK , rga
		
! coefficient for terminal velocity, vt
		REAL, DIMENSION(1:LN) :: am, bm              ! vt=-am*M**bm
		REAL, DIMENSION(1:LN) :: ar, br &            ! vt=-ar*R**br
		                        ,expbr,exp3,expbr3,maxr 
		DATA am/0.45795E6,4.962E3/
		DATA bm/0.666667,0.333333/
!		DATA sig/sig1,sig2/
!		DATA exp3/expval,exqval/
		
! VM and VN are mass and number sedimentation velocities for the log-normal distributions
		REAL, DIMENSION(N0:N1) :: DRT1D, DZM, DZT, FM, VM,VN,VBARM, VBARN, FN, rainrate,DNK
		REAL :: BINPCP, DT, ACCPR 
		INTEGER :: L, K
		
! convert coefficients
		DO L=1,LN
			ar(L)=am(L)*(4.*rpi/3.)**bm(L)
			br(L)=3.*bm(L)
		ENDDO
		
		maxr(1)=rad(ithresh+1)   ! to limit the mean radius
		maxr(2)=rad(LPX) 
		exp3(1)=exp(4.5*pval*pval)
		exp3(2)=exp(4.5*qval*qval)
		expbr(1)=exp(br(1)*br(1)*pval*pval/2.)
		expbr(2)=exp(br(2)*br(2)*qval*qval/2.)
		expbr3(1)=exp( (br(1)+3.)*(br(1)+3.)*pval*pval/2. )
		expbr3(2)=exp( (br(2)+3.)*(br(2)+3.)*qval*qval/2. )
		
! get the value of fraction for truncated distribution  
! assume fractions as 1 for now
!		DO K=N0,N1-1
!			CALL trunc(xmin,xmax,rg1,sig1,pval,fac1,1.)
!		ENDDO
		
!  starting loop for sedimentation: initialization of fluxes
!  at top of boundary and specification of order of advection
!  scheme.
!
!		FM(N1-1)=0.
!		FN(N1-1)=0.
! zero initial rainrate
		ACCPR=0.
		DO K=N0,N1
			rainrate(k)=0.
		ENDDO
		
		DO L=1,LN
			DO K=N0,N1
				IF((AMK(L,K).GT.0.).AND.(ANK(L,K).GT.0.))THEN
					rga(L,K)=rg(ank(L,K),amk(L,K),1./exp3(L))
					rga(L,K)=max(0.,min(rga(L,K),maxr(L)))
					VN(K)=-ar(L)*rga(L,K)**br(L)*expbr(L)
					VM(K)=-ar(L)*rga(L,K)**br(L)*expbr3(L)/exp3(L)
				ELSE
					VM(K)=0.
					VN(K)=0.
				ENDIF
				VBARM(K)=MAX(0.01*VM(K)*DT*DZT(K),-1.)
				VBARN(K)=MAX(0.01*VN(K)*DT*DZT(K),-1.)
				FM(K)=AMK(L,K)*VBARM(K)
				FN(K)=ANK(L,K)*VBARN(K)
				                                                                                                        
				BINPCP=-1.*FM(k)*DNK(k)/DZT(k)  ! temp variable for rain rate
				rainrate(k)=rainrate(k)+BINPCP/(dt*rhowtr)  !grid scale precip at each level, m s-1
			ENDDO
!
			DO K=N0,N1-1
				AMK(L,K)=AMK(L,K)-(FM(K+1)*DNK(k+1)/DNK(k)-FM(K))
				ANK(L,K)=ANK(L,K)-(FN(K+1)*DNK(k+1)/DNK(k)-FN(K))
				DRT1D(K)=DRT1D(K)-(FM(K+1)*DNK(k+1)/DNK(k)-FM(K))
			ENDDO
			AMK(L,N1)=AMK(L,N1)+FM(N1) 
			ANK(L,N1)=ANK(L,N1)+FN(N1) 
			DRT1D(N1)=DRT1D(N1)+FM(N1) 
!
!
			BINPCP=-1.*FM(1)*DNK(1)/DZT(1)
			ACCPR=ACCPR+BINPCP/rhowtr  !grid scale precup at surface in each timestep
!			PCPRR=PCPRR+BINPCP/Dt

		ENDDO
!
		RETURN
	END SUBROUTINE SEDIM2
!
!------------------------------------------------------------------------------
! FLUX: A function used by the sedmientation scheme derived from
! smolarkiewicz.
!
	REAL FUNCTION FLUX(X1,X2,A)
		IMPLICIT NONE
		REAL :: X1,X2,A 
		FLUX=((A+ABS(A))*X1+(A-ABS(A))*X2)*0.5
		RETURN
	END FUNCTION FLUX
!
!------------------------------------------------------------------------------
! VNEW: A function used by the sedimentation scheme derived from 
! smolarkiewicz.  Only used in higher order implementations of the 
! former.
!
	REAL FUNCTION VNEW(X1,X2,A)
		IMPLICIT NONE
		REAL :: X1,X2,A 
		REAL, parameter ::  EPS = 1.E-30
		VNEW=(ABS(A)-A*A)*(X2-X1)/(X2+X1+EPS)
		RETURN
	END FUNCTION VNEW
!
!------------------------------------------------------------------------------
! DATA: contains routines for reading values of collection
! and breakup kernels as well as various other constants and defn.s
!
	SUBROUTINE DATA35()
!	SUBROUTINE DATA35( ALP, BET, REYN, &
!	                   ym1, yn1, yn2, yym1, yyn1, yyn2, &
!	                   sc0, sc3, sd0, sd3 )   ! , LPA, LPX, LPA1, ir, COLFLG, drzflg )
	
		IMPLICIT NONE
		
!		PARAMETER(tbd=30, ithresh=12, rpi=3.141592654)
!		REAL, DIMENSION(1:LPX) :: ALP, BET, REYN
		REAL :: x1, x2, x3, x4, rg, ym2, yym2
		INTEGER :: I, L, K, M
		
!		REAL, DIMENSION(1:tbd,1:tbd) :: yym1, yyn1, yyn2 
!		REAL, DIMENSION(1:tbd,1:tbd,1:tbd,1:tbd) :: ym1, yn1, yn2 
!		REAL, DIMENSION(1:ir,1:lpx) :: sc0, sc3, sd0, sd3
		
!
! calculating the mass categories (c.g.s) with the lowest radius
! of 3.125 microns and mass doubling every bin
!
		rad(1)=1.5625*1.E-04
		X(1)=4.*rpi/3.*rad(1)**3
		DO L=2,LPA1
			X(L)=2.*X(L-1)
			rad(L)=(3.*X(L)/(4.*rpi) )**(1./3.)
		ENDDO
		
!		L=1
!		P=2.0**(1/L)
!		AP=0.5+(P+1.0)*(P+1.0)/(8.0*P)
!		B=1./3.
!		XI=0.5*(1.+(1.+0.5*B)**(1.-B)*(1.-0.5*(1.-B))**B)
		
		IF (colflg) THEN
			PRINT*,'Reading in Big Collection Table - Patience'
			READ(12,*) wgtrm1, wgtrm2, wgtrn1, wgtrn2
			DO i=1, tbd
				DO k=1, tbd
					DO l=1, tbd
						DO m=1, tbd
!							READ(12,*) x1, x2, x3, x4, ym1(i,k,l,m), yn1(i,k,l,m),
!							           ym2,yn2(i,k,l,m)
							READ(12,*) ym1(i,k,l,m), yn1(i,k,l,m), yn2(i,k,l,m)
						ENDDO
					ENDDO
				ENDDO
			ENDDO
			READ(13,*) wgtcm, wgtcn
			DO i=1, tbd
				DO k=1, tbd
!					READ(13,*) x1, x2, yym1(i,k), yyn1(i,k), yym2, yyn2(i,k)
					READ(13,*) yym1(i,k), yyn1(i,k), yyn2(i,k)
				ENDDO
			ENDDO
			mincm=wgtcm*1000.
			mincn=wgtcn*1000.
			minrm=wgtrm1*1000.
			minrn=wgtrn1*1000.
			
			PRINT*,'Finished Reading in both Collection Tables'
			WRITE(*,*) 'wgtrm1, wgtrm2, wgtrn1, wgtrn2: ', wgtrm1, wgtrm2, wgtrn1, wgtrn2
			WRITE(*,*) 'wgtcm, wgtcn: ', wgtcm, wgtcn
		ENDIF
		
		
		IF (DRZFLG.EQ.1) THEN
			DO i=1, ir
				READ(30,*) rg, (sc0(i,l),l=1,ithresh)
				READ(31,*) rg, (sc3(i,l),l=1,ithresh)
				READ(32,*) rg, (sd0(i,l),l=1,lpx-ithresh)
				READ(33,*) rg, (sd3(i,l),l=1,lpx-ithresh)
			ENDDO
			PRINT*,'Finished Reading Sedimentation Table'
		ENDIF
		
!
! Terminal fall velocity (cm/s) coefficients, and reynolds number
!
		DO K=1,LPX
			If (K.GE.1.AND.K.LT.15) THEN
				BET(K)=2./3.
				ALP(K)=0.45795E+06
			ELSEIF (K.GE.15.AND.K.LT.25) THEN
				BET(K)=1./3.
				ALP(K)=4.962E+03
			ELSEIF (K.GE.25.AND.K.LT.32) THEN
				BET(K)=1./6.
				ALP(K)=1.732E+03
			ELSEIF (K.GE.32) THEN
				BET(K)=0.
				ALP(K)=917.
			ENDIF
			REYN(K)=(2.*rad(k)*1.275e-03/1.72e-04)*ALP(K)*X(K)**BET(K)
		ENDDO
		
		RETURN
	END SUBROUTINE DATA35

!------------------------------------------------------------------------------
	
	SUBROUTINE trunc(rmin, rmax, rg, sig, p, fac, pow)
		IMPLICIT NONE
		REAL :: rmin, rmax, rg, sig, p, pow, rt
		REAL, INTENT(out) :: fac
		DOUBLE PRECISION ::  zmax, zmin, Fmax, Fmin, ymax, ymin
#if ( FC == PGI )
		DOUBLE PRECISION :: DERF
#endif

! * *
! Truncated lognormal distribution
! * *

!************************************************************
! Set the lower and upper truncations dmin and dmax, resp.
!************************************************************
!  n= order of moment

		ymax=(alog(rmax/rg))/p - pow*p
		ymin=(alog(rmin/rg))/p - pow*p

		rt=sqrt(2.)
		ymax=ymax/rt
		ymin=ymin/rt

!******************
! error functions
!******************
		zmax=DERF(ymax)
		zmin=DERF(ymin)

		Fmax=0.5*(1. + zmax )
		Fmin=0.5*(1. + zmin )

		fac=SNGL(Fmax-Fmin)

		RETURN
	END SUBROUTINE trunc
	
!------------------------------------------------------------------------------
	
	REAL FUNCTION rg(an,am,p)
		IMPLICIT NONE
		REAL :: an, am, p
		REAL, PARAMETER :: const = 0.238732414   ! 3/(4 pi)
!		data ex1 /0.333333333/
		IF (an.gt.0..and.am.gt.0.) then
			rg=(am/an*const*p)**ex1
		ELSE
			rg=0.
		ENDIF
		RETURN
	END FUNCTION rg
	
!------------------------------------------------------------------------------
! Cond and Evap with some corrections (Shouping Wang, April 1998)
!------------------------------------------------------------------------------
	
	SUBROUTINE cond35( tau, rg1, anc, rcl, rg2, anr, rr, dqcr )
	
! ----------------------------------------------------
! Updates N;M for cloud and rain due to condensation
! Also updates rg1, rg2
!-----------------------------------------------------
		IMPLICIT NONE
		REAL, PARAMETER ::  xmult = 1.611991954   ! (4/3 pi)^0.333333
		REAL, INTENT(IN) :: tau 
		REAL :: rg1, rg2, anc, anr, rcl, rr
		REAL :: fac3, far3, facn, facm, fac, dmc, dmco, dn, xcrit, rcrit, xmin, xmax
		REAL :: rr2,anr2,rg22
		REAL :: dqcr,qcr0
		
		fac3=0.   ! initialize the temporary variable
		far3=0.   ! initialize the temporary variable
		facn=0.   ! initialize the temporary variable
		facm=0.   ! initialize the temporary variable
		fac =0.   ! initialize the temporary variable
		dn=0.     ! initialize the temporary variable
		dmc=0.    ! initialize the temporary variable
		dmco=0.   ! initialize the temporary variable
		
		qcr0 = rcl+rr   ! record the total liquid water before new condensation
		
!.... calculate the critical radius of cloud drops above which the drops will transfer to rain
		xcrit=(x(ithresh+1)**ex2 - tau)**(1.5)
		rcrit=(3.*xcrit/(4.*rpi))**ex1
		xmin=rcrit
!H. Wang		xmax=rad(ithresh+1)
		xmax=rad(LPX)
		
		IF (anc*1.0e6 .GT. 0.1) THEN  !avoid rg1=0, if N_c is too low, pass
!..... calclate the cloud drop number that will transfer to rain
			CALL trunc(xmin,xmax,rg1,sig1,pval,facn,0.)
			
!..... calculate newly condensed water on the existing cloud drops which will be transfered to
!..... rain drops
			CALL trunc(xmin,xmax,rg1,sig1,pval,facm,1.)
			
!..... calculate the mass transfer from the old cloud drops to rain due to condensation
			CALL trunc(xmin,xmax,rg1,sig1,pval,fac3,3.)
			
!.... the total mass transfer from clouds to rain
			dmco=fac3*rcl
			dmc=dmco+1.5*tau*anc*rg1*exp1h*facm*xmult ! transfer of old + new condensed
			
! H. Wang			xmin=rad(1)
			xmin=rad(1) !0.1
			xmax=rcrit
			
!..... calculate newly condensed water on the existing cloud drops which will remain cloud drops
			CALL trunc(xmin,xmax,rg1,sig1,pval,fac,1.)   ! int r n(r) dr
			
!..... here subtract ONLY the old cloud mass that will be transfered to rain
			rcl=rcl+1.5*tau*anc*rg1*exp1h*fac*xmult-dmco ! updating cloud mass
			dn=facn*anc
			anc=anc-dn ! dn is corresponding to dmc
		ENDIF

! H. Wang		xmin=rad(ithresh)
		xmin=rad(ithresh+1) ! 0.1
		xmax=rad(LPX)
!cc H.Wang,------ this if condition prevents the initiation of rain ---------
!ccccccccc!		IF (anr*1.0e6 .GE. 0.1) then

! diagnose the mean radius of rain drop to decide if put back to cloud
! H.Wang 11/29/2007
!		rr2=rr+1.5*tau*anr*rg2*exq1h*fac*xmult+dmc  !updating rain mass
!		anr2=anr+dn
!		rg22=rg(anr2,rr2,exqvali)

!		write(*,*)'r_th, rg1,rcrit,rg2',rad(ithresh),rg1,rcrit,rg22

!		IF ((anr+dn)*5.e-4 .GE. 0.1) THEN
		IF ((anr+dn)*1.e6 .GE. 0.1) THEN
		
!			IF (rg22.GE.rad(ithresh)) THEN

!.... calculate newly condensed water on the existing rain drops to avoid rg2=0
			IF (rg2.GT.0.) THEN
				CALL trunc(xmin,xmax,rg2,sig2,qval,far3,1.)
			ELSE
				far3=0.
			ENDIF

!.... add the TOTAL mass transfer from cloud to rain (NOT ONLY the old mass transfer, should include
!.... the newly condensed water on these old transfered cloud droplets.
			rr=rr+1.5*tau*anr*rg2*exq1h*far3*xmult+dmc   !updating rain mass
			anr=anr+dn
!			rg2=rr/anr*exqvali
! H.Wang added ----------
		ELSE
			anc=anc+dn
			rcl=rcl+dmco 
! -----------------------
		ENDIF
		
!		rg1=rcl/anc*expvali
		
		dqcr=rcl+rr-qcr0   ! change of liquid water
		
		RETURN
	END SUBROUTINE cond35

!------------------------------------------------------------------------------
	
	SUBROUTINE evap35( tau, rg1, anc, rcl, rg2, anr, rr, dqcr, dncr )

! ----------------------------------------------------
! Updates N;M for cloud and rain due to evaporation
! Also updates rg1, rg2
!-----------------------------------------------------
		IMPLICIT NONE
		
		REAL, PARAMETER ::  xmult = 1.611991954   ! (4/3 pi)^0.333333
		REAL, INTENT(IN) :: tau 
		REAL :: rg1, rg2, anc, anr, rcl, rr
		REAL :: fac, facnu, facmu, facnl, facr3, facc3, dmr, dmc, &
		        xcrit, rcrit_c, rcrit_r, xmin, xmax
		REAL :: dqcr, qcr0,dncr,ncr0
		
		fac  =0.   ! initialize the temporary variable
		facnl=0.   ! initialize the temporary variable
		facnu=0.   ! initialize the temporary variable
		facmu=0.   ! initialize the temporary variable
		facr3=0.   ! initialize the temporary variable
		facc3=0.   ! initialize the temporary variable
		dmc=0.   ! initialize the temporary variable
		dmr=0.   ! initialize the temporary variable
		
		qcr0 = rcl+rr   ! record the total liquid water before new evaporation
		ncr0 = anc+anr   ! record the total number of drops before new evaporation
		
! -----------------------------------------------
! cloud # increasing due to evap of raindrops
!------------------------------------------------
!..... 1st, calculate the critical radius below which the rain drops will
!.....   become cloud drops due to evaporation;
		
		xcrit=(x(ithresh+1)**ex2 - tau)**(1.5)
		rcrit_r=(3.*xcrit/(4.*rpi))**ex1
		
! H. Wang		xmin=rad(ithresh)
		xmin=rad(1)   ! 0.1
		xmax=rcrit_r
		
		IF (anr*1.0e6 .GT. 0.1) THEN
		
!.... calculate the rain number that will transfer to cloud due to evaporation
			CALL trunc(xmin,xmax,rg2,sig2,qval,facnu,0.)
			
!.... calculate the rain mass loss due to evap. for those rain drops that will transfer to clouds
			CALL trunc(xmin,xmax,rg2,sig2,qval,facmu,1.)
			
!.... calculate the mass transfer from old rain drops to cloud due to evaporation
			CALL trunc(xmin,xmax,rg2,sig2,qval,facr3,3.)
			
!.... the total rain transfer = old drops transfer - the new rain mass loss (tau<0)
			dmr= facr3*rr+1.5*tau*anr*rg2*exq1h*facmu*xmult ! Note the net should be positive
			dmr=max(dmr,0.0)
!.. 
!-- a bug fixed here (dmr and facnu are used later even if there were not computed)------
!-- H.Wang 11/16/2007-------
		ELSE
			dmr=0.
			facnu=0.
!---------------------------
		ENDIF
		
		
! -----------------------------------------------
! cloud # decreasing due to complete evap
!------------------------------------------------
		
		xcrit=(x(1)**ex2 - tau)**(1.5)
		rcrit_c=(3.*xcrit/(4.*rpi))**ex1
		
! H. Wang		xmin=rad(1)
		xmin=rad(1)  ! 0.1
		xmax=rcrit_c
		
		IF (anc*1.0e6 .GT. 0.1) THEN
!..... calculate the cloud number loss due to complete evap.
			CALL trunc(xmin,xmax,rg1,sig1,pval,facnl,0.)
			
!..... calculate the cloud mass loss due to complete evap.
			CALL trunc(xmin,xmax,rg1,sig1,pval,facc3,3.)
			
			dmc=facc3*rcl
		ENDIF
! -----
		
		xmin=rcrit_c
! H. Wang		xmax=rad(ithresh+1)
		xmax=rad(LPX)
		
		IF (anc*1.0e6 .GT. 0.1) THEN
!... calculate cloud mass loss due to evap. for those cloud drops that will remain cloud drops
			CALL trunc(xmin,xmax,rg1,sig1,pval,fac,1.)
			
			rcl=rcl+1.5*tau*anc*rg1*exp1h*fac*xmult -dmc + dmr
			anc=anc-facnl*anc+facnu*anr ! facnl*anc -> dmc, facnu*anr -> dmr
		ENDIF
		
		xmin=rcrit_r
		xmax=rad(LPX)
		
		IF (anr*1.0e6 .GE. 0.1) THEN
!.... calculate the mass loss for those rain drops that will remain in rain
			CALL trunc(xmin,xmax,rg2,sig2,qval,fac,1.)
!			rg2=rr/anr*exqvali
!..... updating rain mass with the loss due to direct evap. and the old rain drop transfer to clouds
			rr=rr+1.5*tau*anr*rg2*exq1h*fac*xmult-facr3*rr !updating with the loss
			anr=anr-facnu*anr
		ENDIF
		
!		rg1=rcl/anc*expvali
		
		dqcr=rcl+rr-qcr0   ! change of liquid water
		dncr=ncr0-anc-anr   ! change of number
		
		RETURN
	END SUBROUTINE evap35
	
!------------------------------------------------------------------------------
	
	SUBROUTINE nnn( ank, amk, rg1, rg2, an1, am1, an2, am2 )
!	SUBROUTINE nnn( ank, amk, rg1, rg2, an1, am1, an2, am2, sc0, sc3, sd0, sd3)
	
		IMPLICIT NONE
		REAL, INTENT(IN) :: rg1,rg2
		REAL :: an1,an2,am1,am2
		REAL,dimension(1:LPX) :: ank, amk
!		REAL, DIMENSION(1:ir,1:lpx) :: sc0,sc3,sd0,sd3
		
		INTEGER:: L,il,ik
		REAL :: sumsd0, sumsd3
		
! calculate moments in bin by integrating lognormal function
		
		il=int(2.e4*rg1-2.)
		ik=int(2.e4*rg2-2.)
		
!		print *,'il,ik,i,k  ',il,ik,rg1,rg2,i,k
		
		il=max(1,(min(il,37)) )
!		ik=max(49,(min(ik,797)) )
		ik=max(49,(min(ik,2000)) )
		
		IF (rg1.GT.0.) THEN
!		IF (rg1.GT.rad(1)) THEN
			DO L=1,ithresh
				ank(L)=an1*sc0(il,L)
				amk(L)=am1*sc3(il,L)
			ENDDO
		ELSE
			DO L=1,ithresh
				ank(L)=0.
				amk(L)=0.
			ENDDO
		ENDIF
		
!		IF (rg2.EQ.0..OR.ik.EQ.ithresh.OR.ik.EQ.47) THEN
		IF (rg2.EQ.0.) THEN
			DO L=ithresh+1,lpx
				ank(L)=0.
				amk(L)=0.
			ENDDO
		ELSE IF (rg2.LE.rad(ithresh+1)) THEN
!			DO l=1,ithresh
!				ank(l)=ank(l)+an2*sc0(il,l)
!				amk(l)=amk(l)+am2*sc3(il,l)
!			ENDDO
			
			ank(ithresh+1)=an2   
			amk(ithresh+1)=am2   
			DO l=ithresh+2,lpx
				ank(L)=0.
				amk(L)=0.
			ENDDO
			
		ELSE IF (rg2.GT.rad(ithresh+1).AND.rg2.LT.rad(lpx)) THEN
			DO L=ithresh+1,lpx
				ank(L)=an2*sd0(ik,l-ithresh)
				amk(L)=am2*sd3(ik,l-ithresh)
			ENDDO
			
		ELSE IF (rg2.GE.rad(lpx)) THEN
			ank(lpx)=an2
			amk(lpx)=am2
			DO l=ithresh+1,lpx-1
				ank(L)=0.
				amk(L)=0.
			ENDDO
		
		ENDIF
		
		RETURN
	END SUBROUTINE nnn
	
!------------------------------------------------------------------------------
! subroutine to initialize arrays
	SUBROUTINE azero(nx,a1)
		IMPLICIT NONE
		INTEGER :: nx,i
		REAL, DIMENSION(1:nx) :: a1
		DO i = 1,nx
			a1(i) = 0.0
		ENDDO
		RETURN
	END SUBROUTINE azero
	
	SUBROUTINE azero2(nx,a1,a2)
		IMPLICIT NONE
		INTEGER :: nx,i
		REAL, DIMENSION(1:nx) :: a1, a2
		DO i = 1,nx
			a1(i) = 0.0
			a2(i) = 0.0
		ENDDO
		RETURN
	END SUBROUTINE azero2

!------------------------------------------------------------------------------
	
	REAL FUNCTION FLHV (t)
		IMPLICIT NONE
!+
!+ purpose:  This function calculates the latent heat of vaporization for  +
!+           water as per P&K eqn. 4-85a. The result is then converted to  +
!+           ergs/g.  T is deg K.                                          +
!+
		REAL :: t, rga, rlhv
		rga  = 0.167 + (3.67e-4 * t)
		rlhv = 597.3 * (273.15 / t)**rga
		FLHV = rlhv / 2.38844e-08
		RETURN
	END FUNCTION FLHV
	
!------------------------------------------------------------------------------
	
	REAL FUNCTION FPHI1 (t,s,rlv)
		IMPLICIT NONE
!+
!+ purpose:  This function calculates a coefficient in DSDT.  +
!+
		REAL, PARAMETER :: g=980.0
		REAL, PARAMETER :: rv= 4.615e6
		REAL, PARAMETER :: rd=2.8704e6
		REAL, PARAMETER :: cp=1.0048e7
		REAL :: t1, t2, t, s, rlv
		t1 = rlv / (rv * t**2 * cp)
		t2 = 1./ (rd * t)
		FPHI1 = (s*g) * (t1 - t2)
		RETURN
	END FUNCTION FPHI1
	
!------------------------------------------------------------------------------
	
	REAL FUNCTION es0(t)
		IMPLICIT NONE
		REAL, PARAMETER :: c0 = 610.5851
		REAL, PARAMETER :: c1 = 44.40316
		REAL, PARAMETER :: c2 = 1.430341
		REAL, PARAMETER :: c3 = .2641412E-1
		REAL, PARAMETER :: c4 = .2995057E-3
		REAL, PARAMETER :: c5 = .2031998E-5
		REAL, PARAMETER :: c6 = .6936113E-8
		REAL, PARAMETER :: c7 = .2564861E-11
		REAL, PARAMETER :: c8 =-.3704404E-13
		REAL, PARAMETER :: CP = 1004.0 
		REAL, PARAMETER :: RCP = 0.285856574 
		REAL :: tc, t
		tc=t-273.16
		ES0=C0+TC*(C1+TC*(C2+TC*(C3+TC*(C4+TC*(C5+TC*(C6+TC*(C7+TC*C8)))))))
		RETURN
	END FUNCTION es0
	
!------------------------------------------------------------------------------
! Calculate effect of latent heating on potential temperature
	
	SUBROUTINE LHONTH( dqcr,qv0,tk,dtk)
! -- This routine is to calculate the change of potential temperature
! -- due to condensation/evaporation.  --H.Wang 12/18/2007
! dqcr -- change in qc and qr due to cond/evap, kg/kg
! dtk  -- change in temperature, K
! dth  -- change in potential temperature, K
! qv0  -- current qv, kg/kg
! pre  -- pressure, Pa
! th0  -- potential temp. before updating, K
! th1  -- updated potential temp., K
		
		IMPLICIT NONE
		
		REAL, PARAMETER :: xv0=2.5e6, xv1=2370., ctok=273.15, &
		                   cpd=1005., c0=0.84, k0=0.286,      &
		                   p0=1.e5,  epsi=1.e-15
		REAL, INTENT(IN ) :: dqcr, qv0, tk
		REAL, INTENT(OUT) :: dtk
		REAL :: pii, xlv, cpm
!		pii = (pre/p0)**k0   ! this can be provided by ARW
!		tk  = th0*pii        ! get temperature in K  !!!NEEDED in WRF
		cpm = cpd*( 1.+c0*max(qv0,epsi) )   ! compute specific heat capacity for moist air
		xlv = xv0-xv1*(tk-ctok) ! compute latent heat constant at tk
		dtk=dqcr*xlv/cpm
!		dth=dtk/pii
!		th1=th0 + dtk/pii
	END SUBROUTINE LHONTH
	
!------------------------------------------------------------------------------
	
	REAL FUNCTION opd(nb,rg,val,dz,rha)
		IMPLICIT NONE
!+
!+ purpose:  This function calculates optical depth for log-normal distribution  +
!+
! nb -- number concentration, #/g
! rg -- geometric mean radius, cm
! val-- log(sig)
! dz -- the geometric thickness of the model layer, m
! rha-- air density, g/cm^-3
	
		REAL :: nb, rg, cc, dz, rha, val
		cc=exp(2.*val*val)
		opd=2.*rpi*nb*rha*rg*rg*cc*dz*100.
		RETURN
	END FUNCTION opd

! +----------------------------------------------------------------------------+
END MODULE module_mp_feingold2m
! +----------------------------------------------------------------------------+

#endif /*ESRL*/
