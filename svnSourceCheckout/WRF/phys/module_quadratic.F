! $Id: module_quadratic.F 193 2010-07-13 21:50:35Z ldgrant@uwm.edu $
#ifdef TESTCASES
MODULE module_quadratic

! CREATED BY BRIAN GRIFFIN!!!

! This module contains Subroutine quadratic_derivative.
! This module also contains Subroutine quadratic_derivative_scalar.
! This module also contains Subroutine quadratic_derivative_1dim.

! Subroutine quadratic_derivative finds the derivative at a central
! level assuming a parabolic (or linear) fit between three data points
! on three consecutive levels.

! We have three vertical levels k-1, k, and k+1.
! These vertical levels have heights z(k-1), z(k), and z(k+1).
! At these vertical levels and corresponding heights are values for
! certain variables, such as Theta, QV, U, V, etc.
! At each of the three levels, we can make a point (z, met_var), where
! met_var stands for a general meteorological variable. (x,y) form!
! The values of these meteorological variables may change linearly
! with respect to height.  However, they may also not line up linearly.
! However, with only three points, one can find a parabolic curve which
! goes through all three points.  Once one has the equation for a curve,
! one can take the derivative of that equation to find the rate of change
! of a meteorological variable with respect to height.

! This will also work if the line connecting the three points is linear.
! For the parabolic equation y = Ax^2 + Bx + C, the value of A would be 0.

! For the very top and bottom levels, we can use the same three-point
! curve we use for the second-from-top and second-from-bottom levels,
! respectively.  We just use the height value (or x-value) for the top
! or bottom level instead of the central level for that individual
! calculation.  This will still not be as accurate as if it were a
! central level.

! The new Subroutine quadratic_derivative_m2t computes the derivative of
! a value located at momentum levels.  When it comes time to enter the 
! height coordinate back into the equation y = Ax + B at the end of the
! subroutine, it uses a thermodynamic level height coordinate instead of 
! a momentum level height coordinate.

IMPLICIT NONE

PUBLIC :: quadratic_derivative

PRIVATE ! default scope

CONTAINS

!=========================================================================

SUBROUTINE quadratic_derivative (value, z, derivative, &
                         ids, ide, jds, jde, kds, kde, &
                         ims, ime, jms, jme, kms, kme, &
                         its, ite, jts, jte, kts, kte  )

! Imported variables
                                                                                                                             
INTEGER, INTENT (IN):: ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte

REAL, DIMENSION (ims:ime, kms:kme, jms:jme), INTENT (IN):: value, &
                                                               z

REAL, DIMENSION (ims:ime, kms:kme, jms:jme), INTENT (OUT):: derivative

! Local Variables

REAL:: xa_1, xa_2, xa_3, xb_1, xb_2, xb_3, xc_1, xc_2, xc_3, &
       y_1, y_2, y_3, x_1, x_2, x_3, factor_21, factor_31, &
       factor_32, A, B, C

INTEGER:: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k

!-------------------------------------------------------------------------

i_start = its
i_end = MIN(ide-1, ite)
j_start = jts
j_end = MIN(jde-1, jte)
k_start = kts
k_end = MIN(kde-1, kte)

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      DO k = k_start+1, k_end-1, 1

         ! We have three points: (x_1,y_1), (x_2,y_2), (x_3,y_3)

         y_1 = value(i,k-1,j)
         y_2 = value(i,k,j)
         y_3 = value(i,k+1,j)
         x_1 = z(i,k-1,j)
         x_2 = z(i,k,j)
         x_3 = z(i,k+1,j)

         ! We plug these points into the quadratic equation:
         ! y = Ax^2 + Bx + C

         ! We end up with three equation (one for each set of points),
         ! as well as three unknown constants (A, B, and C).
         ! y_1 = A(x_1)^2 + B(x_1) + C
         ! y_2 = A(x_2)^2 + B(x_2) + C
         ! y_3 = A(x_3)^2 + B(x_3) + C

         ! We introduce a 4 x 3 matrix used for solving A, B, and C.
         ! We are using the Gaussian Elimination Method to solve this.

         !    A        B     C
         ! (x_1)^2   (x_1)   1   |   y_1
         ! (x_2)^2   (x_2)   1   |   y_2
         ! (x_3)^2   (x_3)   1   |   y_3

         xa_1 = x_1**2
         xa_2 = x_2**2
         xa_3 = x_3**2
         xb_1 = x_1
         xb_2 = x_2
         xb_3 = x_3
         xc_1 = 1
         xc_2 = 1
         xc_3 = 1

         ! The current 4 x 3 matrix is:

         !   A        B        C
         ! (xa_1)   (xb_1)   (xc_1)   |   y_1
         ! (xa_2)   (xb_2)   (xc_2)   |   y_2
         ! (xa_3)   (xb_3)   (xc_3)   |   y_3

         ! The first step of the Gaussian Elimination Method.
         ! -(xa_2/xa_1)*(first row) + (second row) = (new second row)

         factor_21 = xa_2/xa_1
         xa_2 = xa_2 - (factor_21)*xa_1
         xb_2 = xb_2 - (factor_21)*xb_1
         xc_2 = xc_2 - (factor_21)*xc_1
         y_2  =  y_2 - (factor_21)*y_1

         ! The second step of the Gaussian Elimination Method.
         ! -(xa_3/xa_1)*(first row) + (third row) = (new third row)

         factor_31 = xa_3/xa_1
         xa_3 = xa_3 - (factor_31)*xa_1
         xb_3 = xb_3 - (factor_31)*xb_1
         xc_3 = xc_3 - (factor_31)*xc_1
         y_3  =  y_3 - (factor_31)*y_1
         
         ! The current 4 x 3 matrix is:

         !   A        B        C
         ! (xa_1)   (xb_1)   (xc_1)   |   y_1
         !   0      (xb_2)   (xc_2)   |   y_2
         !   0      (xb_3)   (xc_3)   |   y_3

         ! The third step of the Gaussian Elimination Method.
         ! -(xa_3/xa_2)*(second row) + (third row) = (new third row)

         factor_32 = xb_3/xb_2
         xb_3 = xb_3 - (factor_32)*xb_2
         xc_3 = xc_3 - (factor_32)*xc_2
         y_3  =  y_3 - (factor_32)*y_2

         ! The current 4 x 3 matrix is:

         !   A        B        C
         ! (xa_1)   (xb_1)   (xc_1)   |   y_1
         !   0      (xb_2)   (xc_2)   |   y_2
         !   0        0      (xc_3)   |   y_3

         ! Now we can solve for C.
         ! (xc_3)C = y_3

         C = y_3/xc_3

         ! Now we can solve for B.
         ! (xb_2)B + (xc_2)C = y_2

         B = (y_2 - xc_2*C)/xb_2

         ! Finally, we can solve for A.
         ! (xa_1)A + (xb_1)B + (xc_1)C = y_1

         A = (y_1 - xc_1*C - xb_1*B)/xa_1

         ! We have now successfully solved for y = Ax^2 + Bx + C.
         ! The derivative of that equation is y = 2Ax + B.

         IF (k == k_start+1) THEN
            ! For bottom level.
            derivative(i,k_start,j) = 2*A*x_1 + B
            ! For a central level.
            derivative(i,k,j) = 2*A*x_2 + B
         ELSEIF (k > k_start+1 .AND. k < k_end-1) THEN
            ! For a central level.
            derivative(i,k,j) = 2*A*x_2 + B
         ELSEIF (k == k_end-1) THEN
            ! For a central level.
            derivative(i,k,j) = 2*A*x_2 + B
            ! For top level.
            derivative(i,k_end,j) = 2*A*x_3 + B
         ENDIF

      ENDDO
   ENDDO
ENDDO
         
END SUBROUTINE quadratic_derivative

!=========================================================================

SUBROUTINE quadratic_derivative_scalar (value, z, n_scalar, &
                                       species, derivative, &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte  )

! Imported variables
                                                                                                                             
INTEGER, INTENT (IN):: ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte, &
                       n_scalar, species

REAL, DIMENSION (ims:ime, kms:kme, jms:jme, n_scalar), INTENT (IN):: value

REAL, DIMENSION (ims:ime, kms:kme, jms:jme), INTENT (IN):: z

REAL, DIMENSION (ims:ime, kms:kme, jms:jme), INTENT (OUT):: derivative

! Local Variables

REAL:: xa_1, xa_2, xa_3, xb_1, xb_2, xb_3, xc_1, xc_2, xc_3, &
       y_1, y_2, y_3, x_1, x_2, x_3, factor_21, factor_31, &
       factor_32, A, B, C

INTEGER:: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k

!-------------------------------------------------------------------------

i_start = its
i_end = MIN(ide-1, ite)
j_start = jts
j_end = MIN(jde-1, jte)
k_start = kts
k_end = MIN(kde-1, kte)

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      DO k = k_start+1, k_end-1, 1

         ! We have three points: (x_1,y_1), (x_2,y_2), (x_3,y_3)

         y_1 = value(i,k-1,j,species)
         y_2 = value(i,k,j,species)
         y_3 = value(i,k+1,j,species)
         x_1 = z(i,k-1,j)
         x_2 = z(i,k,j)
         x_3 = z(i,k+1,j)

         ! We plug these points into the quadratic equation:
         ! y = Ax^2 + Bx + C

         ! We end up with three equation (one for each set of points),
         ! as well as three unknown constants (A, B, and C).
         ! y_1 = A(x_1)^2 + B(x_1) + C
         ! y_2 = A(x_2)^2 + B(x_2) + C
         ! y_3 = A(x_3)^2 + B(x_3) + C

         ! We introduce a 4 x 3 matrix used for solving A, B, and C.
         ! We are using the Gaussian Elimination Method to solve this.

         !    A        B     C
         ! (x_1)^2   (x_1)   1   |   y_1
         ! (x_2)^2   (x_2)   1   |   y_2
         ! (x_3)^2   (x_3)   1   |   y_3

         xa_1 = x_1**2
         xa_2 = x_2**2
         xa_3 = x_3**2
         xb_1 = x_1
         xb_2 = x_2
         xb_3 = x_3
         xc_1 = 1
         xc_2 = 1
         xc_3 = 1

         ! The current 4 x 3 matrix is:

         !   A        B        C
         ! (xa_1)   (xb_1)   (xc_1)   |   y_1
         ! (xa_2)   (xb_2)   (xc_2)   |   y_2
         ! (xa_3)   (xb_3)   (xc_3)   |   y_3

         ! The first step of the Gaussian Elimination Method.
         ! -(xa_2/xa_1)*(first row) + (second row) = (new second row)

         factor_21 = xa_2/xa_1
         xa_2 = xa_2 - (factor_21)*xa_1
         xb_2 = xb_2 - (factor_21)*xb_1
         xc_2 = xc_2 - (factor_21)*xc_1
         y_2  =  y_2 - (factor_21)*y_1

         ! The second step of the Gaussian Elimination Method.
         ! -(xa_3/xa_1)*(first row) + (third row) = (new third row)

         factor_31 = xa_3/xa_1
         xa_3 = xa_3 - (factor_31)*xa_1
         xb_3 = xb_3 - (factor_31)*xb_1
         xc_3 = xc_3 - (factor_31)*xc_1
         y_3  =  y_3 - (factor_31)*y_1
         
         ! The current 4 x 3 matrix is:

         !   A        B        C
         ! (xa_1)   (xb_1)   (xc_1)   |   y_1
         !   0      (xb_2)   (xc_2)   |   y_2
         !   0      (xb_3)   (xc_3)   |   y_3

         ! The third step of the Gaussian Elimination Method.
         ! -(xa_3/xa_2)*(second row) + (third row) = (new third row)

         factor_32 = xb_3/xb_2
         xb_3 = xb_3 - (factor_32)*xb_2
         xc_3 = xc_3 - (factor_32)*xc_2
         y_3  =  y_3 - (factor_32)*y_2

         ! The current 4 x 3 matrix is:

         !   A        B        C
         ! (xa_1)   (xb_1)   (xc_1)   |   y_1
         !   0      (xb_2)   (xc_2)   |   y_2
         !   0        0      (xc_3)   |   y_3

         ! Now we can solve for C.
         ! (xc_3)C = y_3

         C = y_3/xc_3

         ! Now we can solve for B.
         ! (xb_2)B + (xc_2)C = y_2

         B = (y_2 - xc_2*C)/xb_2

         ! Finally, we can solve for A.
         ! (xa_1)A + (xb_1)B + (xc_1)C = y_1

         A = (y_1 - xc_1*C - xb_1*B)/xa_1

         ! We have now successfully solved for y = Ax^2 + Bx + C.
         ! The derivative of that equation is y = 2Ax + B.

         IF (k == k_start+1) THEN
            ! For bottom level.
            derivative(i,k_start,j) = 2*A*x_1 + B
            ! For a central level.
            derivative(i,k,j) = 2*A*x_2 + B
         ELSEIF (k > k_start+1 .AND. k < k_end-1) THEN
            ! For a central level.
            derivative(i,k,j) = 2*A*x_2 + B
         ELSEIF (k == k_end-1) THEN
            ! For a central level.
            derivative(i,k,j) = 2*A*x_2 + B
            ! For top level.
            derivative(i,k_end,j) = 2*A*x_3 + B
         ENDIF

      ENDDO
   ENDDO
ENDDO
         
END SUBROUTINE quadratic_derivative_scalar

!=========================================================================

SUBROUTINE quadratic_derivative_1dim (value, z, derivative, &
                              ids, ide, jds, jde, kds, kde, &
                              ims, ime, jms, jme, kms, kme, &
                              its, ite, jts, jte, kts, kte  )

! Imported variables
                                                                                                                             
INTEGER, INTENT (IN):: ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte

REAL, DIMENSION (kms:kme), INTENT (IN):: value

REAL, DIMENSION (ims:ime, kms:kme, jms:jme), INTENT (IN):: z

REAL, DIMENSION (ims:ime, kms:kme, jms:jme), INTENT (OUT):: derivative

! Local Variables

REAL:: xa_1, xa_2, xa_3, xb_1, xb_2, xb_3, xc_1, xc_2, xc_3, &
       y_1, y_2, y_3, x_1, x_2, x_3, factor_21, factor_31, &
       factor_32, A, B, C

INTEGER:: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k

!-------------------------------------------------------------------------

i_start = its
i_end = MIN(ide-1, ite)
j_start = jts
j_end = MIN(jde-1, jte)
k_start = kts
k_end = MIN(kde-1, kte)

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      DO k = k_start+1, k_end-1, 1

         ! We have three points: (x_1,y_1), (x_2,y_2), (x_3,y_3)

         y_1 = value(k-1)
         y_2 = value(k)
         y_3 = value(k+1)
         x_1 = z(i,k-1,j)
         x_2 = z(i,k,j)
         x_3 = z(i,k+1,j)

         ! We plug these points into the quadratic equation:
         ! y = Ax^2 + Bx + C

         ! We end up with three equation (one for each set of points),
         ! as well as three unknown constants (A, B, and C).
         ! y_1 = A(x_1)^2 + B(x_1) + C
         ! y_2 = A(x_2)^2 + B(x_2) + C
         ! y_3 = A(x_3)^2 + B(x_3) + C

         ! We introduce a 4 x 3 matrix used for solving A, B, and C.
         ! We are using the Gaussian Elimination Method to solve this.

         !    A        B     C
         ! (x_1)^2   (x_1)   1   |   y_1
         ! (x_2)^2   (x_2)   1   |   y_2
         ! (x_3)^2   (x_3)   1   |   y_3

         xa_1 = x_1**2
         xa_2 = x_2**2
         xa_3 = x_3**2
         xb_1 = x_1
         xb_2 = x_2
         xb_3 = x_3
         xc_1 = 1
         xc_2 = 1
         xc_3 = 1

         ! The current 4 x 3 matrix is:

         !   A        B        C
         ! (xa_1)   (xb_1)   (xc_1)   |   y_1
         ! (xa_2)   (xb_2)   (xc_2)   |   y_2
         ! (xa_3)   (xb_3)   (xc_3)   |   y_3

         ! The first step of the Gaussian Elimination Method.
         ! -(xa_2/xa_1)*(first row) + (second row) = (new second row)

         factor_21 = xa_2/xa_1
         xa_2 = xa_2 - (factor_21)*xa_1
         xb_2 = xb_2 - (factor_21)*xb_1
         xc_2 = xc_2 - (factor_21)*xc_1
         y_2  =  y_2 - (factor_21)*y_1

         ! The second step of the Gaussian Elimination Method.
         ! -(xa_3/xa_1)*(first row) + (third row) = (new third row)

         factor_31 = xa_3/xa_1
         xa_3 = xa_3 - (factor_31)*xa_1
         xb_3 = xb_3 - (factor_31)*xb_1
         xc_3 = xc_3 - (factor_31)*xc_1
         y_3  =  y_3 - (factor_31)*y_1
         
         ! The current 4 x 3 matrix is:

         !   A        B        C
         ! (xa_1)   (xb_1)   (xc_1)   |   y_1
         !   0      (xb_2)   (xc_2)   |   y_2
         !   0      (xb_3)   (xc_3)   |   y_3

         ! The third step of the Gaussian Elimination Method.
         ! -(xa_3/xa_2)*(second row) + (third row) = (new third row)

         factor_32 = xb_3/xb_2
         xb_3 = xb_3 - (factor_32)*xb_2
         xc_3 = xc_3 - (factor_32)*xc_2
         y_3  =  y_3 - (factor_32)*y_2

         ! The current 4 x 3 matrix is:

         !   A        B        C
         ! (xa_1)   (xb_1)   (xc_1)   |   y_1
         !   0      (xb_2)   (xc_2)   |   y_2
         !   0        0      (xc_3)   |   y_3

         ! Now we can solve for C.
         ! (xc_3)C = y_3

         C = y_3/xc_3

         ! Now we can solve for B.
         ! (xb_2)B + (xc_2)C = y_2

         B = (y_2 - xc_2*C)/xb_2

         ! Finally, we can solve for A.
         ! (xa_1)A + (xb_1)B + (xc_1)C = y_1

         A = (y_1 - xc_1*C - xb_1*B)/xa_1

         ! We have now successfully solved for y = Ax^2 + Bx + C.
         ! The derivative of that equation is y = 2Ax + B.

         IF (k == k_start+1) THEN
            ! For bottom level.
            derivative(i,k_start,j) = 2*A*x_1 + B
            ! For a central level.
            derivative(i,k,j) = 2*A*x_2 + B
         ELSEIF (k > k_start+1 .AND. k < k_end-1) THEN
            ! For a central level.
            derivative(i,k,j) = 2*A*x_2 + B
         ELSEIF (k == k_end-1) THEN
            ! For a central level.
            derivative(i,k,j) = 2*A*x_2 + B
            ! For top level.
            derivative(i,k_end,j) = 2*A*x_3 + B
         ENDIF

      ENDDO
   ENDDO
ENDDO
         
END SUBROUTINE quadratic_derivative_1dim

!=========================================================================

SUBROUTINE quadratic_derivative_m2t (value, z_at_w, z, derivative, &
                                     ids, ide, jds, jde, kds, kde, &
                                     ims, ime, jms, jme, kms, kme, &
                                     its, ite, jts, jte, kts, kte  )

! Imported variables
                                                                                                                             
INTEGER, INTENT (IN):: ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte

REAL, DIMENSION (ims:ime, kms:kme, jms:jme), INTENT (IN):: value, &
                                                          z_at_w, &
                                                               z

REAL, DIMENSION (ims:ime, kms:kme, jms:jme), INTENT (OUT):: derivative

! Local Variables

REAL:: xa_1, xa_2, xa_3, xb_1, xb_2, xb_3, xc_1, xc_2, xc_3, &
       y_1, y_2, y_3, x_1, x_2, x_3, factor_21, factor_31, &
       factor_32, A, B, C, tmp_xa1, tmp_xb1, tmp_xc1, tmp_y1

INTEGER:: i_start, i_end, j_start, j_end, k_start, k_end, i, j, k

!-------------------------------------------------------------------------

i_start = its
i_end = MIN(ide-1, ite)
j_start = jts
j_end = MIN(jde-1, jte)
k_start = kts
k_end = MIN(kde-1, kte)

DO i = i_start, i_end, 1
   DO j = j_start, j_end, 1
      DO k = k_start+1, k_end-1, 1

         ! We have three points: (x_1,y_1), (x_2,y_2), (x_3,y_3)

         y_1 = value(i,k-1,j)
         y_2 = value(i,k,j)
         y_3 = value(i,k+1,j)
         x_1 = z_at_w(i,k-1,j)
         x_2 = z_at_w(i,k,j)
         x_3 = z_at_w(i,k+1,j)

         ! We plug these points into the quadratic equation:
         ! y = Ax^2 + Bx + C

         ! We end up with three equation (one for each set of points),
         ! as well as three unknown constants (A, B, and C).
         ! y_1 = A(x_1)^2 + B(x_1) + C
         ! y_2 = A(x_2)^2 + B(x_2) + C
         ! y_3 = A(x_3)^2 + B(x_3) + C

         ! We introduce a 4 x 3 matrix used for solving A, B, and C.
         ! We are using the Gaussian Elimination Method to solve this.

         !    A        B     C
         ! (x_1)^2   (x_1)   1   |   y_1
         ! (x_2)^2   (x_2)   1   |   y_2
         ! (x_3)^2   (x_3)   1   |   y_3

         xa_1 = x_1**2
         xa_2 = x_2**2
         xa_3 = x_3**2
         xb_1 = x_1
         xb_2 = x_2
         xb_3 = x_3
         xc_1 = 1
         xc_2 = 1
         xc_3 = 1

         ! The current 4 x 3 matrix is:

         !   A        B        C
         ! (xa_1)   (xb_1)   (xc_1)   |   y_1
         ! (xa_2)   (xb_2)   (xc_2)   |   y_2
         ! (xa_3)   (xb_3)   (xc_3)   |   y_3

         ! If xa_1 = 0, swap the row with the third row.

         IF (x_1 == 0.0) THEN
            tmp_xa1 = xa_1
            tmp_xb1 = xb_1
            tmp_xc1 = xc_1
            tmp_y1 = y_1

            xa_1 = xa_3
            xb_1 = xb_3
            xc_1 = xc_3
            y_1 = y_3

            xa_3 = tmp_xa1
            xb_3 = tmp_xb1
            xc_3 = tmp_xc1
            y_3 = tmp_y1
         ENDIF

         ! The first step of the Gaussian Elimination Method.
         ! -(xa_2/xa_1)*(first row) + (second row) = (new second row)

         factor_21 = xa_2/xa_1
         xa_2 = xa_2 - (factor_21)*xa_1
         xb_2 = xb_2 - (factor_21)*xb_1
         xc_2 = xc_2 - (factor_21)*xc_1
         y_2  =  y_2 - (factor_21)*y_1

         ! The second step of the Gaussian Elimination Method.
         ! -(xa_3/xa_1)*(first row) + (third row) = (new third row)

         factor_31 = xa_3/xa_1
         xa_3 = xa_3 - (factor_31)*xa_1
         xb_3 = xb_3 - (factor_31)*xb_1
         xc_3 = xc_3 - (factor_31)*xc_1
         y_3  =  y_3 - (factor_31)*y_1
         
         ! The current 4 x 3 matrix is:

         !   A        B        C
         ! (xa_1)   (xb_1)   (xc_1)   |   y_1
         !   0      (xb_2)   (xc_2)   |   y_2
         !   0      (xb_3)   (xc_3)   |   y_3

         ! The third step of the Gaussian Elimination Method.
         ! -(xa_3/xa_2)*(second row) + (third row) = (new third row)

         factor_32 = xb_3/xb_2
         xb_3 = xb_3 - (factor_32)*xb_2
         xc_3 = xc_3 - (factor_32)*xc_2
         y_3  =  y_3 - (factor_32)*y_2

         ! The current 4 x 3 matrix is:

         !   A        B        C
         ! (xa_1)   (xb_1)   (xc_1)   |   y_1
         !   0      (xb_2)   (xc_2)   |   y_2
         !   0        0      (xc_3)   |   y_3

         ! Now we can solve for C.
         ! (xc_3)C = y_3

         C = y_3/xc_3

         ! Now we can solve for B.
         ! (xb_2)B + (xc_2)C = y_2

         B = (y_2 - xc_2*C)/xb_2

         ! Finally, we can solve for A.
         ! (xa_1)A + (xb_1)B + (xc_1)C = y_1

         A = (y_1 - xc_1*C - xb_1*B)/xa_1

         ! We have now successfully solved for y = Ax^2 + Bx + C.
         ! The derivative of that equation is y = 2Ax + B.

         IF (k == k_start+1) THEN
            ! For bottom level.
            derivative(i,k_start,j) = 2*A*z(i,k-1,j) + B
            ! For a central level.
            derivative(i,k,j) = 2*A*z(i,k,j) + B
         ELSEIF (k > k_start+1 .AND. k < k_end-1) THEN
            ! For a central level.
            derivative(i,k,j) = 2*A*z(i,k,j) + B
         ELSEIF (k == k_end-1) THEN
            ! For a central level.
            derivative(i,k,j) = 2*A*z(i,k,j) + B
            ! For top level.
            derivative(i,k_end,j) = 2*A*z(i,k+1,j) + B
         ENDIF

      ENDDO
   ENDDO
ENDDO
         
END SUBROUTINE quadratic_derivative_m2t

!=========================================================================

END MODULE module_quadratic
#endif /*TESTCASES*/
