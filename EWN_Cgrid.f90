program EWN_Cgrid
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  
! C-grid airfoil grid generation using Thompson's method 
!  with orthogonal boundary condition applied over airfoil
!
! Author: Shao-Chi Huang(George)
!         Graduate Researcher
!         University of Wasington CFM Lab.
! 
! Last Modification date: AUG/28/2021
! email: shaochi@uw.edu  
! License: GNU General Public License v3.0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   implicit none
   INTERFACE ! Interface for subroutine naca_airfoil_profile
      SUBROUTINE naca_airfoil_profile(NACA,imax_airfoil,x_profile,y_profile)
      integer, INTENT(IN) :: NACA, imax_airfoil
      real, dimension(:,:), allocatable, intent(out) :: x_profile, y_profile
      END SUBROUTINE naca_airfoil_profile
   END INTERFACE
   INTERFACE ! Interface for subroutine EWN_solver
      subroutine EWN_solver(x_profile, y_profile, imax_airfoil,NACA,imax,jmax)
         real, dimension(:,:), allocatable, intent(inout) :: x_profile, y_profile
         integer, intent(in) :: NACA, imax_airfoil
         integer, intent(out) :: imax, jmax
      end subroutine EWN_solver
   END INTERFACE
   INTERFACE ! Interface for subroutine grid_property
      subroutine grid_property(imax,jmax,imax_airfoil,NACA)
      integer, intent(in) :: NACA, imax_airfoil, imax, jmax
         
      end subroutine grid_property
   END INTERFACE

   integer :: i ! Used for loop counter
   integer :: User_input_airfoil, GP ! numbers used for decision making
   integer :: NACA, imax_airfoil ! NACA number, grid points over one side of airfoil 
   real, dimension(:,:), allocatable :: x_profile, y_profile ! Defined for airfoil profile
   character(len=4) :: naca_number, imax_profile_s ! NACA number and grid points over airfoil in characters
   character(len=50) :: file_name ! Define filename   
   
   double precision, dimension(:,:), allocatable :: x_grid, y_grid ! Defined for airfoil grid 
   integer :: imax, jmax ! Computational domain size

   logical :: file_exists ! logical check if file exist
   ! Allocate array
   
   

   write(*,*) 'Generate NACA 4 digit airfoil profile? (Yes(1)/No(0))' ! Decision for airfoil profile generation
   read(*,*) User_input_airfoil

   write(*,*) ''
   write(*,*) 'Input feasible NACA 4 digit numbers' ! User define NACA 4 - digit number
   read(*,*) NACA
   write(naca_number,'(i0.4)') NACA ! Write  NACA number into string  
   write(*,*) ''
   write(*,*) 'Grid property files include, Skewness, Aspect ratio, cell area,'
   write(*,*) 'growth rate in i and j directions and Fluent import format.'
   write(*,*) 'Generate grid property files? (Yes(1)/No(0))' ! Decision for grid property files
   read(*,*) GP

   SELECT CASE (User_input_airfoil)
      CASE (1)
         WRITE(*,*) ''
         WRITE(*,*) 'NACA 4-digit airfoil generation requested'        
         write(*,*) 'Input number of grid points on airfoil upper surface' ! prompt user input airfoil grid points
         read(*,*) imax_airfoil  ! user input
         write(imax_profile_s,'(i0.4)') imax_airfoil ! Write number into string
         allocate(x_profile(imax_airfoil*2-1,1), y_profile(imax_airfoil*2-1,1))
         file_name = 'airfoil_profile_naca'//naca_number//'_ia'//trim(imax_profile_s)//'.dat'

         call naca_airfoil_profile(NACA,imax_airfoil,x_profile,y_profile)
         imax_airfoil = imax_airfoil*2-1 ! Define variable to repersent grid points over entire airfoil

      CASE(0)
         WRITE(*,*) '' 
         WRITE(*,*) 'Please load NACA airfoil in format: (airfoil_profile_nacaXXXX_iaYYYY.dat)'
         WRITE(*,*) 'XXXX represents NACA 4 digit ; YYYY represents grid points over airfoil'
         write(*,*) 'For file format, please order grid points as example file'
         write(*,*) ''
         write(*,*) 'Input the number of grid points over entire airfoil:' ! prompt user input for nodes on one side
         read(*,*) imax_airfoil ! user input
         allocate(x_profile(imax_airfoil,1), y_profile(imax_airfoil,1))
         write(imax_profile_s,'(i0.4)') imax_airfoil ! Write number into string
         file_name = 'airfoil_profile_naca'//naca_number//'_ia'//trim(imax_profile_s)//'.dat'
         INQUIRE(FILE=trim(file_name), EXIST=file_exists)
         if (file_exists) then
            open (unit=7, file=trim(file_name), status='old', action='read')
            do i = 1,2 ! Allows the read function to skip first 2 lines of .dat file
                  read(7,*)
            end do
            do i = 1,imax_airfoil  ! Load data into i location  
                  read(7,100) x_profile(i,1), y_profile(i,1)
                  100 format(E15.8,1x,E15.8)
            end do 
            close(7)
            print *, 'User defined airfoil profile data file located and read'
         
         else
            write(6,*) "File not found, please check input file location and format"
            go to 900
         end if         
      CASE DEFAULT
         write(*,*) ''
         WRITE(*,*) "Erroneous input, please try again"
         go to 900
   END SELECT
   call EWN_solver(x_profile, y_profile,imax_airfoil,NACA,imax, jmax) ! Begin poisson solver 
   allocate(x_grid(imax,jmax),y_grid(imax,jmax)) ! Allocate matrix

   select case (GP)
      case (1)
         write(*,*) ''
         write(*,*) 'Grid property files generation chosen'
         call grid_property(imax,jmax,imax_airfoil,NACA)
      case (0)
         write(*,*) ''
         write(*,*) 'No grid property file generation reqired'
      case default
         write(*,*) ''
         write(*,*) 'Erroneous input, generating property files'
         call grid_property(imax,jmax,imax_airfoil,NACA)
   end select
   900 continue
end program EWN_Cgrid

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! subroutines 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine naca_airfoil_profile(NACA,imax_airfoil,x_profile,y_profile)
   integer, INTENT(IN) :: NACA, imax_airfoil
   real, dimension(:,:), allocatable, intent(out) :: x_profile, y_profile
   
   real, dimension(:,:), allocatable :: x,y,yc, dyc, yt, yu, yl, xu, xl, theta
   integer :: i
   integer :: imax  ! nodes on one side
   integer :: iimax ! Number of nodes over airfoil
   integer :: N1, N2, N3
   real :: m, p, t
   character(len=4) :: naca_number,iimax_s
   character(len=50) :: file_name 
   real :: rs = 0.055

   imax = imax_airfoil

   !  The NACA four-digit wing sections define the profile by
   !  1. First digit describing maximum camber as percentage of the chord. 
   !  2. Second digit describing the distance of maximum camber from the airfoil leading edge in tenths of the chord.
   !  3. Last two digits describinnag maximum thickness of the airfoil as percent of the chord. 

   ! allocate matrix
   allocate(yc(imax,1),dyc(imax,1),yt(imax,1),xu(imax,1),xl(imax,1),yu(imax,1),yl(imax,1),theta(imax,1))    
   allocate(x(iimax,1),y(iimax,1))
   ! Calculate tion based on user input
   N1 = NACA/1000
   N2 = (NACA-N1*1000)/100
   N3 = NACA-N1*1000-N2*100
   m = N1/100.
   p = N2/10.
   t = N3/100.
   iimax = imax*2-1
   print *, ''
   print *, 'First digit', N1
   print *, 'Second digit', N2
   print *, 'Third and forth digit', N3
   print *, 'Max Camber',m
   print *, 'Max Camber Location',p
   write (*,100) 'Max Thickness',t
   100 format ('',A14,f8.2)

   write(naca_number,'(i0.4)') NACA ! Write  NACA number into string
   print *, 'NACA',naca_number
   write(iimax_s,'(i0.4)') iimax ! Write number into string
   print *, 'Total grid points over airfoil is : ',iimax_s
   ! go to 900
   file_name = 'airfoil_profile_naca'//naca_number//'_ia'//trim(iimax_s)//'.dat'
   ! x stretching with hypertangent function
   do i = 1,imax
      ! x(i,1) = 1.*( 1- ( tanh(rs*((imax-1)-i+1)) / (tanh(rs*(imax-1))) )) ! One sided stretching
      x(i,1) = 0.5*( 1- ( tanh(rs*((imax-1)/2-i+1)) / (tanh(rs*(imax-1)/2)) )) ! Two sided stretching
   end do
   ! Calculate chamber line 
   do i = 1, imax
      if (x(i,1)<p) then ! 0 < x < p
         yc(i,1) = m/p**2*(2.*p*x(i,1)-x(i,1)**2)
         dyc(i,1) = 2.*m/p**2*(p-x(i,1))
      else               ! p <= x < 1
         yc(i,1) = m/(1-p)**2*((1.-2.*p)+2.*p*x(i,1)-x(i,1)**2)
         dyc(i,1) = 2.*m/(1-p)**2*(p-x(i,1))
      end if
      theta(i,1) = atan(dyc(i,1))
   end do
   do i = 1,imax ! Thickness calculation
      yt(i,1) =  5*t*(0.2969*sqrt(x(i,1))-0.1260*x(i,1)-0.3516*x(i,1)**2+0.2843*x(i,1)**3-0.1036*x(i,1)**4)
   end do
   do i = 1, imax ! Calculate upper and lower nodes
   xu(i,1) = x(i,1) - yt(i,1)*sin(theta(i,1))
   xl(i,1) = x(i,1) + yt(i,1)*sin(theta(i,1))
   yu(i,1) = yc(i,1) + yt(i,1)*cos(theta(i,1))
   yl(i,1) = yc(i,1) - yt(i,1)*cos(theta(i,1))
   end do
   do i = 1,imax ! Re-order nodes
      x(i,1) = xl(imax+1-i,1)
      y(i,1) = yl(imax+1-i,1)
   end do
   do i = imax+1,iimax ! Re-order nodes
      x(i,1) = xu(i-imax+1,1)
      y(i,1) = yu(i-imax+1,1)
   end do
   ! Close gap
   y(iimax,1) = 0.
   y(1,1) = 0.

   open(unit=1, file=trim(file_name), status='replace') ! Generate output file
      write(1,400) 
      400 format ('VARIABLES = "x", "y"')
      write(1,300) iimax, 1
      300 format ('ZONE F=POINT, I=', I4, ', J=', I4)
      do i = 1, iimax
         write(1,200) x(i,1), y(i,1)
         200 format (E15.8,1x,E15.8)
      end do
   close(1)
   print *, ''
   print *, 'Airfoil Generation Complete'
   print *, 'L.E. node spacing', sqrt((x(imax+1,1)-x(imax,1))**2 + (y(imax+1,1)-y(imax,1))**2)
   print *, 'T.E. node spacing', sqrt((x(2,1)-x(1,1))**2 + (y(2,1)-y(1,1))**2)
   x_profile = x ; y_profile = y
   return
end subroutine

subroutine EWN_solver(x_profile, y_profile,imax_airfoil,NACA,imax,jmax)
   real, dimension(:,:), allocatable, intent(inout) :: x_profile, y_profile
   integer, intent(in) :: NACA, imax_airfoil
   integer, intent(out) :: imax, jmax ! imax and jmax represents the number of grid points in the xi and eta direction, respectively.


   double precision :: pi = 4*atan(1.)

   double precision :: P, Q , v1x,v1y,v2x,v2y,v3x,v3y,v4x,v4y

   double precision :: a2x,a1y,a2y, slope, constant, xa,xb,xtt,ytt
   double precision, dimension(1,2) :: xab
   double precision :: alpha, betta, gamma, delta
   double precision, dimension(:,:), allocatable :: x, y, ndis, xn,yn
   ! double precision :: a1 = 0., c1 = 0. ! Source coefficient for i
   double precision :: a2 = 200000000000., c2 = 0.25 ! Source coefficient for j (works with 2000000000) use 20000000. (on 199*100)
   double precision :: a3 = 200000000000., c3 = 0.25 ! Source coefficient for j in iw area (works with 2000000000) use 20000000.

   double precision :: sgn
   double precision :: xold, yold, far_a, far_b
   double precision :: w = 0.5 ! SOR coefficient
   double precision rx, ry, r, arc
   real, parameter :: tolerance = 1.0e-6 ! Convergence criterion
   integer, parameter :: maxiter = 100000
   double precision, dimension(maxiter) :: rbar
   integer :: i, j, n, finaliteration
   integer :: ia  ! Nodes over airfoil
   integer :: iw  ! Nodes in trailing edge (Use 99) 

   double precision :: rs = 0.075 ! Grid stretching coefficient in j
   double precision :: rs2 = 0.042 ! Grid stretching coefficient in i (trailing edge)
   double precision :: rs3 = 0.042 ! Grid stretching coefficient in i (Source term Q)
   double precision :: lu = 50.0 ! Domain size upsteam (The actual length on x-axis is lu-1)
   double precision :: ld = 40.0 ! Domain size downstream
   integer :: nodes = 25 ! Used for keeping orthogonality near airfoil
   double precision :: ortho_f = 0.02 ! 0.02 symmetric

   ! SET TO (lens=6) for naca 6 series
   character(len=4) :: naca_number, imax_s, jmax_s,ia_s ! Allocate for integer to character
   character(len=50) :: file_name, file_name2, file_name3, file_name4, file_name5


   double precision :: b,pl,ql
   double precision, dimension(:,:), allocatable :: x2,y2
   double precision, dimension(:,:), allocatable :: x_xi,y_xi,x_dxi,y_dxi,x_eta,y_eta,x_deta,y_deta,g11,g22,p0,q0

   double precision, dimension(:,:), allocatable :: dxl, dxu
   integer, dimension(:,:), allocatable :: dxui, dxli

   ia = imax_airfoil
   write(*,*) ''
   write(*,*) 'Input nodes in the wake (One side)' ! prompt user input
   read(*,*) iw  ! user input
   write(*,*) 'Input number of grid points in the eta direction (Direction vertical to airfoil)' ! prompt user input
   read(*,*) jmax  ! user input
   imax = ia+iw*2 ! Define imax
   ! SET TO (i0.6) for naca 6 series
   write(naca_number,'(i0.4)') NACA ! Write  NACA number into string
   write(imax_s,'(i0.4)') imax ! Write number into string
   print *, 'Number of grid points in the xi direction is : ',imax_s ! Used for file imax*jmax
   write(jmax_s,'(i0.4)') jmax ! Write number into string
   print *, 'Number of grid points in the eta direction is : ',jmax_s ! Used for file imax*jmax
   write(ia_s,'(i0.4)') ia ! Write number into string
   print *, 'Number of grid points over the airfoil is : ',ia_s ! Used for reading airfoil file
   
   print *, 'The airfoil is NACA',naca_number ! Output user input NACA number
   ! Define file names  
   file_name = 'airfoil_profile_naca'//naca_number//'_ia'//trim(ia_s)//'.dat'  ! File name of airfoil data .dat file already generated
   file_name2 = 'initial_lines_naca'//naca_number//'_'//imax_s//'x'//jmax_s//'.dat' ! File name for inital lines of domain
   file_name3 = 'initial_mesh_naca'//naca_number//'_'//imax_s//'x'//jmax_s//'.dat' ! File name of initial mesh generated
   file_name4 = 'mesh_naca'//naca_number//'_'//imax_s//'x'//jmax_s//'.dat' ! File name for generated mesh
   file_name5 = 'residual_naca'//naca_number//'_'//imax_s//'x'//jmax_s//'.dat' ! File name for residual


   allocate(x(imax+2,jmax+1), y(imax+2,jmax+1)) ! Allocate Matrix
   allocate(ndis(imax,1)) ! Allocate for j2 j3 calculation
   allocate(xn(imax,1),yn(imax,1)) ! Allocate for j2 j3 calculation normal line  
   allocate(x2(imax,jmax+1),y2(imax,jmax+1)) ! Allocate for ghost cell
   ! Allocate for boundary orthogonality
   allocate(x_xi(imax,1),y_xi(imax,1),x_dxi(imax,1),y_dxi(imax,1),x_eta(imax,1),y_eta(imax,1),x_deta(imax,1),y_deta(imax,1))
   allocate(g11(imax,1),g22(imax,1),p0(imax,1),q0(imax,1)) 

   allocate(dxu(imax,1),dxl(imax,1),dxui(1,jmax),dxli(1,jmax)) 
   ! Physical Domain  
   !                  C                   D
   !                 *  * * * * * * * * * *
   !              *  *                    *
   !           *     *                    *
   !         *       *                    *
   !       *         *                    *
   !      *     **   * G                  * H
   !      *   *    * *  * * * * * * * * * * 
   !      *     **   * F                  * E
   !       *         *                    *
   !         *       *                    *
   !           *     *                    *
   !              *  *                    *
   !                 *  * * * * * * * * * *
   !                  B                    A
   ! Computational Domain
   !
   !   A = (1,jmax) ; B = (iw+1,jmax)
   !   C = (ia+iw,jmax) ; D = (imax,jmax)
   !
   !    A_______B_______C_______D
   !    |                       | 
   !    |                       |
   !    |                       |
   !    E_______F_______G_______H
   !
   !

   ! Load Naca airfoil to Generate Line F ---> G
   do i = iw+1,iw+ia  ! Load data into i location
      x(i,1) = x_profile(i-iw,1)
      y(i,1) = y_profile(i-iw,1)
   end do 

   ! Use orthogonal property to find Point C
   v1x = x(iw+ia-1,1)-x(iw+ia,1) ! v2x = x(iw+ia,jmax)-x(iw+ia,1)
   v1y = y(iw+ia-1,1)- y(iw+ia,1) ! v2y = y(iw+ia,jmax)- y(iw+ia,1)
   v2x = sqrt(lu**2/(1+v1x**2/v1y**2)) ! Farfeild set at "lu".
   x(iw+ia,jmax) = v2x + x(iw+ia,1)
   v2y = -v1x*v2x/v1y
   y(iw+ia,jmax) = v2y + y(iw+ia,1)

   ! generate line G ---> C ; slope = v2y/v2x ;  equation =  v2y/v2x * x  -v2y/v2x = y
   do j = 2, jmax-1 ! Orthogonal line generation from end of airfoil with wall spacing from JSC af-se
   y(iw+ia,j) = (y(iw+ia,jmax)-y(iw+ia,1))*( 1.- ( tanh(rs*((jmax-1)-j+1)) / (tanh(rs*(jmax-1))) ) )
   x(iw+ia,j) = (y(iw+ia,j) + v2y/v2x) * v2x/v2y
   end do
   ! Use orthogonal property to find Point B
   v3x = x(iw+2,1)-x(iw+1,1) ! v4x = x(iw+1,jmax)-x(iw+1,1)
   v3y = y(iw+2,1)- y(iw+1,1) ! v4y = y(iw+1,jmax)- y(iw+1,1)
   v4x = sqrt(lu**2/(1+v3x**2/v3y**2))
   x(iw+1,jmax) = v4x + x(iw+1,1)
   v4y = -v3x*v4x/v3y
   y(iw+1,jmax) = v4y + y(iw+1,1)

   ! generate line F ---> B
   do j = 2, jmax-1 ! Orthogonal line generation from end of airfoil with wall spacing from JSC af-se
   y(iw+1,j) = (y(iw+1,jmax)-y(iw+1,1))*( 1.- ( tanh(rs*((jmax-1)-j+1)) / (tanh(rs*(jmax-1))) ) )
   x(iw+1,j) =  (y(iw+1,j) + v4y/v4x) * v4x/v4y
   end do
   far_a = acos((v2x*v4x+v2y*v4y)/lu**2) ! Calculate angle enclosed by two vectors orthogonal to airfoil trailing edge
   far_b = atan(v4x/v4y) ! Calculate angle from x axis of the vector (iw+1,jmax)-(iw+1,1) 
   ! print *, far_a,far_b ! CHK angles
   ! generate far-field circle with uniform spacing (Line B ---> C) 
   do i = iw+2, iw+ia-1
   x(i,jmax) = -lu*sin((i-iw)*(2*pi-far_a)/(ia-1.)+far_b)+1
   y(i,jmax) = -lu*cos((i-iw)*(2*pi-far_a)/(ia-1.)+far_b)
   end do

   print *, 'x(iw+1,1),y(iw+1,1)',x(iw+1,1),y(iw+1,1) ! CHK points generated
   print *, 'x(iw+ia,1),y(iw+ia,1)',x(iw+ia,1),y(iw+ia,1)
   print *, 'x(iw+1,jmax),y(iw+1,jmax)',x(iw+1,jmax),y(iw+1,jmax)
   print *, 'x(iw+ia,jmax),y(iw+ia,jmax)',x(iw+ia,jmax),y(iw+ia,jmax)
   print *, 'delta y at T.E', y(iw+1,1)-y(iw+1,2)
   do i = iw+1,iw+ia ! For the i range over airfoil, move y values to "lu" afterit decreases after x > 1
   if (x(i,jmax)>1) then
      if (y(i,jmax)>0) then
      y(i,jmax) = lu
      else if (y(i,jmax)<0) then
      y(i,jmax) = -lu
      end if
   end if
   end do
   x(imax,:) = ld +1 ! Define outlet x value
   x(1,:) = ld+1 ! Define outlet x value
   ! Generate line A ---> B
   do i = 1,iw+1
   x(iw+2-i,jmax) = (x(imax,jmax)-x(iw+1,jmax))*( 1- ( tanh(rs2*(iw-i+1)) / (tanh(rs2*(iw))) ) ) + x(iw+1,jmax)
   y(i,jmax) = y(iw+1 ,jmax)
   end do
   ! Generate line E ---> F
   do i = 1,iw
   x(iw+2-i,1) = (x(imax,1)-x(iw+1,1))*( 1- ( tanh(rs2*(iw-i+1)) / (tanh(rs2*(iw))) ) ) + x(iw+1,1)
   y(i,1) = y(iw+1 ,1)
   end do
   ! Generate line D ---> C
   do i = iw+ia,imax
   x(i,jmax) = (x(imax,jmax)-x(iw+ia,jmax))*( 1- ( tanh(rs2*(iw-i+1.+iw+ia-1)) / (tanh(rs2*(iw))) ) ) + x(iw+ia,jmax)
   y(i,jmax) = y(iw+ia ,jmax)
   end do
   ! Generate line H ---> G
   do i = iw+ia,imax
   x(i,1) = (x(imax,1)-x(iw+ia,1))*( 1- ( tanh(rs2*(iw-i+1.+iw+ia-1)) / (tanh(rs2*(iw))) ) ) + x(iw+ia,1)
   y(i,1) = y(iw+ia ,1)
   end do
   do j = 2,jmax-1
   y(imax,j) = y(iw+ia,j) ! Generate line H ---> D
   y(1,j) = y(iw+1,j) ! Generate line E ---> A
   end do
   print *, 'delta x at T.E', x(iw+1,1)-x(iw+2,1), x(iw,1)-x(iw+1,1) ! Display TE grid point spacing.

   !    A_______B_______C_______D
   !    |                       | 
   !    |                       |
   !    |                       |
   !    E_______F_______G_______H
   ! CHK generated lines
   open(unit=1, file=trim(file_name2), status='replace') ! Output initial lines
   write(1,610) 
   610 format ('VARIABLES = "x", "y"')
   write(1,620) imax+imax+jmax+jmax+jmax+jmax, 1
   620 format ('ZONE F=POINT, I=', I4, ', J=', I4)
   210 format (E15.8,1x,E15.8)
   do i = 1, imax
      write(1,210) x(i,1), y(i,1)  ! Line E ---> H
   end do 
   do j = 1, jmax
      write(1,210) x(1,j),y(1,j) ! Line E ---> A
      end do  
   do i = 1, imax
      write(1,210) x(i,jmax), y(i,jmax) ! Line A ---> D
   end do 
   do j = 1, jmax
      write(1,210) x(imax,j),y(imax,j) ! Line H ---> D
   end do   
   do j = 1, jmax ! Last two are the orthogonal lines from airfoil trailing edge
      write(1,210) x(iw+ia,j),y(iw+ia,j) ! Line G ---> C
   end do  
   do j = 1, jmax
      write(1,210) x(iw+1,j),y(iw+1,j) ! Line F ---> B
   end do     
   close(1)

   ! compute initial conditions using orthogonal properties
   do i =iw+2,iw+ia-1
   do j = 2,jmax-1
      a2x = x(i+1,1) - x(i,1)    ! a1x = xtt - x(i,1)
      a2y = y(i+1,1) - y(i,1)    ! a1y = ytt - y(i,1)
      if (y(i,1) > 0) then
         ytt = sqrt(10.0**2/(1+a2y**2/a2x**2)) + y(i,1)
         a1y = ytt - y(i,1)
         xtt = -a1y*a2y/a2x + x(i,1)
      else
         ytt = -sqrt(10.0**2/(1+a2y**2/a2x**2)) + y(i,1)
         a1y = ytt - y(i,1)
         xtt = -a1y*a2y/a2x + x(i,1)
      end if
      slope = (ytt-y(i,1))/(xtt-x(i,1))
      constant = y(i,1) - slope*x(i,1)
      ! Rearrange farfield nodes
      xa = (-(2*slope*constant-2) + sqrt(((2*slope*constant-2))**2-4*(slope**2+1)*(constant**2-lu**2+1)))&
            /2/(slope**2+1)
      xb = (-(2*slope*constant-2) - sqrt(((2*slope*constant-2))**2-4*(slope**2+1)*(constant**2-lu**2+1)))&
            /2/(slope**2+1)
      xab(1,1) = xa
      xab(1,2) = xb 
      if (y(i,1)>0) then
      if (slope>0) then
         if (i < 129+129+10) then
            x(i,jmax) = minval(xab)
         else
            x(i,jmax) = maxval(xab)
         end if
      else
         x(i,jmax) = minval(xab)
      end if
      else if (y(i,1) == 0) then
            y(i,jmax) = 0.
            x(i,jmax) = -(lu-1.)
      else
      if (slope>0) then
            x(i,jmax) = minval(xab)
      else
            x(i,jmax) = maxval(xab)
      end if
      end if
      y(i,jmax) = slope*x(i,jmax) + constant
   
      x(i,j) = (x(i,jmax)-x(i,1))*( 1- ( tanh(rs*((jmax-1)-j+1)) / (tanh(rs*(jmax-1))) ) ) + x(i,1)
      y(i,j) = slope*x(i,j) + constant
   end do
   end do
   do i = iw+1,iw+ia
   if (x(i,jmax)>1) then
      if (y(i,jmax) > 0) then
      y(i,jmax) = lu
      else
      y(i,jmax) = -lu
      end if
   end if
   end do
   do j = 2,jmax ! initial mesh in AFT block
   do i = iw+ia,imax
      x(i,j) = (x(imax,j)-x(iw+ia,j))*( 1- ( tanh(rs2*(iw-i+1.+iw+ia-1)) / (tanh(rs2*(iw))) ) ) + x(iw+ia,j)
      y(i,j) = y(iw+ia ,j)
   end do 
   end do
   do i = iw+ia,imax
   y(i,1) = y(iw+ia,1)
   y(i,jmax) = lu
   end do
   do j = 2,jmax
   do i = 1,iw+1
      x(iw+2-i,j) = (x(imax,j)-x(iw+1,j))*( 1- ( tanh(rs2*(iw-i+1)) / (tanh(rs2*(iw))) ) ) + x(iw+1,j)
      y(i,j) = y(iw+1 ,j)
   end do 
   end do


   open(unit=10, file=trim(file_name3), status='replace') ! Output initial mesh
   write(10,420) 
   420 format ('VARIABLES = "x", "y"')
   ! write(10,320) imax, jmax
   write(10,320) 257, 129
   320 format ('ZONE F=POINT, I=', I4, ', J=', I4)
   do j = 1, jmax
     do i = 1, imax
      write(10,220) x(i,j), y(i,j)
      220 format (E15.8,1x,E15.8)
      end do
   end do
   close(10)

   open(unit=5, file='test_airfoil.dat',status = 'replace') ! File for mesh progression during solver implementation
      write(5,*) 'DATA'  
   close(5)

   do j = 2,jmax+1 ! Make Room for ghost cell
   do i = 1,imax
      x2(i,j) = x(i,j-1)
      y2(i,j) = y(i,j-1)
   end do
   end do
   do j = 2,2  ! Compute values for orthogonality over airfoil and j = 2
   do i = 2,imax-1
      x_xi(i,1) = (0.5*(x2(i+1,j)-x2(i-1,j)))
      y_xi(i,1) = (0.5*(y2(i+1,j)-y2(i-1,j)))
      x_dxi(i,1) = (x2(i+1,j)-2*x2(i,j)+x2(i-1,j))
      y_dxi(i,1) = (y2(i+1,j)-2*y2(i,j)+y2(i-1,j))
      g11(i,1) = ((x2(i+1,j)-x2(i-1,j))/2)**2+((y2(i+1,j)-y2(i-1,j))/2)**2
      x2(i,j-1) = x2(i,j) - ( -1.0*(0.5*(y2(i+1,j)-y2(i-1,j)))  )*&
         ( -1.0*((0.5*(y2(i+1,j)-y2(i-1,j))))*(x2(i,3)-x2(i,2)) + ((0.5*(x2(i+1,j)-x2(i-1,j))))*(y2(i,3)-y2(i,2)) )/g11(i,1)
      y2(i,j-1) = y2(i,j) - ( (0.5*(x2(i+1,j)-x2(i-1,j))) )*&
         ( -1.0*((0.5*(y2(i+1,j)-y2(i-1,j))))*(x2(i,3)-x2(i,2)) + ((0.5*(x2(i+1,j)-x2(i-1,j))))*(y2(i,3)-y2(i,2)) )/g11(i,1)
      g22 = (( -1.0*(0.5*(y2(i+1,j)-y2(i-1,j)))  )*&
            ( -1.0*((0.5*(y2(i+1,j)-y2(i-1,j))))*(x2(i,3)-x2(i,2)) &
            + ((0.5*(x2(i+1,j)-x2(i-1,j))))*(y2(i,3)-y2(i,2)) )/sqrt(g11(i,1)))**2+&
            (( (0.5*(x2(i+1,j)-x2(i-1,j))) )*&
            ( -1.0*((0.5*(y2(i+1,j)-y2(i-1,j))))*(x2(i,3)-x2(i,2)) + &
            ((0.5*(x2(i+1,j)-x2(i-1,j))))*(y2(i,3)-y2(i,2)) )/sqrt(g11(i,1)))**2
         ! print *, x2(i,j-1),g22
   end do
   end do

   ! solve Poisson equation
   n = 0
   write(*,800) 
   800 format ('Iteration   Residual')
   do
   n = n + 1 ! number of iterations
   if (n == maxiter + 1) then ! Exit condition at max_iteration
      finaliteration = n - 1   ! Final iteration, since n = n+1
      exit
   end if
      do i = 2,imax-1 
      x_xi(i,1) = (0.5*(x2(i+1,2)-x2(i-1,2))) ! Values for boundary orthogonality are kept the same, always j = 2
      y_xi(i,1) = (0.5*(y2(i+1,2)-y2(i-1,2)))
      x_dxi(i,1) = (x2(i+1,2)-2*x2(i,2)+x2(i-1,2))
      y_dxi(i,1) = (y2(i+1,2)-2*y2(i,2)+y2(i-1,2))
      g11(i,1) = ((x2(i+1,2)-x2(i-1,2))/2)**2+((y2(i+1,2)-y2(i-1,2))/2)**2

      g22(i,1) = (( -1.0*(0.5*(y2(i+1,2)-y2(i-1,2)))  )*&
            ( -1.0*((0.5*(y2(i+1,2)-y2(i-1,2))))*(x2(i,3)-x2(i,2)) + ((0.5*(x2(i+1,2)-x2(i-1,2))))*(y2(i,3)-y2(i,2)) )&
            /sqrt(g11(i,1)))**2+&
            (( (0.5*(x2(i+1,2)-x2(i-1,2))) )*&
            ( -1.0*((0.5*(y2(i+1,2)-y2(i-1,2))))*(x2(i,3)-x2(i,2)) + ((0.5*(x2(i+1,2)-x2(i-1,2))))*(y2(i,3)-y2(i,2)) )&
            /sqrt(g11(i,1)))**2
      x_eta(i,1) = x2(i,3)-x2(i,2)
      y_eta(i,1) = y2(i,3)-y2(i,2)
      x_deta(i,1) = x2(i,3)-2*x2(i,2)+x2(i,1)
      y_deta(i,1) = y2(i,3)-2*y2(i,2)+y2(i,1)
      end do
   do j = 3, jmax ! Start with computing P and Q source terms in iterior
      do i = 2, imax-1
      xold = x2(i,j) ! Set old values of x and y
      yold = y2(i,j)
      if (i > iw) then ! if condition for the wake source term is not the same as the airfoil ; Calculate P and Q for line and point source term
         if(i<(iw+ia+1)) then ! airfoil
            Pl = 0.       
            Ql = -a2*sgn(dble(j)-2.)*exp(-c2*abs(j-2.)) 
         else ! wake after airfoil
            Pl = 0.           
            Ql = -a3*sgn(dble(j)-2.)*exp(-c3*abs(j-2.))
            Ql = Ql*(1-(tanh(rs3*(iw-1-(3*iw+2*ia+1-i)+iw+ia+1))/tanh(rs3*(iw-1))))
         
         end if
      else ! wake before airfoil
         Pl = 0.     
            Ql = -a3*sgn(dble(j)-2.)*exp(-c3*abs(j-2.))
            Ql = Ql*(1-(tanh(rs3*(iw-1-i+1))/tanh(rs3*(iw-1))))
      
      end if
      ! Calculate P and Q for orthogonality
      p0(i,1) = -( (x_xi(i,1)*x_dxi(i,1))+(y_xi(i,1)*y_dxi(i,1)) )/(g11(i,1)) - &
            ( (x_xi(i,1)*x_deta(i,1))+(y_xi(i,1)*y_deta(i,1)) )/(g22(i,1))
      q0(i,1) = -( (x_eta(i,1)*x_deta(i,1))+(y_eta(i,1)*y_deta(i,1)) )/(g22(i,1)) - &
            ( (x_eta(i,1)*x_dxi(i,1))+(y_eta(i,1)*y_dxi(i,1)) )/(g11(i,1))

      if (j < nodes) then ! Blending function for the two Ps and Qs
         b = 1. ! Let orthogonality stronger for the first few lines
      elseif (i > iw) then
         if(i<(iw+ia+1)) then
            b = exp(-dble(j-nodes)/jmax/ortho_f )
         else
            b = exp(-dble(j-nodes)/jmax/ortho_f )
         end if
      else
         b = exp(-dble(j-nodes)/jmax/ortho_f )
      end if
      ! b = 0.
      p = b*1.*p0(i,1)+ pl ! Actual P applied for updating x and y location
      q = b*1.*q0(i,1) + ql ! Actual Q applied for updating x and y location

      alpha = 0.25*((x2(i,j+1)-x2(i,j-1))**2 + (y2(i,j+1)-y2(i,j-1))**2) ! Matrix values to update x and y
      gamma = 0.25*((x2(i+1,j)-x2(i-1,j))**2 + (y2(i+1,j)-y2(i-1,j))**2)
      betta = 0.25*((x2(i+1,j)-x2(i-1,j))*(x2(i,j+1)-x2(i,j-1)) &
                  +(y2(i+1,j)-y2(i-1,j))*(y2(i,j+1)-y2(i,j-1)))
      delta = 0.0625*((x2(i+1,j)-x2(i-1,j))*(y2(i,j+1)-y2(i,j-1)) &
                     -(x2(i,j+1)-x2(i,j-1))*(y2(i+1,j)-y2(i-1,j)))**2
      
      x2(i,j) = (alpha*(x2(i-1,j)+x2(i+1,j)) & 
                  - 0.5*betta*(x2(i+1,j+1)-x2(i-1,j+1)-x2(i+1,j-1)+x2(i-1,j-1)) + gamma*(x2(i,j-1)+x2(i,j+1)) &
                  + 0.5*delta*P*(x2(i+1,j)-x2(i-1,j)) + 0.5*delta*Q*(x2(i,j+1)-x2(i,j-1)))/(2.*(alpha + gamma))
      
      y2(i,j) = (alpha*(y2(i-1,j)+y2(i+1,j)) &
                  - 0.5*betta*(y2(i+1,j+1)-y2(i-1,j+1)-y2(i+1,j-1)+y2(i-1,j-1)) + gamma*(y2(i,j-1)+y2(i,j+1)) &
                  + 0.5*delta*P*(y2(i+1,j)-y2(i-1,j)) + 0.5*delta*Q*(y2(i,j+1)-y2(i,j-1)))/(2.*(alpha + gamma))

      x2(i,j) = xold + w*(x2(i,j)-xold) ! new x
      y2(i,j) = yold + w*(y2(i,j)-yold) ! new y
      if (isnan(x2(i,j))) then ! Stop program if nan is found
         print *, 'x NaN found, exit',i,j
         exit
      end if
      if (isnan(y2(i,j))) then ! Stop program if nan is found
         print *, 'y NaN found, exit',i,j
         exit
      end if
      end do
   end do
   ! Update ghost cell j = 1
   do j = 2,2
      do i = 2,imax-1
      x_xi = (0.5*(x2(i+1,j)-x2(i-1,j))) ! Values for calculating x and y for j = 1 based on j = 2
      y_xi = (0.5*(y2(i+1,j)-y2(i-1,j)))
      x_dxi = (x2(i+1,j)-2*x2(i,j)+x2(i-1,j))
      y_dxi = (y2(i+1,j)-2*y2(i,j)+y2(i-1,j))
      g11 = ((x2(i+1,j)-x2(i-1,j))/2)**2+((y2(i+1,j)-y2(i-1,j))/2)**2
      x2(i,j-1) = x2(i,j) - ( -1.0*(0.5*(y2(i+1,j)-y2(i-1,j)))  )*&
            ( -1.0*((0.5*(y2(i+1,j)-y2(i-1,j))))*(x2(i,3)-x2(i,2)) + ((0.5*(x2(i+1,j)-x2(i-1,j))))*(y2(i,3)-y2(i,2)) )/g11(i,1)
      y2(i,j-1) = y2(i,j) - ( (0.5*(x2(i+1,j)-x2(i-1,j))) )*&
            ( -1.0*((0.5*(y2(i+1,j)-y2(i-1,j))))*(x2(i,3)-x2(i,2)) + ((0.5*(x2(i+1,j)-x2(i-1,j))))*(y2(i,3)-y2(i,2)) )/g11(i,1)
      end do
   end do
      
   do i = 1, imax ! over the farfield, keep inlet as semi circle
      if (x2(i,jmax) < 0) then 
      arc = atan(y2(i,jmax)/(x2(i,jmax)-1)) ! Calculate angle
      x2(i,jmax+1) = -lu*cos(arc) + 1
      y2(i,jmax+1) = -lu*sin(arc)
      elseif (x2(i,jmax) < 1) then 
      arc = atan(y2(i,jmax)/(1 - x2(i,jmax)))
      x2(i,jmax+1) = 1 - lu*cos(arc)
      y2(i,jmax+1) = lu*sin(arc)
      end if
   end do
   do i = 1, imax ! For far field on top and bottom, keep y value the same
      if (x2(i,jmax) > 1) then 
         if (y2(i,jmax)> 0) then
         y2(i,jmax+1) = lu
         else
         y2(i,jmax+1) = -lu
         end if
      x2(i,jmax+1) = x2(i,jmax)
      end if
   end do

   do j = 3,jmax ! let far field on rightside be orthogonal by using same y value for imax and imax-1, 1 and 2
      y2(imax,j) = y2(imax-1,j)
      y2(1,j) = y2(2,j)
   end do

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!      
   ! compute residuals
   r = 0.
   do j= 3, jmax ! compute residual for interior
      do i=2, imax-1
      if (i > iw) then ! if condition for the wake source term is not the same as the airfoil ; Calculate P and Q for line and point source term
         if(i<(iw+ia+1)) then ! airfoil
            Pl = 0.       
            Ql = -a2*sgn(dble(j)-2.)*exp(-c2*abs(j-2.)) 
         else ! wake after airfoil
            Pl = 0.    
         
            Ql = -a3*sgn(dble(j)-2.)*exp(-c3*abs(j-2.))
            Ql = Ql*(1-(tanh(rs3*(iw-1-(3*iw+2*ia+1-i)+iw+ia+1))/tanh(rs3*(iw-1))))
         
         end if
      else ! wake before airfoil
         Pl = 0.     
         
            Ql = -a3*sgn(dble(j)-2.)*exp(-c3*abs(j-2.))
            Ql = Ql*(1-(tanh(rs3*(iw-1-i+1))/tanh(rs3*(iw-1))))
         
      end if

      
      if (j < nodes) then ! Blending function for the two Ps and Qs
         b = 1. ! Let orthogonality stronger for the first few lines
   
      elseif (i > iw) then
         if(i<(iw+ia+1)) then
            b = exp(-dble(j-nodes)/jmax/ortho_f )
         else
            b = exp(-dble(j-nodes)/jmax/ortho_f )
         end if
      else
         b = exp(-dble(j-nodes)/jmax/ortho_f )
      end if
      ! b = 0.
      p = b*1.*p0(i,1)+ pl
      q = b*1.*q0(i,1) + ql

      alpha = 0.25*((x2(i,j+1)-x2(i,j-1))**2 + (y2(i,j+1)-y2(i,j-1))**2)
      gamma = 0.25*((x2(i+1,j)-x2(i-1,j))**2 + (y2(i+1,j)-y2(i-1,j))**2)
      betta = 0.25*((x2(i+1,j)-x2(i-1,j))*(x2(i,j+1)-x2(i,j-1)) &
                  +(y2(i+1,j)-y2(i-1,j))*(y2(i,j+1)-y2(i,j-1)))
      delta = 0.0625*((x2(i+1,j)-x2(i-1,j))*(y2(i,j+1)-y2(i,j-1)) &
                     -(x2(i,j+1)-x2(i,j-1))*(y2(i+1,j)-y2(i-1,j)))**2

      rx = (alpha*(x2(i-1,j)-2.*x2(i,j)+x2(i+1,j)) &
                  - 0.5*betta*(x2(i+1,j+1)-x2(i-1,j+1)-x2(i+1,j-1)+x2(i-1,j-1)) + gamma*(x2(i,j-1)-2.*x2(i,j)+x2(i,j+1)) &
                  + 0.5*delta*P*(x2(i+1,j)-x2(i-1,j)) + 0.5*delta*Q*(x2(i,j+1)-x2(i,j-1)))
      
      ry = (alpha*(y2(i-1,j)-2.*y2(i,j)+y2(i+1,j)) &
                  - 0.5*betta*(y2(i+1,j+1)-y2(i-1,j+1)-y2(i+1,j-1)+y2(i-1,j-1)) + gamma*(y2(i,j-1)-2.*y2(i,j)+y2(i,j+1)) &
                  + 0.5*delta*P*(y2(i+1,j)-y2(i-1,j)) + 0.5*delta*Q*(y2(i,j+1)-y2(i,j-1)))

      r = (abs(rx) + abs(ry))/2. + r      
      end do
   end do

   rbar(n) = r/((imax-2)*(jmax-2))
   if (real(n)/10. == real(n/10)) then
      write(*,700) n, rbar(n)
      700 format (I10.10,1x,E15.8)
   end if
   if (isnan(rbar(n))) then
      print *, 'NaN found in residual, exit'
      finaliteration = n
      exit
   end if
   if (rbar(n) > rbar(n-1)) then ! Check to see if residuals keep increasing
      if (rbar(n-1) > rbar(n-2)) then
      if (rbar(n-2) > rbar(n-3)) then
         if (rbar(n-3) > rbar(n-4)) then
            if (rbar(n-4) > rbar(n-5)) then
            if (rbar(n-5) > rbar(n-6)) then
               if (rbar(n-6) > rbar(n-7)) then
                  print *, 'Residual increasing, exit'
                  finaliteration = n
                  exit
               end if
            end if
            end if
         end if
      end if
      end if
   end if
   if (rbar(n) == rbar(n-1)) then ! Check to see if residuals are stuck
      if (rbar(n-1) == rbar(n-2)) then
      if (rbar(n-2) == rbar(n-3)) then
         if (rbar(n-3) == rbar(n-4)) then
            print *, 'Residual stuck, exit'
            finaliteration = n
            exit
         end if
      end if
      end if
   end if
   if (rbar(n) <= tolerance) then ! Check convergent criterion
      finaliteration = n
      exit
   end if
   if (real(n)/1000. == real(n/1000)) then ! write to test file every 1000 iterations
      open(unit=5, file='test_airfoil.dat',status = 'replace')
      write(5,400) 
      write(5,300) imax, jmax
      do j = 2, jmax+1
      do i = 1, imax
         write(5,200) x2(i,j), y2(i,j)
      end do
      end do  
      close(5)
   end if 
   end do

   ! write mesh
   open(unit=2, file=(file_name4), status='replace')
   write(2,400) 
   400 format ('VARIABLES = "x", "y"')
   write(2,300) imax, jmax
   300 format ('ZONE F=POINT, I=', I4, ', J=', I4)
   do j = 2, jmax+1
      do i = 1, imax
      write(2,200) x2(i,j), y2(i,j)
      200 format (E15.8,1x,E15.8)
      end do
   end do
   close(2)
   print *, 'Mesh written'
   ! write residual
   open(unit=15, file=(file_name5), status='replace')
   write(15,500) 
   500 format ('"Iteration", "Residual"')
   do n = 1, finaliteration
      write(15,600) n, rbar(n)
      600 format (I10.10,1x,E15.8)
   end do
   close(15)
   print *, 'Residuals written'

   ! 900 continue
   

   return
end subroutine

subroutine grid_property(imax,jmax,imax_airfoil,NACA)
   integer, intent(in) :: NACA, imax_airfoil, imax, jmax

   real, dimension(:,:), allocatable :: x, y, ndis, xn,yn
    
   integer :: i, j
   integer :: ia  ! Nodes over airfoil

   character(len=4) :: naca_number, imax_s, jmax_s,ia_s ! Allocate for integer to character
   character(len=50) :: file_name, file_name2, file_name4, file_name5,file_name7

   real :: x_xi, y_xi, x_eta, y_eta, L2Norm
   real, dimension(:,:), allocatable :: g12, theta, theta_dev, h_xi, h_eta, thd
   real :: a1,a2,a3,a4,v1x,v1y,v2x ,v2y ,v3x,v3y ,v4x ,v4y ,v5x ,v5y ,v6x ,v6y ,v7x ,v7y ,v8x ,v8y , amax,amin
   real, dimension(:,:), allocatable :: a, skewness, skt
   real :: v1,v2,v5,v6
   real, dimension(:,:), allocatable :: aspect_ratio, ast
   real, dimension(:,:), allocatable :: cell_area
   real, dimension(:,:), allocatable :: i_space, j_space
   real, dimension(:,:), allocatable :: i_gr, j_gr

   ! Plot3D for Fluent
   integer,parameter :: nbl=1
   integer,dimension(nbl) :: idim1,kdim 
   real, dimension(:,:), allocatable :: x2,y2

   real, dimension(:,:), allocatable :: x3,y3

   ia = imax_airfoil
   write(naca_number,'(i0.4)') NACA ! Write  NACA number into string
   print *, 'The airfoil is NACA',naca_number
   write(imax_s,'(i0.4)') imax ! Write number into string
   print *, 'imax is : ',imax_s ! Used for file imax*jmax
   write(jmax_s,'(i0.4)') jmax ! Write number into string
   print *, 'jmax is : ',jmax_s ! Used for file imax*jmax
   write(ia_s,'(i0.4)') ia ! Write number into string
   print *, 'Node over airfoil is : ',ia_s ! Used for reading airfoil file
   file_name = 'mesh_naca'//naca_number//'_'//imax_s//'x'//jmax_s//'.dat'  ! Define file names 
   file_name2 = 'theta_naca'//naca_number//'_'//imax_s//'x'//jmax_s//'.dat'
   file_name4 = 'cell_data_naca'//naca_number//'_'//imax_s//'x'//jmax_s//'.dat'
   file_name5 = 'ij_node_data_naca'//naca_number//'_'//imax_s//'x'//jmax_s//'.dat'
   file_name7 = 'mesh_fluent_naca'//naca_number//'_'//imax_s//'x'//jmax_s//'.grd'

   allocate(x(imax+2,jmax+2), y(imax+2,jmax+2)) ! Allocate Matrix
   allocate(ndis(ia,1)) ! Allocate for j2 j3 calculation
   allocate(xn(imax,1),yn(imax,1)) ! Allocate for j2 j3 calculation normal line
   allocate(theta(imax+2,jmax+2),theta_dev(imax+2,jmax+2),g12(imax+2,jmax+2))
   allocate(h_eta(imax+2,jmax+2),h_xi(imax+2,jmax+2),thd(imax+2,jmax+2))
   allocate(a(4,1),skt(2,1),skewness(imax-1,jmax-1))
   allocate(ast(4,1),aspect_ratio(imax-1,jmax-1))
   allocate(cell_area(imax-1,jmax-1))
   allocate(i_space(imax+2,jmax+2),j_space(imax+2,jmax+2))
   allocate(i_gr(imax+2,jmax+2),j_gr(imax+2,jmax+2))
   allocate(x2(imax,jmax), y2(imax,jmax))
   allocate(x3(imax,jmax+8), y3(imax,jmax+8))

   open (unit=7, file=trim(file_name), status='old', action='read') ! Read file
   do i = 1,2 ! Allows the read function to skip first 2 lines
      read(7,*)
   end do
   do j = 2,jmax+1
   do i = 2,imax+1  
      read(7,100) x(i,j), y(i,j)
      100 format(E15.8,1x,E15.8)
   end do
   end do 
   close(7)
   ! i = 1 and imax+2
   do j = 2,jmax+1  ! Write into ghost cell
      x(1,j) = x(2,j)
      y(1,j) = y(2,j)
      x(imax+2,j) = x(imax+1,j)
      y(imax+2,j) = y(imax+1,j)
   end do
   ! j = 1 and jmax+2
   do i = 2,imax+1
      x(i,1) = x(i,2)
      y(i,1) = y(i,2)
      x(i,jmax+2) = x(i,jmax+1)
      y(i,jmax+2) = y(i,jmax+1)
   end do
   print *, 'Mesh file located and read'
   print *, 'Start computation'
   
   h_xi = 0.
   h_eta= 0.
   g12 = 0.
   theta = 0.
   theta_dev = 0.
   thd = 0.
   ! Compute orthogonality
   open(unit=3, file=trim(file_name2), ACTION="write", STATUS="replace")
      write(3,*) 'VARIABLES = "x", "y", "theta", "g12","theta dev" '
      write(3,*) 'ZONE I=', imax,   ', J=', jmax, 'F=POINT'
      do j = 2,jmax+1
         do i = 2,imax+1
         
         x_xi  = 0.5*(x(i+1,j)-x(i-1,j))
         y_xi  = 0.5*(y(i+1,j)-y(i-1,j))
         x_eta = 0.5*(x(i,j+1)-x(i,j-1))
         y_eta = 0.5*(y(i,j+1)-y(i,j-1))
         h_xi(i,j) = sqrt(x_xi**2+y_xi**2)
         h_eta(i,j)= sqrt(x_eta**2+y_eta**2)
         g12(i,j) = x_xi*x_eta+y_xi*y_eta
         theta(i,j) = acos(g12(i,j)/h_xi(i,j)/h_eta(i,j))
         theta_dev(i,j) = 0.5*pi-theta(i,j)
         thd(i,j) = (theta_dev(i,j)*180/pi)**2

         WRITE(3,*) x(i,j), y(i,j), theta(i,j)*180/pi, g12(i,j),abs(theta_dev(i,j)*180/pi)
         enddo
      enddo
   close(3)
   L2Norm = sqrt(sum(thd))

   ! compute skewness
   skewness = 0.
   do j = 2,jmax
      do i = 2,imax
         a = 0.
         skt = 0.
         v1x = x(i,j+1)-x(i,j) !v1 = (i,j+1)-(i,j)
         v1y = y(i,j+1)-y(i,j)
         v2x = x(i+1,j)-x(i,j) !v2 = (i+1,j)-(i,j)
         v2y = y(i+1,j)-y(i,j)
         v3x = x(i,j) - x(i,j+1) !v3 = (i,j) - (i,j+1)
         v3y = y(i,j) - y(i,j+1)
         v4x = x(i+1,j+1) - x(i,j+1) !v4 = (i+1,j+1) - (i,j+1)
         v4y = y(i+1,j+1) - y(i,j+1)
         v5x = x(i,j+1) - x(i+1,j+1) !v5 = (i,j+1) - (i+1,j+1)
         v5y = y(i,j+1) - y(i+1,j+1)
         v6x = x(i+1,j) - x(i+1,j+1) !v6 = (i+1,j) - (i+1,j+1)
         v6y = y(i+1,j) - y(i+1,j+1)
         v7x = x(i+1,j+1)-x(i+1,j) !v7 = (i+1,j+1)-(i+1,j)
         v7y = y(i+1,j+1)-y(i+1,j)
         v8x = x(i,j)-x(i+1,j) !v8 = (i,j)-(i+1,j)
         v8y = y(i,j)-y(i+1,j)
         a1 = acos( (v1x*v2x+v1y*v2y)/(sqrt(v1x**2+v1y**2)*sqrt(v2x**2+v2y**2)) )
         a2 = acos( (v3x*v4x+v3y*v4y)/(sqrt(v3x**2+v3y**2)*sqrt(v4x**2+v4y**2)) )
         a3 = acos( (v5x*v6x+v5y*v6y)/(sqrt(v5x**2+v5y**2)*sqrt(v6x**2+v6y**2)) )
         a4 = acos( (v7x*v8x+v7y*v8y)/(sqrt(v7x**2+v7y**2)*sqrt(v8x**2+v8y**2)) )
         a(1,1) = a1
         a(2,1) = a2
         a(3,1) = a3
         a(4,1) = a4
         amax = maxval(a)
         amin = minval(a)
         skt(1,1) = (amax-pi/2)/(pi/2)
         skt(2,1) = (pi/2-amin)/(pi/2)
         skewness(i-1,j-1) = maxval(skt)
      end do
   end do
   aspect_ratio = 0.
   ! Comput aspect ratio
   do j = 2,jmax
      do i = 2, imax
         ast = 0.
         v1x = x(i,j+1)-x(i,j) !v1 = (i,j+1)-(i,j)
         v1y = y(i,j+1)-y(i,j)
         v2x = x(i+1,j)-x(i,j) !v2 = (i+1,j)-(i,j)
         v2y = y(i+1,j)-y(i,j)
         v5x = x(i,j+1) - x(i+1,j+1) !v5 = (i,j+1) - (i+1,j+1)
         v5y = y(i,j+1) - y(i+1,j+1)
         v6x = x(i+1,j) - x(i+1,j+1) !v6 = (i+1,j) - (i+1,j+1)
         v6y = y(i+1,j) - y(i+1,j+1)
         v1 = sqrt(v1x**2+v1y**2)
         v2 = sqrt(v2x**2+v2y**2)
         v5 = sqrt(v5x**2+v5y**2)
         v6 = sqrt(v6x**2+v6y**2)
         ast(1,1) = v1
         ast(2,1) = v2
         ast(3,1) = v5
         ast(4,1) = v6
         aspect_ratio(i-1,j-1) = maxval(ast)/minval(ast)
      end do
   end do
   ! Calculate cell area by cross product
   cell_area = 0.
   do j = 2,jmax
      do i = 2, imax
         v1x = x(i,j+1)-x(i,j) !v1 = (i,j+1)-(i,j)
         v1y = y(i,j+1)-y(i,j)
         v2x = x(i+1,j)-x(i,j) !v2 = (i+1,j)-(i,j)
         v2y = y(i+1,j)-y(i,j)
         cell_area(i-1,j-1) = sqrt((v1x*v2y-v2x*v1y)**2) 
      end do
   end do
   open(unit=5, file=trim(file_name4), ACTION="write", STATUS="replace") ! output results
      write(5,*) 'TITLE = "Cell Center Data"'
      write(5,*) 'VARIABLES = "x", "y", "Aspect Ratio" ,"Skewness","Cell Area"'
      write(5,*) 'ZONE I=', imax,   ', J=', jmax
      write(5,*) 'DATAPACKING=BLOCK'
      write(5,*) 'VARLOCATION=([3]=CELLCENTERED,[4]=CELLCENTERED,[5]=CELLCENTERED)'
      do j = 2,jmax+1
         do i = 2,imax+1
         WRITE(5,*) x(i,j)
         enddo
      enddo
      do j = 2,jmax+1
         do i = 2,imax+1
         WRITE(5,*) y(i,j)
         enddo
      enddo
      do j = 2,jmax
         do i = 2,imax
         WRITE(5,*) aspect_ratio(i-1,j-1)
         enddo
      enddo
      do j = 2,jmax
         do i = 2,imax
         WRITE(5,*) skewness(i-1,j-1)
         enddo
      enddo
      do j = 2,jmax
         do i = 2,imax
         WRITE(5,*) cell_area(i-1,j-1)
         enddo
      enddo
   close(5)
   print *, 'file 1'

   ! Calculate i node spacing
   i_space = 0.
   j_space = 0.
   i_gr = 0.
   j_gr = 0.
   do j = 2,jmax+1
      do i = 2, imax
         i_space(i+1,j) = sqrt((x(i,j)-x(i+1,j))**2+ (y(i,j)-y(i+1,j))**2)
      end do
   end do
   i_space(2,:) = i_space(3,:)
   ! Calculate j node spacing
   do i = 2,imax+1
      do j = 2, jmax
         j_space(i,j+1) = sqrt((x(i,j)-x(i,j+1))**2+ (y(i,j)-y(i,j+1))**2)
      end do
   end do
   j_space(:,2) = j_space(:,3)

   ! Calculate i node spacing
   do j = 2,jmax+1
      do i = 2, imax-1
         i_gr(i+1,j) = i_space(i+2,j)/i_space(i+1,j)
      end do
   end do
   i_gr(2,:) = i_gr(3,:)
   i_gr(imax+1,:) = i_gr(imax,:)
   ! Calculate j node spacing
   do i = 2,imax+1
      do j = 2, jmax-1
         j_gr(i,j+1) = j_space(i,j+2)/j_space(i,j+1)
      end do
   end do
   j_gr(:,2) = j_gr(:,3)
   j_gr(:,jmax+1) = j_gr(:,jmax)

   open(unit=7, file=trim(file_name5), ACTION="write", STATUS="replace") ! output results
      write(7,*) 'TITLE = "i j spacing"'
      write(7,*) 'VARIABLES = "x", "y", "i node spacing","j node spacing", "i growth rate", "j growth rate" '
      write(7,*) 'ZONE I=', imax,   ', J=', jmax
      write(7,*) 'DATAPACKING=BLOCK'
      do j = 2,jmax+1
         do i = 2,imax+1
         WRITE(7,*) x(i,j)
         enddo
      enddo
      do j = 2,jmax+1
         do i = 2,imax+1
         WRITE(7,*) y(i,j)
         enddo
      enddo
      do j = 2,jmax+1
         do i = 2, imax+1
            WRITE(7,*) i_space(i,j)
         end do
      end do
      do j = 2,jmax+1
         do i = 2, imax+1
            WRITE(7,*) j_space(i,j)
         end do
      end do
      do j = 2,jmax+1
         do i = 2, imax+1
            WRITE(7,*) i_gr(i,j)
         end do
      end do
      do j = 2,jmax+1
         do i = 2, imax+1
            WRITE(7,*) j_gr(i,j)
         end do
      end do
   close(7)
   print *, 'file 2'

   ! Write into Fluent import format
   do j = 1,jmax
      do i = 1,imax
         x2(i,j) = x(i+1,j+1)
         y2(i,j) = y(i+1,j+1)
      end do
   end do

   idim1 = imax !# of grid pts in x
   kdim = jmax !# of grid pts in z

   open (unit = 24,file=trim(file_name7), action= 'write',status='replace') ! Output file in format for Fluent
      write (24,*) nbl
      write (24,*) idim1(1),kdim(1)

      write (24,*) ((x2(i,j),i=1,idim1(1)),j=1,kdim(1)),&
      ((y2(i,j),i=1,idim1(1)),j=1,kdim(1)) !write grid into plot3d format
   close(24)


   print *,  'Grid Property Generation Complete'
end subroutine grid_property

double precision function sgn(k)
   implicit none
   double precision, intent(in) :: k
   if (k > 0.) then
   sgn = 1.0
   else if (k == 0.) then
   sgn = 0.0
   else
   sgn = -1.0
   end if
end function sgn
