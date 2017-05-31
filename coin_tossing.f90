!Fortran forgram for tossing a coin of N trails and It will genarate a Histogram of the data in file.
!Authour:   ANJI BABU KAPAKAYALA
!           IIT KANPUR, INDIA
PROGRAM coin_tossing
   INTEGER :: head,tail,i,N,r,nbin,bin,j
   REAL*8::prob_h,prob_t,avg_r,u,r0,x,rmax,rmin,width
   REAL*8, allocatable::prob(:)
   CHARACTER(len=7) :: date
   REAL*8 :: start, finish
   CALL cpu_time(start)
   CALL get_DDMonYY(date)
   PRINT*,"No of tosses ="
   READ*,N
!  PRINT*,"No of tosses given = ",N
   OPEN(1, file = "coin_toss.out")
   OPEN(2, file = "summary.out")
   head=0
   tail=0
   x=0.d0
!========================Histogram input================================================
!we are generating random numbers from zero to 1 so, rmin=-0.5, rmax=1.5 for our convienence

   rmin=-0.5 
   rmax=1.5
   width=0.01

   nbin=nint(((rmax-rmin)/width)) + 1     !calculating Number of Bins 
   PRINT*, 'nbin =',nbin
   ALLOCATE(prob(nbin+1))                 
   prob = 0.0
   OPEN(40,file="histogram.dat")
!==============================writing output into file================================
    call my_details
        DO i = 1,N
           CALL random_number(u)
           r = int((u)+ 0.5) 
!----------------Tossing Coin---------------!
               IF (r==0) THEN
                  tail = tail + 1
                  WRITE(1,*)r,"tail"
               ELSE IF (r==1) THEN
                  head = head + 1
                  WRITE(1,*),r,"head"
               END IF
!------------histogram-----------------------!
           bin =int((r-rmin)/width)+1
              IF(bin <= nbin+1)THEN
                prob(bin) = prob(bin) +1.00
              END IF     
!------------Avarage Value of r---------------!
           x=x+real(r)                                      !Sum of all r vaues 
!          WRITE(*,*)r
        END DO

!--------------------------------
   avg_r = x/real(N)                                        !calculating avarage value 


   prob_h=real(head)/real(N)                                !probability of getting head
   prob_t=real(tail)/real(N)                                !probability of getting tail

!===================== writing output into file=========================================================#

   WRITE(1,*)"==================================================================================#"
   WRITE(1,*)
   WRITE(1,*)"distribution of data to plot histogram"
   WRITE(1,*)
   WRITE(1,*)
   WRITE(1,*)
                do j=1,nbin
                  write(40,*) real(j-1)*width+rmin, prob(j)/dfloat(N) !printing x vs P(x) vaules
                  write(1,*) real(j-1)*width+rmin, prob(j)/dfloat(N) !printing x vs P(x) vaules
                end do
   WRITE(2,*)"==================================================================================="
   WRITE(2,*)
   WRITE(2,*)
   WRITE(2,*)"Summary :"
   WRITE(2,*)
  ! WRITE(2,11)N
  ! 11 FORMAT (1X,"Total No. of Tosses =",2X,I7)
   WRITE(2,*)
   WRITE(2,*)"Total No of Tosses                    =",N
   WRITE(2,*)
   WRITE(2,*)"No of heads                           =", head
   WRITE(2,*)
   WRITE(2,*)"No of tails                           =",tail
   WRITE(2,*)
   WRITE(2,*)"Probability of getting head           ="
   WRITE(2,"(F5.2)")prob_h
   WRITE(2,*)
   WRITE(2,*)"Probability of getting tail           ="
   WRITE(2,"(F5.2)")prob_t
   WRITE(2,*)
   WRITE(2,*)"Total Probability                     =",prob_h+prob_t
   WRITE(2,*)
   WRITE(2,*)"avg value of r                        =",avg_r
   WRITE(2,*)    
   WRITE(2,*)
   WRITE(2,*)
   WRITE(2,*) "================================================================================#"
   WRITE(2,*)
   WRITE(2,*) "As you see in above results,as  r value becomes larger the probability to get head and tail are equal (50:50) &
&.and by looking at above distribution data we can say that this distribution does not follow Gaussian distribution,&
&it follows uniform distribution."
   WRITE(2,*)
   WRITE(2,*)"========================================================================================"
   WRITE(2,*)  "Distribution of data has stored in file called 'histogram.dat'"
   WRITE(2,*)"========================================================================================"
   WRITE(2,*)
   WRITE(2,*)
   WRITE(2,*)
     call cpu_time(finish)
   WRITE(2,*)
   WRITE(2,*)
   WRITE(2,*)"Calculation Time (seconds)= "
   WRITE(2,"(F6.3)")finish-start
   WRITE(2,*)
   WRITE(2,*)
   WRITE(2,*)"Date = ",date
   WRITE(2,*)
   WRITE(2,*)
   WRITE(2,*) "================================================================================#"
   WRITE(2,*)
   WRITE(2,*)
   WRITE(2,*)"Congratulations ! ...... Program executed successfully"
   print '("Time = ",f6.3," seconds.")',finish-start
   PRINT*,
   PRINT*,
   PRINT*,"Congratulations ! ......Your Program executed successfully"
   CLOSE(1)
   CLOSE(2)
   CLOSE(40)
   DEALLOCATE(prob)
END PROGRAM coin_tossing
!================================================================================================
  SUBROUTINE get_DDMonYY(date)
    CHARACTER(len=7), INTENT(out) :: date
    CHARACTER(len=2) :: dd
    CHARACTER(len=3) :: mons(12)
    CHARACTER(len=4) :: yyyy
    INTEGER :: values(8)
    mons = ['Jan','Feb','Mar','Apr','May','Jun',&
      'Jul','Aug','Sep','Oct','Nov','Dec']
    CALL DATE_AND_TIME(VALUES=values)
    WRITE(  dd,'(i2)') values(3)
    WRITE(yyyy,'(i4)') values(1)
    date = dd//mons(values(2))//yyyy(3:4)
  END SUBROUTINE get_DDMonYY
!==================================================================================================
  SUBROUTINE my_details
   WRITE(2,*)
   WRITE(2,*) "####################################################################################################"
   WRITE(2,*) "#                                                                                                  #"
   WRITE(2,*) "#                   Computer programs for analyzing distributions                                  #"
   WRITE(2,*) "#                              KAPAKAYALA ANJI BABU                                                #"
   WRITE(2,*) "#                                IIT KANPUR, INDIA.                                                #"
   WRITE(2,*) "#                                                                                                  #"
   WRITE(2,*) "####################################################################################################"
   WRITE(2,*)
   END SUBROUTINE my_details
!====================================================================================#
!                  Written by ANJI BABU KAPAKAYALA                                   #
!                        IIT KANPUR, INDIA.                                          #
!====================================================================================#




