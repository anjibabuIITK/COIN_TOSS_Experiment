PROGRAM die
IMPLICIT NONE
REAL  :: x,Prob_n1,Prob_n2,Prob_n3,Prob_n4,Prob_n5,Prob_n6
INTEGER :: n1,n2,n3,n4,n5,n6,Ntimes,numbr,i
!Initilization
 n1=0;n2=0;n3=0;n4=0;n5=0;n6=0
print*, "Enter No of trails : "
read*, Ntimes
Do i=1,Ntimes
   call random_number(x)              !genarate random number x
   numbr=INT(x*6.0+1.0)               !number=rand*(MAX_value-Min_value+1.0)+Min_value
   SELECT CASE (numbr)
    CASE (1) 
      n1=n1+1
    CASE (2)   
      n2=n2+1
    CASE (3)   
     n3=n3+1
    CASE (4)   
     n4=n4+1
    CASE (5)   
     n5=n5+1
    CASE (6) 
     n6=n6+1
    CASE DEFAULT
    PRINT*, "ERROR in random number genaration "
   END SELECT
!print*, numbr ,n1,n2,n3,n4,n5,n6
end do
Prob_n1=n1/float(Ntimes)
Prob_n2=n2/float(Ntimes)
Prob_n3=n3/float(Ntimes)
Prob_n4=n4/float(Ntimes)
Prob_n5=n5/float(Ntimes)
Prob_n6=n6/float(Ntimes)

WRITE(*,"(F5.3)")Prob_n1,Prob_n2,Prob_n3,Prob_n4,Prob_n5,Prob_n6

END PROGRAM die
