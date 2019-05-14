!Write a program that will read a 3 X 3 matrix from a data file. 
!In the program, include a subroutine that will generate any cofactor cof of the matrix mat. Call the subroutine cofactor and use these arguments:

 program gcofactor
 implicit none
 !required vriables and allocation
    real,dimension(3,3)::mat
    integer::i,j,row,col
    print*,'enter row and col for finding its cofactor'
    read*,i,j 
 
 
 !read matrix
    open(13,file='file2.txt')
    do row=1,3
    do col=1,3
        read(13,*)mat(row,col)
    end do
    end do
 !echo matrix
    print*,"mat"
    call outputra(3,mat)
 
 
 !call subroutine for cofactor
    call cofactor(i,j,mat)
 
 
 end program gcofactor
 
 
 !subroutine and funcn
 
  	subroutine cofactor(i,j,mat)
  	implicit none
  	real :: mat(3,3),minor(2,2),cof
  	integer :: elrow,elcol,i,j
  	! cof – the cofactor of matrix mat for element i,j
  	!first find minor 
  	!write valid element matrix->file1.txt
  	open(12,file="file1.txt")
  	do elrow=1,3
  	    do elcol=1,3
  	        if(elrow/=i .and. elcol/=j)then
  	        write(12,*)mat(elrow,elcol)
  	        end if
  	    end do
  	end do 
  	!read file1->minor(2,2)
  	close(12,status="KEEP")
  	open(12,file="file1.txt")
  	do elrow=1,2
  	    do elcol=1,2
  	    read(12,*)minor(elrow,elcol)
  	    end do
  	end do  
  	
  	close(12,status="DELETE")        
	!cof=(-1)**i+j*minor
    cof=((-1)**(i+j))*(minor(1,1)*minor(2,2)-minor(2,1)*minor(1,2))
    
    print*,"cofactor(",i,j,")=  ",cof
    end subroutine cofactor
    
     subroutine  outputra(size,ra)
  implicit none
  !will output a real square array nicely
  integer                               :: size,row,col
  real,dimension(size,size)             :: ra
  character                             :: reply*1
  do row =1,size
     write(*,10) (ra(row,col),col=1,size)   
     10    format(100f10.2)
     !as we don't know how many numbers are to be output, 
     !specify  !more than we need - the rest are ignored
  end do
  print*,'__________________________________________________'
  print*,'Hit a key and  press enter to continue'
  read *,reply
  end subroutine  outputra
  !---------------------------------------------------------
  subroutine  fill_array(size,ra)
  implicit none                         
  !fills the array by prompting from keyboard
  integer        :: row,col,size
  real           :: num
  real, dimension(size,size) :: ra
  do row=1,size
     do col=1,size
       print *, row,col
       read *,num
       ra(row,col)=num
     end do
  end do
  end subroutine  fill_array          
