!det=a11*cof(a11)+..........+a1N*cof(a1N)
!cof(aij)=(-1)**i+j * det(minor(aij))
!minor contains N2-2N+1 elemnts
program determinantN
    !required variables and allocation
        real,allocatable,dimension(:,:)::mat
        integer::size,row,col
        real::determinant,cofactor,det
     !ask for size of square matrix
         print*,'give size of squre matrix'
         read*,size
         allocate(mat(size,size))
     !bring in matrix
        open(9,file="file2.txt")
	    do row=1,size
		do col=1,size
			read(9,*)mat(row,col)
		end do
	    end do		
     !echo matrix
     print*,'MAT->'
     call outputra(size,mat)
     
     !output determinant
     det=determinant(mat,size)
     
     print*,'det= ',det
     
end program determinantN

!subroutines and functions
function cofactor(i,j,mat,size)
    implicit none
    integer::i,j,size,row,col
    real,dimension(size,size)::mat
    real::minor(size-1,size-1)
    real::cofactor,determinant
    open(12,file="file1.txt")
  	do row=1,size
  	    do col=1,size
  	        if(row/=i .and. col/=j)then
  	        write(12,*)mat(row,col)
  	        end if
  	    end do
  	end do 
  	!read file1->minor(2,2)
  	close(12,status="KEEP")
  	open(12,file="file1.txt")
  	do row=1,size-1
  	    do col=1,size-1
            read(12,*)minor(row,col)
  	    end do
  	end do 
  	close(12,status="DELETE")
  	
    cofactor=((-1)**(i+j))*determinant(minor,size-1)
    
    
    
    
end function cofactor

function determinant(mat,size)
    implicit none
    integer::size,row,col
    real::mat(size,size),cofactor,determinant,temp
    if(size==1)then
    determinant=mat(1,1)
    else
        temp=0.0
         row=1
            do col=1,size
                temp=temp+mat(row,col)*cofactor(row,col,mat,size)
            end do
        determinant=temp
    end if
    

end function determinant

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
