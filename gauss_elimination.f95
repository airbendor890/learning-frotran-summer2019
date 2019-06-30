subroutine GAUSS(n,a,b)
!n=number of unknowns and equations(input)
!a=n*n matrix0f coeeficients
!b=right hand side vector
!the solution is returned in vector b (output)
!s(i)=size of row i of matrix a.

real::a(n,n),b(n),s(n)
integer::p,q

!initialise s  *****************************************
do i=1,n
  s(i)=0
 do j=1,n 
  absij=abs(a(i,j))
  if(absij>s(i)) then
    s(i)=absij
  end if
 end do
end do

!elimination begins  **********************************

do k=1,n-1
  !determine pivot row p *******************
  absaks=abs(a(k,k))/s(k)
  p=k
  do i=k+1,n
    absais=abs(a(i,k))/s(i)
    if(absais<=absaks) then
      exit
    end if
    absaks<=absais
    p=i
  end do
  if(absaks==0.) then
    print*,'NO solution'
    return
  end if
  
  if(p/=k)then   !interchange row kand row p.
   do q=k,n
     temp=a(k,q)
     a(k,q)=a(p,q)
     a(p,q)=temp
   end do 
   temp=b(k)
   b(k)=b(p)
   b(p)=temp
  end if
  !eliminate
  do i=k+1,n
    ratio=a(i,k)/a(k,k) 
    b(i)=b(i)-ratio*b(k)
    do j=k+1,n   
	 	a(i,j)=a(i,j)-ratio*a(k,j)
    end do
  end do   
end do

if(a(n,n)==0) then
  print*,'no solution'
  return	
end if

!back substitution begins***************
b(n)=b(n)/a(n,n)
do i=n-1,1,-1
  sum=0
  do j=i+1,n
  	sum=sum+a(i,j)*b(j)
  end do
  b(i)=(b(i)-sum)/a(i,i)
end do

return
  


end subroutine GAUSS
  


  