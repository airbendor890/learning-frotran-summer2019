program main
implicit none
integer::n,i
real,allocatable,dimension(:)::d,a,b,c
print*,'give n '
read*,n
allocate(d(n),a(n),c(n),b(n))
print*,'give respective vectors'
print*,'give diagonal vector'

do i=1,n
  read*,d(i)
end do

print*,'give superdiagonal vector with a(n)=0 '

do i=1,n
  read*,a(i)
end do

print*,'give subdiagonal vector with c(1)=0 '

do i=1,n
  read*,c(i)
end do

print*,'give right side vector '

do i=1,n
  read*,b(i)
end do


call THOMAS(n,d,a,b,c)

print*,d
print*,b
print*,a
print*,c

end program main
!///////////////////////////////////////////////////////////////////////////////////////
subroutine THOMAS(n,d,a,b,c)

!n=number of unknowns in tridiagonal system(input)
!d=diagonal vector(input)
!a=suprediagonal vector with a(n)=0(input)
!c=subdiagonal vector with c(1)=0 input
!b=right hand side vector  (input)
!the solution is returned in b  (output)

real::d(n),a(n),c(n),b(n)
integer::k
real::ratio
!elimination begins-------------------------------
do k=1,n-1
  
  if(d(k)==0)then
    print*,'no solution'
    return
  end if
  
  ratio=c(k+1)/d(k)
  d(k+1)=d(k+1)-(ratio*a(k))
  b(k+1)=b(k+1)-(ratio*b(k))

end do

if(d(n)==0)then
	print*,'no solution'
    return
end if

print*,d
print*,b
print*,a
 print*,'press enter'
 pause

 
!back substitution begins
b(n)=b(n)/d(n)
do k=n-1,1,-1
  b(k)=(b(k)-a(k)*b(k+1))/d(k)
end do
return
end subroutine THOMAS  	

    