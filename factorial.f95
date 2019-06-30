program main  
implicit none
integer n,f
print*,'give n '
read*,n
f=1
call fact(n,f)
print*,'   ',f

end program main

recursive subroutine fact(n,f)
integer n,f
if(n==0 .or. n==1)then
	return
else
  f=f*n
  call fact(n-1,f)
 end if 
end subroutine fact