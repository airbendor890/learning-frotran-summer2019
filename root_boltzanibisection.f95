program realroot
implicit none
real::fx,a,b,r
print*,'give the isolation interval a and b '
read*,a
read*,b
call computation(r,a,b,fx)

print*,'root is  ',r


end program realroot

function fx(x)
implicit none
real::fx,x
!function name= expression
fx=x*3-2*x-5
return

end function fx

recursive subroutine computation(r,a,b,fx)
implicit none
real::r,a,b,fx
r=.5*(a+b)
if( abs(fx(r))<0.00000001 ) then
  !r is the root
  stop
  
else if(fx(a)*fx(r)<0) then
  !set b=r
  b=r
  pause
  call computation(r,a,b,fx)
  else 
    a=r
    call computation(r,a,b,fx)
end if

end subroutine computation