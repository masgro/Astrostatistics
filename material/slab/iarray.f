	subroutine iarray
        implicit none

c	initializes all the arrays

        integer l,j,rj
        real dthet,halfw
	include 'arrays.txt'
	include 'blob.txt'

	 do l=1,nmu
	  energy(l)=0.
          sigmai(l)=0.
          erri(l)=0.
          intensity(l)=0.
	 end do

         do l=1,nlev
          jplus(l)=0.
          jminus(l)=0.
          hplus(l)=0.
          hminus(l)=0.
          kplus(l)=0.
          kminus(l)=0.
         end do

c      mubins = number of bins in range 0--90 degrees -- dthet=1/mubins
	dthet=1./mubins
	halfw=.5*dthet
	do j=1,mubins
	  rj=j-1
	  theta(j)=acos(rj*dthet+halfw)*180./3.14159265
	end do
	
	return
	end
