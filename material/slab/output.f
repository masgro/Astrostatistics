      subroutine output
      implicit none

      integer l
      include 'arrays.txt'
      include 'blob.txt'

c    change numbers to normalised energies and intensities
      do l=1,mubins
         intensity(l)=energy(l)/
     $   (2.*nphotons*cos(theta(l)*3.14159265/180.))*mubins
	 sigmai(l)=sqrt(erri(l))/nphotons
	 energy(l)=energy(l)/nphotons
      end do

      do l=1,nlevel+1
         jplus(l)=jplus(l)/nphotons
         jminus(l)=jminus(l)/nphotons
         hplus(l)=hplus(l)/nphotons
         hminus(l)=hminus(l)/nphotons
         kplus(l)=kplus(l)/nphotons
         kminus(l)=kminus(l)/nphotons
      end do

      open(unit=15,file='intensity.dat',status='unknown')
      do l=mubins,1,-1
	 write(15,900) theta(l),energy(l),sigmai(l),intensity(l)
      end do
      close(15)

      open(unit=15,file='moments.dat',status='unknown')
      do l=1,nlevel+1
         write(15,800) jplus(l),jminus(l),hplus(l),hminus(l),
     $                 kplus(l),kminus(l)
      end do
      close(15)


800   format(6(1x,1pe10.3))
900   format(f6.1,4(1x,1pe10.3))

      return
      end
