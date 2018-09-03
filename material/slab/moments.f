      subroutine moments(x1,y1,z1,x2,y2,z2,cost,nlevel)
c **************************************************************************
C this subroutine calculates the intensity moments at (nlevel+1) levels in 
c a plane parallel atmosphere.  the moments are split into plus and 
c minus components.  this gives the contribution to the J, H, and K 
c moments from photons travelling up and down.  the actual moments are 
c simply the sum of the plus and minus arrays -- i.e. j(i)=jplus(i)+jminus(i)
c ***************************************************************************

      implicit none
      include 'arrays.txt'

      integer i,l1,l2,nlevel
      real x1,y1,z1,x2,y2,z2,cost

      if((z1.gt.0.).and.(z2.gt.0.).and.
     $   (int(z1*nlevel).eq.int(z2*nlevel))) return

      if(cost.gt.0.) then
        if(z1.le.0.)then
          l1=1
        else
          l1=int(z1*nlevel)+2
        end if
        if(z2.ge.1.) then
          l2=nlevel+1
        else
          l2=int(z2*nlevel)+1
        end if
        do i=l1,l2
           jplus(i)=jplus(i)+1./cost
           hplus(i)=hplus(i)+1.
           kplus(i)=kplus(i)+cost
        end do
      elseif(cost.lt.0.) then
        l1=int(z1*nlevel)+1
        if(z2.le.0.)then
          l2=1
        else
          l2=int(z2*nlevel)+2
        end if
        do i=l2,l1
           jminus(i)=jminus(i)+1./(abs(cost))
           hminus(i)=hminus(i)-1
           kminus(i)=kminus(i)+abs(cost)
        end do
      end if

      return
      end
