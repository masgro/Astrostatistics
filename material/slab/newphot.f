      subroutine newphoton(sint,cost,sinp,cosp,phi,xtot,ytot,ztot)
      implicit none

      include 'random.txt'

      real sint,cost,sinp,cosp,phi,xtot,ytot,ztot,ran2

c *************************************************************************
c ************** emit photons isotropically from the origin ***************
      cost=sqrt(ran2(i1))
      sint=sqrt(1.-cost*cost)
      phi=2.*3.14159265*ran2(i1)
      cosp=cos(phi)
      sinp=sin(phi)
      xtot=0.
      ytot=0.
      ztot=0.
c *************************************************************************

      return
      end
