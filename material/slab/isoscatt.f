      subroutine isoscatt(sint,cost,sinp,cosp,phi)
      implicit none

      include 'random.txt'

      real sint,cost,sinp,cosp,phi,ran2

      cost=2.*ran2(i1)-1.
      sint=sqrt(1.-cost*cost)
      phi=2.*3.14159265*ran2(i1)
      cosp=cos(phi)
      sinp=sin(phi)

      return
      end

