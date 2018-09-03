      program planepar
c**********************************************************************
c        program follows photons through a plane parallel atmosphere.        
c**********************************************************************

      implicit none

      real taumax,tau,s,albedo
      real ran2
      real sint,cost,sinp,cosp,phi,xtot,ytot,ztot,x1,y1,z1,x2,y2,z2
      integer i,l,aflag

      include 'arrays.txt'
      include 'blob.txt'
      include 'random.txt'

c ... load parameters. Those not passed as arguments are in common blocks.
      namelist/slab/nphotons,i1,mubins,taumax,nlevel,albedo
      open(5,file='slab.par',status='unknown')
      read(5,slab)
      close(5)

c     initialize arrays
      call iarray

c******************** do loop over each stellar photon ****************

      do i=1,nphotons

	 if (mod(i,10000).eq.0) then
	     print *,i,' stellar photons completed'
	 endif

100      continue
	 
	 call newphoton(sint,cost,sinp,cosp,phi,xtot,ytot,ztot)

	 aflag=0

c     photon scatters at random angles and tau's until it exits
	 do while ((ztot.ge.0.).and.(ztot.le.1.))
	    x1=xtot
            y1=ytot
            z1=ztot
            tau=-alog(ran2(i1))
            s=tau/taumax
            xtot=xtot+s*sint*cosp
            ytot=ytot+s*sint*sinp
            ztot=ztot+s*cost
            x2=xtot
            y2=ytot
            z2=ztot
            call moments(x1,y1,z1,x2,y2,z2,cost,nlevel)
            if((ztot.lt.0.).or.(ztot.gt.1.)) goto 20
	    if(ran2(i1).lt.albedo) then
	       call isoscatt(sint,cost,sinp,cosp,phi)
	    else
	       aflag=1
	       go to 20
	    end if
         end do

20      continue

c    test whether photon is "inside star", if it is start a new photon
	if(ztot.lt.0.) then
	     go to 100
	end if

	if(aflag.eq.0) then
	   l=int(mubins*cost)+1
	   erri(l)=erri(l)+1.
	   energy(l)=energy(l)+1.
        end if

      end do
      
c     ********************************************************

c     put results into intensities.
      call output

      end
