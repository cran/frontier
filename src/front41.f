 
      subroutine front41(
     $  imArg, ipcArg, iceptArg,
     $  nnArg, ntArg, nobArg, nbArg, nmuArg, netaArg,
     $  iprintArg, indicArg, tolArg, tol2Arg, bignumArg,
     $  step1Arg, igrid2Arg, gridnoArg, maxitArg, bmuArg,
     $  mrestartArg, frestartArg, nrestartArg,
     $  nStartVal, startVal, nRowData, nColData, dataTable,
     $  nParamTotal, ob, ga, gb, startLogl, y, h, fmleLogl,
     $  nIter, icodeArg, nfunctArg )
c       FRONTIER version 4.1d by Tim Coelli.   
c       (with a very few contributions by Arne Henningsen)
c       This program uses the Davidon-Fletcher-Powell algorithm to
c       estimate two forms of the stochastic frontier production function.
c       The first is the error components model described in Battese and
c       Coelli (1992) in the JPA, and the second is the TE effects model
c       described in Battese and Coelli (1995), in Empirical Economics.
c       A large proportion of the search, convrg, mini and eta 
c       subroutines are taken from the book: Himmelblau (1972, appendix b). 
c       The remainder of the program is the work of Tim Coelli.   
c       Any person is welcome to copy and use this program free of charge.
c       If you find the program useful, a contribution of A$200 
c       to help defray some of the author's costs would be appreciated, but
c       is in no way obligatory.       
c       Please note that the author takes no responsibility for any
c       inconvenience caused by undetected errors. If an error is 
c       detected the author would appreciate being informed. He may be  
c       contacted via email at tcoelli@metz.une.edu.au .
c       See: Coelli (1996), CEPA Working Papers 96/07, University of New 
c       England, Armidale, NSW 2351, Australia. for details on the use
c       of this program.  
c       last update = 25/April/2008
c       Since version 4.1d, the user might specify the name of the
c       instruction file by an (optional) argument at the command line.
c       Hence, this programme can be run automatically (non-interactively) now.
c
c       nb = number of coefficients of the frontier model (possibly including intercept)
c       nz = number of coefficients (slopes + possibly intercept) 
c            of the inefficiency model
c       nr = 1 + nb + nz (third dimension of the data array that always
c            includes a "column" for indicating whether the observation
c            for the specific combination of time and individual exists)
      implicit double precision (a-h,o-z)
      dimension startVal(nStartVal)
      dimension dataTable(nRowData,nColData)
      dimension ob(nParamTotal)
      dimension ga(nbArg)
      dimension gb(nParamTotal)
      dimension y(nParamTotal)
      dimension h(nParamTotal,nParamTotal)
      common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit,icode
      common/four/frestart,mrestart,nrestart
      common/one/nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
      common/two/fx,fy
      common/five/tol,tol2,bmu,bignum,step1,gridno,igrid2

      im=imArg
      ipc=ipcArg
      nn=nnArg
      nt=ntArg
      nob=nobArg
      nb=nbArg
      nmu=nmuArg
      neta=netaArg
      iprint=iprintArg
      indic=indicArg
      tol=tolArg
      tol2=tol2Arg
      bmu=bmuArg
      mrestart=mrestartArg
      frestart=frestartArg
      nrestart=0
      bignum=bignumArg
      step1=step1Arg
      igrid2=igrid2Arg
      gridno=gridnoArg
      maxit=maxitArg
      icode=0
      nfunct=0   
      ndrv=0 
      call info( nStartVal, startVal, nRowData, nColData, dataTable,
     $  nParamTotal, ob, ga, gb, fxs, y, h )
      startLogl = -fxs
      fmleLogl = -fx
      nIter = iter
      icodeArg = icode
      nrestartArg = nrestart
      nfunctArg = nfunct
      end
 
      subroutine mini(yy,xx,sv,ob,ga,gb,fxs,y,h)
c       contains the main loop of this iterative program. 
      implicit double precision (a-h,o-z)
      common/one/nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
      common/two/fx,fy
      common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit,icode
      common/four/frestart,mrestart,nrestart
      dimension yy(nn,nt),xx(nn,nt,nr),sv(n)
      dimension ob(n),ga(nb),gb(n),x(:),y(n),s(:)
      dimension h(n,n),delx(:),delg(:),gx(:),gy(:)
      allocatable :: x,s,delx,delg,gx,gy
      allocate(x(n),s(n))
      allocate(delx(n),delg(n),gx(n),gy(n))
      do 98 i=1,n   
      gx(i)=dble(0)  
      gy(i)=dble(0)
      delx(i)=dble(0)  
  98    continue
  107 if ((igrid.eq.1).and.(nrestart.eq.0)) then
      call grid(x,y,yy,xx,ob,ga,gb)  
      if (im.eq.1) call fun1(gb,fxs,yy,xx)
      if (im.eq.2) call fun2(gb,fxs,yy,xx)
      else   
      do 131 i=1,n   
      y(i)=sv(i) 
      x(i)=sv(i) 
  131   continue   
      if (im.eq.1) call fun1(x,fx,yy,xx) 
      if (im.eq.2) call fun2(x,fx,yy,xx) 
      fy=fx
      fxs=fx
      end if 
      iter=0 
      if (im.eq.1) call der1(x,gx,yy,xx) 
      if (im.eq.2) call der2(x,gx,yy,xx) 
      if (iprint.ne.0) then 
      call intpr( 'iteration', -1, iter, 1 )
      call intpr( 'function evaluations', -1, nfunct, 1 )
      call dblepr( 'log-likelihood value', -1, -fy, 1 )   
      call dblepr( 'parameters', -1, y, n )
      endif
      if (maxit.eq.0) goto 70
   5    do 20 i=1,n 
      do 10 j=1,n
   10   h(i,j)=dble(0) 
   20   h(i,i)=dble(1) 
      if(iprint.ne.0) then
      call intpr( 'gradient step', -1, 0, 0 )
      endif
      do 30 i=1,n
   30   s(i)=-gx(i)
   40 icode=0
      call search(x,y,s,gx,delx,yy,xx)
      iter=iter+1   
      if (iter.ge.maxit) then
      icode=10
      goto 70
      endif  
      if(fy.gt.fx) goto 5
      if (im.eq.1) call der1(y,gy,yy,xx)
      if (im.eq.2) call der2(y,gy,yy,xx) 
      call convrg(ipass,x,y) 
      if (ipass.eq.1.) then
      if ((iter.eq.1).and.(icode.eq.5).and.(nrestart.le.mrestart)) then
      call dblepr( 'restarting with starting values multiplied by',
     $  -1, frestart, 1 )
      do 108 i=1,n
      sv(i)=x(i)*frestart
  108 continue
      nrestart=nrestart+1
      goto 107
      else
      goto 70
      endif
      endif
      if (iprint.ne.0) then
      printcon=dble(iter)/dble(iprint)-dble(iter/iprint)
      if (printcon.eq.dble(0)) then   
      call intpr( 'iteration', -1, iter, 1 )
      call intpr( 'function evaluations', -1, nfunct, 1 )
      call dblepr( 'log-likelihood value', -1, -fy, 1 )   
      call dblepr( 'parameters', -1, y, n )
      endif
      endif  
      do 50 i=1,n
      delg(i)=gy(i)-gx(i)
      delx(i)=y(i)-x(i)  
      gx(i)=gy(i)
   50   x(i)=y(i)  
      fx=fy  
      call eta(h,delx,delg,gx)
      do 60 i=1,n
      s(i)=dble(0)   
      do 60 j=1,n
   60   s(i)=s(i)-h(i,j)*gy(j)
      goto 40
   70   continue  
      if (iprint.ne.0) then 
      call intpr( 'iteration', -1, iter, 1 )
      call intpr( 'function evaluations', -1, nfunct, 1 )
      call dblepr( 'log-likelihood value', -1, -fy, 1 )
      call dblepr( 'parameters', -1, y, n )
      endif
      deallocate(x,s,delx,delg,gx,gy)
      return 
      end
 
      subroutine convrg(ipass,x,y)   
c       tests the convergence criterion.  
c       the program is halted when the proportional change in the log-
c       likelihood and in each of the parameters is no greater than   
c       a specified tolerance.
      implicit double precision (a-h,o-z)
      common/two/fx,fy
      common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit,icode
      common/five/tol,tol2,bmu,bignum,step1,gridno,igrid2
      dimension x(n),y(n)
      xtol=tol   
      ftol=tol   
      if(dabs(fx).le.ftol) goto 10   
      if(dabs((fx-fy)/fx).gt.ftol) goto 60   
      goto 20
   10   if(dabs(fx-fy).gt.ftol) goto 60
   20   do 40 i=1,n
      if(dabs(x(i)).le.xtol) goto 30 
      if(dabs((x(i)-y(i))/x(i)).gt.xtol) goto 60 
      goto 40
   30   if(dabs(x(i)-y(i)).gt.xtol) goto 60 
   40   continue
      ipass=1
      if (icode.eq.0) icode=1
      return 
   60   ipass=2
      return
      end
 
      subroutine eta(h,delx,delg,gx) 
c       calculates the direction matrix (p).  
      implicit double precision (a-h,o-z)
      common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit,icode
      common/five/tol,tol2,bmu,bignum,step1,gridno,igrid2
      dimension h(n,n),delx(n),delg(n),gx(n)
      dimension hdg(:),dgh(:),hgx(:)  
      allocatable :: hdg,dgh,hgx
      allocate(hdg(n),dgh(n),hgx(n))
      dxdg=dble(0)   
      dghdg=dble(0)  
      do 20 i=1,n
      hdg(i)=dble(0) 
      dgh(i)=dble(0) 
      do 10 j=1,n
      hdg(i)=hdg(i)-h(i,j)*delg(j)   
   10   dgh(i)=dgh(i)+delg(j)*h(j,i)   
      dxdg=dxdg+delx(i)*delg(i)  
   20   dghdg=dghdg+dgh(i)*delg(i) 
      do 30 i=1,n
      do 30 j=1,n
   30   h(i,j)=h(i,j)+delx(i)*delx(j)/dxdg+hdg(i)*dgh(j)/dghdg 
      do 117 i=1,n   
  117   h(i,i)=dabs(h(i,i))
      do 132 i=1,n   
      hgx(i)=dble(0) 
      do 132 j=1,n   
      hgx(i)=hgx(i)+h(i,j)*gx(j) 
  132   continue   
      hgxx=dble(0)
      gxx=dble(0) 
      do 133 i=1,n   
      hgxx=hgxx+hgx(i)**2
      gxx=gxx+gx(i)**2   
  133   continue   
      c=dble(0)   
      do 134 i=1,n   
      c=c+hgx(i)*gx(i)   
  134   continue   
      c=c/(hgxx*gxx)**dble(0.5)
      if(dabs(c).lt.dble(1)/bignum) then
      call intpr( 'ill-conditioned eta', -1, 0, 0 )
      do 136 i=1,n   
      do 137 j=1,n   
  137   h(i,j)=dble(0)
  136   h(i,i)=delx(i)/gx(i)  
      endif  
      deallocate(hdg,dgh,hgx)
      return 
      end
 
      subroutine search(x,y,s,gx,delx,yy,xx)  
c       unidimensional search (coggin) to determine optimal step length
c       determines the step length (t) using a unidimensional search. 
      implicit double precision (a-h,o-z)
      common/one/nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
      common/two/fx,fy
      common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit,icode
      common/five/tol,tol2,bmu,bignum,step1,gridno,igrid2
      dimension x(n),y(n),s(n),gx(n),delx(n)
      dimension yy(nn,nt),xx(nn,nt,nr)
      iexit=0
      ntol=0 
      ftol=tol2  
      ftol2=ftol/dble(100)   
      fa=fx  
      fb=fx  
      fc=fx  
      da=dble(0) 
      db=dble(0) 
      dc=dble(0) 
      k=-2   
      m=0
      step=step1 
      d=step 
      if(indic.eq.2.or.iter.eq.0) goto 1 
      dxnorm=dble(0) 
      snorm=dble(0)  
      do 102 i=1,n   
      dxnorm=dxnorm+delx(i)*delx(i)  
  102   snorm=snorm+s(i)*s(i) 
      if(indic.eq.1.and.dxnorm.ge.snorm) goto 1  
      ratio=dxnorm/snorm 
      step=dsqrt(ratio)  
      d=step 
   1    do 2 i=1,n  
   2    y(i)=x(i)+d*s(i)
      if (im.eq.1) call fun1(y,f,yy,xx) 
      if (im.eq.2) call fun2(y,f,yy,xx) 
      if (f.ne.f) then
      call intpr( 'function value in search procedure is NaN', 
     $   -1, 0, 0 )
      return
      endif
      k=k+1
      if(f-fa) 5,3,6 
   3    do 4 i=1,n  
   4    y(i)=x(i)+da*s(i)   
      fy=fa  
      if(iprint.ne.0) then
      call intpr( 'search failed. fn val indep of search direction',
     $  -1, 0, 0 )
      endif
      goto 326   
   5    fc=fb   
      fb=fa  
      fa=f   
      dc=db  
      db=da  
      da=d   
      d=dble(2)*d+step   
      goto 1 
   6    if(k) 7,8,9 
   7    fb=f
      db=d   
      d=-d   
      step=-step 
      goto 1 
   8    fc=fb   
      fb=fa  
      fa=f   
      dc=db  
      db=da  
      da=d   
      goto 21
   9    dc=db   
      db=da  
      da=d   
      fc=fb  
      fb=fa  
      fa=f   
   10   d=dble(0.5)*(da+db)  
      do 11 i=1,n
   11   y(i)=x(i)+d*s(i)   
      if (im.eq.1) call fun1(y,f,yy,xx) 
      if (im.eq.2) call fun2(y,f,yy,xx) 
   12   if((dc-d)*(d-db)) 15,13,18 
   13   do 14 i=1,n
   14   y(i)=x(i)+db*s(i)  
      fy=fb  
      if(iexit.eq.1) goto 32 
      if(iprint.ne.0) then
      call intpr( 'search failed. loc of min limited by rounding',
     $ -1, 0, 0 )
      endif
      goto 325   
   15   if(f-fb) 16,13,17  
   16   fc=fb  
      fb=f   
      dc=db  
      db=d   
      goto 21
   17   fa=f   
      da=d   
      goto 21
   18   if(f-fb) 19,13,20  
   19   fa=fb  
      fb=f   
      da=db  
      db=d   
      goto 21
   20   fc=f   
      dc=d   
   21   a=fa*(db-dc)+fb*(dc-da)+fc*(da-db) 
      if(a) 22,30,22
   22   d=dble(0.5)*
     $  ((db*db-dc*dc)*fa+(dc*dc-da*da)*fb+(da*da-db*db)*fc)/a   
      if((da-d)*(d-dc)) 13,13,23 
   23   do 24 i=1,n
   24   y(i)=x(i)+d*s(i)   
      if (im.eq.1) call fun1(y,f,yy,xx) 
      if (im.eq.2) call fun2(y,f,yy,xx) 
      if(dabs(fb)-ftol2) 25,25,26
   25   a=dble(1)  
      goto 27
   26   a=dble(1)/fb   
   27   if((dabs(fb-f)*a)-ftol) 28,28,12   
   28   iexit=1
      if(f-fb) 29,13,13  
   29   fy=f   
      goto 32
   30   if(m) 31,31,13 
   31   m=m+1  
      goto 10
   32   do 99 i=1,n
      if(y(i).ne.x(i)) goto 325  
   99   continue   
      goto 33
  325   if(ntol.ne.0.and.iprint.eq.1) then
      call intpr( 'tolerance reduced so many time(s):', -1, ntol, 1 )
      endif
  326   if(fy.lt.fx) return   
      do 101 i=1,n   
      if(s(i).ne.-gx(i)) return  
  101   continue  
      call intpr( 'search failed on gradient step, termination',
     $  -1, 0, 0 )
      icode=6
      return 
   33   if(ntol.eq.10) goto 34
      iexit=0
      ntol=ntol+1
      ftol=ftol/dble(10)  
      goto 12
  34    if(iprint.ne.0) then
      call intpr( 'pt better than entering pt cannot be found', 
     $  -1, 0, 0 )
      endif
      icode=5
      return 
      end
 
      subroutine check(b)
c       checks if params are out of bounds & adjusts if required. 
      implicit double precision (a-h,o-z)
      common/one/nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
      common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit,icode
      common/five/tol,tol2,bmu,bignum,step1,gridno,igrid2
      dimension b(n)
      n1=nb+nz+1
      n2=nb+nz+2
      bi=dsqrt(bignum) 
      if(b(n1).le.dble(0)) b(n1)=dble(1)/bi  
      if(b(n2).le.dble(1)/bi) b(n2)=dble(1)/bi   
      if(b(n2).ge.dble(1)-dble(1)/bi) b(n2)=dble(1)-dble(1)/bi   
      bound=bmu*dsqrt(b(n1)*b(n2))
      if((im.eq.1).and.(nmu.eq.1).and.(bmu.gt.dble(0))) then
      n3=nb+nz+3
      if(b(n3).gt.bound) b(n3)=bound
      if(b(n3).lt.-bound) b(n3)=-bound
      endif
      return 
      end
 
      subroutine fun1(b,a,yy,xx)
c       calculates the negative of the log-likelihood function of the
c       error components model.
      implicit double precision (a-h,o-z)
      common/one/nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
      common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit,icode
      data pi/3.1415926/ 
      dimension b(n),yy(nn,nt),xx(nn,nt,nr)
      call check(b)  
      a=dble(0) 
      f=dble(nn)    
      fnt=dble(nt)
      ftot=dble(nob) 
      s2=b(nb+1)
      g=b(nb+2)
      u=dble(0)
      e=dble(0)
      if (nmu.eq.1) then 
      u=b(nb+3)
      if (neta.eq.1) e=b(nb+4)
      else
      if (neta.eq.1) e=b(nb+3)
      endif
      sc=dble(1)
      if (ipc.eq.2) sc=-dble(1)
      a=dble(0.5)*ftot*(dlog(dble(2)*pi)+dlog(s2))    
      a=a+dble(0.5)*(ftot-f)*dlog(dble(1)-g)    
      z=u/(s2*g)**dble(0.5)
      a=a+f*dislog(z)
      a=a+dble(0.5)*f*z**2  
      a2=dble(0)
      do 132 i=1,nn   
      epr=dble(0)    
      do 103 l=1,nt   
      if (xx(i,l,1).ne.dble(0)) then
      call resid(b,i,l,yy,xx,ee)
      epr=epr+ee*dexp(-e*(dble(l)-fnt))   
      end if
  103   continue   
      epe=dble(0)
      do 101 l=1,nt   
      if (xx(i,l,1).ne.dble(0)) epe=epe+dexp(-dble(2)*e*(dble(l)-fnt))    
  101   continue   
      zi=(u*(dble(1)-g)-sc*g*epr)/
     $  (g*(dble(1)-g)*s2*(dble(1)+(epe-dble(1))*g))**dble(0.5)
      a=a+dble(0.5)*dlog(dble(1)+(epe-dble(1))*g)   
      a=a-dislog(zi)
      do 133 l=1,nt   
      if (xx(i,l,1).ne.dble(0)) then
      call resid(b,i,l,yy,xx,ee)
      a2=a2+ee**2 
      end if
 133    continue    
      a=a-dble(0.5)*zi**2   
 132    continue    
      a=a+dble(0.5)*a2/((dble(1)-g)*s2) 
      nfunct=nfunct+1 
      return
      end   

      subroutine der1(b,gx,yy,xx)   
c       calculates the first-order partial derivatives of the negative
c       of the log-likelihood function of the error components model.
      implicit double precision (a-h,o-z)
      common/one/nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
      common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit,icode
      dimension b(n),gx(n),yy(nn,nt),xx(nn,nt,nr) 
      call check(b)  
      f=dble(nn)    
      ftot=dble(nob) 
      fnt=dble(nt)  
      n1=nb+nz+1
      n2=nb+nz+2
      s2=b(n1)
      g=b(n2)
      u=dble(0)
      e=dble(0)
      if (nmu.eq.1) then 
      n3=nb+nz+3
      u=b(n3)
      if (neta.eq.1) then 
      n4=nb+nz+4
      e=b(n4)
      endif
      else
      if (neta.eq.1) then 
      n4=nb+nz+3
      e=b(n4)
      endif
      endif
      sc=dble(1)
      if (ipc.eq.2) sc=-dble(1)
      z=u/(s2*g)**dble(0.5)
      do 106 j=1,n        
      gx(j)=dble(0)
 106    continue
      gx(n1)=dble(0.5)*ftot/s2-dble(0.5)*f*(dendis(z)+z)*z/s2
      gx(n2)=-dble(0.5)*(ftot-f)/(dble(1)-g)-
     $  dble(0.5)*f*(dendis(z)+z)*z/g
      
      do 105 i=1,nn
      epr=dble(0)    
      epe=dble(0)
      do 103 l=1,nt   
      if (xx(i,l,1).ne.dble(0)) then
      call resid(b,i,l,yy,xx,ee)
      epr=epr+ee*dexp(-e*(dble(l)-fnt))   
      epe=epe+dexp(-dble(2)*e*(dble(l)-fnt))    
      end if
  103   continue   
      zi=(u*(dble(1)-g)-sc*g*epr)/
     $  (g*(dble(1)-g)*s2*(dble(1)+(epe-dble(1))*g))**dble(0.5)
      
      do 132 j=1,nb
      do 134 l=1,nt   
      if(xx(i,l,1).ne.dble(0)) then
      call resid(b,i,l,yy,xx,ee)
      gx(j)=gx(j)-xx(i,l,j+1)*ee/(s2*(dble(1)-g))
      endif
 134    continue    
      xpe=dble(0)
      do 146 l=1,nt
      if(xx(i,l,1).ne.dble(0)) then
      xpe=xpe+xx(i,l,j+1)*dexp(-e*(dble(l)-fnt))
      endif
 146    continue
      d=(dendis(zi)+zi)*g*xpe*sc
      gx(j)=gx(j)-d/
     $  (g*(dble(1)-g)*s2*(dble(1)+(epe-dble(1))*g))**dble(0.5)   
 132    continue    
      
      gx(n1)=gx(n1)+dble(0.5)*(dendis(zi)+zi)*zi/s2
      ss=dble(0)
      do 138 l=1,nt   
      if(xx(i,l,1).ne.dble(0)) then
      call resid(b,i,l,yy,xx,ee)
      ss=ss+ee**2 
      endif
 138    continue
      gx(n1)=gx(n1)-dble(0.5)*ss/((dble(1)-g)*s2**2)   
      
      gx(n2)=gx(n2)+dble(0.5)*ss/((dble(1)-g)**2*s2)   
      gx(n2)=gx(n2)+dble(0.5)*(epe-dble(1))/(dble(1)+(epe-dble(1))*g)  
      d=g*(dble(1)-g)*(dble(1)+(epe-dble(1))*g) 
      dzi=-(u+sc*epr)*d   
      c=dble(0.5)*(u*(dble(1)-g)-sc*g*epr) 
      dzi=dzi-c*
     $  ((dble(1)-dble(2)*g)+(epe-dble(1))*g*(dble(2)-dble(3)*g))  
      dzi=dzi/(d**dble(1.5)*s2**dble(0.5))    
      gx(n2)=gx(n2)-(dendis(zi)+zi)*dzi
  
      if (nmu.eq.1) then  
      gx(n3)=gx(n3)+dble(1)/(s2*g)**dble(0.5)*(dendis(z)+z)
      d=(dendis(zi)+zi)*(dble(1)-g)
      gx(n3)=gx(n3)-d/
     $  (g*(dble(1)-g)*s2*(dble(1)+(epe-dble(1))*g))**dble(0.5) 
      end if
  
      if (neta.eq.1) then 
      de=dble(0)
      d=dble(0) 
      do 152 l=1,nt   
      if (xx(i,l,1).eq.1) then  
      t=dble(l)
      de=de-dble(2)*(t-fnt)*dexp(-dble(2)*e*(t-fnt))    
      call resid(b,i,l,yy,xx,ee)
      d=d+(t-fnt)*dexp(-e*(t-fnt))*ee
      end if
  152   continue   
      dd=(g*(dble(1)-g)*s2*(dble(1)+(epe-dble(1))*g)) 
      d=d*g*dd*sc
      c=u*(dble(1)-g)-sc*g*epr  
      c=c*dble(0.5)*g**2*(dble(1)-g)*s2*de    
      dzi=(d-c)/dd**dble(1.5)
      gx(n4)=gx(n4)-(dendis(zi)+zi)*dzi
      gx(n4)=gx(n4)+g/dble(2)*de/(dble(1)+(epe-dble(1))*g)   
      end if
  105   continue
  
      ndrv=ndrv+1
      return
      end   


      subroutine fun2(b,a,yy,xx)
c       calculates the negative of the log-likelihood function of the
c       TE effects model.
      implicit double precision (a-h,o-z)
      common/one/nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
      common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit,icode
      dimension b(n),yy(nn,nt),xx(nn,nt,nr)
      data pi/3.1415926/ 
      call check(b)  
      s2=b(nb+nz+1) 
      g=b(nb+nz+2)  
      ss=(g*(dble(1)-g)*s2)**dble(0.5)  
      sc=dble(1)
      if (ipc.eq.2) sc=-dble(1)
      a=dble(0)   
      do 10 i=1,nn   
      do 10 l=1,nt
      if (xx(i,l,1).ne.dble(0)) then
      call resid(b,i,l,yy,xx,ee)
      zd=dble(0)  
      if (nz.ne.0) then  
      do 12 j=nb+1,nb+nz
      zd=zd+xx(i,l,j+1)*b(j) 
   12   continue   
      endif  
      us=(dble(1)-g)*zd-sc*g*ee  
      d=zd/(g*s2)**dble(0.5)   
      ds=us/ss   
      a=a-dble(0.5)*dlog(dble(2)*pi)-dble(0.5)*dlog(s2)-
     $  (dislog(d)-dislog(ds))-dble(0.5)*(ee+sc*zd)**2/s2 
      endif
   10   continue   
      a=-a   
      nfunct=nfunct+1
      return 
      end
 
      subroutine der2(b,gx,yy,xx)   
c       calculates the first-order partial derivatives of the negative
c       of the log-likelihood function of the TE effects model.   
      implicit double precision (a-h,o-z)
      common/one/nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
      common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit,icode
      dimension b(n),gx(n),yy(nn,nt),xx(nn,nt,nr)
      call check(b)  
      s2=b(nb+nz+1) 
      g=b(nb+nz+2)  
      ss=(g*(dble(1)-g)*s2)**dble(0.5)  
      sc=dble(1)
      if (ipc.eq.2) sc=-dble(1)
      do 9 j=1,n 
      gx(j)=dble(0)   
   9    continue   
      do 10 i=1,nn   
      do 10 l=1,nt
      if (xx(i,l,1).ne.dble(0)) then
      call resid(b,i,l,yy,xx,ee)
      zd=dble(0)  
      if (nz.ne.0) then  
      do 12 j=nb+1,nb+nz
      zd=zd+xx(i,l,j+1)*b(j) 
   12   continue   
      endif  
      us=(dble(1)-g)*zd-sc*g*ee  
      d=zd/(g*s2)**dble(0.5)   
      ds=us/ss   
      do 13 j=1,nb   
      gx(j)=gx(j)+xx(i,l,j+1)*((ee+sc*zd)/s2+sc*dendis(ds)*g/ss)
   13   continue   
      if (nz.ne.0) then  
      do 14 j=nb+1,nb+nz
      gx(j)=gx(j)-xx(i,l,j+1)*((sc*ee+zd)/s2
     +  +dendis(d)/(g*s2)**dble(0.5)-dendis(ds)*(dble(1)-g)/ss)
   14   continue   
      endif  
      gx(nb+nz+1)=gx(nb+nz+1)-dble(0.5)/s2
     + *(dble(1)-(dendis(d)*d-dendis(ds)*ds)-(ee+sc*zd)**2/s2)
      gx(nb+nz+2)=gx(nb+nz+2)+dble(0.5)*(dendis(d)*d/g
     +  -dendis(ds)/ss*(zd/g+sc*ee/(dble(1)-g)))
c       gx(nb+nz+2)=gx(nb+nz+2)+dble(0.5)*(dendis(d)*d/g-dendis(ds)*
c    +  (dble(2)*(ee+zd)/ss+ds*(dble(1)-dble(2)*g)/(g*(dble(1)-g))))
      endif
   10   continue   
      do 15 j=1,n
      gx(j)=-gx(j)   
   15   continue   
      ndrv=ndrv+1
      return 
      end

      subroutine resid(b,i,l,yy,xx,ee)
c       calculates the residual for a single observation, 
c       i.e. e = y - x ' b 
      implicit double precision (a-h,o-z)
      common/one/nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
      common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit,icode
      dimension b(n),yy(nn,nt),xx(nn,nt,nr)
      xb=dble(0)
      do 102 j=1,nb   
      xb=xb+b(j)*xx(i,l,j+1)   
  102   continue
      ee=yy(i,l)-xb
      return
      end   
 
      subroutine info( nStartVal, startVal,
     $  nRowData, nColData, dataTable, 
     $  nParamTotal, ob, ga, gb, fxs, y, h )
c       accepts instructions from the terminal or from a file and 
c       also reads data from a file.  
      implicit double precision (a-h,o-z)
      common/one/nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
      common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit,icode
      dimension yy(:,:),xx(:,:,:),mm(:),sv(:),xxd(:)
      dimension startVal(nStartVal)
      dimension dataTable(nRowData,nColData)
      dimension ob(nParamTotal)
      dimension ga(nb)
      dimension gb(nParamTotal)
      dimension y(nParamTotal)
      dimension h(nParamTotal,nParamTotal)
      allocatable :: yy,xx,mm,sv,xxd
      igrid=1
      nz=0
      if ((nn*nt).lt.nob) then   
      call intpr( 'the total number of obsns exceeds the product of',
     $  -1, 0, 0 )
      call intpr( 'the number of firms by the number of years - bye!',
     $  -1, 0, 0 )
      icode=101
      return  
      end if
      if (im.eq.1) then
      nb=nb
      nz=0
      nr=1+nb
      n=nb+nz+2+nmu+neta
      else
      nz=neta
      neta=0
      nz=nz+nmu
      nb=nb
      nr=1+nb+nz   
      n=nb+nz+2
      endif
      if (n.ne.nParamTotal) then
      call intpr( 'internal error: calculated variable ''n''',
     $  -1, 0, 0 )
      call intpr( 'is not equal to argument ''nParamTotal''', 
     $ -1, 0, 0 )
      icode=102
      return
      endif
      allocate (sv(n))
      if (nStartVal.eq.n) then
      igrid=0
      do 148 i=1,n
      sv(i)=startVal(i)
  148   continue  
      else if (nStartVal.gt.1) then
      call intpr( 'wrong number of starting values', -1, 0, 0 )
      icode=103
      return
      endif
      allocate(yy(nn,nt),xx(nn,nt,nr),mm(nn),xxd(nr-nmu*(im-1)))
      do 135 i=1,nn
      mm(i)=0
      do 135 l=1,nt
      xx(i,l,1)=dble(0)
  135   continue
      if ((2+nr-nmu*(im-1)).ne.nColData) then
      call intpr( 'internal error: 2 + nr - nmu * (im-1)',-1, 0, 0 )
      call intpr( 'is not equal to argument ''nColData''',-1, 0, 0 )
      icode=109
      return
      endif
      do 134 k=1,nob  
      fii=dataTable(k,1)
      ftt=dataTable(k,2)
      yyd=dataTable(k,3)
      do 143 i=2,(nr-nmu*(im-1))
      xxd(i)=dataTable(k,2+i)
  143 continue
      i=int(fii)   
      l=int(ftt)   
      mm(i)=mm(i)+1
      xx(i,l,1)=dble(1)
      yy(i,l)=yyd
      do 136 j=2,nb+1
      xx(i,l,j)=xxd(j)
  136   continue
      if ((im.eq.2).and.(nz.gt.0)) then
      if (nmu.eq.1) xx(i,l,nb+2)=dble(1)
      if ((nz-nmu).gt.0) then
      do 154 j=nb+nmu+2,nr
      xx(i,l,j)=xxd(j-nmu)
  154   continue
      endif
      endif
      if (i.lt.1) then  
      call intpr( 'error - a firm number is < 1', -1, 0, 0 )
      icode=104 
      return
      else if (i.gt.nn) then 
      call intpr( 'error - a firm number is > number of firms', 
     $  -1, 0, 0 )
      icode=105
      return
      else if (l.lt.1) then  
      call intpr( 'error - a period number is < 1', -1, 0, 0 )
      icode=106
      return
      else if (l.gt.nt) then 
      call intpr( 'error - a period number is > number of periods',
     $  -1, 0, 0 )
      icode=107
      return
      end if
  134   continue   
      do 149 i=1,nn   
      if (mm(i).eq.0) then  
      call intpr( 'error - there are no observations on firm',
     $  -1, i, 1 )
      icode=108
      return
      end if
  149   continue   
      call mini(yy,xx,sv,ob,ga,gb,fxs,y,h)
      deallocate(yy,xx,mm,sv,xxd)
      return 
      end


 
      subroutine grid(x,y,yy,xx,ob,ga,gb)
c       does a grid search across gamma
      implicit double precision (a-h,o-z)
      common/one/nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
      common/two/fx,fy
      common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit,icode
      common/five/tol,tol2,bmu,bignum,step1,gridno,igrid2
      dimension x(n),y(n),yy(nn,nt),xx(nn,nt,nr),ob(n),ga(nb),gb(n)
      data pi/3.1415926/ 
      n1=nb+nz+1
      n2=nb+nz+2
      sc=dble(1)
      if (ipc.eq.2) sc=-dble(1)
      var=ob(nb+1)*dble(nob-nb)/dble(nob)
      do 131 i=1,nb+1
      y(i)=ob(i) 
  131   continue   
      do 132 i=nb+1,n
      y(i)=dble(0)
  132   continue   
      fx=bignum  
      y6b=gridno 
      y6t=dble(1)-gridno 
      nloop=ceiling((y6t-y6b+gridno)/gridno)
      do 137 j=1,nloop
      y6=y6b+(j-1)*gridno
      y(n2)=y6   
      y(n1)=var/(dble(1)-dble(2)*y(n2)/pi) 
      c=(y(n2)*y(n1)*2/pi)**dble(0.5)  
      if (nb.gt.0) then
      do 139 i=1,nb
      y(i)=ob(i)+ga(i)*c*sc
  139 continue   
      endif
      if (im.eq.1) call fun1(y,fy,yy,xx) 
      if (im.eq.2) call fun2(y,fy,yy,xx) 
      if(fy.lt.fx) then  
      fx=fy  
      do 138 i=1,n   
      x(i)=y(i)  
  138   continue   
      end if 
  137   continue   
      if(igrid2.eq.1) then   
      bb1=x(n2)-gridno/dble(2)   
      bb2=x(n2)+gridno/dble(2)   
      bb3=gridno/dble(10) 
      nloop=ceiling((bb2-bb1+bb3)/bb3)
      do 140 j=1,nloop
      y6=bb1+(j-1)*bb3
      y(n2)=y6   
      y(n1)=var/(dble(1)-dble(2)*y(n2)/pi) 
      c=(y(n2)*y(n1)*2/pi)**dble(0.5)  
      if (nb.gt.0) then
      do 144 i=1,nb
      y(i)=ob(i)+ga(i)*c*sc
  144 continue   
      endif
      if (im.eq.1) call fun1(y,fy,yy,xx) 
      if (im.eq.2) call fun2(y,fy,yy,xx) 
      if(fy.lt.fx) then  
      fx=fy  
      do 141 i=1,n   
      x(i)=y(i)  
  141   continue   
      end if 
  140   continue   
      end if 
      do 142 i=1,n   
      gb(i)=x(i) 
      y(i)=x(i)  
  142   continue   
      fy=fx  
      return 
      end
 
 
 
      double precision function dendis(a)
c       calculates den(a) / dis(a)
      implicit double precision (a-h,o-z)
      dendis=dexp(denlog(a)-dislog(a))
      return
      end
