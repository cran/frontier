 
	subroutine front41(
     $  imArg, ipcArg,
     $  nnArg, ntArg, nobArg, nbArg, nmuArg, netaArg,
     $  iprintArg, indicArg, tolArg, tol2Arg, bignumArg,
     $  step1Arg, igrid2Arg, gridnoArg, maxitArg, bmuArg,
     $  nStartVal, startVal, nRowData, nColData, dataTable,
     $  nParamTotal, ob, obse, olsLogl, gb, startLogl, y, h, fmleLogl,
     $  nIter )
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
	implicit double precision (a-h,o-z)
	dimension startVal(nStartVal)
	dimension dataTable(nRowData,nColData)
	dimension ob(nParamTotal)
	dimension obse(nParamTotal)
	dimension gb(nParamTotal)
	dimension y(nParamTotal)
	dimension h(nParamTotal,nParamTotal)
	common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit   
	common/one/fx,fy,fxols,nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
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
	bignum=bignumArg
	step1=step1Arg
	igrid2=igrid2Arg
	gridno=gridnoArg
	maxit=maxitArg
	nfunct=0   
	ndrv=0 
	call info( nStartVal, startVal, nRowData, nColData, dataTable,
     $  nParamTotal, ob, obse, gb, fxs, y, h )
	olsLogl = -fxols
      startLogl = -fxs
	fmleLogl = -fx
	nIter = iter
	end
 
	subroutine mini(yy,xx,mm,sv,ob,obse,gb,fxs,y,h)
c       contains the main loop of this iterative program. 
	implicit double precision (a-h,o-z)
	common/one/fx,fy,fxols,nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
	common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit   
	dimension yy(nn,nt),xx(nn,nt,nr),mm(nn),sv(n)
	dimension ob(n),gb(n),obse(n),x(:),y(n),s(:)   
	dimension h(n,n),delx(:),delg(:),gx(:),gy(:)
	allocatable :: x,s,delx,delg,gx,gy
	allocate(x(n),s(n))
	allocate(delx(n),delg(n),gx(n),gy(n))
	do 98 i=1,n   
	gx(i)=0.0  
	gy(i)=0.0  
  98    continue   
	call ols(ob,obse,yy,xx)  
	if (igrid.eq.1) then   
	call grid(x,y,yy,xx,ob,gb)  
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
	call result(yy,xx,mm,h,y,sv,ob,obse,gb)
	iter=0 
	if (im.eq.1) call der1(x,gx,yy,xx) 
	if (im.eq.2) call der2(x,gx,yy,xx) 
	if (iprint.ne.0) write(6,*)
	if (iprint.ne.0) write(6,301) iter,nfunct,-fy   
	nc=1   
  305 if (iprint.ne.0) write(6,302) (y(i),i=nc,min(n,nc+4))   
	nc=nc+5
	if(nc.le.n) goto 305   
	if (maxit.eq.0) goto 70
   5    do 20 i=1,n 
	do 10 j=1,n
   10   h(i,j)=0.0 
   20   h(i,i)=1.0 
	if(iprint.ne.0) write(6,2100)  
 2100   format(' gradient step')  
	do 30 i=1,n
   30   s(i)=-gx(i)
   40   call search(x,y,s,gx,delx,yy,xx)
	iter=iter+1   
	if (iter.ge.maxit) then
	write(6,*) 'maximum number of iterations reached'  
	goto 70
	endif  
   7    if(fy.gt.fx) goto 5 
	if (im.eq.1) call der1(y,gy,yy,xx) 
	if (im.eq.2) call der2(y,gy,yy,xx) 
	call convrg(ipass,x,y) 
	if (ipass.eq.1.) goto 70   
	if (iprint.ne.0) then 
	printcon=float(iter)/float(iprint)-float(iter/iprint)
	if (printcon.eq.0.0) then   
	write(6,301) iter,nfunct,-fy   
	nc=1   
  304   write(6,302) (y(i),i=nc,min(n,nc+4))   
	nc=nc+5
	if(nc.le.n) goto 304   
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
	s(i)=0.0   
	do 60 j=1,n
   60   s(i)=s(i)-h(i,j)*gy(j) 
	goto 40
   70   continue  
	if (iprint.ne.0) write(6,301) iter,nfunct,-fy
	nc=1   
  303 if (iprint.ne.0)  write(6,302) (y(i),i=nc,min(n,nc+4))   
	nc=nc+5
	if(nc.le.n) goto 303   
  301   format(' iteration = ',i5,'  func evals =',i7,'  llf =',e16.8) 
  302   format(4x,5e15.8)  
	deallocate(x,s,delx,delg,gx,gy)
	return 
	end
 
	subroutine convrg(ipass,x,y)   
c       tests the convergence criterion.  
c       the program is halted when the proportional change in the log-
c       likelihood and in each of the parameters is no greater than   
c       a specified tolerance.
	implicit double precision (a-h,o-z)
	common/one/fx,fy,fxols,nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
	common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit   
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
	return 
   60   ipass=2 
	return 
	end
 
	subroutine eta(h,delx,delg,gx) 
c       calculates the direction matrix (p).  
	implicit double precision (a-h,o-z)
	common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit   
	common/five/tol,tol2,bmu,bignum,step1,gridno,igrid2
	dimension h(n,n),delx(n),delg(n),gx(n)
	dimension hdg(:),dgh(:),hgx(:)  
	allocatable :: hdg,dgh,hgx
	allocate(hdg(n),dgh(n),hgx(n))
	dxdg=0.0   
	dghdg=0.0  
	do 20 i=1,n
	hdg(i)=0.0 
	dgh(i)=0.0 
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
	hgx(i)=0.0 
	do 132 j=1,n   
	hgx(i)=hgx(i)+h(i,j)*gx(j) 
  132   continue   
	hgxx=0.
	gxx=0. 
	do 133 i=1,n   
	hgxx=hgxx+hgx(i)**2
	gxx=gxx+gx(i)**2   
  133   continue   
	c=0.   
	do 134 i=1,n   
	c=c+hgx(i)*gx(i)   
  134   continue   
	c=c/(hgxx*gxx)**0.5
	if(dabs(c).lt.1.0/bignum) then
	write(6,*) 'ill-conditioned eta'   
	do 136 i=1,n   
	do 137 j=1,n   
  137   h(i,j)=0.0
  136   h(i,i)=delx(i)/gx(i)  
	endif  
	deallocate(hdg,dgh,hgx)
	return 
	end
 
	subroutine search(x,y,s,gx,delx,yy,xx)  
c       unidimensional search (coggin) to determine optimal step length
c       determines the step length (t) using a unidimensional search. 
	implicit double precision (a-h,o-z)
	common/one/fx,fy,fxols,nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
	common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit   
	common/five/tol,tol2,bmu,bignum,step1,gridno,igrid2
	dimension x(n),y(n),s(n),gx(n),delx(n)
	dimension yy(nn,nt),xx(nn,nt,nr)
	iexit=0
	ntol=0 
	ftol=tol2  
	ftol2=ftol/100.0   
	fa=fx  
	fb=fx  
	fc=fx  
	da=0.0 
	db=0.0 
	dc=0.0 
	k=-2   
	m=0
	step=step1 
	d=step 
	if(indic.eq.2.or.iter.eq.0) goto 1 
	dxnorm=0.0 
	snorm=0.0  
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
	k=k+1  
	if(f-fa) 5,3,6 
   3    do 4 i=1,n  
   4    y(i)=x(i)+da*s(i)   
	fy=fa  
	if(iprint.ne.0) write(6,2100)  
 2100   format(' search failed. fn val indep of search direction')
	goto 326   
   5    fc=fb   
	fb=fa  
	fa=f   
	dc=db  
	db=da  
	da=d   
	d=2.0*d+step   
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
   10   d=0.5*(da+db)  
	do 11 i=1,n
   11   y(i)=x(i)+d*s(i)   
	if (im.eq.1) call fun1(y,f,yy,xx) 
	if (im.eq.2) call fun2(y,f,yy,xx) 
   12   if((dc-d)*(d-db)) 15,13,18 
   13   do 14 i=1,n
   14   y(i)=x(i)+db*s(i)  
	fy=fb  
	if(iexit.eq.1) goto 32 
	if(iprint.ne.0) write(6,2500)  
 2500   format(' search failed. loc of min limited by rounding')  
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
   22   d=0.5*((db*db-dc*dc)*fa+(dc*dc-da*da)*fb+(da*da-db*db)*fc)/a   
	if((da-d)*(d-dc)) 13,13,23 
   23   do 24 i=1,n
   24   y(i)=x(i)+d*s(i)   
	if (im.eq.1) call fun1(y,f,yy,xx) 
	if (im.eq.2) call fun2(y,f,yy,xx) 
	if(dabs(fb)-ftol2) 25,25,26
   25   a=1.0  
	goto 27
   26   a=1.0/fb   
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
  325   if(ntol.ne.0.and.iprint.eq.1) write(6,3000) ntol  
 3000   format(1x,'tolerance reduced',i1,'time(s)')   
  326   if(fy.lt.fx) return   
	do 101 i=1,n   
	if(s(i).ne.-gx(i)) return  
  101   continue  
	write(6,5000)  
 5000   format(' search failed on gradient step, termination')
	return 
   33   if(ntol.eq.5) goto 34  
	iexit=0
	ntol=ntol+1
	ftol=ftol/10.  
	goto 12
  34    if(iprint.ne.0) write(6,2000)   
 2000   format(' pt better than entering pt cannot be found') 
	return 
	end
 
	subroutine check(b,xx)
c       checks if params are out of bounds & adjusts if required. 
	implicit double precision (a-h,o-z)
	common/one/fx,fy,fxols,nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
	common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit   
	common/five/tol,tol2,bmu,bignum,step1,gridno,igrid2
	dimension b(n),xx(nn,nt,nr)
	n1=nr+1
	n2=nr+2
	bi=dsqrt(bignum) 
	if(b(n1).le.0.0) b(n1)=1.0/bi  
	if(b(n2).le.1.0/bi) b(n2)=1.0/bi   
	if(b(n2).ge.1.0-1.0/bi) b(n2)=1.0-1.0/bi   
	bound=bmu*dsqrt(b(n1)*b(n2))
	if((im.eq.1).and.(nmu.eq.1).and.(bmu.gt.0.0)) then
	n3=nr+3
	if(b(n3).gt.bound) b(n3)=bound
	if(b(n3).lt.-bound) b(n3)=-bound
	endif
	return 
	end
 
	subroutine fun1(b,a,yy,xx)
c       calculates the negative of the log-likelihood function of the
c       error components model.
	implicit double precision (a-h,o-z)
	common/one/fx,fy,fxols,nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
	common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit   
	data pi/3.1415926/ 
	dimension b(n),yy(nn,nt),xx(nn,nt,nr)
	call check(b,xx)  
	a=0.0 
	f=dfloat(nn)    
	fnt=dfloat(nt)
	ftot=dfloat(nob) 
	s2=b(nb+1)
	g=b(nb+2)
	u=0.0
	e=0.0
	if (nmu.eq.1) then 
	u=b(nb+3)
	if (neta.eq.1) e=b(nb+4)
	else
	if (neta.eq.1) e=b(nb+3)
	endif
	sc=1.
	if (ipc.eq.2) sc=-1.
	a=0.5*ftot*(dlog(2.0*pi)+dlog(s2))    
	a=a+0.5*(ftot-f)*dlog(1.0-g)    
	z=u/(s2*g)**0.5
	a=a+f*dislog(z)
	a=a+0.5*f*z**2  
	a2=0.0
	do 132 i=1,nn   
	epr=0.0    
	do 103 l=1,nt   
	if (xx(i,l,1).ne.0.0) then
	ee=yy(i,l)  
	do 102 j=1,nb   
	ee=ee-b(j)*xx(i,l,j)   
  102   continue   
	epr=epr+ee*dexp(-e*(dfloat(l)-fnt))   
	end if
  103   continue   
	epe=0.0
	do 101 l=1,nt   
	if (xx(i,l,1).ne.0.0) epe=epe+dexp(-2.0*e*(dfloat(l)-fnt))    
  101   continue   
	zi=(u*(1.0-g)-sc*g*epr)/(g*(1.0-g)*s2*(1.0+(epe-1.0)*g))**0.5
	a=a+0.5*dlog(1.0+(epe-1.0)*g)   
	a=a-dislog(zi)
	do 133 l=1,nt   
	if (xx(i,l,1).ne.0.0) then
	ee=yy(i,l)  
	do 134 j=1,nb   
	ee=ee-b(j)*xx(i,l,j)   
 134    continue    
	a2=a2+ee**2 
	end if
 133    continue    
	a=a-0.5*zi**2   
 132    continue    
	a=a+0.5*a2/((1.0-g)*s2) 
	nfunct=nfunct+1 
	return
	end   

	subroutine der1(b,gx,yy,xx)   
c       calculates the first-order partial derivatives of the negative
c       of the log-likelihood function of the error components model.
	implicit double precision (a-h,o-z)
	common/one/fx,fy,fxols,nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
	common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit   
	dimension b(n),gx(n),yy(nn,nt),xx(nn,nt,nr) 
	call check(b,xx)  
	f=dfloat(nn)    
	ftot=dfloat(nob) 
	fnt=dfloat(nt)  
	n1=nr+1
	n2=nr+2
	s2=b(n1)
	g=b(n2)
	u=0.0
	e=0.0
	if (nmu.eq.1) then 
	n3=nr+3
	u=b(n3)
	if (neta.eq.1) then 
	n4=nr+4
	e=b(n4)
	endif
	else
	if (neta.eq.1) then 
	n4=nr+3
	e=b(n4)
	endif
	endif
	sc=1.
	if (ipc.eq.2) sc=-1.
	z=u/(s2*g)**0.5
	do 106 j=1,n        
	gx(j)=0.0
 106    continue
	gx(n1)=0.5*ftot/s2-0.5*f*(dendis(z)+z)*z/s2
	gx(n2)=-.5*(ftot-f)/(1.-g)-.5*f*(dendis(z)+z)*z/g
	
	do 105 i=1,nn
	epr=0.0    
	epe=0.0
	do 103 l=1,nt   
	if (xx(i,l,1).ne.0.0) then
	ee=yy(i,l)  
	do 102 j=1,nb   
	ee=ee-b(j)*xx(i,l,j)   
  102   continue   
	epr=epr+ee*dexp(-e*(dfloat(l)-fnt))   
	epe=epe+dexp(-2.0*e*(dfloat(l)-fnt))    
	end if
  103   continue   
	zi=(u*(1.0-g)-sc*g*epr)/(g*(1.0-g)*s2*(1.0+(epe-1.0)*g))**0.5
	
	do 132 j=1,nb   
	do 134 l=1,nt   
	if(xx(i,l,1).ne.0.0) then
	ee=yy(i,l)  
	do 135 k=1,nb   
	ee=ee-xx(i,l,k)*b(k)   
 135    continue    
	gx(j)=gx(j)-xx(i,l,j)*ee/(s2*(1.-g))
	endif
 134    continue    
	xpe=0.0
	do 146 l=1,nt
	if(xx(i,l,1).ne.0.0)xpe=xpe+xx(i,l,j)*dexp(-e*(dfloat(l)-fnt))
 146    continue
	d=(dendis(zi)+zi)*g*xpe*sc
	gx(j)=gx(j)-d/(g*(1.0-g)*s2*(1.0+(epe-1.0)*g))**0.5   
 132    continue    
	
	gx(n1)=gx(n1)+.5*(dendis(zi)+zi)*zi/s2
	ss=0.0
	do 138 l=1,nt   
      if(xx(i,l,1).ne.0.0) then
	ee=yy(i,l)  
	do 139 j=1,nb   
	ee=ee-xx(i,l,j)*b(j)   
 139    continue    
	ss=ss+ee**2 
      endif
 138    continue
	gx(n1)=gx(n1)-0.5*ss/((1.0-g)*s2**2)   
	
	gx(n2)=gx(n2)+0.5*ss/((1.0-g)**2*s2)   
	gx(n2)=gx(n2)+0.5*(epe-1.0)/(1.0+(epe-1.0)*g)  
	d=g*(1.-g)*(1.0+(epe-1.0)*g) 
	dzi=-(u+sc*epr)*d   
	c=0.5*(u*(1.0-g)-sc*g*epr) 
	dzi=dzi-c*((1.0-2.0*g)+(epe-1.0)*g*(2.0-3.0*g))  
	dzi=dzi/(d**1.5*s2**0.5)    
	gx(n2)=gx(n2)-(dendis(zi)+zi)*dzi
  
	if (nmu.eq.1) then  
	gx(n3)=gx(n3)+1./(s2*g)**0.5*(dendis(z)+z)
	d=(dendis(zi)+zi)*(1.-g)
	gx(n3)=gx(n3)-d/(g*(1.-g)*s2*(1.+(epe-1.)*g))**.5  
	end if
  
	if (neta.eq.1) then 
	de=0.0
	d=0.0 
	do 152 l=1,nt   
	if (xx(i,l,1).eq.1) then  
	t=dfloat(l)
	de=de-2.0*(t-fnt)*dexp(-2.0*e*(t-fnt))    
	ee=yy(i,l)  
	do 153 j=1,nb   
	ee=ee-xx(i,l,j)*b(j)   
  153   continue   
	d=d+(t-fnt)*dexp(-e*(t-fnt))*ee
	end if
  152   continue   
	dd=(g*(1.0-g)*s2*(1.0+(epe-1.0)*g)) 
	d=d*g*dd*sc
	c=u*(1.0-g)-sc*g*epr  
	c=c*0.5*g**2*(1.0-g)*s2*de    
	dzi=(d-c)/dd**1.5    
	gx(n4)=gx(n4)-(dendis(zi)+zi)*dzi
	gx(n4)=gx(n4)+g/2.0*de/(1.0+(epe-1.0)*g)   
	end if
  105   continue
  
	ndrv=ndrv+1
	return
	end   


	subroutine fun2(b,a,yy,xx)
c       calculates the negative of the log-likelihood function of the
c       TE effects model.
	implicit double precision (a-h,o-z)
	common/one/fx,fy,fxols,nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
	common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit   
	dimension b(n),yy(nn,nt),xx(nn,nt,nr)
	data pi/3.1415926/ 
	call check(b,xx)  
	s2=b(nr+1) 
	g=b(nr+2)  
	ss=(g*(1.-g)*s2)**0.5  
	sc=1.0
	if (ipc.eq.2) sc=-1.0
	a=0.   
	do 10 i=1,nn   
	do 10 l=1,nt
	if (xx(i,l,1).ne.0.0) then
	xb=0.  
	do 11 j=1,nb   
	xb=xb+xx(i,l,j)*b(j) 
   11   continue   
	ee=(yy(i,l)-xb)
	zd=0.  
	if (nz.ne.0) then  
	do 12 j=nb+1,nr
	zd=zd+xx(i,l,j)*b(j) 
   12   continue   
	endif  
	us=(1.-g)*zd-sc*g*ee  
	d=zd/(g*s2)**0.5   
	ds=us/ss   
	a=a-0.5*dlog(2.*pi)-0.5*dlog(s2)-(dislog(d)-dislog(ds))
     +  -0.5*(ee+sc*zd)**2/s2 
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
	common/one/fx,fy,fxols,nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
	common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit   
	dimension b(n),gx(n),yy(nn,nt),xx(nn,nt,nr)
	call check(b,xx)  
	s2=b(nr+1) 
	g=b(nr+2)  
	ss=(g*(1.-g)*s2)**0.5  
	sc=1.0
	if (ipc.eq.2) sc=-1.0
	do 9 j=1,n 
	gx(j)=0.   
   9    continue   
	do 10 i=1,nn   
	do 10 l=1,nt
	if (xx(i,l,1).ne.0.0) then
	xb=0.  
	do 11 j=1,nb   
	xb=xb+xx(i,l,j)*b(j) 
   11   continue   
	ee=(yy(i,l)-xb)
	zd=0.  
	if (nz.ne.0) then  
	do 12 j=nb+1,nr
	zd=zd+xx(i,l,j)*b(j) 
   12   continue   
	endif  
	us=(1.-g)*zd-sc*g*ee  
	d=zd/(g*s2)**0.5   
	ds=us/ss   
	do 13 j=1,nb   
	gx(j)=gx(j)+xx(i,l,j)*((ee+sc*zd)/s2+sc*dendis(ds)*g/ss)
   13   continue   
	if (nz.ne.0) then  
	do 14 j=nb+1,nr
	gx(j)=gx(j)-xx(i,l,j)*((sc*ee+zd)/s2+dendis(d)/(g*s2)
     +  **0.5-dendis(ds)*(1.-g)/ss)
   14   continue   
	endif  
	gx(nr+1)=gx(nr+1)-0.5/s2*(1.-(dendis(d)*d-dendis(ds)
     +  *ds)-(ee+sc*zd)**2/s2)
	gx(nr+2)=gx(nr+2)+0.5*(dendis(d)*d/g-dendis(ds)
     +  /ss*(zd/g+sc*ee/(1.-g)))
c       gx(nr+2)=gx(nr+2)+0.5*(dendis(d)*d/g-dendis(ds)*
c    +  (2.*(ee+zd)/ss+ds*(1.-2.*g)/(g*(1.-g))))
	endif
   10   continue   
	do 15 j=1,n
	gx(j)=-gx(j)   
   15   continue   
	ndrv=ndrv+1
	return 
	end
 
	subroutine info( nStartVal, startVal,
     $  nRowData, nColData, dataTable, 
     $  nParamTotal, ob, obse, gb, fxs, y, h )
c       accepts instructions from the terminal or from a file and 
c       also reads data from a file.  
	implicit double precision (a-h,o-z)
	common/one/fx,fy,fxols,nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
	common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit   
	character chst
	dimension yy(:,:),xx(:,:,:),mm(:),sv(:),xxd(:)
	dimension startVal(nStartVal)
	dimension dataTable(nRowData,nColData)
	dimension ob(nParamTotal)
	dimension obse(nParamTotal)
	dimension gb(nParamTotal)
	dimension y(nParamTotal)
	dimension h(nParamTotal,nParamTotal)
	allocatable :: yy,xx,mm,sv,xxd
	igrid=1
	nz=0
	if ((nn*nt).lt.nob) then   
	write(6,*) ' the total number of obsns exceeds the product of'    
	write(6,*) ' the number of firms by the number of years - bye!'   
	stop  
	end if
	if (im.eq.1) then
	nb=nb+1
	nr=nb
	n=nr+2+nmu+neta
	else
	nz=neta
	neta=0
	nz=nz+nmu
	nb=nb+1
	nr=nb+nz   
	n=nr+2
	endif
	if (n.ne.nParamTotal) then
	write(6,*) 'internal error: calculated variable ''n'''
	write(6,*) 'is not equal to argument ''nParamTotal'''
	stop
	endif
	allocate (sv(n))
	if (nStartVal.eq.n) then
	igrid=0
	do 148 i=1,n
	sv(i)=startVal(i)
  148   continue  
	else if (nStartVal.gt.1) then
	write(6,*) 'wrong number of starting values'
	stop
	endif
	allocate(yy(nn,nt),xx(nn,nt,nr),mm(nn),xxd(nr))
	do 135 i=1,nn
	mm(i)=0
	do 135 l=1,nt
	xx(i,l,1)=0.0
  135   continue
	do 134 k=1,nob  
	ndat=nr
	if(im.eq.2) ndat=nr-nmu
	fii=dataTable(k,1)
	ftt=dataTable(k,2)
	yyd=dataTable(k,3)
	xxd(2:)=dataTable(k,4:)
	i=int(fii)   
	l=int(ftt)   
	mm(i)=mm(i)+1
	xx(i,l,1)=1.0
	yy(i,l)=yyd
	do 136 j=2,nb
	xx(i,l,j)=xxd(j)
  136   continue
	if ((im.eq.2).and.(nz.gt.0)) then
	if (nmu.eq.1) xx(i,l,nb+1)=1.0
	if ((nz-nmu).gt.0) then
	do 154 j=nb+nmu+1,nr
	xx(i,l,j)=xxd(j-nmu)
  154   continue
	endif
	endif
	if (i.lt.1) then  
	write(6,*) ' error - a firm number is < 1'    
	stop  
	else if (i.gt.nn) then 
	write(6,*) ' error - a firm number is > number of firms'
	stop  
	else if (l.lt.1) then  
	write(6,*) ' error - a period number is < 1'  
	stop  
	else if (l.gt.nt) then 
	write(6,*) ' error - a period number is > number of periods' 
	stop  
	end if
  134   continue   
	do 149 i=1,nn   
	if (mm(i).eq.0) then  
	write(6,66) i   
   66   format(' error - there are no observations on firm ',i6)
	stop  
	end if
  149   continue   
	call mini(yy,xx,mm,sv,ob,obse,gb,fxs,y,h)
	deallocate(yy,xx,mm,sv,xxd)
	return 
	end


	subroutine result(yy,xx,mm,h,y,sv,ob,obse,gb)
c       presents estimates, covariance matrix, standard errors and t-ratios,
c       as well as presenting many results including estimates of technical  
c       efficiency.   
	implicit double precision (a-h,o-z)
	common/one/fx,fy,fxols,nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
	common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit   
	common/five/tol,tol2,bmu,bignum,step1,gridno,igrid2
	dimension yy(nn,nt),xx(nn,nt,nr),mm(nn)
	dimension h(n,n),y(n),sv(n),ob(n),obse(n),gb(n)
	data pi/3.1415926/ 
	n1=nr+1
	n2=nr+2
	n3=nr+3
	n4=nr+4
	if((nmu.eq.0).and.(neta.eq.1)) n4=nr+3
	fnob=dfloat(nob) 
	fnb=dfloat(nb) 
	os2=ob(nb+1)*(fnob-fnb)/fnob
	fxols=fnob/2.0*(dlog(2.0*pi)+dlog(os2)+1.0)  
	
	return 
	end
 

 
	subroutine grid(x,y,yy,xx,ob,gb)
c       does a grid search across gamma
	implicit double precision (a-h,o-z)
	common/one/fx,fy,fxols,nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
	common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit   
	common/five/tol,tol2,bmu,bignum,step1,gridno,igrid2
	dimension x(n),y(n),yy(nn,nt),xx(nn,nt,nr),ob(n),gb(n)
	data pi/3.1415926/ 
	n1=nr+1
	n2=nr+2
	sc=1.0
	if (ipc.eq.2) sc=-1.0
	var=ob(nb+1)*float(nob-nb)/float(nob)
	b0=ob(1)   
	do 131 i=1,nb+1
	y(i)=ob(i) 
  131   continue   
	do 132 i=nb+1,nr   
	y(i)=0.
  132   continue   
	fx=bignum  
	y6b=gridno 
	y6t=1.0-gridno 
	nloop=ceiling((y6t-y6b+gridno)/gridno)
	do 137 j=1,nloop
	y6=y6b+(j-1)*gridno
	y(n2)=y6   
	y(n1)=var/(1.-2.*y(n2)/pi) 
	c=(y(n2)*y(n1)*2/pi)**0.5  
	y(1)=b0+c*sc
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
	bb1=x(n2)-gridno/2.0   
	bb2=x(n2)+gridno/2.0   
	bb3=gridno/10.0 
	nloop=ceiling((bb2-bb1+bb3)/bb3)
	do 140 j=1,nloop
	y6=bb1+(j-1)*bb3
	y(n2)=y6   
	y(n1)=var/(1.-2.*y(n2)/pi) 
	c=(y(n2)*y(n1)*2/pi)**0.5  
	y(1)=b0+c*sc
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
 
 
 
	subroutine invert(xx,n)
c       finds the inverse of a given matrix.  
	implicit double precision (a-h,o-z)
	common/five/tol,tol2,bmu,bignum,step1,gridno,igrid2
	dimension xx(n,n)   
	dimension ipiv(:)
	allocatable :: ipiv
	allocate(ipiv(n))
	do 1 i=1,n 
   1    ipiv(i)=0   
	do 11 i=1,n
	amax=0.
	do 5 j=1,n 
	if(ipiv(j))2,2,5   
   2    if(dabs(xx(j,j))-amax) 4,4,3
   3    icol=j  
	amax=dabs(xx(j,j)) 
   4    continue
   5    continue
	ipiv(icol)=1   
	if(amax-1.0/bignum)6,6,7   
   6    write(6,*) 'singular matrix'
	stop   
   7    continue
	amax=xx(icol,icol) 
	xx(icol,icol)=1.0  
	do 8 k=1,n 
   8    xx(icol,k)=xx(icol,k)/amax  
	do 11 j=1,n
	if(j-icol)9,11,9   
   9    amax=xx(j,icol) 
	xx(j,icol)=0.  
	do 10 k=1,n
   10   xx(j,k)=xx(j,k)-xx(icol,k)*amax
   11   continue   
	deallocate(ipiv)
	return 
	end
 
 
	subroutine ols(ob,obse,yy,xx)
c       calculates the ols estimates and their standard errors.       
	implicit double precision (a-h,o-z)
	common/one/fx,fy,fxols,nn,nz,nb,nr,nt,nob,nmu,neta,ipc,im
	common/three/n,nfunct,ndrv,iter,indic,iprint,igrid,maxit   
	dimension ob(n),obse(n),yy(nn,nt),xx(nn,nt,nr)
	dimension xpx(:,:),xpy(:),mx(:)
	allocatable :: xpx,xpy,mx
	allocate(xpx(nb,nb),xpy(nb),mx(nb))
c       calculate x'x and x'y 
	do 131 k=1,nb  
	do 132 j=1,nb  
	xpx(k,j)=0.0   
	do 132 i=1,nn  
	do 132 l=1,nt
	if (xx(i,l,1).ne.0.0) xpx(k,j)=xpx(k,j)+xx(i,l,k)*xx(i,l,j)
  132   continue   
	xpy(k)=0.0 
	do 131 i=1,nn  
	do 131 l=1,nt
	if (xx(i,l,1).ne.0.0) xpy(k)=xpy(k)+xx(i,l,k)*yy(i,l)
  131   continue   
c       determine correct scaling for x'x 
	do 120 k=1,nb  
	h=(1.0-dlog10(xpx(k,k)))/2.0   
	if (h.lt.0.0) goto 121 
	mx(k)=h
	goto 120   
  121   mx(k)=h-1 
  120   continue  
c       scale, invert and then scale back 
	is=0   
  123   is=is+1   
	do 122 k=1,nb  
	do 122 j=1,nb  
	xpx(k,j)=xpx(k,j)*10.0**(mx(k)+mx(j))  
  122   continue  
	if (is.eq.1) then
	call invert(xpx,nb)
	goto 123   
	endif  
c       calculate b=inv(x'x)x'y   
	do 133 k=1,nb  
	ob(k)=0.0  
	do 133 j=1,nb  
	ob(k)=ob(k)+xpx(k,j)*xpy(j)
  133   continue   
	ss=0.0 
	do 134 i=1,nn  
	do 134 l=1,nt
	if (xx(i,l,1).ne.0.0) then
	ee=yy(i,l)   
	do 135 k=1,nb  
	ee=ee-xx(i,l,k)*ob(k)
  135   continue   
	ss=ss+ee**2
	endif
  134   continue   
	ob(nb+1)=ss/dfloat(nob-nb)  
	do 136 k=1,nb  
	obse(k)=(ob(nb+1)*xpx(k,k))**0.5   
  136   continue   
	deallocate(xpx,xpy,mx)
	return 
	end
 
      double precision function dendis(a)
c       calculates den(a) / dis(a)
      implicit double precision (a-h,o-z)
      dendis=dexp(denlog(a)-dislog(a))
      return
      end
