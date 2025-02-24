      implicit none
      character*80  inp_name, out_name
      character*32768 out_line
      character*87 line
      real val(100,100), ex(100), elab(100), sigfus(100), dumm
      integer  out_length, istr_alen, i_c, itab,ntabs,icol, iex, nex
      write(*,*) ' Input file name: '
      read(*,'(A)') inp_name
      open(100,file=inp_name,status='old')
      write(*,*) ' Output file name: '
      read(*,'(A)') out_name
      open(200,file=out_name,status='unknown')
      i_c = 1
      itab = 0
      iex = 0
 3    continue
      read(100,'(A)',end=20) line
c      write(*,'(A)') line
      if (line(6:10).ne.'E_lab') goto 3
      read(100,*,err=7)
 5    iex = iex+1
      read(100,*,err=7) elab(iex), dumm, ex(iex), sigfus(iex)
c      write(*,*) elab(iex)
      goto 5
 7    continue
      nex = iex-1
      write(*,*)' Number of beam energies: ', nex
      out_line = ' E_lab    E*     XsecTot '
 10   continue
      read(100,'(A)',end=20) line
      if (line(3:8).ne.'E*/MeV') goto 10
      itab = itab+1
      out_length = istr_alen(out_line)
      out_line  = out_line(1:out_length)//line(9:)
      goto 10
 20   continue
      close(100)
      ntabs = itab
      write(*,'(I10,A)') itab, ' tables found'
      out_length = istr_alen(out_line)
      write(*,*)' Out length: ', out_length
      write(200,'(A)') out_line(1:out_length)
      open(100,file=inp_name,status='old')
      itab = 0
 30   continue
      read(100,'(A)',end=40) line
      if (line(3:8).ne.'E*/MeV') goto 30
c      write(*,'(A)')' Reading data '
      do iex = 1, nex
         read(100,*)  dumm, (val(iex,icol), icol=itab*8+1, itab*8+8)
c         read(100,*) ex(iex)
c         write(*,*) ex(iex)
      enddo
      itab = itab+1
      if (itab < ntabs) goto 30
 40   continue
      do iex = 1, nex
         write(200,'(2(1X,F7.1),1X,E10.4,1000(1X,E9.3))') 
     +        elab(iex),  ex(iex), sigfus(iex),
     +        (val(iex,icol),icol=1,ntabs*8)
      enddo
      close(2)
      end

      
