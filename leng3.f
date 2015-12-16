c      function leng(ch)
      integer function leng(ch)

c     returns length of character string
c     leng: cuts blank spaces
c     leng2: also cuts null characters
c     leng3: runs from front and stops at first SPACE or NULL
 
      character ch*(*)
      integer l
       
      l=len(ch)
      do lengi=1,l
        if(ichar(ch(lengi:lengi)) .eq. 32 .or. 
     &              ichar(ch(lengi:lengi)) .eq. 0) then
          leng=lengi-1
          return
        endif
      enddo
      leng=0
      return
      end

