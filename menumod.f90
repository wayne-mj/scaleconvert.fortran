module menumodule
    implicit none

    contains
    
    subroutine mainmenu()
        print *, "Scale Model Railway Calculator"
        print *, " "
        print *, "1) Scale conversion"
        print *, "2) Feet table"
        print *, "3) Inches table"
        print *, "4) Helix estimation tool"
        print *, "5) Quit"
        print *, " "
        write (*,'(A)', advance='no') "Enter your choice: "
    end subroutine mainmenu
end module menumodule