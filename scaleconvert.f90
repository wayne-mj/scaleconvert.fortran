program scaleconvert
    use fractionmodule
    use scaleconvertmodule

    implicit none
    character(len=1) :: newline = achar(10)
    character(len=1) :: quote = achar(39)
    character(len=1) :: doublequote = achar(34)
    integer :: feet, inches, numerator, denominator
    character(len=1) :: delim
    real :: scale, a,b,c,d
    
    type(fractiontype), dimension(3) :: f1
    character(len=100) :: test, test1, test2, test3, test4, test5
    type(imperialtype):: imp
    type(scaletype) :: imp2
    
    ! f1 = closestimperialfraction(.65)
    ! print *, f1(1)%unit, f1(1)%numerator, f1(1)%denominator, f1(1)%status
    ! print *, f1(2)%unit, f1(2)%numerator, f1(2)%denominator, f1(2)%status
    ! print *, f1(3)%unit, f1(3)%numerator, f1(3)%denominator, f1(3)%status

    print *, "Type four numbers:"
    read (*,*) a,b,c,d

    print *, "You entered: ", a,b,c,d

    test = "1" // quote // " 1 1/4" // doublequote
    test1 = "1'"
    test2 = "1 1 1/4"
    test3 = "1"
    test4 = "1'"
    test5 = "1'"
    
    scale = 87.1_8

    ! print *, test
    ! imp = str2imp(test)
    ! imp2 = imperialscale(imp, scale)
    ! print *, imp%feet, imp%inches%unit, imp%inches%numerator, imp%inches%denominator, imp%inches%status
    ! write (*, '(3F10.2)') imp2%imperialdecimal, imp2%metricdecimal, imp2%scale
    ! print *, imp2%imperialdecimal, imp2%metricdecimal, imp2%scale
    ! print *, ""
    ! print *, imp2%imperialbelow%inches%unit, imp2%imperialbelow%inches%numerator, imp2%imperialbelow%inches%denominator, imp2%imperialbelow%inches%status
    ! print *, ""
    ! print *, imp2%imperialvalue%inches%unit, imp2%imperialvalue%inches%numerator, imp2%imperialvalue%inches%denominator, imp2%imperialvalue%inches%status
    ! print *, ""
    ! print *, imp2%imperialabove%inches%unit, imp2%imperialabove%inches%numerator, imp2%imperialabove%inches%denominator, imp2%imperialabove%inches%status

    print *, test
    imp = str2imp(test)
    imp2 = imperialscale(imp, 87.1)
    print *, imp%feet, imp%inches%unit, imp%inches%numerator, imp%inches%denominator, imp%inches%status
    write (*, '(3F10.2)') imp2%imperialdecimal, imp2%metricdecimal, imp2%scale
    print *, ""

    print *, test1
    imp = str2imp(test1)
    imp2 = imperialscale(imp, 87.1)
    print *, imp%feet, imp%inches%unit, imp%inches%numerator, imp%inches%denominator, imp%inches%status
    write (*, '(3F10.2)') imp2%imperialdecimal, imp2%metricdecimal, imp2%scale
    print *, ""

    print *, test2
    imp = str2imp(test2)
    imp2 = imperialscale(imp, 87.1)
    print *, imp%feet, imp%inches%unit, imp%inches%numerator, imp%inches%denominator, imp%inches%status
    write (*, '(3F10.2)') imp2%imperialdecimal, imp2%metricdecimal, imp2%scale
    print *, ""

    print *, test3
    imp = str2imp(test3)
    imp2 = imperialscale(imp, 87.1)
    print *, imp%feet, imp%inches%unit, imp%inches%numerator, imp%inches%denominator, imp%inches%status
    write (*, '(3F10.2)') imp2%imperialdecimal, imp2%metricdecimal, imp2%scale
    print *, ""

    print *, test4
    imp = str2imp(test4)
    imp2 = imperialscale(imp, 87.1)
    print *, imp%feet, imp%inches%unit, imp%inches%numerator, imp%inches%denominator, imp%inches%status
    write (*, '(3F10.2)') imp2%imperialdecimal, imp2%metricdecimal, imp2%scale
    
    print *, ""
    print *, test5
    imp = str2imp(test5)
    imp2 = imperialscale(imp, 87.1)
    print *, imp%feet, imp%inches%unit, imp%inches%numerator, imp%inches%denominator, imp%inches%status
    write (*, '(3F10.2)') imp2%imperialdecimal, imp2%metricdecimal, imp2%scale
    


    !test = "32212  1 1/2"

    ! write (*, '(A)', advance='no') "Enter you measurement (x" // quote  //  " x x/x" // doublequote // "):"
    ! read (*,'(A)') test
    ! print *, trim(test),"****"
    ! imp = string2imperial(trim(test))
    ! print *, imp%feet, imp%inches%unit, imp%inches%numerator, imp%inches%denominator, imp%inches%status

    ! imp2 = imperialscale(imp, 87.)

    ! print *, imp2%imperialdecimal, imp2%metricdecimal, imp2%scale
    !print *, imp2%feet, imp2%inches%unit, imp2%inches%numerator, imp2%inches%denominator, imp2%inches%status
    ! test = '1/2'
    ! f1 = returnfraction(test)
    ! print *, "Test: ", test
    ! print *, "Fraction: ", f1%unit, f1%numerator, f1%denominator, f1%status

end program scaleconvert
