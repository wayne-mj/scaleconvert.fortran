module scaleconverttypes
    use fractionmodule
    
    ! Type to hold imperial measurements
    type :: imperialtype
        integer :: feet
        type(fractiontype) :: inches
        character(len=32) :: status
    end type imperialtype

    ! Type to hold scale conversions
    type :: scaletype
        type(imperialtype) :: original          ! Original imperial value
        real :: imperialdecimal                 ! Decimal representation of imperial value
        real :: metricdecimal                   ! Decimal representation of metric value
        real :: scale                           ! Scale factor
        type(imperialtype) :: imperialbelow     ! Closest imperial value below
        type(imperialtype) :: imperialvalue     ! Imperial value at scale x / 100,000 simplified
        type(imperialtype) :: imperialabove     ! Closest imperial value above
    end type scaletype

    ! Delimiters
    character(len=1) :: feetdelim = "'"
    character(len=1) :: inchdelim = '"'
    character(len=1) :: slashdelim = "/"
    character(len=1) :: ratiodelim = ":"
    character(len=1) :: spacedelim = " "

    ! Constants
    integer, parameter :: inchprecision = 64    ! 64 (1), 32 (2), 16(4), 8(8), 4(16), 2(32), 1(64)
    real, parameter :: inch2mm = 25.4
    real, parameter :: baseDenominator = 10e+4
    real, parameter :: precision = 2.0
end module scaleconverttypes