module errorcheckmod
    contains
    integer function errorcheck(check, string) result(ierr)
        implicit none
        logical,intent(IN) :: check
        character(len=*),intent(IN)  :: string
    
        ierr=0
        if (.not.check) then
            print *,string//' Restarting calculation'
            ierr=-1
            !print *,string//' Run failed'
            !stop
        end if
    end function
end module

program giftswap
    use errorcheckmod
    implicit none
    integer, parameter :: gift_group_size = 8
    !integer, parameter :: gift_group_size = 1000000
    integer, parameter :: max_checks = 1000
    real    :: rand
    integer :: give_list(gift_group_size), partner_list(gift_group_size)
    integer :: recv_list(gift_group_size), free_list(gift_group_size)
    integer :: last_spot, random_spot
    integer :: gift_giver, random_person, icheck, ierr, nchecks
    logical :: partner_assigned, running
    integer :: iunit
    character(len=100) :: fmt1
    
    running=.true.
    restart: do while (running) ! itry=1,max_checks
        do gift_giver=1,gift_group_size
            free_list(gift_giver)=gift_giver
        end do
        partner_list(1:8)=[2,1, 4,3, 6,5, 8,7]
        
        give_list(:)=0
        recv_list(:)=0
        last_spot=gift_group_size
        
        ! pick a recipient for each member
        do gift_giver=1,gift_group_size
            ! check if run failed on last iteration
            if (gift_giver==gift_group_size) then
                ierr = errorcheck(free_list(1)/=gift_giver, 'Last person got themselves!')
                if (ierr/=0) cycle restart
                ierr = errorcheck(free_list(1)/=recv_list(gift_giver), 'Two people got eachother!')
                if (ierr/=0) cycle restart
                ierr = errorcheck(free_list(1)/=partner_list(gift_giver), 'Last member got their partner!')
                if (ierr/=0) cycle restart
            end if
        
            ! assign random person
            partner_assigned=.false.
            do icheck=1,max_checks
                ! random number generator
                call random_number(rand)
                random_spot = int(rand*(gift_group_size-gift_giver+1)) + 1
                random_person = free_list(random_spot)
        
                ! no picking yourself
                if (random_person == gift_giver) cycle ! try again.
                ! check to make sure 2 people didn't get eachother
                if (give_list(random_person) == gift_giver) cycle ! try again
                ! check to make sure no one got their partner
                if (partner_list(gift_giver) == random_person) cycle ! try agaiin
        
                ! if everything is ok, assign recipient
                give_list(gift_giver) = random_person
                recv_list(random_person) = gift_giver
        
                ! update list of who is free. move last person to free'd spot
                free_list(random_spot) = free_list(last_spot)
                ! update new last spot
                free_list(last_spot)   = 0
                last_spot = gift_group_size-gift_giver
                partner_assigned=.true.
        
                exit !exits random number loop
            end do
            ierr = errorcheck(partner_assigned, 'Max checks reached!')
            if (ierr/=0) cycle restart
        end do
        running=.false. ! finished. stop running
    end do restart

    print *,'Printing gift give list'
    iunit=915
    open(unit=iunit,file='output.txt',form='formatted')
    fmt1="(A,I7,A,I7)"
    do gift_giver=1,gift_group_size
        write(iunit,fmt1) 'member ',gift_giver,' gives to member ',give_list(gift_giver)
!        write(fmt1,6) name_list(gift_giver), name_list(give_list(gift_giver))
    end do
    close(iunit)
end program

