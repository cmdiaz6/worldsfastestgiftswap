function assign_list(gift_group_size, max_checks, partner_list)
    give_list = zeros(Int64, gift_group_size)
    free_list = collect(1:gift_group_size)
    recv_list = zeros(Int64, gift_group_size)
    
    # pick a recipient for each member
    for gift_giver=1:gift_group_size
        # check if run failed on last iteration
        if gift_giver==gift_group_size
            # checks if last person got themself
            @assert free_list[1]!=gift_giver "Last person got themselves! Run failed"
            # checks if 2 people got eachother
            @assert free_list[1]!=recv_list[gift_giver] "Two people got eachother! Run failed"
            # check partner
            @assert free_list[1]!=partner_list[gift_giver] "Last member got their partner! Run failed"
        end
    
        # assign random person
        run_passed=false
        for icheck=1:max_checks
            last_spot = gift_group_size-gift_giver+1
            # random number generator
            random_spot = rand(1:last_spot)
            random_person = free_list[random_spot]
            random_person == gift_giver && continue # try again. no picking yourself
    
            # check to make sure 2 people didn't get eachother
            give_list[random_person] == gift_giver && continue # try again
    
            # check to make sure no one got their partner
            partner_list[gift_giver] == random_person && continue # try again
    
            # if everything is ok, assign recipient
            give_list[gift_giver] = random_person
            recv_list[random_person] = gift_giver
    
            # update list of who is free. move last person to free'd spot
            free_list[random_spot] = free_list[last_spot]
            # update new last spot
            free_list[last_spot] = 0
            run_passed=true
    
            break #exits random number loop
        end
        @assert run_passed string(max_checks," checks reached! Run failed")
    end
    return give_list
end

const gift_group_size = 8
#const gift_group_size = 1000000
const max_checks = 1000
const partner_list = zeros(Int64, gift_group_size)
const partner_list[1:8] = [2,1, 4,3, 6,5, 8,7]

give_list = @time assign_list(gift_group_size, max_checks, partner_list)

println("Printing give list")
io = open("output.txt","w")
for gift_giver=1:size(give_list,1)
    write(io, "member $gift_giver gives to ", string(give_list[gift_giver]), " \n")
end
close(io)
