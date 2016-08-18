#!/bin/bash

main()
{
    clean_up
    compile
    test
}

clean_up() 
{ 
    rebar clean 
}

compile() 
{ 
    rebar compile 
}

test()
{
    erl -pa ebin/ -eval 'ess_graphics:t().' -s init stop
#    erl -pa ebin/ -eval 'calibrator:run_dir().' -s init stop
#   erl -pa ebin/ -eval 'calibrator:run("test/erlslim.erl").' -s init stop
#   rebar eunit
}


functions_in_a_module ()
{
    erlc -o ebin/ -pa ebin/ test/functions_in_a_module.erl > /tmp/res
    grep -qF 'number_of_functions_per_module 3' /tmp/res
    [[ $? == 0 ]] && echo 'PASSED' || echo 'FAILED'
}

record_definitions_per_module ()
{
    erlc -o ebin/ -pa ebin/ test/record_definitions_in_a_module.erl > /tmp/res
    grep -qF 'number_of_record_definitions_per_module 4' /tmp/res
    [[ $? == 0 ]] && echo 'PASSED' || echo 'FAILED'
} 

includes_per_module ()
{ 
    erlc -o ebin/ -pa ebin/ test/includes_per_module.erl > /tmp/res
    grep -qF 'number_of_includes_per_module 2' /tmp/res
    [[ $? == 0 ]] && echo 'PASSED' || echo 'FAILED'    
}

defines_in_module ()
{
    echo 'NOT APPLICABLE'
}

number_of_variable_steppings_new1_new2_etc ()
{
    erlc -o ebin/ -pa ebin/ test/variable_stepping.erl > /tmp/res
    grep -qF 'variable_stepping ' /tmp/res
    [[ $? == 0 ]] && echo 'PASSED' || echo 'FAILED'
}

main