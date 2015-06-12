#!/bin/bash

main()
{
    clean_up
    compile_parse_transform
    test_ expressions_per_line_numbers
    test_ expressions_per_function
    test_ functions_in_a_module
}

clean_up() 
{ 
    rebar clean 
}

compile_parse_transform() 
{ 
    rebar compile 
}

test_ ()
{
    echo -n $@ ' = '
    eval $@
}

expressions_per_line_numbers ()
{
    erlc test/expressions_per_line.erl -o ebin/ -pa ebin/ > /tmp/res
    grep -qF 'number_of_expressions_per_line [{9,2},{10,3},{16,2},{22,5}]' /tmp/res
    [[ $? == 0 ]] && echo 'PASSED' || echo 'FAILED'    
}

expressions_per_function ()
{
    erlc test/expressions_per_function.erl -o ebin/ -pa ebin/ > /tmp/res
    grep -qF 'number_of_expressions_per_function [{a,1,17},{b,0,1}]' /tmp/res
    [[ $? == 0 ]] && echo 'PASSED' || echo 'FAILED'    
}

functions_in_a_module ()
{
    erlc test/functions_in_a_module.erl -o ebin/ -pa ebin/ > /tmp/res
    grep -qF 'number_of_functions_per_module 3' /tmp/res
    [[ $? == 0 ]] && echo 'PASSED' || echo 'FAILED'
}

main