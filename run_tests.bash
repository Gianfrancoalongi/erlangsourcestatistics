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
    rebar eunit
}

expressions_per_line_numbers ()
{
    erlc -o ebin/ -pa ebin/ test/expressions_per_line.erl > /tmp/res
    grep -qF 'number_of_expressions_per_line [{9,2},{10,3},{16,2},{22,5}]' /tmp/res
    [[ $? == 0 ]] && echo 'PASSED' || echo 'FAILED'    
}

expressions_per_function ()
{
    erlc  -o ebin/ -pa ebin/ test/expressions_per_function.erl > /tmp/res
    grep -qF 'number_of_expressions_per_function [{{a,1},17},{{b,0},1}]' /tmp/res
    [[ $? == 0 ]] && echo 'PASSED' || echo 'FAILED'    
}

functions_in_a_module ()
{
    erlc -o ebin/ -pa ebin/ test/functions_in_a_module.erl > /tmp/res
    grep -qF 'number_of_functions_per_module 3' /tmp/res
    [[ $? == 0 ]] && echo 'PASSED' || echo 'FAILED'
}

function_clauses_per_function ()
{
    erlc -o ebin/ -pa ebin/ test/function_clauses_per_function.erl  > /tmp/res
    grep -qF 'number_of_function_clauses_per_function [{{a,3},6},{{b,2},4},{{c,1},2}]' /tmp/res
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

structural_depth ()
{
    erlc -o ebin/ -pa ebin/ test/structural_depth.erl > /tmp/res
    grep -qF 'structural_depth ' /tmp/res
    [[ $? == 0 ]] && echo 'PASSED' || echo 'FAILED'
}

main