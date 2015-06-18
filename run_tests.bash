#!/bin/bash

main()
{
    clean_up
    compile_parse_transform
    test_ expressions_per_line_numbers
    test_ expressions_per_function
    test_ functions_in_a_module
    test_ function_clauses_per_function    
    test_ number_of_expressions_in_module
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

number_of_expressions_in_module ()
{
    erlc -o ebin/ -pa ebin/ test/number_of_expressions_in_module.erl  > /tmp/res
    grep -qF 'number_of_expressions_in_module 17' /tmp/res
    [[ $? == 0 ]] && echo 'PASSED' || echo 'FAILED'
}

main