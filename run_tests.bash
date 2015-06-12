#!/bin/bash

main()
{
    clean_up
    compile_parse_transform
    test_ expressions_per_line_numbers
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
    echo -n $@ ':'
    eval $@
}

expressions_per_line_numbers()
{
    erlc test/expressions_per_line.erl -o ebin/ -pa ebin/ > /tmp/res
    grep -qF 'number_of_expressions_per_line [{9,2},{10,3},{16,2},{22,5}]' /tmp/res
    [[ $? == 0 ]] && echo 'PASSED' || echo 'FAILED'    
}

main