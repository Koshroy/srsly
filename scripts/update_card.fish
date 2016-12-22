#!/usr/bin/env fish

set iso_date_spec "%Y-%m-%dT%H:%M:%SZ"

function usage
        set app_name update_card
        echo "Usage:" $app_name "-c [choice] <card-name.json>"
end

function next_expiration_date -a choice level ef
        set whole_ef (echo $ef | sed 's/\..*//')
        set days_forward (math "$whole_ef * (3^$level)")
        echo (date -j -v 0M -v 0S -v "+$days_forward"d "+$iso_date_spec")
end

function next_level -a choice level
        if test $level -eq 0
                echo 0
        else
                echo (math "1+$level")
        end
end

function next_ef -a level ef
        set ef_prime (math $ef "-0.8 + 0.28*$level" - "0.02*$level*$level")
        if math "$ef_prime < 1.3" > /dev/null
                echo 1.3
        else
                echo $ef_prime
        end
end

set default_ef 2.5

getopts $argv | while read -l key value
        switch $key
                case c
                        set choice $value
                case _
                        set card_fname $value
        end
end

if test -z "$choice" -o -z "$card_fname"
        echo "Parameters missing"
        usage
        exit 1
end

set level (jq -r '.level' $card_fname)
set ef (jq -r '.learning.ef' $card_fname | sed 's/null//')
if test -z $ef
        set ef $default_ef
end

set new_level (next_level $choice $level)
set new_date (next_expiration_date $choice $level $ef)
set new_ef (next_ef $level $ef)
set temp_fname (mktemp update_card.XXXXX)

jq ".level = $new_level | .expires = \"$new_date\" | .learning.ef = $new_ef" $card_fname > $temp_fname
cp $temp_fname $card_fname
