#!/usr/bin/env fish

function usage
        set app_name expired
        echo "Usage:" $app_name "-d [card-dir] (-e)"
end

function get_expired_dirs -a card_dir
        # Changed to the schedule directory
        pushd $card_dir
        
        set sched_dirs (find schedule -mindepth 1 -maxdepth 1 -not -path . -type d -print)
        set current_date (date "+%s")

        for expired_date_dir in $sched_dirs
                set expired_date_str (basename $expired_date_dir)
                set expired_date (date -j -f "%Y-%m-%dT%H:%M:%SZ" $expired_date_str "+%s")
                if test $expired_date -le $current_date
                        echo $expired_date_dir
                end
        end

        popd
end

getopts $argv | while read -l key value
        switch $key
                case d
                        set card_dir $value
                case e
                        set only_expired "true"
        end
end

if test -z $card_dir
        echo "Please give a card directory"
        usage
        exit 1
end

for expired_card_dir in (get_expired_dirs $card_dir)
        printf "$card_dir/%s\n" $expired_card_dir
end

if test -z "$only_expired"
        for fname in (find "$card_dir/cards" -name "*.json" -print)
                if test -n (jq -r '.expires' $fname)
                        echo $fname
                end
        end
end
