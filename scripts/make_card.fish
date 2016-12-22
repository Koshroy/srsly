#!/usr/bin/env fish

getopts $argv | while read -l key value
        switch $key
                case f
                        set front_text $value
                case front
                        set front_text $value
                case b
                        set back_text $value
                case back
                        set back_text $value
        end
