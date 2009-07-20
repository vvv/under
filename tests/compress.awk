#!/usr/bin/awk -f

########################################################################
## $ cat example
## s1  -- a statement
##   is split on several
##   lines
##
## -- this is a comment
##
## s2
##   -- this line is empty, it does not continue `s2'
##   is being continued despite commented line in the middle
##
## s3
##
## s4
## s5
##   67
##   89
########################################################################
## $ ./compress.awk example
## s1 is split on several lines
## s2 is being continued despite commented line in the middle
## s3
## s4
## s5 67 89
########################################################################

/--/ { # rstrip comments
    sub(/--.*$/, ""); # XXX strings can contain "--"; they are not comments
}

/[^ \t]/ {
    if (cont && /^[^ \t]/) # begin new ``statement''
	printf "\n";

    # Trailing whitespace + '\n' + leading whitespace are being
    # replaced with single whitespace.
    sub(/[ \t]+$/, "");
    sub(/^[ \t]+/, " ");

    printf "%s", $0;
    cont = 1;
}

END { if (cont) printf "\n"; }
