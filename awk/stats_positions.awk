BEGIN {
    FS="\t";
    total = 0;
    suspended = 0;
    ease = 0;
    interval = 0;
    box = 0;
    due = 0;
    now = strftime("%FT%T", systime(), 1);
}

{
    total += 1;

    type = $3;
    by_type[type] += 1;

    ease += $6;
    box += $7;
    interval += $8;

    if ($4 == "1") {
        suspended += 1;
    }
    if ($4 == "0" && $9 < now) {
        due += 1;
    }
}

END {
    print "total" "\t" total;
    print "suspended" "\t" suspended;
    print "due" "\t" due;
    for (var in by_type) {
        print "type-" var "\t" by_type[var];
    }
    print "avg-ease" "\t" ease / NR;
    print "avg-box" "\t" box / NR;
    print "avg-interval" "\t" interval / NR;
}
