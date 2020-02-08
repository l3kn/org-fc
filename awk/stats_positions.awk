BEGIN {
    FS="\t";
    total = 0;
    suspended = 0;
    ease = 0;
    interval = 0;
    box = 0;
    due = 0;
    now = strftime("%FT%TZ", systime(), 1);
    n_stats = 0;
}

{
    total += 1;

    type = $3;
    by_type[type] += 1;

    # Don't collect ease / box / interval stats for suspended cards
    if ($4 == "1") {
        suspended += 1;
    } else {
        ease += $6;
        box += $7;
        interval += $8;
        n_stats++;
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
    print "avg-ease" "\t" ease / n_stats;
    print "avg-box" "\t" box / n_stats;
    print "avg-interval" "\t" interval / n_stats;
}
