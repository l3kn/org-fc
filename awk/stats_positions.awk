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
    print "("
    print "  :total " total;
    print "  :suspended " suspended;
    print "  :due " due;
    for (var in by_type) {
        print "  :type-" var " " by_type[var];
    }

    if (n_stats > 0) {
        print "  :avg-ease " ease / n_stats;
        print "  :avg-box " box / n_stats;
        print "  :avg-interval " interval / n_stats;
    } else {
        print "  :avg-ease " 0.0;
        print "  :avg-box " 0;
        print "  :avg-interval " 0.0;
    }
    print ")"
}
