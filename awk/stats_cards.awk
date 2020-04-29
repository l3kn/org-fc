BEGIN {
    FS="\t";
    total = 0;
    n_suspended = 0;

    t_day = time_days_ago(1);
    t_week = time_days_ago(7);
    t_month = time_days_ago(30);

    created["day"] = 0;
    created["week"] = 0;
    created["month"] = 0;
}

{
    total += 1;

    type = $3;
    by_type[type] += 1;
    suspended = $4 == "1";

    if (suspended) {
        n_suspended++;
    } else {
        if ($5 > t_day) {
            created["day"]++;
        }

        if ($5 > t_week) {
            created["week"]++;
        }

        if ($5 > t_month) {
            created["month"]++;
        }
    }
}

END {
    print "("
    print "  :total " total;
    print "  :suspended " n_suspended;
    print "  :created-day " created["day"];
    print "  :created-week " created["week"];
    print "  :created-month " created["month"];
    for (var in by_type) {
        print "  :type-" var " " by_type[var];
    }
    print ")"
}
