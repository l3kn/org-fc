BEGIN {
    FS="\t";
    total = 0;
    suspended = 0;

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

    if ($4 == "1") {
        suspended++;
    }

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

END {
    print "total" "\t" total;
    print "suspended" "\t" suspended;
    print "created-day" "\t" created["day"];
    print "created-week" "\t" created["week"];
    print "created-month" "\t" created["month"];
    for (var in by_type) {
        print "type-" var "\t" by_type[var];
    }
}
