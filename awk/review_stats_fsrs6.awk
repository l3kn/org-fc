BEGIN {
    FS = "\t"
    t_day = time_days_ago(1);
    t_week = time_days_ago(7);
    t_month = time_days_ago(30);
}

{
    date = $1;
    rating = $8;

    # Ensure the algorithm used is a fsrs variant
    if (NF == 10 && $10 ~ /^fsrs6/) {
        if (date > t_day) {
            ratings_day[rating]++;
            n_day++;
        }

        if (date > t_week) {
            ratings_week[rating]++;
            n_week++;
        }

        if (date > t_month) {
            ratings_month[rating]++;
            n_month++;
        }

        ratings_all[rating]++;
        n_all++;
    }
}

END {
    print "("
    print "  :all"
    report(ratings_all, n_all);
    print "  :month"
    report(ratings_month, n_month);
    print "  :week"
    report(ratings_week, n_week);
    print "  :day"
    report(ratings_day, n_day);
    print ")"
}

function report(values, n) {
    print "  (:total " or_default(n, 0)                   \
        " :again " or_default(values["again"], 0)         \
        " :hard " or_default(values["hard"], 0)           \
        " :good " or_default(values["good"], 0)           \
        " :easy " or_default(values["easy"], 0)           \
        ")"
}
