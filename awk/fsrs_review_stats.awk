BEGIN {
    FS = "\t"
    t_day = strftime("%FT00:00:00Z", systime());
    # t_day = time_days_ago(1);
    t_week = time_days_ago(7);
    t_month = time_days_ago(30);
}

{
    date = $1;
    state = $5;
    rating = $6;

    if (date > t_day) {
        ratings_day[state][rating]++;
        n_day[state]++;
    }

    if (date > t_week) {
        ratings_week[state][rating]++;
        n_week[state]++;
    }

    if (date > t_month) {
        ratings_month[state][rating]++;
        n_month[state]++;
    }

    ratings_all[state][rating]++;
    n_all[state]++;
}

END {
    print "("
    print "  new"

    print "("
    print "  :all"
    report(ratings_all["new"], n_all["new"]);
    print "  :month"
    report(ratings_month["new"], n_month["new"]);
    print "  :week"
    report(ratings_week["new"], n_week["new"]);
    print "  :day"
    report(ratings_day["new"], n_day["new"]);
    print ")"

    print "  learning"

    print "("
    print "  :all"
    report(ratings_all["learning"], n_all["learning"]);
    print "  :month"
    report(ratings_month["learning"], n_month["learning"]);
    print "  :week"
    report(ratings_week["learning"], n_week["learning"]);
    print "  :day"
    report(ratings_day["learning"], n_day["learning"]);
    print ")"

    print "  relearning"

    print "("
    print "  :all"
    report(ratings_all["relearning"], n_all["relearning"]);
    print "  :month"
    report(ratings_month["relearning"], n_month["relearning"]);
    print "  :week"
    report(ratings_week["relearning"], n_week["relearning"]);
    print "  :day"
    report(ratings_day["relearning"], n_day["relearning"]);
    print ")"

    print "  review"

    print "("
    print "  :all"
    report(ratings_all["review"], n_all["review"]);
    print "  :month"
    report(ratings_month["review"], n_month["review"]);
    print "  :week"
    report(ratings_week["review"], n_week["review"]);
    print "  :day"
    report(ratings_day["review"], n_day["review"]);
    print ")"

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
