BEGIN {
    FS = "\t"
    t_day = time_days_ago(1);
    t_week = time_days_ago(7);
    t_month = time_days_ago(30);
    min_box = or_default(min_box, 0);
}

{
    date = $1;
    file = $2;
    id = $3;
    position = $4;
    ease = $5;
    box = $6;
    interval = $7;
    rating = $8;

    if (box >= min_box) {
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
    report(ratings_all, n_all);
    report(ratings_month, n_month);
    report(ratings_week, n_week);
    report(ratings_day, n_day);
}

function report(values, n) {
    if (n == 0) {
        print 0 "\t" 0 "\t" 0 "\t" 0 "\t" 0;
    } else {
        print n "\t" values["again"] "\t" values["hard"] "\t" values["good"] "\t" values["easy"] ;

    }
}
