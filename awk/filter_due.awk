BEGIN {
    FS="\t";
    now = strftime("%FT%TZ", systime(), 1);
}

$4 == "0" && $9 < now {
    print $0
}
