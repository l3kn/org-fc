BEGIN {
    FS="\t";
    now = strftime("%FT%T", systime(), 1);
}

$4 == "0" && $9 < now {
    print $0
}
