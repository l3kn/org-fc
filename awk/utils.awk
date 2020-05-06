## Helper functions

# Remove all whitespace in str
function trim(str) {
    gsub(/[ \t]/, "", str);
    return str;
}

# Remove all whitespace around str
function trim_surrounding(str) {
    gsub(/^[ \t]*/, "", str);
    gsub(/[ \t]*$/, "", str);
    return str;
}

# Time n days before the current time
function time_days_ago(n) {
    return strftime("%FT%TZ", systime() - 24 * 60 * 60 * n, 1);
}

function or_default(var, def) {
    return (var != "") ? var : def;
}

# Combine two tag strings
function combine_tags(tags1, tags2) {
    if (tags1 == "") {
        return tags2;
    } else if (tags2 == "") {
        return tags1;
    } else {
        return substr(tags1, 0, length(tags1) - 1) tags2
    }
}

# Convert an ISO8601 timestamp to an Emacs timestamp
# (second_upper_16_bit, second_lower_16_bit)
function parse_time(time) {
    # mktime expects a format of "YYYY MM DD HH MM SS"
    # and doesn't care about the trailing space left by the "Z"
    gsub(/[\-T:Z]/, " ", time);

    ts = mktime(time, 1);
    ts_h = rshift(ts, 16);
    ts_l = and(ts, 0xffff);

    return "(" ts_h " " ts_l ")";
}

# TODO: I'm sure there are cases not covered by this
function escape_string(str) {
    gsub(/\\/, "\\\\", str);
    gsub(/"/, "\\\"", str);
    return "\"" str "\"";
}
