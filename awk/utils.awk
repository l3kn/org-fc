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
    return strftime("%FT%T", systime() - 24 * 60 * 60 * n, 1);
}

function or_default(var, def) {
    return var ? var : def;
}
