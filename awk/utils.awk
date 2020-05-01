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
    } else {
        return substr(tags1, 0, length(tags1) - 1) tags2
    }
}
