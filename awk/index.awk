### Commentary
#
# This file implements a parser that reads org files, extracts data
# relevant to org-fc and prints it as an S-expression so it can be
# parsed with EmacsLisp's read function.
#
# The org format is mostly line based.
# A small state machine is used to keep track of where we are in a file,
# (e.g. inside a card, reading heading properties, reading review data).
#
# Some parsing of review data columns is done.
#
# The position is escaped as a string and the due date is converted
# into Emacs's date format because it's a bit faster in AWK than in
# EmacsLisp.
#
# All other columns of the review data table are assumed to be numeric
# values and included in the output S-expression without any escaping.
#
# Because of the complicated rules used by org-mode to determine a
# heading's tags, inherited (file / parent heading) and local tags are
# tracked separately and later combined using an org-mode function.
#
### Code

BEGIN {
    # The only time we're interested in multiple fields is when
    # parsing the review data drawer.
    #
    # Treating whitespace as part of the field separator instead of
    # stripping it from the fields afterwards is a bit faster.
    FS="[ \t]*|[ \t]*";

    now = strftime("%FT%TZ", systime(), 1);

    fc_tag = ":" or_default(fc_tag, "fc") ":";
    suspended_tag = ":" or_default(suspended_tag, "suspended") ":";
    review_data_drawer = ":" or_default(review_data_drawer, "REVIEW_DATA") ":";
    type_property = or_default(type_property, "FC_TYPE");
    cloze_type_property = or_default(cloze_type_property, "FC_CLOZE_TYPE");
    created_property = or_default(created_property, "FC_CREATED");
    blocked_by_property = or_default(blocked_by_property, "FC_BLOCKED_BY");
    priority_property = or_default(priority_property, "FC_PRIORITY");

    # Small state machine to make sure cards are in the correct format
    state = 0;
    state_file = 0;
    state_card = 1;
    state_properties = 2;
    state_properties_done = 3;
    state_review_data = 4;
    state_review_data_body = 5;
    state_review_data_done = 6;

    print "(";
}

## File Parsing

BEGINFILE {
    # Reset filetags
    delete parent_tags;
    file_title = "";
    parent_tags[0] = "";
    state = state_file;

    print "  (" \
        ":path " escape_string(FILENAME) \
        " :cards (";
}

ENDFILE {
    # On `BEGINFILE` we don't know the file's title yet so we output
    # it once done processing the rest of the file.
    print "  )  :title " (file_title ? escape_string(file_title) : "nil") ")";
}

## File Tags

match($0, /^#\+(FILETAGS|filetags):[ \t]+(.*)/, a) {
    # Combine tags to handle multiple FILETAGS lines
    parent_tags[0] = combine_tags(a[2], parent_tags[0]);
    next;
}

## File Title

match($0, /^#\+(TITLE|title):[ \t]+(.*)/, a) {
    # Combine tags to handle multiple FILETAGS lines
    file_title = a[2]
    next;
}

## Heading Parsing

match($0, /^(\*+)[ \t]+(.*)$/, a) {
    level = length(a[1]);
    title = a[2];
    tags = "";

    # tag re based on org-tag-re
    # this only guarantees that there is at least one tab/space
    # between the headline text and the tags.
    # TODO: Do this in a single match
    if (match(title, /^(.*)[ \t]+(:([[:alnum:]_@#%]+:)+)$/, b) != 0) {
        title = b[1];
        # remove trailing tabs/spaces
        sub(/[ \t]*$/, "", title);
        tags = b[2];
    }
    parent_tags[level] = tags;

    id = "none";

    if (tags ~ fc_tag) {
        state = state_card;
        suspended = (tags ~ suspended_tag);
    }
    next;
}

## Drawer Parsing

/:PROPERTIES:/ {
    if (state == state_card) {
        state = state_properties;
        delete properties;
    }
    next;
}

$0 ~ review_data_drawer {
    # Make sure the review data comes after the property drawer
    if (state == state_properties_done) {
        delete review_data_columns;
        review_data_ncolumns = 0;

        delete review_data;
        review_index = 1;

        state = state_review_data;
    }
    next;
}

/:END:/ {
    if (state == state_properties) {
        state = state_properties_done;
    } else if (state == state_review_data_body) {
        state = state_review_data_done;
        # Card header
        inherited_tags = "";
        for (i = 0; i < level; i++) {
            inherited_tags = combine_tags(inherited_tags, parent_tags[i]);
        }
        local_tags = parent_tags[level];

        cloze_type = ""
        if (cloze_type_property in properties)
            cloze_type = " :cloze-type " properties[cloze_type_property]

        print "    (" \
            ":id " escape_string(properties["ID"])  \
            " :title " escape_string(title)  \
            " :type " properties[type_property]     \
            cloze_type                                            \
            " :created " parse_time(properties[created_property]) \
            " :blocked-by " escape_string(properties[blocked_by_property]) \
            " :priority " escape_string(properties[priority_property]) \
            " :suspended " (suspended ? "t" : "nil")   \
            " :inherited-tags " escape_string(inherited_tags)  \
            " :local-tags " escape_string(local_tags)          \
            " :positions (";

        # Card positions
        for (i = 1; i < review_index; i++) {
            print "      (";
            for (j = 1; j <= review_data_ncolumns; j++) {
                col = review_data_columns[j];
                val = review_data[i][col];

                # TODO: extract values as strings, parse in Emacs when
                # necessary.
                if (col == "due") {
                    val = parse_time(val);
                } else if (col == "position") {
                    val = escape_string(val);
                }
                print "        :" col " " val;
            }
            print "      )";
        }
        print "    ))";
    }
    next;
}

## Property Parsing

(state == state_properties) && match($0, /^[ \t]*:([a-zA-Z0-9_]+):[ \t]*(.+)$/, a)  {
    properties[a[1]] = trim_surrounding(a[2]);
    next;
}

## Review data parsing

# Table separator
(state == state_review_data) && /^\|[-+]+\|$/ {
    state = state_review_data_body;
    next;
}

# Column Names
# NOTE: This line comes before the table separator in the file but to
# keep the regex simple, we match it later.
(state == state_review_data) && /^\|.*\|$/ {
    # Skip the first and last empty fields
    for (i = 2; i <= (NF - 1); i++) {
        review_data_columns[i - 1] = $i;
    }
    review_data_ncolumns = NF - 2;
    next;
}

# Positions are collected in an array first,
# in case the review drawer is broken.
(state == state_review_data_body) && /^\|.*\|$/ {
    if (NF == (review_data_ncolumns + 2)) {
        for (i = 2; i <= (NF - 1); i++) {
            column = review_data_columns[i - 1];
            review_data[review_index][column] = $i;
        }
        review_index += 1;
    }
    next;
}

END {
    print ")";
}
