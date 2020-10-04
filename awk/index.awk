BEGIN {
    # The only time we're interested in multiple fields is when
    # parsing the review data drawer.
    FS="|";

    now = strftime("%FT%TZ", systime(), 1);

    fc_tag = ":" or_default(fc_tag, "fc") ":";
    suspended_tag = ":" or_default(suspended_tag, "suspended") ":";
    review_data_drawer = ":" or_default(review_data_drawer, "REVIEW_DATA") ":";
    type_property = or_default(type_property, "FC_TYPE");
    cloze_type_property = or_default(cloze_type_property, "FC_CLOZE_TYPE");
    created_property = or_default(created_property, "FC_CREATED");

    # Small state machine to make sure cards are in the correct format
    state = 0;
    state_file = 0;
    state_card = 1;
    state_properties = 2;
    state_properties_done = 3;
    state_review_data = 4;
    state_review_data_done = 5;

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
    print ")  :title " (file_title ? escape_string(file_title) : "nil") ")";
}

## File Tags

match($0, /^#\+FILETAGS:[ \t]+(.*)/, a) {
    # Combine tags to handle multiple FILETAGS lines
    parent_tags[0] = combine_tags(a[1], parent_tags[0]);
    next;
}

## File Title

match($0, /^#\+TITLE:[ \t]+(.*)/, a) {
    # Combine tags to handle multiple FILETAGS lines
    file_title = a[1]
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
    if (match(title, /^(.*)[ \t]+(:([a-zA-Z0-9_@#%]+:)+)$/, b) != 0) {
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
        delete review_data;
        review_index = 1;
        state = state_review_data;
    }
    next;
}

/:END:/ {
    if (state == state_properties) {
        state = state_properties_done;
    } else if (state == state_review_data) {
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
            " :suspended " (suspended ? "t" : "nil")   \
            " :inherited-tags " escape_string(inherited_tags)  \
            " :local-tags " escape_string(local_tags)          \
            " :positions (";

        # Card positions
        for (i = 1; i < review_index; i++) {
            print "      ("               \
                ":position " escape_string(review_data[i]["position"])  \
                " :ease " review_data[i]["ease"]                \
                " :box " review_data[i]["box"]                  \
                " :interval " review_data[i]["interval"]        \
                " :due " parse_time(review_data[i]["due"])      \
                ")"
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

# TODO: Explicit match, to check for broken drawers
#
# Positions are collected in an array first,
# in case the review drawer is broken.
(state == state_review_data) && /^\|.*\|$/ {
    # check NF to skip the |--+--| table separator
    # match on $2 to skip the table header
    if (NF == 7 && $2 !~ "position") {
        review_data[review_index]["position"] = trim($2);
        review_data[review_index]["ease"] = trim($3);
        review_data[review_index]["box"] = trim($4);
        review_data[review_index]["interval"] = trim($5);
        review_data[review_index]["due"] = trim_surrounding($6);
        review_index += 1;
    }
    next;
}

END {
    print ")";
}
