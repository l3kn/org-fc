BEGIN {
    FS="|";
    now = strftime("%FT%TZ", systime(), 1);

    fc_tag = ":" or_default(fc_tag, "fc") ":";
    suspended_tag = ":" or_default(suspended_tag, "suspended") ":";
    review_data_drawer = ":" or_default(review_data_drawer, "REVIEW_DATA") ":";
    type_property = or_default(type_property, "FC_TYPE");
    created_property = or_default(created_property, "FC_CREATED");

    print "(";
}

BEGINFILE {
    # Stack of parent headline tags, level 0 is used for filetags
    parent_tags[0] = "";
}

## Filetags

match($0, /#\+FILETAGS:[ \t]+(.*)/, a) {
    parent_tags[0] = a[1];
}

## Heading Parsing

match($0, /^(\*)+[ \t]+.*$/, a) {
    level = length(a[1]);
    tags = ""

    # tag re based on org-tag-re
    if (match($0, /^\*+[ \t]+.*[ \t]+(:([a-zA-Z0-9_@#%]+:)+)$/, b) != 0) {
            tags = b[1];
    }
    parent_tags[level] = tags;

    id = "none";

    if (tags ~ fc_tag) {
        in_card = 1;
        suspended = (tags ~ suspended_tag);
    } else {
        in_card = 0;
    }

    last_level = level;
    next
}

## Property parsing

in_card && /:PROPERTIES:/ {
    in_properties = 1;
    delete properties;
}

in_properties && match($0, /^[ \t]*:([a-zA-Z0-9_]+):[ \t]*(.+)$/, a)  {
    properties[a[1]] = trim_surrounding(a[2]);
}

in_properties && /:END:/ {
    in_properties = 0;
}

## Review data parsing

in_card && $0 ~ review_data_drawer {
    in_data = 1;
}

in_data && /:END:/ {
    in_data = 0;
}

in_data && /^\|.*\|$/ {
    # Make sure we're inside a data block,
    # check NF to skip the |--+--| table separator
    # match on $2 to skip the table header
    if (in_data == 1 && NF == 7 && $2 !~ "position") {
        id = properties["ID"];
        type = properties[type_property];

        position = trim($2);
        ease = trim($3);
        box = trim($4);
        interval = trim($5);
        due      = trim_surrounding($6);

        inherited_tags = "";
        for (i = 0; i < level; i++) {
            inherited_tags = combine_tags(inherited_tags, parent_tags[i]);
        }
        local_tags = parent_tags[level];

        if (!(filter_due == "1") || (due < now && suspended == "0")) {
            print "(" \
                ":path \"" FILENAME "\"" \
                " :id \"" id "\"" \
                " :type " type \
                " :suspended " (suspended ? "t" : "nil")  \
                " :position \"" position "\"" \
                " :ease " ease \
                " :box " box \
                " :interval " interval \
                " :due \"" due "\"" \
                " :inherited-tags \"" inherited_tags "\"" \
                " :local-tags \"" local_tags "\"" \
                ")"
        }
    }
}

END {
    print ")";
}
