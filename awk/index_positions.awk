BEGIN {
    FS="|";
    now = strftime("%FT%TZ", systime(), 1);

    fc_tag = ":" or_default(fc_tag, "fc") ":";
    suspended_tag = ":" or_default(suspended_tag, "suspended") ":";
    review_data_drawer = ":" or_default(review_data_drawer, "REVIEW_DATA") ":";
    type_property = or_default(type_property, "FC_TYPE");
    created_property = or_default(created_property, "FC_CREATED");
}

## Heading Parsing

/^\*+[ \t]+.*$/ {
    # tag re based on org-tag-re
    match($0, /^\*+[ \t]+.*[ \t]+(:([a-zA-Z0-9_@#%]+:)+)$/, a)
    tags = a[1]

    id = "none";

    if (tags ~ fc_tag) {
        in_card = 1;
        suspended = (tags ~ suspended_tag);
    } else {
        in_card = 0;
    }
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

        if (!(filter_due == "1") || (due < now && suspended == "0")) {
            print FILENAME "\t" id "\t" type "\t" suspended "\t" position "\t" ease "\t" box "\t" interval "\t" due "\t" inherited_tags "\t" local_tags;
        }
    }
}
