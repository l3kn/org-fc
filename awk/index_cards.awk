BEGIN {
    FS="|";

    fc_tag = ":" or_default(fc_tag, "fc") ":";
    suspended_tag = ":" or_default(suspended_tag, "suspended") ":";
    review_data_drawer = ":" or_default(review_data_drawer, "REVIEW_DATA") ":";
    type_property = or_default(type_property, "FC_TYPE");
    created_property = or_default(created_property, "FC_CREATED");

    print "(";
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
    id = properties["ID"];
    type = properties[type_property];
    created = properties[created_property];
    print "  (" \
        ":path \"" FILENAME "\"" \
        " :id \""  id "\"" \
        " :type " type \
        " :suspended " (suspended ? "t" : "nil") \
        " :created \"" created "\"" \
        ")"

    in_properties = 0;
    in_card = 0;
}

END {
    print ")";
}
