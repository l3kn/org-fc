BEGIN {
    FS = "\t"
    print "("
}

{
    date = $1;
    file_path = $2;
    card_id = $3;
    position_name = $4;
    rating = $8;

    if ((filter_card_id == "any" || filter_card_id == card_id) && \
        (filter_position_name == "any" || filter_position_name == position_name)) {
        print "(" \
            ":date " escape_string(date) \
            " :card_id " escape_string(card_id) \
            " :position_name " escape_string(position_name) \
            " :rating " escape_string(rating) \
            ")"
    }
}

END {
    print ")"
}
