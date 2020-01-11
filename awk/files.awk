BEGIN {
    FS="|";
}

BEGINFILE {
    has_card = 0;
}

# Flashcard headings
/^\*+ .*:fc:.*$/ {
    has_card = 1;
}

ENDFILE {
    if (has_card == 1) {
        print FILENAME;
    }
}
