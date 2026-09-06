static const char *const multilib_raw[] = {
". !mmach=bbcb !mmach=bbcmaster !mmach=c64;",
"bbcb mmach=bbcb !mmach=bbcmaster !mmach=c64;",
"bbcm !mmach=bbcb mmach=bbcmaster !mmach=c64;",
"c64 !mmach=bbcb !mmach=bbcmaster mmach=c64;",
NULL
};

static const char *const multilib_reuse_raw[] = {
NULL
};

static const char *const multilib_matches_raw[] = {
"mmach=bbcb mmach=bbcb;",
"mmach=bbcmaster mmach=bbcmaster;",
"mmach=c64 mmach=c64;",
NULL
};

static const char *multilib_extra = "";

static const char *const multilib_exclusions_raw[] = {
NULL
};

static const char *multilib_options = "mmach=bbcb/mmach=bbcmaster/mmach=c64";
