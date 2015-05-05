
/* Minutes from UTC to local time */
#ifdef TIMEZONE
#undef TIMEZONE
#endif

#ifdef TIMEZONE_NS
#undef TIMEZONE_NS
#endif

#define TIMEZONE		(10800)
#define TIMEZONE_NS		(TIMEZONE * 1000000000ULL)

#define SECS_TO_NS(secs)	((secs) * 1000000000ULL)
#define MS_TO_NS(ms)		((ms) * 1000000ULL)
#define US_TO_NS(ms)		((ms) * 1000ULL)
#define NS_TO_SECS(ns)		((ns) / 1000000000ULL)
#define NS_TO_MS(ns)		((ns) / 1000000ULL)
#define NS_TO_US(ns)		((ns) / 1000ULL)

void get_time_values_from_xen(void);
void dump_time_values(void);
