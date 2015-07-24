#pragma once

#include <stdint.h>
#include "arch_event.h"

void events_poll(uint64_t ticks);
int events_do_pending(void);

#if LING_XEN  // test!
void events_init(void);
void event_bind(uint32_t port, event_entry_t entry, void *data);
void event_unbind(uint32_t port);
uint32_t event_alloc_unbound(domid_t remote_domid);
uint32_t event_bind_interdomain(domid_t remote_domid, uint32_t remote_port);
uint32_t event_bind_virq(uint32_t virq, event_entry_t entry, void *data);
void event_kick(uint32_t port);

static inline void event_clear(uint32_t port)
{
    shared_info_t *s = HYPERVISOR_shared_info;
    synch_clear_bit(port, &s->evtchn_pending[0]);
}

static inline void event_mask(uint32_t port)
{
    shared_info_t *s = HYPERVISOR_shared_info;
    synch_set_bit(port, &s->evtchn_mask[0]);
}
#endif

//EOF
