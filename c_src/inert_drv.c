/* Copyright (c) 2013, Michael Santos <michael.santos@gmail.com>
 * 
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#include <stdio.h>
#include <errno.h>

#include <sys/time.h>
#include <sys/resource.h>

#include <string.h>

#include "erl_driver.h"
#include "ei.h"

#define INERT_EBADFD    "ebadfd"
#define INERT_EINVAL    "einval"

#define INERT_FDSET     1
#define INERT_FDCLR     2

typedef struct {
    ErlDrvPort port;
} inert_drv_t;

typedef union {
    ErlDrvEvent ev;
    int32_t fd;
} inert_fd_t;

static int inert_copy(char **, ErlDrvSizeT *, char *, size_t);

    static ErlDrvData
inert_drv_start(ErlDrvPort port, char *buf)
{
    inert_drv_t *d = NULL;
    struct rlimit rlim = {0};

    d = driver_alloc(sizeof(inert_drv_t));
    if (!d)
        return ERL_DRV_ERROR_ERRNO;

    (void)memset(d, 0, sizeof(inert_drv_t));
    d->port = port;

    if (getrlimit(RLIMIT_NOFILE, &rlim) < 0)
        return ERL_DRV_ERROR_ERRNO;

    if (rlim.rlim_cur < rlim.rlim_max)
        (void)setrlimit(RLIMIT_NOFILE, &rlim);

    return (ErlDrvData)d;
}

    static void
inert_drv_stop(ErlDrvData drv_data)
{
    driver_free(drv_data);
}

    static ErlDrvSSizeT
inert_drv_control(ErlDrvData drv_data, unsigned int command,
        char *buf, ErlDrvSizeT len,
        char **rbuf, ErlDrvSizeT rlen)
{
    inert_drv_t *d = (inert_drv_t *)drv_data;

    inert_fd_t event = {0};
    int rv = 0;

    event.fd = (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3];

    if (event.fd < 0) {
        if (inert_copy(rbuf, &rlen, INERT_EBADFD, sizeof(INERT_EBADFD)-1) < 0)
            return -1;

        return rlen;
    }

    switch (command) {
        case INERT_FDSET:
            rv = driver_select(d->port, event.ev, ERL_DRV_READ|ERL_DRV_USE, 1);
            break;
        case INERT_FDCLR:
            rv = driver_select(d->port, event.ev, ERL_DRV_READ|0, 0);
            break;
        default:
            if (inert_copy(rbuf, &rlen, INERT_EINVAL, sizeof(INERT_EINVAL)-1) < 0)
                return -1;

            return rlen;
    }

    *rbuf = NULL;
    return rv;
}

    static void
inert_drv_ready_input(ErlDrvData drv_data, ErlDrvEvent event)
{
    inert_drv_t *d = (inert_drv_t *)drv_data;
    int32_t fd = ((inert_fd_t)event).fd;
    char res[4] = {0};

    (void)driver_select(d->port, event, ERL_DRV_READ, 0);

    res[0] = (fd >> 24) & 0xff;
    res[1] = (fd >> 16) & 0xff;
    res[2] = (fd >> 8) & 0xff;
    res[3] = fd & 0xff;

    (void)driver_output(d->port, res, sizeof(res));
}

static int
inert_copy(char **rbuf, ErlDrvSizeT *rlen, char *buf, size_t buflen) {
    if (buflen > *rlen)
        *rbuf = driver_alloc(buflen);

    if (*rbuf == NULL) {
        *rlen = 0;
        return -1;
    }

    (void)memset(*rbuf, 0, *rlen);
    (void)memcpy(*rbuf, buf, buflen);

    *rlen = buflen;
    return 0;
}

ErlDrvEntry inert_driver_entry = {
    NULL,                           /* F_PTR init, called when driver is loaded */
    inert_drv_start,                /* L_PTR start, called when port is opened */
    inert_drv_stop,                 /* F_PTR stop, called when port is closed */
    NULL,                           /* F_PTR output, called when erlang has sent */
    inert_drv_ready_input,          /* F_PTR ready_input, called when input descriptor ready */
    NULL,                           /* F_PTR ready_output, called when output descriptor ready */
    "inert_drv",                    /* char *driver_name, the argument to open_port */
    NULL,                           /* F_PTR finish, called when unloaded */
    NULL,                           /* void *handle, Reserved by VM */
    inert_drv_control,              /* F_PTR control, port_command callback */
    NULL,                           /* F_PTR timeout, reserved */
    NULL,                           /* F_PTR outputv, reserved */
    NULL,                           /* F_PTR ready_async, only for async drivers */
    NULL,                           /* F_PTR flush, called when port is about 
                                       to be closed, but there is data in driver 
                                       queue */
    NULL,                           /* F_PTR call, much like control, sync call
                                       to driver */
    NULL,                           /* F_PTR event, called when an event selected 
                                       by driver_event() occurs. */
    ERL_DRV_EXTENDED_MARKER,        /* int extended marker, Should always be 
                                       set to indicate driver versioning */
    ERL_DRV_EXTENDED_MAJOR_VERSION, /* int major_version, should always be 
                                       set to this value */
    ERL_DRV_EXTENDED_MINOR_VERSION, /* int minor_version, should always be 
                                       set to this value */
    0,                              /* int driver_flags, see documentation */
    NULL,                           /* void *handle2, reserved for VM use */
    NULL,                           /* F_PTR process_exit, called when a 
                                       monitored process dies */
    NULL                            /* F_PTR stop_select, called to close an 
                                       event object */
};

DRIVER_INIT(inert_drv) /* must match name in driver_entry */
{
    return &inert_driver_entry;
}
