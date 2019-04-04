//
//  myoslog.h
//  OSLogMixed
//
//  Created by Ravi on 7/30/18.
//  Copyright © 2018 swe. All rights reserved.
//

#ifndef myoslog_h
#define myoslog_h

#include <os/log.h>
#include <os/base.h>

void
my_os_log_impl_wrapper(
                       const void * dso,
                       os_log_t h,
                       os_log_type_t type,
                       const char * fmt,
                       const uint8_t *buf,
                       uint32_t len);

#endif /* myoslog_h */
