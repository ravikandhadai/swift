//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "myoslog.h"
#include <os/log.h>

void
my_os_log_impl_wrapper(
    const void * dso,
    os_log_t h,
    os_log_type_t type,
    const char * fmt,
    const uint8_t *buf,
    uint32_t len)
{
  _os_log_impl((void*)dso, h, type, fmt, (uint8_t *)buf, len);
}
