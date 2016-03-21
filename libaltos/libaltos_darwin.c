/*
 * Copyright © 2016 Keith Packard <keithp@keithp.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along
 * with this program; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

#include "libaltos_private.h"
#include "libaltos_posix.h"

#include <IOKitLib.h>
#include <IOKit/usb/USBspec.h>
#include <sys/param.h>
#include <paths.h>
#include <CFNumber.h>
#include <IOBSD.h>

/* Mac OS X don't have strndup even if _GNU_SOURCE is defined */
char *
altos_strndup (const char *s, size_t n)
{
    size_t len = strlen (s);
    char *ret;

    if (len <= n)
       return strdup (s);
    ret = malloc(n + 1);
    strncpy(ret, s, n);
    ret[n] = '\0';
    return ret;
}

struct altos_list {
	io_iterator_t iterator;
	int ftdi;
};

static int
get_string(io_object_t object, CFStringRef entry, char *result, int result_len)
{
	CFTypeRef entry_as_string;
	Boolean got_string;

	entry_as_string = IORegistryEntrySearchCFProperty (object,
							   kIOServicePlane,
							   entry,
							   kCFAllocatorDefault,
							   kIORegistryIterateRecursively);
	if (entry_as_string) {
		got_string = CFStringGetCString(entry_as_string,
						result, result_len,
						kCFStringEncodingASCII);

		CFRelease(entry_as_string);
		if (got_string)
			return 1;
	}
	return 0;
}

static int
get_number(io_object_t object, CFStringRef entry, int *result)
{
	CFTypeRef entry_as_number;
	Boolean got_number;

	entry_as_number = IORegistryEntrySearchCFProperty (object,
							   kIOServicePlane,
							   entry,
							   kCFAllocatorDefault,
							   kIORegistryIterateRecursively);
	if (entry_as_number) {
		got_number = CFNumberGetValue(entry_as_number,
					      kCFNumberIntType,
					      result);
		if (got_number)
			return 1;
	}
	return 0;
}

PUBLIC struct altos_list *
altos_list_start(void)
{
	struct altos_list *list = calloc (sizeof (struct altos_list), 1);
	CFMutableDictionaryRef matching_dictionary = IOServiceMatching("IOUSBDevice");
	io_iterator_t tdIterator;
	io_object_t tdObject;
	kern_return_t ret;
	int i;

	ret = IOServiceGetMatchingServices(kIOMasterPortDefault, matching_dictionary, &list->iterator);
	if (ret != kIOReturnSuccess) {
		free(list);
		return NULL;
	}
	list->ftdi = 0;
	return list;
}

PUBLIC struct altos_list *
altos_ftdi_list_start(void)
{
	struct altos_list *list = altos_list_start();

	if (list)
		list->ftdi = 1;
	return list;
}

PUBLIC int
altos_list_next(struct altos_list *list, struct altos_device *device)
{
	io_object_t object;
	char serial_string[128];

	for (;;) {
		object = IOIteratorNext(list->iterator);
		if (!object)
			return 0;

		if (!get_number (object, CFSTR(kUSBVendorID), &device->vendor) ||
		    !get_number (object, CFSTR(kUSBProductID), &device->product))
			continue;
		if (get_string (object, CFSTR("IOCalloutDevice"), device->path, sizeof (device->path)) &&
		    get_string (object, CFSTR("USB Product Name"), device->name, sizeof (device->name)) &&
		    get_string (object, CFSTR("USB Serial Number"), serial_string, sizeof (serial_string))) {
			device->serial = atoi(serial_string);
			return 1;
		}
	}
}

PUBLIC void
altos_list_finish(struct altos_list *list)
{
	IOObjectRelease (list->iterator);
	free(list);
}

struct altos_bt_list {
	int		sock;
	int		dev_id;
	int		rsp;
	int		num_rsp;
};

#define INQUIRY_MAX_RSP	255

struct altos_bt_list *
altos_bt_list_start(int inquiry_time)
{
	return NULL;
}

int
altos_bt_list_next(struct altos_bt_list *bt_list,
		   struct altos_bt_device *device)
{
	return 0;
}

void
altos_bt_list_finish(struct altos_bt_list *bt_list)
{
}

void
altos_bt_fill_in(char *name, char *addr, struct altos_bt_device *device)
{
	strncpy(device->name, name, sizeof (device->name));
	device->name[sizeof(device->name)-1] = '\0';
	strncpy(device->addr, addr, sizeof (device->addr));
	device->addr[sizeof(device->addr)-1] = '\0';
}

struct altos_file *
altos_bt_open(struct altos_bt_device *device)
{
	return NULL;
}
