/* The saved Hackage login, as a generic password in the user's default
 * keychain. Each function returns 0 (errSecSuccess) or an OSStatus.
 *
 * The SecKeychain calls are deprecated in favour of SecItem, which needs
 * CFDictionaries for every call; these still work and are far simpler. */

#pragma clang diagnostic ignored "-Wdeprecated-declarations"

#include <Security/Security.h>
#include <stdlib.h>
#include <string.h>

static OSStatus find(const char *service, const char *account, UInt32 *size, void **data, SecKeychainItemRef *item)
{
    return SecKeychainFindGenericPassword(NULL, (UInt32)strlen(service), service,
                                          (UInt32)strlen(account), account, size, data, item);
}

int32_t cabalist_keychain_write(const char *service, const char *account, const void *data, uint32_t size)
{
    SecKeychainItemRef item = NULL;
    OSStatus s = find(service, account, NULL, NULL, &item);
    if (s == errSecSuccess) {
        s = SecKeychainItemModifyAttributesAndData(item, NULL, size, data);
        CFRelease(item);
        return s;
    }
    if (s != errSecItemNotFound)
        return s;
    return SecKeychainAddGenericPassword(NULL, (UInt32)strlen(service), service,
                                         (UInt32)strlen(account), account, size, data, NULL);
}

/* On success *data is a copy of the secret, to be released with free(). */
int32_t cabalist_keychain_read(const char *service, const char *account, void **data, uint32_t *size)
{
    void *p = NULL;
    UInt32 n = 0;
    OSStatus s = find(service, account, &n, &p, NULL);
    if (s != errSecSuccess)
        return s;
    *data = malloc(n ? n : 1);
    if (*data == NULL) {
        SecKeychainItemFreeContent(NULL, p);
        return errSecAllocate;
    }
    memcpy(*data, p, n);
    *size = n;
    memset(p, 0, n);
    SecKeychainItemFreeContent(NULL, p);
    return errSecSuccess;
}

int32_t cabalist_keychain_delete(const char *service, const char *account)
{
    SecKeychainItemRef item = NULL;
    OSStatus s = find(service, account, NULL, NULL, &item);
    if (s != errSecSuccess)
        return s;
    s = SecKeychainItemDelete(item);
    CFRelease(item);
    return s;
}
