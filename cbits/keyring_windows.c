/* The saved Hackage login, as a generic credential in Windows Credential
 * Manager. Each function returns 0 on success, else a Win32 error code. */

#include <windows.h>
#include <wincred.h>
#include <stdlib.h>
#include <string.h>

DWORD cabalist_cred_write(const wchar_t *target, const wchar_t *user, const wchar_t *comment, const BYTE *blob, DWORD size)
{
    CREDENTIALW c;
    memset(&c, 0, sizeof c);
    c.Type = CRED_TYPE_GENERIC;
    c.TargetName = (LPWSTR)target;
    c.UserName = (LPWSTR)user;
    c.Comment = (LPWSTR)comment;
    c.CredentialBlob = (LPBYTE)blob;
    c.CredentialBlobSize = size;
    c.Persist = CRED_PERSIST_LOCAL_MACHINE;
    return CredWriteW(&c, 0) ? 0 : GetLastError();
}

/* On success *blob is a copy of the secret, to be released with free(). */
DWORD cabalist_cred_read(const wchar_t *target, BYTE **blob, DWORD *size)
{
    PCREDENTIALW c;
    if (!CredReadW(target, CRED_TYPE_GENERIC, 0, &c))
        return GetLastError();
    *size = c->CredentialBlobSize;
    *blob = malloc(*size ? *size : 1);
    if (*blob == NULL) {
        CredFree(c);
        return ERROR_NOT_ENOUGH_MEMORY;
    }
    memcpy(*blob, c->CredentialBlob, *size);
    SecureZeroMemory(c->CredentialBlob, c->CredentialBlobSize);
    CredFree(c);
    return 0;
}

DWORD cabalist_cred_delete(const wchar_t *target)
{
    return CredDeleteW(target, CRED_TYPE_GENERIC, 0) ? 0 : GetLastError();
}
