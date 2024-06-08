#include <stdio.h>

#include <windows.h>

/* For debugging */
//#define EARLY_CONSOLE_ALLOC


int main(int argc, const char **argv);


int early_console_alloc(void) {
    if (!AllocConsole()) {
        return -1;
    }

    errno_t err = 0;
    FILE *con_stdin;
    FILE *con_stdout;
    FILE *con_stderr;

    err = freopen_s(&con_stdin, "CONIN$", "r", stdin);
    if (err) {
        return err;
    }

    err = freopen_s(&con_stdout, "CONOUT$", "w", stdout);
    if (err) {
        return err;
    }

    err = freopen_s(&con_stderr, "CONOUT$", "w", stderr);
    if (err) {
        return err;
    }

    return 0;
}


int APIENTRY WinMain(_In_ HINSTANCE hInstance,
                     _In_opt_ HINSTANCE hPrevInstance,
                     _In_ LPWSTR    lpCmdLine,
                     _In_ int       nCmdShow)
{
    (void)hInstance;
    (void)hPrevInstance;
    (void)lpCmdLine;
    (void)nCmdShow;

#ifdef EARLY_CONSOLE_ALLOC
    int err = early_console_alloc();
    if (err) {
        char msg_buf[128];
        sprintf(msg_buf, "early_console_alloc() failed: %d", err);
        MessageBox(NULL, msg_buf, "Error", MB_ICONEXCLAMATION | MB_OK);
        return 1;
    }
#endif /* EARLY_CONSOLE_ALLOC */

    return main(__argc, __argv);
}
