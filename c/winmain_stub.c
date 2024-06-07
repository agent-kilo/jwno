#include <stdio.h>

#include <windows.h>


#pragma comment(lib,"comctl32.lib")    
#pragma comment(linker,"\"/manifestdependency:type='win32' \
name='Microsoft.Windows.Common-Controls' version='6.0.0.0' \
processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")


int main(int argc, const char **argv);


int APIENTRY WinMain(_In_ HINSTANCE hInstance,
                     _In_opt_ HINSTANCE hPrevInstance,
                     _In_ LPWSTR    lpCmdLine,
                     _In_ int       nCmdShow)
{
    (void)hInstance;
    (void)hPrevInstance;
    (void)lpCmdLine;
    (void)nCmdShow;

    AllocConsole();

    FILE *con_stdin;
    FILE *con_stderr;
    FILE *con_stdout;
    freopen_s(&con_stdin, "CONIN$", "r", stdin);
    freopen_s(&con_stdout, "CONOUT$", "w", stdout);
    freopen_s(&con_stderr, "CONOUT$", "w", stderr);

    return main(__argc, __argv);
}
