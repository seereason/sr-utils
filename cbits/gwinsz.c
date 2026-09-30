#include <sys/ioctl.h>

unsigned long c_get_window_size(void) {
#ifdef TIOCGWINSZ
	struct winsize w;
	if (ioctl (0, TIOCGWINSZ, &w) >= 0)
		return (w.ws_row << 16) + w.ws_col;
	else
#endif
		/* no terminal (e.g. wasm32-wasi): 25 rows, 80 columns */
		return 0x190050;
}
