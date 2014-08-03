#pragma once

int console_is_initialized(void);
int console_write(char *buf, int len);

int ser_cons_present(void);
int ser_cons_write(char *bur, int len);

//EOF
