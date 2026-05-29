#ifndef GPIO_H
#define GPIO_H

#ifdef __cplusplus
extern "C" {
#endif

int gpio_init();
int gpio_write(int pin, int val);
int gpio_read(int pin);
int gpio_setup_watchdog(int pin);

#ifdef __cplusplus
}
#endif

#endif
