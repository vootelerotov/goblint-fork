#include <linux/module.h>       /* Needed by all modules */
#include <linux/init.h>         /* Needed for the macros */

static int i;


static int __init hello_start(void){
  return 0;
}
static void __exit hello_end(void){
  i++; //NOWARN
}

module_init(hello_start);
module_exit(hello_end);
