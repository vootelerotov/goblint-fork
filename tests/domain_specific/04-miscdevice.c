#include <linux/module.h>       /* Needed by all modules */
#include <linux/init.h>         /* Needed for the macros */
#include <linux/fs.h>           /* Needed for file operations and file */
#include <linux/miscdevice.h>   /* Needed for miscdevice */


static dev_t devid;

static struct cdev my_cdev;

static int file_open(struct inode *inode, struct file *file)
{
  return 0;
}


static struct file_operations file_ops = {
  .owner   = THIS_MODULE,
  .open    = file_open
};

static struct miscdevice misc_device = {
	.minor		= 1,
	.name		= "test",
	.fops		= &file_ops
};

static int __init hello_start(void){
  misc_register(&misc_device); //NORACE
  return 0;
}

static void __exit hello_end(void){
  misc_deregister(&misc_device);
}

module_init(hello_start);
module_exit(hello_end);
