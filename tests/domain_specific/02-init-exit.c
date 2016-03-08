#include <linux/module.h>       /* Needed by all modules */
#include <linux/init.h>         /* Needed for the macros */
#include <linux/fs.h>           /* Needed for file operations and file */

static dev_t devid;

static struct cdev my_cdev;

static int i;

static int file_open(struct inode *inode, struct file *file)
{
	printk(KERN_INFO "Opening file \n");
	return 0;
}

static struct file_operations my_file_ops = {
  .owner   = THIS_MODULE,
  .open    = file_open
};


static int __init hello_start(void){
  alloc_chrdev_region(&devid, 0, 1, "mydriver");
  cdev_init(&my_cdev, &my_file_ops);
  cdev_add(&my_cdev, devid, 1);
  i++; //NORACE
  return 0;
}

static void __exit hello_end(void){
  int j = i; // NOWARN
}

module_init(hello_start);
module_exit(hello_end);
