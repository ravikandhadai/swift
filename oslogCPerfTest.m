#import <Foundation/Foundation.h>
#include <stdio.h>
#include <os/log.h>
#include <sys/time.h>
#include <string.h>

// Run command: clang -o oslogCPerfTest oslogCPerfTest.c -O

int main(int argc, const char * argv[]) {
  //os_log_t test_log;
  //test_log = os_log_create("OSLog Test", "msg");
  long long privateID = 0x79abcdef;
  long long filePermissions = 0777;
  long long pid = 122225;

  char string1[500];
  strcpy(string1, "some large text that wouldn't fit into small string");
  char *dynamicString =
    strcat(string1,
           "Another suffix so that the string representation is complex");

  NSArray* nsArray = @[@0, @1, @2];

  struct timeval t1, t2;
  double elapsedTime;

    // start timer
  gettimeofday(&t1, NULL);
   
 int i = 0;
 for (i = 0; i < 100000; i++) {
//  os_log_error(OS_LOG_DEFAULT, "Access prevented: process %lld initiated by user: %{private}lld attempted resetting permissions to %llo", pid, privateID,
//      filePermissions);
   //os_log(OS_LOG_DEFAULT, "Test message %d", argc);
   //os_log_error(OS_LOG_DEFAULT, "some random string %s", dynamicString);
   os_log_error(OS_LOG_DEFAULT, "some random object %@", nsArray);
 }

  gettimeofday(&t2, NULL);

    // compute and print the elapsed time in millisec
  elapsedTime = (t2.tv_sec - t1.tv_sec) * 1000.0;      // sec to ms
  elapsedTime += (t2.tv_usec - t1.tv_usec) / 1000.0;   // us to ms

   printf("c os_log time: %lfms\n", elapsedTime);
  
  return 0;
}

