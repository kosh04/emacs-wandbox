// #wandbox compiler: clang-3.3-c
// #wandbox compiler-option: -lm

#include <stdio.h>
#include <math.h>

int main(int argc, char *argv[])
{
  printf("pi=%1.10g", 4*atan(1));
  return 0;
}
