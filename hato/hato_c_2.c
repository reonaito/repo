#include <stdio.h>

int main(int argc, char* argv[]){
  printf("窓は閉めましたよね？ (y/n)\n");
  while(1){
    char str[256];
    scanf("%s", str);
    if (str[0] != 'y'){
      printf("窓を閉めてください．鳩が入ります．\n");
    }
    else break;
  }
  printf("お疲れ様でした．\n");
  return 0;
}
