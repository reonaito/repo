#include <stdio.h>

int main(int argc, char* argv[]){
  printf("窓は閉めましたよね？ (y/n)\n");
  while(1){
    char c;
    scanf("%c", &c);
    if (c != 'y'){
      printf("窓を閉めてください．鳩が入ります．\n");
    }
    else break;
  }
  printf("お疲れ様でした．\n");
  return 0;
}
