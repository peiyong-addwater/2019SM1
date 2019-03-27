#include <stdio.h>
        int f(int x, int y)
        {
                return 10 * x + y;
        }

        int main(void)
        {
                int i, j;

                i = 0;
                j = f(++i, ++i);
                printf("%d\n", j);
		return 0;
        }
