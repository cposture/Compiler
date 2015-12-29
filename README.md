# PL0语言说明 

PL0语言可看成是Pascal[1]语言的子集，它的编译程序是一个编译解释执行系统

# 目前实现的功能

> 说明：本实验是基于学校老师已给代码的基础上补充新的功能，原有的功能包括WRITE、READ、IF...THEN... 
WHILE、VAR等。

1. +=、\*=、/=、-=
2. /\* \*/形式注释
3. FOR...STEP...UNTIL...DO语句
4. IF...THEN...ELSE...语句
5. CHAR、DOUBLE类型
6. CHAR型数组，目前支持常量下标
7. 与或非运算符

# 编译运行

```bash
$ make
$ ./compiler type.PL0 # 说明程序的第一个参数是位于同目录的PL0源代码文件名
```
