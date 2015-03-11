# Typed Assembly Language #
An implementation of the first level (control flow safety) of [Typed Assembly Language](http://www.cs.cornell.edu/talc/) in OCaml

## 文件说明 ##

### main.ml ###
初始化和程序入口
包括与一段TAL源代码相对应的抽象语法树，初始机器状态ms和人为给定的代码段堆类型Psi
因为well-typed的TAL代码会无限执行下去，因此可以在eval函数中指定执行的步数step

### syntax.ml ###
TAL的语法定义，类型定义，以及打印功能函数

### coretal.ml ###
TAL的核心部分
包括 evaluation rules 和 typing rules
evaluation函数 和 typing函数
以及实现先做typing再做evaluation的函数eval


## 使用方法 ##

### 编译命令 ###
```
ocamlbuild main.native
```

### 运行命令 ###
```
./main.native > results
```
在results中查看结果

由于没有实现语法分析功能，tal的源代码是写死main.ml中的
每次修改tal源代码都需要重新build

