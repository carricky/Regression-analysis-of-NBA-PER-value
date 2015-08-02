# Regression-analysis-of-NBA-PER-value
Regression analysis of NBA players' PER value based on R

---
title: "NBA球员效率值（PER）分析"
author: "高思远"
date: "Monday, December 29, 2014"
output: html_document
highlight: zenburn
---
#背景
>NBA球员效率指数是由ESPN专家约翰·霍林格提出的球员价值评估数据体系。利用**PER值**，可以将球员所有表现记录下来，然后加权集成，综合而成，便可以对不同位置、不同年代的球员进行评估和比较。

　　而在这里，我利用代表球员场上攻防表现的几个常用数据，对**PER值**进行回归分析，我选取的回归变量主要是

* mp-上场时间
* ts-真实命中率
* trb-总篮板比率
* ast-助攻比率
* stl-抢断比率
* blk-盖帽比率
* tov-失误比率
* usg-进攻中占用回合比率

***

#数据的读入
```{r}
data=read.csv("data2.csv");
per=data[,4];
mp=data[,3];
ts=data[,5];
trb=data[,10];
ast=data[,11];
stl=data[,12];
blk=data[,13];
tov=data[,14];
usg=data[,15];
```

***

#自变量的选择
　　由于模型中自变量比较多（8个），所以我决定使用**基于AIC准则的逐步回归法**来进行自变量选择。
```{r}
mydata=data.frame(mp,ts,trb,ast,stl,blk,tov,usg,per);
min.model=lm(per~1,data=mydata)
step.model=step(min.model,direction="both",
                scope=(~mp+ts+trb+ast+stl+blk+tov+usg))
summary(step.model)
```

　　这说明mp（球员上场时间）与效率值之间没有直接联系，这也与我们的实际经验相符。

***

#检验
##模型检验
　　画出*残差图*，在残差图中我们不能看出什么特别的规律，所以认为关于模型的假设成立。

```{r}
mydata=data.frame(per,ts,trb,ast,stl,blk,tov,usg);
lm.sol=lm(per~ts+trb+ast+stl+blk+tov+usg,data=mydata);
plot(lm.sol,which=1)
```

##数据检验
###异常点检验
　　首先求出**学生化残差**r.std
```{r}
r.std=rstandard(lm.sol)
per.fit=predict(lm.sol);
plot(r.std~per.fit)
text(per.fit,r.std,type="1:53");
r.std
```

　　发现3、7、25号球员**学生化残差**$|r_i|>2$，为异常点，使用`outlierTest()`命令也可验证

```{r}
library(car);
outlierTest(lm.sol)
```

　　再对**影响力**进行分析
```{r}
influence.measures(lm.sol)
```

　　再进行**D-W检验**
```{r}
library(car);
durbinWatsonTest(lm.sol)
```

　　查表可知，对于7个自变量50个样本，$D_u=1.73668$，而这里**DW统计量**为1.86，介于$Du$和$4-Du$之间，故我们接受不相关性假设。

　　最后，做出QQ图
```{r}
plot(lm.sol,2)
```

##回归方程、系数显著性检验
```{r}
summary(lm.sol)
```
　　通过$F_H=158.3$及$p-value<2.2e-16$我们认为回归自变量对因变量有显著作用。而每个回归系数的$p-value$都小于0.05，故我们认为每个回归自变量也都是显著的。

##检验总结
　　可以看出，3号和7号点是**异常点**，1、2、3、4、5、27号点是**强影响点**，检查后发现数据来源没有过失，但通过分析发现1、2、5、27号点对应球员上场时间很少，总共只有几十分钟，数据没有代表性，而3号球员的数据明显与其他球员有较大差距，于是决定删除这些点，同时加入五个上场时间比较多的球员数据，以减少其他强影响点的影响。综上，由得到的**QQ图**、**残差图**以及**Durbin-Watson检验**，我们接受**线性假设**、**方差齐性假设**、**不相关性假设**和**正态性假设**。

***

#多重共线性分析
　　对修改后的数据进行**多重共线性分析**，求出**VIF值***和**Kappa值**
```{r}
data=read.csv("data3.csv");
per=data[,4];
ts=data[,5];
trb=data[,10];
ast=data[,11];
stl=data[,12];
blk=data[,13];
tov=data[,14];
usg=data[,15];
mydata=data.frame(per,ts,trb,ast,stl,blk,tov,usg);
lm.sol=lm(per~ts+trb+ast+stl+blk+tov+usg,data=mydata);
summary(lm.sol)
library(DAAG)
vif(lm.sol)
X=cbind(ts,trb,ast,stl,blk,tov,usg);
rho=cor(X);
eigen(rho);
kappa(rho,exact=TRUE)
```
　　我们发现所有的**VIF**都不大于10，而且**Kappa值**只有23.9，故认为数据不存在多重共线性关系。




