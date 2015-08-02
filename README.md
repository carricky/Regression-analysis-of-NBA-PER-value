# Regression-analysis-of-NBA-PER-value
Regression analysis of NBA players' PER value based on R

---
date: "Monday, December 29, 2014"
---
#Background
>The player efficiency rating (PER) is John Hollinger's all-in-one basketball rating, which attempts to boil down all of a player's contributions into one number. Using a detailed formula, Hollinger developed a system that rates every player's statistical performance.[1]

Here, the variable I chooose are as follows,

* mp-minutes played
* ts-true shooting rate
* trb-total rebound ratio
* ast-assistant ratio
* stl-steal ratio
* blk-block ratio
* tov-turn-over ratio
* usg-usage ratio

***

#Load data
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

#Choose the independent variable
　　I used the AIC(Akaike information criterion) to choose the variable
```{r}
mydata=data.frame(mp,ts,trb,ast,stl,blk,tov,usg,per);
min.model=lm(per~1,data=mydata)
step.model=step(min.model,direction="both",
                scope=(~mp+ts+trb+ast+stl+blk+tov+usg))
summary(step.model)
```

Results shows that the minutes player has nothing to do with the efficiency of a player

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

 1."Calculating PER". http://www.basketball-reference.com/about/per.html. Retrieved 5/9/2013. Check date values in: |accessdate= (help)



