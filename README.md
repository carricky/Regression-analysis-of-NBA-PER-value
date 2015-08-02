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

***

#Testing
##Model testing
Draw the residual plot

```{r}
mydata=data.frame(per,ts,trb,ast,stl,blk,tov,usg);
lm.sol=lm(per~ts+trb+ast+stl+blk+tov+usg,data=mydata);
plot(lm.sol,which=1)
```

##Data testing
###Outlier test
Get **student residual**r.std
```{r}
r.std=rstandard(lm.sol)
per.fit=predict(lm.sol);
plot(r.std~per.fit)
text(per.fit,r.std,type="1:53");
r.std
```
Verify the outlier using `outlierTest()`

```{r}
library(car);
outlierTest(lm.sol)
```

Analysing **influence**
```{r}
influence.measures(lm.sol)
```

Implementing**D-W test**
```{r}
library(car);
durbinWatsonTest(lm.sol)
```

Finally，draw QQ plot
```{r}
plot(lm.sol,2)
```

##Test of regression equation and the coefficients' significance
```{r}
summary(lm.sol)
```

##Test summary
From the above test result, we can see that Number 3 and 7 player are outliers. Number 1,2,3,4,5,27 are points with strong influence, but after the further examination, the data source are correct, but from further analysis, we notice that 1,2,5,27 player has little play time, each in total less than 100 minutes. Thus they are not representative enough. And Number 3 players data is very different from others. Thus I decide to remove those players and add 5 more other players data with more playing time to reduce the influence of other points. 
    
Above all, from the above test we can verify the assumptions made: Linearity Assumption, Independence Assumption, Normality Assumpiton and Homogeneity-of-variance Assumption. Feel free to run the code and get the results.

***

#Multicollinearity test
　via VIF value and Kappa value
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
We found all the **VIF**are less than 10，and **Kappa value** equals 23.9，so there is no Multicollinearity here

#Result
The final result of the equation is y=-17.285+42.635*ts+0.2889*trb+0.173*ast+1.216*stl+0.561*blk-0.444*tov+0.386*usg, and this conforms to the normal practice. 

P.S. Feel free to run the code and get the results, please contact me if you have suggestions on the analysis of the results. 
#Reference
 1."Calculating PER". http://www.basketball-reference.com/about/per.html. Retrieved 5/9/2013. Check date values in: |accessdate= (help)



