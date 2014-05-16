#An attempt to do mixed effects models on my data
#2014
#

#Need this package to do this
reqire(nlme)

#En modell."The current model has fixed effects exactly like PROC MIXED, associated test very close"


lSymm <- lme(RT ~  Block * C,data=data, random= list(Id = pdSymm(~fblock-1)), method  = 'ML')
summary(lSymm)

lNull <- lme(RT ~ Block * C, data=data, random=  ~1|Id, method='REML')
summary(lNull)


#testar göra något "typ" som i boken
lSymm <- lme(RT ~ Block,data=data, random=~1|Id)
summary(lSymm)

#model for only sound distraction
lSymm <- lme(y1 ~  block,data=distr, random= list(id = pdSymm(~fblock-1)), method  = 'ML')
summary(lSymm)

#model for only vib distraction
lSymm <- lme(y3 ~  block,data=distr, random= list(id = pdSymm(~fblock-1)), method  = 'ML')
summary(lSymm)

#testar
lSymm2 <- lme(y3 ~  block + y1 + y2 + y3 + y4,data=aa, random= list(id = pdSymm(~fblock-1)), method  = 'ML')
summary(lSymm2)

x.models <- dlply(x.melt, .var = c("variable", "time_point"), .fun = function(x) 
  
  lmer(scale(value) ~ (1|target) + (1|perceiver), data= x)))