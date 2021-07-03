library(boot)

#LOOCV
model_glm <- glm(mpg~cyl,data=mtcars)
coef(model_glm)
# (Intercept)    cyl 
# 37.88458    -2.87579 
lm(mpg~cyl,data=mtcars)

cv.err <- cv.glm(mtcars,model_glm)
cv.err$delta
#[1] 11.20843 11.18290

#5 fold
index_glm <- glm(mpg~cyl,data=mtcars)

k5 <- cv.glm(mtcars,index_glm, K=5)
names(k5)
k5$delta #[1] 11.95784 11.67405

#10 fold
k10 <- cv.glm(mtcars,index_glm,K=10)
k10$delta #[1] 11.53479 11.43822

