library(tidyverse)
n <- 1e3
nPerLoc <- 4
classes <- LETTERS[1:6]
set.seed(1234567)
probs <- runif(length(classes), min=0, max=1)
probs <- probs / max(probs)
probs

PA <- expand.grid(pid = seq_len(n),
                  i = seq_len(nPerLoc),
                  class = classes)
USED <- data.frame(pid = seq_len(n),
                   i = 0L,
                   class = sample(classes, size = n, prob = probs, replace = TRUE))
head(PA)
head(USED)

PA$label = 0L
USED$label = 1L

dat <- bind_rows(PA, USED) %>% 
  as_tibble() %>% 
  arrange(pid, i)
dat

dat %>% count(class)

dat <- dat %>% 
  mutate(class = factor(class, levels = LETTERS[1:6]))
levels(dat$class)
dat %>% count(class)

rsf <- glm(label ~ class, data = dat, family = binomial("logit"))
summary(rsf)
probs

rsf$coefficients[-1]

rsf
predict(rsf)

plot(predict(rsf), predict(rsf, type="response"), pch=16, cex=0.75)


coeffs <- c(rsf$coefficients[1] - qlogis(1/(4*6)),
            rsf$coefficients[-1])
coeffs
names(coeffs)[1] <- "classA"

plot(qlogis(probs), coeffs, col=1:6, pch=16)



x <- seq(-10, +10, length.out = 1001)
plot(x, plogis(x), type="l")


x <- rep(LETTERS[1:6], 3)
x

y <- as.factor(x)
y <- factor(x, levels = sort(unique(x)))

y <- factor(x, levels = c("Z", sort(unique(x))))
y <- factor(x, levels = rev(sort(unique(x))))

y <- factor(x, levels = c("F",sort(unique(x))[-6]))


as.integer(y)
levels(y)

rep(1:4, times = 3)
rep(1:4, each = 10)

