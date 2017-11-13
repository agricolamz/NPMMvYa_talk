library(dplyr)
library(ggplot2)
library(tibble)
library(tidyr)
diff <- seq(-1, 1, by = 0.01)
p_question <- (1-sqrt(diff^2))*1/3

my_dirichlet <- data_frame(p_plus = (1 - p_question)/2 + diff/2,
                          p_minus = (1 - p_question)/2 - diff/2,
                          p_question,
                          difference = diff)

# check summation ---------------------------------------------------------
my_dirichlet %>% 
  rowwise() %>% 
  mutate(sum = sum(p_question, p_plus, p_minus)) %>% 
  View

my_dirichlet %>% 
  gather(key = p, value = pdf, p_plus:p_question) %>% 
  mutate(p = factor(p))->
  my_dirichlet_gather

levels(my_dirichlet_gather$p) <- c("p(∗)", "p(+)", "p(?)")
my_dirichlet_gather$p <- relevel(my_dirichlet_gather$p, "p(+)")

my_dirichlet_gather %>%
  ggplot(aes(difference, pdf, fill = p))+
  geom_ribbon(aes(ymax = pdf), ymin = 0, show.legend = F)+
  facet_wrap(~p, nrow = 3)+
  geom_hline(yintercept = 1/3, lty = 3)+
  theme_bw()+
  labs(x = "p(+) - p(∗)")+
  scale_y_continuous(breaks = round(c(0, 3:5*25/100, 2, 1/3), 2))

# create data -------------------------------------------------------------
library(VGAM)
df <- as_tibble(rdiric(100, as.matrix(my_dirichlet[,-4]), is.matrix.shape = T))

colnames(df) <- c("p(+)", "p(∗)", "p(?)")

df %>% 
  gather(key = col, value = value, `p(+)`:`p(?)`) %>%
  mutate(col = factor(col, levels = c("p(+)", "p(∗)", "p(?)"))) %>%
  ggplot(aes(value))+
  geom_histogram()+
  facet_wrap(~col)+
  theme_bw()

df %>% 
  mutate(diff = `p(+)` - `p(∗)`) %>% 
  gather(key = col, value = value, `p(+)`:`p(?)`) %>%
  mutate(col = factor(col, levels = c("p(+)", "p(∗)", "p(?)"))) %>%
  ggplot(aes(diff, value))+
  geom_point(shape = ".")+
  facet_wrap(~col)

# here should be regression -----------------------------------------------
fit <- vglm(cbind(`p(+)`, `p(∗)`, `p(?)`) ~ 1, dirichlet,
            data = df, trace = TRUE, crit = "coef")
Coef(fit)
coef(fit, matrix = TRUE)
head(fitted(fit))

# this part is visualisation ----------------------------------------------
library(dirichlet)
f <- function(v) {
  ddirichlet(v, Coef(fit))
}
mesh <- simplex_mesh(.0025) %>%
  as.data.frame %>%
  tbl_df

mesh$f <- mesh %>%
  apply(1, function(v)
    f(bary2simp(v)))

(p <- ggplot(mesh, aes(x, y)) +
    geom_raster(aes(fill = f)) +
    coord_equal(xlim = c(0, 1), ylim = c(0, .85)))

points <- ddata %>%
  simp2bary %>%
  as.data.frame %>%
  tbl_df %>%
  rename(x = V1, y = V2)

p + geom_point(data = points,
               color = "orange",
               alpha = .3)

ddata %>% 
  mutate(difference = y1 - y2) %>%
  gather(key = y, value = value, y1:y3) %>%
  ggplot(aes(difference, value, color = y))+
  geom_point()+
  facet_wrap(~y, nrow = 3)+
  theme_bw()+
  labs(x = "p(+) - p(∗)")
