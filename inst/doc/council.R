## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----votingdata---------------------------------------------------------------
# packages
library(eurlex)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggiraph)

# pull Council voting data
cns_votes_raw <- elx_council_votes()

# vote level, keep only votes with disagreements
votes_dis <- cns_votes_raw %>% 
  select(voteProc, starts_with("countryCode")) %>% 
  select(-countryCodeNotParticipatingGrouped) %>% 
  distinct() %>% 
  filter((!is.na(countryCodeAgainstGrouped) & !is.na(countryCodeAbstainedGrouped))) %>% 
  pivot_longer(cols = starts_with("country"), names_to = "vote", values_to = "country") %>% 
  separate_rows(country, sep = "\\|") %>% 
  drop_na() %>% 
  mutate(vote = case_when(str_detect(vote, "Favour") ~ 1L,
                          str_detect(vote, "Absta") ~ 2L,
                          str_detect(vote, "Against") ~ 3L,
                          T ~ NA_integer_))

# country vote counts
country_votes_n <- votes_dis %>% 
  count(country, vote)

# weighted vote proportion
country_votes_prop <- country_votes_n %>% 
  mutate(value = case_when(vote == 1 ~ n * 1,
                           vote == 2 ~ n * -1,
                           vote == 3 ~ n * -2)) %>% 
  group_by(country) %>% 
  summarise(value = sum(value),
            n_votes = sum(n),
            prop = round(value / n_votes, 3)) %>% 
  ungroup()


## ----votesproportion----------------------------------------------------------
# viz Council votes and weighted proportion
iplot_votes_prop <- country_votes_prop %>% 
  mutate(tooltip = str_c(country,": ", prop, ". Total number of votes: ", n_votes)) %>% 
  ggplot(aes(y = reorder(country, prop), x = prop, yend = reorder(country, prop), xend = 0, color = prop)) + 
  geom_vline(xintercept = c(0.25,0.5,0.75), color = "grey90", lty = 2) +
  geom_point_interactive(aes(tooltip = tooltip, data_id = country),
                         show.legend = FALSE) +
  geom_segment_interactive(aes(tooltip = tooltip, data_id = country),
                           show.legend = FALSE) +
  theme_minimal(base_family = "Arial") +
  theme(legend.position = "top",
        legend.justification = "left",
        legend.title = element_text(face = "italic"),
        plot.background = element_rect(fill = "white", color = "grey88"),
        axis.text = element_text(color = "grey10", size = 12),
        title = element_text(face = "bold", size = 16),
        panel.grid = element_line(color = "grey94"),
        axis.title = element_text(hjust = 1, size = 14),
        plot.subtitle = element_text(face = "italic", size = 15),
        plot.caption = element_text(face = "italic", size = 8),
        strip.text = element_text(hjust = 0, face = "bold")) +
  scale_x_continuous(expand = c(0.01,0)) +
  scale_color_gradient(low = "red", high = "navyblue") +
  labs(x = NULL,
       y = NULL,
       color = NULL,
       fill = NULL,
       title = "Legislative discontent in the Council",
       subtitle = "Weighted proportion of government votes in favour on contested legislation*",
       caption = "* Only legislation with at least one vote not in favour; abstentions (x1) and votes against (x2) are subtracted from votes in favour")

# interactive plot
girafe(ggobj = iplot_votes_prop,
       fonts = list(sans = "Arial"),
       width_svg = 12,
       height_svg = 8,
       options = list(opts_sizing(rescale = TRUE),
                      opts_toolbar(saveaspng = FALSE),
                      opts_tooltip(css = "background-color:gray;color:white;font-style:italic;padding:9px;border-radius:5px;font-size:15px;",
                                   use_fill = TRUE),
                      opts_hover_inv(css = "opacity:0.1;"),
                      opts_hover(css = "fill:green;"))
)


