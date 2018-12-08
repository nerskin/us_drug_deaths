library(tidyverse)
library(sf)
library(fiftystater)#devtools::install_github("wmurphyrd/fiftystater")
library(viridis)
library(gganimate)#devtools::install_github('thomasp85/gganimate')
library(ggthemes)

options(gganimate.dev_args = list(width = 1000, height = 1000))

us_state_map <- fifty_states %>%
	st_as_sf(coords=c('long','lat')) %>%
	group_by(id,piece) %>%
	summarise(do_union=FALSE) %>%
	st_cast('POLYGON') %>%
	group_by(id) %>%
	summarise()

mortality_data <- read_tsv('drug_deaths.tsv',col_types = cols(Year=col_integer())) %>% 
	mutate(state_lower = str_to_lower(State)) %>% 
	mutate(crude_rate = Deaths*1e5/Population)


us_state_map <- us_state_map %>%
	left_join(mortality_data,by=c(id='state_lower')) %>%
  filter(is.na(Notes))

drug_deaths_plot <- us_state_map %>%
	ggplot(aes(fill=crude_rate)) +
	geom_sf() + 
	scale_fill_viridis(option='magma',name = 'Drug-related deaths per 100000 population') + 
	ggtitle('Drug-Related Deaths by Year') + 
	theme_minimal() +
        coord_sf(datum = NA) + 	
	transition_states(Year,1,1) + 
	labs(subtitle='Year: {closest_state}',caption = 'Data: CDC') +
	theme(plot.title = element_text(size=22),plot.subtitle = element_text(size=18))


animate(drug_deaths_plot,nframes=500,fps=5,end_pause = 25,renderer=ffmpeg_renderer(),rewind=TRUE)

anim_save('drug_deaths.mp4')
