library(tidyverse)
library(sf)
albers_proj = "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
read_sf("gz_2010_us_050_00_20m/gz_2010_us_050_00_20m.shp") %>% 
  st_transform(albers_proj) -> counties
read_sf("gz_2010_us_040_00_20m/gz_2010_us_040_00_20m.shp") %>% 
  st_transform(albers_proj) -> states

usmapdata::us_map("states") %>% 
  st_transform(albers_proj) -> statesmap 
usmapdata::us_map("counties") %>% 
  st_transform(albers_proj) -> countiesmap 


# 旋转函数
rotation = function(a){
  r = a * pi / 180 # 角度转弧度
  matrix(c(cos(r), sin(r), -sin(r), cos(r)), 
         nrow = 2, ncol = 2)
}

# 变换阿拉斯加
states %>% 
  filter(STATE == "02") %>% 
  mutate(geometry = geometry * rotation(-50) / 2 + c(300000, -2000000)) %>% 
  st_set_crs(albers_proj) -> alaska 

# 变换夏威夷
states %>% 
  filter(STATE == "15") %>% 
  mutate(geometry = geometry * rotation(-35) + c(3600000, 1800000)) %>% 
  st_set_crs(albers_proj) -> Hawaii 

ggplot(statesmap) + 
  geom_sf() + 
  geom_sf(data = alaska, color = "red") + 
  geom_sf(data = Hawaii, color = "red")

# 保存为 geojson 文件
bind_rows(
  subset(states, !STATE %in% c("15", "02")),
  alaska, Hawaii
) -> newstates 
  
# newstates %>% 
#   select(fips = STATE) %>% 
#   st_transform(4326) %>% 
#   write_sf("newstates.geojson") 

# 分隔线
st_linestring(matrix(c(-121.201171875, 32.76880048488168, 
                       -98.7890625, 25.958044673317843), 
                     nrow = 2, byrow = T)) %>% 
  st_sfc() %>% 
  st_sf() %>% 
  st_set_crs(4326) %>% 
  st_transform(albers_proj) -> line1

st_linestring(matrix(c(-111.5523, 30.62574, 
                       -106.3916015625, 22.63429269379353), 
                     nrow = 2, byrow = T)) %>% 
  st_sfc() %>% 
  st_sf() %>% 
  st_set_crs(4326) %>% 
  st_transform(albers_proj) -> line2

ggplot(statesmap) + 
  geom_sf() + 
  geom_sf(data = alaska, color = "red") + 
  geom_sf(data = Hawaii, color = "red") + 
  geom_sf(data = line1, color = "green") + 
  geom_sf(data = line2, color = "green")

line1 %>% st_intersection(line2) %>% 
  st_transform(4326)

st_combine(c(line1$geometry, line2$geometry)) -> separateline 
  
separateline %>% 
  write_rds("separateline.rds") 

# 保存 shp 数据
newstates %>% 
  select(fips = STATE) %>% 
  left_join(st_drop_geometry(statesmap)) %>% 
  select(colnames(st_drop_geometry(statesmap)), everything()) -> newstates 

dir.create("us_states")
newstates %>% 
  write_sf("us_states/us_states.shp")

dir.create("separate_line")
separateline %>% 
  write_sf("separate_line/separate_line.shp")

# county map 
# 变换阿拉斯加
counties %>% 
  filter(STATE == "02") %>% 
  mutate(geometry = geometry * rotation(-50) / 2 + c(300000, -2000000)) %>% 
  st_set_crs(albers_proj) -> alaska_county 

# 变换夏威夷
counties %>% 
  filter(STATE == "15") %>% 
  mutate(geometry = geometry * rotation(-35) + c(3600000, 1800000)) %>% 
  st_set_crs(albers_proj) -> Hawaii_county 

ggplot(countiesmap) + 
  geom_sf() + 
  geom_sf(data = alaska_county, color = "red") + 
  geom_sf(data = Hawaii_county, color = "red")

# 保存为 geojson 文件
bind_rows(
  subset(counties, !STATE %in% c("15", "02")),
  alaska_county, Hawaii_county
) -> newcounties 

newcounties %>% 
  mutate(fips = paste0(STATE, COUNTY)) %>% 
  select(fips) %>% 
  left_join(st_drop_geometry(countiesmap)) %>% 
  select(colnames(st_drop_geometry(countiesmap)), 
         everything()) -> newcounties

# 保存 shp 数据
dir.create("us_counties")
newcounties %>% 
  write_sf("us_counties/us_counties.shp")
