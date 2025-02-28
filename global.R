library(dplyr);
library(manifoldr);
library(ggplot2);
library(purrr);
library(progress);

slugs <- c('in-the-eventuality-of-a-nuclear-wea' # z9xpo2Zr22FlLCbUGcbD
           ,'will-israel-strike-irans-nuclear-fa' # tPyn05ZAgL
           ,'what-will-trump-do-on-day-1-of-bein'); # gcdQlRAp5g

# Obtain a high-level list of markets, from which the user can choose which ones
# to explore in greater detail. The 'slug' column from this list can be passed
# to get_market() and from there to flattenmarketlistdata()
slurpdownmarkets <- function(max=60000,limit=1000,sort='score',filter='open',contractType='ALL',...){
  pars = list(...,limit=limit,sort=sort,filter=filter,contractType=contractType);
  pb <- progress_bar$new(
    format = "  got :current rows at :tick_rate/sec, total time: :elapsed"
    ,clear = FALSE, total=NA);
  oo <- manifold_api(endpoint = '/v0/search-markets'
                     ,request_type = 'GET',params_list = pars)$content;
  oon <- 0;
  message('Downloading questions from Manifold Markets');
  pb$tick(length(oo));
  while(length(oo)-oon==limit && length(oo) <= max){
    oon <- length(oo);
    oo <- c(oo,manifold_api(endpoint = '/v0/search-markets'
                            ,request_type = 'GET'
                            ,params_list = c(pars,offset=oon))$content);
    pb$tick(length(oo)-oon);
  };
  message('\nDownload done. Preparing data frame.')
  pb$terminate();
  #browser();
  pb <- progress_bar$new(total=length(oo),width=60);
  pb$tick(0);
  sapply(oo,function(xx) {
    if(length(xx$pool)>0) xx$pool <- xx$pool$YES; 
    pb$tick(1);
    as_tibble(xx);},simplify = F) %>% 
    bind_rows() %>% unique() %>% 
    mutate(across(ends_with('Time'),~ as.POSIXct(.x/1000)));
}


# Take the results of get_market() and turn them into a unified data frame 
# with one row per answer (therefore YES/NO markets get one row)
# Will only be tested for BINARY and MULTIPLE_CHOICE type questions for now
# This will not provide performance data over time, that's a future step,
# but it will provide the full current details of each answer, so that 
# time-traces can be chosen from among these
flattenmarketlistdata <- function(data){
  out <- as_tibble(data[sapply(data,length)==1 & sapply(data,is.atomic)]);
  if(hasName(data,'groupSlugs')){
    out <- cbind(out,groupSlugs=paste0(data$groupSlugs,collapse=';'))};
  # out <- as.data.frame(data[names(data)[sapply(data,length)==1]] ) %>% 
  #   cbind(groupSlugs=paste0(data$groupSlugs,collapse=';'));
  # multipe-choice items have a list-type 'answers' field
  if(hasName(data,'answers')){
    out <- sapply(data$answers,as.data.frame,simplify=F) %>% bind_rows() %>%
    left_join(out,.,by=c(id='contractId'),suffix=c('','.answer'));
  };
  out;
}

expandDetails <- function(xx) try(get_market(market_id_or_slug = xx)$content %>% 
                                    flattenmarketlistdata);

# example 

if(!file.exists('cached_market_data.rdat') || 
   difftime(Sys.time(),file.info('cached_market_data.rdat')$mtime
            ,units='days') > 1){
  message('Pull down updated market data');
  market_data <- slurpdownmarkets(max=10000);
  market_data_for_DT <- transmute(market_data
                                  ,closeTime,outcomeType, uniqueBettorCount
                                  , lastUpdatedTime, probability
                                  , question = sprintf('<a href="%s" target="_blank">%s</a>',url,question)
                                  ,action = sprintf('<button class="btn btn-success btn-sm add-button" id="btn_%s"><span>&#9654;</span></button>',slug)
                                  ) %>%
    datatable(escape = FALSE
              , options = list(scrollY = "400px", paging = FALSE)
              , rownames='',selection='none');
  save(market_data,market_data_for_DT,file='cached_market_data.rdat');
} else {
  message('Reusing cached data');
  load('cached_market_data.rdat');
};
# have to use slugs, for some reason the IDs aren't always returning results
# here we collect detailed, answer-level summary info for each market of interest
bar <- market_data$slug %>% intersect(slugs) %>% lapply(expandDetails) %>% bind_rows;
  # lapply(function(xx) {
  #   try(get_market(market_id_or_slug = xx)$content %>% flattenmarketlistdata)
  #   }) %>% 
  # bind_rows;
# here we pull a time-series for each answer/market of interest
baz <- unique(bar$id) %>% sapply(function(xx){
  manifold_api(endpoint='/v0/bets',request_type='GET'
               ,params_list=list(contractId=xx,includeZeroShareRedemptions='true',filterRedemptions='false'))$content}) %>%
  flatten() %>% lapply(flattenmarketlistdata) %>% bind_rows() %>% 
  left_join(bar[,c('id.answer','text')],by=c(answerId='id.answer'));
# plot the time series
baz %>% ggplot(aes(x=updatedTime,y=probBefore
                   ,col=text)) + geom_step()


