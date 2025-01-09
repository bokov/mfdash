library(dplyr);
library(manifoldr);
library(ggplot2);

slugs <- c('in-the-eventuality-of-a-nuclear-wea' # z9xpo2Zr22FlLCbUGcbD
           ,'will-israel-strike-irans-nuclear-fa' # tPyn05ZAgL
           ,'what-will-trump-do-on-day-1-of-bein') # gcdQlRAp5g
;

prepdata <- function(slug){
  # get info about market itself
  market <- get_market(market_id_or_slug=slug)$content;
  marketid <- market$id;
  marketq <- market$question;
  marketmeta <- sapply(market$answers,function(xx)
    with(xx,data.frame(contractId,id,createdTime,text=paste0(marketq,': ',text))),simplify=F);
  marketmeta <- if(length(marketmeta)==0){
    with(market,data.frame(contractId,id='',createdTime,text=marketq))
  } else {bind_rows(marketmeta)}
  # now get historic prices
  rawhistory <- manifold_api(endpoint = '/v0/bets'
                          ,request_type='GET'
                          ,params_list = list(contractId=marketid))$content;
  history <- sapply(rawhistory,function(xx) {
    thisid <- coalesce(xx$answerId,'');
    with(xx,data.frame(amount,shares
                       ,updatedTime=as.POSIXct(updatedTime/1000)
                       ,userId,contractId
                       ,answerId=thisid,probBefore,probAfter))
    },simplify = F) %>% bind_rows
  #history %>% ggplot(aes(x=updatedTime,y=probBefore,col=answerId)) + geom_step()
  browser();
}
