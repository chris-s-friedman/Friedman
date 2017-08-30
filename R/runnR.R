#' You're doing something important
#'
#' Do you ever feel like you are getting enough done. In fact, you've gotten everything you wanted done, but you still have a few more hours at work. Well, use runnR to make that console busy!
#'
#' @param time how long do you want the function to run?
#' @param what What do you want the function to say is processing?
#' @param ran_time Do you want the run time of the function to be random? I do!
#'
#' @examples runnR()
#'
#' @import progress
#'
#' @return
#'
#' @export
runnR <- function(time = sample(100:500, size = 1),
                  what = rep(letters, times = 2),
                  ran_time = TRUE){
  pb <- progress_bar$new(
    format = "(:spin)  processing :what at :rate, got :bytes in :elapsed",
    clear = FALSE, total = 1e7, width = 60)
  for(item in what){
    pbtickr <- function(){
    for (i in 1:time) {
      pb$tick(tokens = list(what = item))
      Sys.sleep(2 / 100)
    }
    pb$tick(1e7)
    invisible()}
    if(ran_time){
      time = sample(100:500, size = 1)
      pbtickr()
    }
    if(!ran_time){
      pbtickr()
    }
  }
}
