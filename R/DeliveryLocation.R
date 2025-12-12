#' 交货地点生成器界面
#'
#' @param colTitles  主页标题
#' @param widthRates 左右比例
#' @param func_left 左函数
#' @param func_right 右函数
#' @param tabTitle 标题
#' @param func_bottom  下面一栏
#'
#' @return 返回值
#' @import tsui
#' @export
#'
#' @examples
#' DeliveryLocationUI()
DeliveryLocationUI <- function(tabTitle ='Delivery Location',
                           colTitles =c('Filter Area','Operation Area','Result Area'),
                           widthRates =c(6,6,12),
                           func_left = DeliveryLocationUI_left,
                           func_right =DeliveryLocationUI_right,
                           func_bottom = DeliveryLocationUI_bottom
) {

  #三栏式设置，可以复制
  res = tsui::uiGen3(tabTitle = tabTitle,colTitles =colTitles,widthRates = widthRates,func_left = func_left,func_right = func_right,func_bottom = func_bottom )
  return(res)

}





#' 请输入文件
#'
#' @return 返回值
#' @export
#'
#' @examples
#' DeliveryLocationUI_left()
DeliveryLocationUI_left <- function() {


  res <- tagList(
    tsui::mdl_text2(id ='text_DeliveryLocation' ,label ='Delivery Location',value = '')







  )
  return(res)

}


#' 生成区域
#'
#' @return 返回值
#' @export
#'
#' @examples
#' DeliveryLocationUI_bottom()
DeliveryLocationUI_right <- function() {
  res <- tagList(



    shiny::actionButton(inputId = 'btn_DeliveryLocation_view',label = 'Search'),

    tsui::mdl_download_button(id = 'dl_DeliveryLocation',label = 'Download')



  )
  return(res)

}


#' 生成区域
#'
#' @return 返回值
#' @export
#'
#' @examples
#' DeliveryLocationUI_bottom()
DeliveryLocationUI_bottom <- function() {
  res <- tagList(
    tsui::uiScrollX(tsui::mdl_dataTable(id = 'DeliveryLocation_resultView',label ='结果显示' ))

  )
  return(res)

}
