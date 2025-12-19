library(shiny)
library(DT)
library(shinyWidgets)
DeliveryLocation_all_columns_ui <- c(
  'Delivery Location',
  'Sales OrderID',
  'Sales OrderQty',
  'PN',
  'Product Name',
  'Sales OrderDate',
  'Delivery Date',
  'Delivery Qty'

)
#设置默认值

DeliveryLocation_default_columns_ui <- c(
  'Delivery Location',
  'Sales OrderID',
  'Sales OrderQty',
  'PN',
  'Product Name',
  'Sales OrderDate'
)

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
                           colTitles =c('Operation Area','Column Setting','Result Area'),
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
    tsui::mdl_text2(id ='text_DeliveryLocation' ,label ='Delivery Location',value = ''),

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
DeliveryLocationUI_right <- function() {
  res <- tagList(

    pickerInput(
      inputId = "DeliveryLocation_column_selector",
      label = "Selected Columns:",
      choices = DeliveryLocation_all_columns_ui,
      selected = DeliveryLocation_default_columns_ui,
      options = list(
        `actions-box` = TRUE,
        `selected-text-format` = "count > 3",
        #`count-selected-text` = "{0} Columns Seleced",
        `count-selected-text` = "Default Value",

        size = 10,
        `live-search` = TRUE,
        `live-search-style` = "contains"
      ),
      multiple = TRUE,
      choicesOpt = list(
        style = rep(("color: black;"), 100)
      )
    ),
    # 全选/取消全选按钮
    actionButton("btn_DeliveryLocation_select_all", "Select All",
                 class = "btn-primary btn-sm",
                 style = "margin-right: 5px;"),
    # actionButton("btn_DeliveryLocation_deselect_all", "Deselect All",
    #              class = "btn-secondary btn-sm"),

    actionButton("btn_DeliveryLocation_defaultValue", "Default Value",
                 class = "btn-primary btn-sm")
    #verbatimTextOutput("DeliveryLocation_selection_info")




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
