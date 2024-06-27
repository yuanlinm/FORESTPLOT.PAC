# FORESTPLOT.PAC
> 一个用于绘制森林图的R包，基于tidyverse，功能及语法简单，可以方便地重编以及输出
> 
> 返回一个ggplot2的对象（object）
## 绘制函数：get_forestplot()

## 安装
```R
devtools::install_github("Backfire991/FORESTPLOT.PAC")
library(FORESTPLOT.PAC)
```

## 示例
```R
get_forestplot(df = res,
               left_side_data = res[,1:4],
               estimate = 'HR', 
               ci_low = 'HR_lower',
               ci_high = 'HR_upper',
               xlimit = c(0.5, 2.5))
```
             
## 参数
```R
#' @param df data_frame, 用于绘图的总数据集，必须提供，请保证与后续左右显示表格层数据集行数一致，本函数不提供数据集各式校验
#' @param left_side_data data_frame, 用于展示在左边的数据表格，必须提供，请保证与后续的置信区间行数一致，本函数不提供格式校验
#' @param right_side_data data_frame, 用于展示在右边的数据表格，可以缺省，本函数不提供格式校验
#' @param estimate vector, 效应值，请保证该效应在尺度或转换上与后续的置信区间一致，本函数不提供期望或自然对数转换
#' @param ci_low vector, 效应置信区间下限，请自行计算置信区间下限，本函数不提供se辅助计算
#' @param ci_high vector, 效应置信区间上限，请自行计算置信区间上限，本函数不提供se辅助计算
#' @param xlimit c(xmin, xmax), 规定x轴的最小值和最大值
#' @param xstep num, 规定x轴的刻度步进
#' @param ref_line_x num, 规定参考线的水平位置
#' @param ref_line_color string，规定参考线的颜色
#' @param ref_line_type string, 规定参考线的类型
#' @param right_mv num，指定一个表格层整体移动参数来调整表格和图形的相对位置
#' @param first_col_mv num, 指定一个表格层第一列移动参数来调整第一列和后续列的相对位置
#' @param last_col_mv num, 指定一个表格层最后一列移动参数来调整最后一列列和前列的相对位置
#' @param chr_in_quote string，指定一个自动置信区间中的分割符号，默认为' to '
#' @param box_shape num，指定图形中间点的形状
#' @param box_size num，指定图形中间点的大小
#' @param box_color string, 指定图形中间点的颜色，本函数不提供fill参数
#' @param headline_size num, 指定纵标目下边线的粗细
#' @param headline_color string，指定纵标目下边线的颜色
#' @param x_line_size num，指定x轴线的粗细
#' @param xline_color string，指定x轴线的颜色
#' @param xline_text_size string，指定x轴字体
#' @param xline_text_color string，指定x轴字体
#' @param xline_text_family string，指定x轴字体
#' @param xline_text_bi string，指定x轴是否需要加粗或斜体
#' @param text_table_size num, 指定图形表格层的文字大小
#' @param text_table_color string，指定图形表格层的文字颜色
#' @param text_table_family string, 指定图形表格层的文字字体，请确保字体可用，本函数不做字体可用性以及UTF-8校验
#' @param text_table_bi string，指定图形表格层文字是否需要加粗或者斜体
#' @param text_heading_size num，指定图形纵标目字体大小
#' @param text_heading_family string, 指定图形纵标目的文字字体，请确保字体可用，本函数不做字体可用性以及UTF-8校验
#' @param global_digits int, 指定图形层全局的数字小数位数，不足则强制补0
#' @import tidyverse 
#' @return A ggplot2 object
```
