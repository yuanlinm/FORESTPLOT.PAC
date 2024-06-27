#' 基于 tidyverse 创建一个绘制森林图的函数，我真的服了现有的这些不符合我的需求的，零零散散只能差强人意的绘制森林图的R包了
#' 这是在赶DDL的时候制作的R包，因此诸多部分在后续会被再次更新
#' 由于语法简单，强烈建议在必要的时候，直接对本函数进行修改以及重编
#' 复杂需求并不支持，复杂需求请自行使用tidyverse绘制
#' 本函数的主要目的是，结合我们常用的，需要在PPT中完成后续的进一步调整的需求之上，快速稳定合理地输出森林图
#' 并尽最大可能减少后续在PPT中的调整门槛和步骤

# 参数
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

#' @export
get_forestplot = function(df = NULL,
                          left_side_data = NULL,
                          right_side_data = NULL,
                          estimate = NULL,
                          ci_low = NULL,
                          ci_high = NULL,
                          xlimit = c(-1, 2),
                          xstep = 0.5,
                          ref_line_x = 0,
                          ref_line_color = 'black',
                          ref_line_type = 'dashed',
                          right_mv = 0.75,
                          first_col_mv = -0.75,
                          last_col_mv = 0.2,
                          chr_in_quote = ' to ',
                          box_shape = 15,
                          box_size = 3,
                          box_color = 'black',
                          headline_size = 0.5,
                          headline_color = 'black',
                          x_line_size = 0.5,
                          xline_color = 'black',
                          xline_text_size = 12,
                          xline_text_color = 'black',
                          xline_text_family = 'Arial',
                          xline_text_bi = 'plain',
                          text_table_size = 4,
                          text_table_color = 'black',
                          text_table_family = 'Arial',
                          text_table_bi = 'plain',
                          text_heading_size = 4,
                          text_heading_family = 'Arial',
                          global_digits = 2) {
  ####
  if(is.null(df)){
    cat('---- df is null, please check your data! ---- \n')
  }
  if(!is.null(df)){
    ###
    df$order_p = c(nrow(df):1)
    left_side_data$order_p = c(nrow(left_side_data):1)

    # 绘制基础森林图图形
    p = ggplot(df, aes(x = !!sym(estimate), y = order_p)) +
      geom_point(shape = box_shape,
                 size = box_size,
                 color = box_color) +
      geom_errorbar(aes(xmin = !!sym(ci_low), xmax = !!sym(ci_high)), width = 0) +
      theme_classic() +
      theme(
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.line.x = element_line(colour = xline_color, linewidth = x_line_size)
      )
    
    # 处理表格层的小数位数
    left_side_data[] <- lapply(left_side_data, function(x) {
      # 检查列是否为数值型
      if (is.numeric(x) && !identical(names(x), 'order_p')) {
        # 如果是数值型，则将小数位数格式化为2位
        return(sprintf(paste0("%.", global_digits, 'f'), x))
      } else {
        # 如果不是数值型，则保持原样
        return(x)
      }
    })
    left_side_data$order_p = as.numeric(left_side_data$order_p)
    
    # 添加表格层
    cols_num_left = length(left_side_data)
    for (i in seq_along(names(left_side_data))) {
      col_name <- names(left_side_data)[i]
      if (col_name != "order_p") {
        hjust_value <- ifelse(i == 1, 0, 0.5)
        col_mvs = -cols_num_left + i + right_mv
        col_mvs = ifelse(i == 1, col_mvs + first_col_mv, col_mvs)
        col_mvs = ifelse(i == max(seq_along(names(
          left_side_data
        ))) - 1, col_mvs + last_col_mv, col_mvs)
        p <- p + geom_text(
          data = left_side_data,
          aes_string(
            x = col_mvs,
            y = "order_p",
            label = sym(col_name)
          ),
          hjust = hjust_value,
          colour = text_table_color,
          size = text_table_size,
          family = text_table_family,
          fontface = text_table_bi
        )
      }
    }
    
    # 添加纵标目
    for (i in seq_along(names(left_side_data))) {
      col_name <- names(left_side_data)[i]
      if (col_name != "order_p") {
        hjust_value <- ifelse(i == 1, 0, 0.5)
        col_mvs = -cols_num_left + i + right_mv
        col_mvs = ifelse(i == 1, col_mvs + first_col_mv, col_mvs)
        col_mvs = ifelse(i == max(seq_along(names(
          left_side_data
        ))) - 1, col_mvs + last_col_mv, col_mvs)
        p <- p + annotate(
          "text",
          x = col_mvs,
          y = max(df$order_p) + 1,
          label = col_name,
          fontface = "bold",
          hjust = hjust_value,
          colour = 'black',
          size = text_heading_size,
          family = text_heading_family
        )
      }
    }
    
    # 添加纵标目边线以及垂直参考线
    p = p +
      geom_hline(
        yintercept = max(df$order_p) + 0.5,
        linetype = "solid",
        size = headline_size,
        color = headline_color
      ) +
      annotate(
        "segment",
        x = 1,
        xend = 1,
        y = min(df$order_p) - 0.5,
        yend = max(df$order_p) + 0.5,
        linetype = ref_line_type,
        colour = ref_line_color,
        size = 0.5
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0.1)))
    
    # 添加图形右边的Estimate列
    df = df %>%
      mutate(Estimate = paste0(
        sprintf("%.2f", !!sym(estimate)),
        " (",
        sprintf("%.2f", !!sym(ci_low)),
        chr_in_quote,
        sprintf("%.2f", !!sym(ci_high)),
        ")"
      ))
    
    # 注意空数据集判断
    # right_side_data = NULL
    if (is.null(right_side_data)) {
      right_side_data = df[, c('Estimate', 'order_p')]
    }
    
    # 顺序指定
    right_side_data$order_p = c(nrow(right_side_data):1)
    
    # 全局最小值，全局最大值
    cols_num_right = length(right_side_data) - 1 + xlimit[2] + 0.15
    p = p + scale_x_continuous(
      scale_x_continuous(expand = expansion(mult = c(0.01, 0.01))),
      breaks = seq(xlimit[1], xlimit[2], by = xstep),
      labels = seq(xlimit[1], xlimit[2], by = xstep),
      limits = c(-cols_num_left + 1, cols_num_right)
    )
    
    # 添加图形右边文字层，并设定默认
    for (i in seq_along(names(right_side_data))) {
      col_name <- names(right_side_data)[i]
      if (col_name != "order_p") {
        col_mvs = xlimit[2] + 0.5 + i - 1
        p <- p + geom_text(
          data = right_side_data,
          aes_string(
            x = col_mvs + 0.2,
            y = "order_p",
            label = sym(col_name)
          ),
          hjust = 0.5,
          colour = text_table_color,
          size = text_table_size,
          family = text_table_family,
          fontface = text_table_bi
        )
      }
    }
    
    # 右边数据纵标目
    for (i in seq_along(names(right_side_data))) {
      col_name <- names(right_side_data)[i]
      if (col_name != "order_p") {
        col_mvs = xlimit[2] + 0.5 + i - 1
        p <- p + annotate(
          "text",
          x = col_mvs,
          y = max(df$order_p) + 1,
          label = col_name,
          fontface = "bold",
          hjust = 0.5,
          colour = 'black',
          size = text_heading_size,
          family = text_heading_family
        )
      }
    }
    
    # 指定图形字体
    p = p + theme(
      axis.text.x = element_text(
        family = xline_text_family,
        size = xline_text_size,
        color = xline_text_color,
        face = xline_text_bi
      )
    )
    
    # 返回ggplot object
    return(p)
  }
}