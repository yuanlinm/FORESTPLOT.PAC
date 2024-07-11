#' 基于 tidyverse 创建一个绘制森林图的函数，我真的服了现有的这些不符合我的需求的，零零散散只能差强人意的绘制森林图的R包了
#' 这是在赶DDL的时候制作的R包，因此诸多部分在后续会被再次更新
#' 由于语法简单，强烈建议在必要的时候，直接对本函数进行修改以及重编
#' 复杂需求并不支持，复杂需求请自行使用tidyverse绘制
#' 本函数的主要目的是，结合我们常用的，需要在PPT中完成后续的进一步调整的需求之上，快速稳定合理地输出森林图
#' 并尽最大可能减少后续在PPT中的调整门槛和步骤

# 参数
#' @param df data_frame, 用于绘图的总数据集，必须提供，请保证与后续左右显示表格层数据集行数一致，本函数不提供数据集格式校验
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
#' @param global_digits int，指定图形表格层全局的数字小数位数，不足则强制补0
#' @param hand_digits_vars vector，指定需要调整小数显示位数的列名称
#' @param hand_digits num，指定需要调整小数显示位数的全局数字小数位数，不足则强制补0
#' @param col_hjust num，指定纵标目到其下边线的距离
#' @param hjust_value num，指定表格层数据对齐方式，0为左对齐，0.5位居中对齐，1为右对齐，默认为0.5，首列强制居左对齐
#' @param table_scale num，指定表格层间距，不影响森林图层scale
#' @param group_var string，指定分组变量列名，可缺省，若使用则需要与group_var_value对应且同时提供
#' @param group_var_value vector，字符串向量，指定首列中无需缩进的分组变量值（亚组名），可缺省
#' @param group_var_value_bold logic，逻辑值，指定是否需要对无需缩进的分组变量值（亚组名）显示加粗，默认为否
#' @param retract_string string，指定需要缩进的分组缩进量，使用半角空格确定，例如"   "，默认为3个半角空格
#' @import tidyverse
#' @return A ggplot2 object

#' @export
get_forestplot = function(df = NULL,
                          left_side_data = NULL,
                          right_side_data = NULL,
                          estimate = NULL,
                          ci_low = NULL,
                          ci_high = NULL,
                          xlimit = c(0.75, 2),
                          xstep = 0.5,
                          ref_line_x = 0,
                          ref_line_color = 'black',
                          ref_line_type = 'dashed',
                          right_mv = 0,
                          first_col_mv = 0,
                          last_col_mv = 0,
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
                          xline_text_family = 'sans',
                          xline_text_bi = 'plain',
                          text_table_size = 4,
                          text_table_color = 'black',
                          text_table_family = 'sans',
                          text_table_bi = 'plain',
                          text_heading_size = 4,
                          text_heading_family = 'sans',
                          global_digits = 2,
                          # new
                          hand_digits_vars = NULL,
                          hand_digits = 3,
                          col_hjust = 0,
                          hjust_value = 0.5,
                          table_scale = 1,
                          group_var = NULL,
                          group_var_value = NULL,
                          group_var_value_bold = FALSE,
                          retract_string = '  ',
                          # new
                          bar_scale = 1
                          ) {
  ################################################################

   if (is.null(df)) {
    cat('---- df is null, please check your data! ---- \n')
  }
  # function main body
  if (!is.null(df)) {
    ###
    df$order_p = c(nrow(df):1)
    left_side_data$order_p = c(nrow(left_side_data):1)

    # 绘制基础森林图图形
    p = ggplot(df, aes(x = !!sym(estimate), y = order_p)) +
      geom_point(shape = box_shape,
                 size = box_size,
                 color = box_color) +
      geom_errorbar(aes(
        xmin = !!sym(ci_low) / bar_scale ,
        xmax = !!sym(ci_high) / bar_scale
      ), width = 0) +
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
    left_side_data[] <- lapply(seq_along(left_side_data), function(index) {
      column <- left_side_data[[index]]
      column_name <- names(left_side_data)[[index]]

      # 检查列是否为数值型、列名不为'order_p'且列名在hand_digits中
      if (is.numeric(column) &&
          column_name != 'order_p' &&
          column_name %in% hand_digits_vars) {
        # 如果是数值型，则将小数位数格式化为global_digits位
        return(sprintf(paste0("%.", hand_digits, 'f'), column))
      } else {
        # 如果不是数值型，则保持原样
        return(column)
      }

      if (is.numeric(column) && column_name != 'order_p') {
        # 如果是数值型，则将小数位数格式化为2位
        return(sprintf(paste0("%.", global_digits, 'f'), x))
      } else {
        # 如果不是数值型，则保持原样
        return(column)
      }
    })

    if (!is.null(right_side_data)) {
      right_side_data[] <- lapply(seq_along(right_side_data), function(index) {
        column <- right_side_data[[index]]
        column_name <- names(right_side_data)[[index]]

        # 检查列是否为数值型、列名不为'order_p'且列名在hand_digits中
        if (is.numeric(column) &&
            column_name != 'order_p' &&
            column_name %in% hand_digits_vars) {
          # 如果是数值型，则将小数位数格式化为global_digits位
          return(sprintf(paste0("%.", hand_digits, 'f'), column))
        } else {
          # 如果不是数值型，则保持原样
          return(column)
        }

        # 检查列是否为数值型
        if (is.numeric(column) && column_name != 'order_p') {
          # 如果是数值型，则将小数位数格式化为2位
          return(sprintf(paste0("%.", global_digits, 'f'), x))
        } else {
          # 如果不是数值型，则保持原样
          return(column)
        }
      })

      # 处理全展示数据NA
      right_side_data = as.data.frame(apply(right_side_data, 2, function(x)
        gsub('NA', NA, x)))
    }

    # 处理全展示数据NA
    left_side_data <- as.data.frame(apply(left_side_data, 2, function(x)
      gsub('NA', '', x)))
    left_side_data$order_p = as.numeric(left_side_data$order_p)

    # 处理首列展示及缩进策略
    if (!is.null(group_var_value) & !is.null(group_var)) {
      bold_rows_y = left_side_data[which(left_side_data[[sym(group_var)]] %in% group_var_value), 'order_p']
      col_name = group_var
      cols_num_left = length(left_side_data)
      col_mvs = -cols_num_left + 1 + right_mv + first_col_mv + + xlimit[1]

      # 修改首列标签，添加缩进
      left_side_data_first_col <- left_side_data %>%
        mutate(
          label = ifelse(
            order_p %in% bold_rows_y,
            as.character(!!sym(col_name)),
            paste0(retract_string, !!sym(col_name))
          ),
          fontface = ifelse(
            order_p %in% bold_rows_y & group_var_value_bold,
            "bold",
            "plain"
          )
        )

      # 绘制图形
      p <- p + geom_text(
        data = left_side_data_first_col,
        aes_string(
          x = col_mvs / table_scale,
          y = "order_p",
          label = 'label',
          fontface = 'fontface'
        ),
        hjust = 0,
        colour = text_table_color,
        size = text_table_size,
        family = text_table_family
      )
    }

    # 不处理首列加粗展示且不缩进策略
    if (is.null(group_var_value) & is.null(group_var)) {
      cols_num_left = length(left_side_data)
      col_mvs = -cols_num_left + 1 + right_mv + first_col_mv + xlimit[1]
      p <- p + geom_text(
        data = left_side_data,
        aes_string(
          x = col_mvs / table_scale,
          y = "order_p",
          label = sym(names(left_side_data)[1])
        ),
        hjust = 0,
        colour = text_table_color,
        size = text_table_size,
        family = text_table_family
      )
    }

    # 添加左边数据其他表格层
    for (i in seq_along(names(left_side_data))[-1]) {
      col_name <- names(left_side_data)[i]
      if (col_name != "order_p") {
        hjust_value_temp <- ifelse(i == 1, 0, hjust_value)
        col_mvs = -cols_num_left + i + right_mv + xlimit[1]
        col_mvs = ifelse(i == 1, col_mvs + first_col_mv, col_mvs)
        col_mvs = ifelse(i == max(seq_along(names(
          left_side_data
        ))) - 1, col_mvs + last_col_mv, col_mvs)
        p <- p + geom_text(
          data = left_side_data,
          aes_string(
            x = col_mvs / table_scale,
            y = "order_p",
            label = sym(col_name)
          ),
          hjust = hjust_value_temp,
          colour = text_table_color,
          size = text_table_size,
          family = text_table_family,
          fontface = text_table_bi
        )
      }
    }

    # 添加左边数据纵标目
    for (i in seq_along(names(left_side_data))) {
      col_name <- names(left_side_data)[i]
      if (col_name != "order_p") {
        hjust_value_temp <- ifelse(i == 1, 0, hjust_value)
        col_mvs = -cols_num_left + i + right_mv + xlimit[1]
        col_mvs = ifelse(i == 1, col_mvs + first_col_mv, col_mvs)
        col_mvs = ifelse(i == max(seq_along(names(
          left_side_data
        ))) - 1, col_mvs + last_col_mv, col_mvs)
        p <- p + annotate(
          "text",
          x = col_mvs / table_scale,
          y = max(df$order_p) + 1 + col_hjust,
          label = col_name,
          fontface = "bold",
          hjust = hjust_value_temp,
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
        sprintf(paste0("%.", global_digits, 'f'), !!sym(estimate)),
        " (",
        sprintf(paste0("%.", global_digits, 'f'), !!sym(ci_low)),
        chr_in_quote,
        sprintf(paste0("%.", global_digits, 'f'), !!sym(ci_high)),
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
    cols_num_lfet = -length(left_side_data) + 1 + xlimit[1] - 0.5
    cols_num_right = length(right_side_data) - 1 + xlimit[2] + 0.5

    p = p + scale_x_continuous(
      breaks = seq(xlimit[1], xlimit[2], by = xstep),
      labels = seq(xlimit[1], xlimit[2], by = xstep),
      limits = c(cols_num_lfet, cols_num_right)
    )

    # 添加图形右边文字层，并设定默认
    for (i in seq_along(names(right_side_data))) {
      col_name <- names(right_side_data)[i]
      if (col_name != "order_p") {
        col_mvs = xlimit[2] - 0.5 + i
        p <- p + geom_text(
          data = right_side_data,
          aes_string(
            x = (col_mvs + 0.1) / table_scale,
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
        col_mvs <- xlimit[2] - 0.5 + i
        p <- p + annotate(
          "text",
          x = (col_mvs + 0.1) / table_scale,
          y = max(df$order_p) + 1 + col_hjust,
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
    # 返回
    return(p)
  }
}


