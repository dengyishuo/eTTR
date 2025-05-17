#
#   eTTR: Enhanced Technical Trading Rules
#
#   Copyright (C) 2017  DengYishuo
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 2 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
.env <- new.env()

.onUnload <- function(libpath) {
  library.dynam.unload("eTTR", libpath)
}


.onLoad <- function(libname, pkgname) {
  # 确保包名匹配｜Ensure package name matches
  if (pkgname != "eTTR") {
    warning("包名不匹配，预期为'eTTR'｜Package name mismatch, expected 'eTTR'")
    return(invisible(NULL))
  }

  # 数据目录路径｜Data directory path
  data_dir <- system.file("data", package = pkgname)
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # 检查网络连接｜Check internet connection
  has_internet <- tryCatch(
    {
      suppressWarnings({
        con <- url("http://www.google.com")
        open(con)
        close(con)
        TRUE
      })
    },
    error = function(e) {
      FALSE
    }
  )

  # 检查更新日期｜Check update date
  date_file <- file.path(data_dir, "last_updated.txt")
  today <- as.character(Sys.Date())

  # 决定是否需要更新｜Determine if update is needed
  needs_update <- !file.exists(date_file) ||
    (readLines(date_file, warn = FALSE) != today)

  # 执行更新逻辑｜Execute update logic
  if (has_internet && needs_update) {
    tryCatch(
      {
        # 检查依赖包｜Check dependencies
        if (!requireNamespace("quantmod", quietly = TRUE)) {
          stop("需要安装quantmod包来下载股票数据｜quantmod package is required to download stock data")
        }

        symbols <- c("AAPL", "TSLA")
        start_date <- as.Date("2010-01-01")
        updated_symbols <- character(0)

        # 下载并保存每只股票数据｜Download and save each stock's data
        for (symbol in symbols) {
          tryCatch(
            {
              # 下载数据｜Download data
              stock_data <- getSymbols(symbol, from = start_date, auto.assign = FALSE)

              # 保存为正确格式｜Save in correct format
              data_path <- file.path(data_dir, paste0(tolower(symbol), ".rda"))
              save(list = symbol, file = data_path, envir = environment())

              updated_symbols <- c(updated_symbols, symbol)
              message(
                "成功更新", symbol, "数据至", data_path,
                "｜Successfully updated ", symbol, " data to ", data_path
              )
            },
            error = function(e) {
              warning(
                "更新", symbol, "数据失败:", e$message,
                "｜Failed to update ", symbol, " data: ", e$message
              )
            }
          )
        }

        # 仅当所有股票都更新成功时才更新日期｜Update date only if all stocks updated successfully
        if (length(updated_symbols) == length(symbols)) {
          writeLines(today, date_file)
          message("数据更新日期：", today, "｜Data updated on: ", today)
        } else {
          warning("部分数据更新失败，日期未更新｜Some data updates failed, date not updated")
        }
      },
      error = function(e) {
        warning(
          "数据更新过程中发生错误:", e$message,
          "｜Error occurred during data update: ", e$message
        )
      }
    )
  } else if (!has_internet) {
    message("无网络连接，使用现有数据｜No internet connection, using existing data")
  } else {
    message("今日已更新数据，使用缓存版本｜Data already updated today, using cached version")
  }
}
