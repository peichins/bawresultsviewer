library(sodium)

user_base <- tibble::tibble(
  user = c("admin", "user_1"),
  password = purrr::map_chr(c("password123" ,"password321"), sodium::password_store),
  permissions = c("admin", "standard"),
  name = c("Admin", "User One")
)

saveRDS(user_base, "user_base.rds")
