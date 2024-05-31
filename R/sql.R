


getSelectConditions <- function (input) {

  conditions <- c(
    paste0(query, "label IN ('", paste(input$speciesInput, collapse = "','"), "')"),
    paste0(query, "score >= ", input$scoreInput[1], " AND score <= ", input$scoreInput[2]),
    paste0(query, "timestamp >= '", input$dateInput[1], "' AND timestamp <= '", input$dateInput[2] + lubridate::days(1), "'"),
      paste0(query, "site IN ('", paste(input$siteInput, collapse = "','"), "')")
  )

  condidtions <- paste(conditions, collapse = " AND ")
  return(condidtions)
}


getSelectTopScoringQuery <- function(input) {

  # Determine the appropriate strftime format based on input$intervalInput
  interval_format <- switch(input$intervalInput,
                            "day" = "%Y-%m-%d",
                            "hour" = "%Y-%m-%d %H",
                            "minute" = "%Y-%m-%d %H:%M",
                            "second" = "%Y-%m-%d %H:%M:%S")

  # Construct the SQL query using glue (or sprintf for more traditional string formatting)
  query <- glue::glue("
    SELECT *
    FROM (
      SELECT *,
             strftime('{interval_format}', timestamp) AS interval
      FROM your_table
    )
    WHERE rowid IN (
      SELECT rowid
      FROM (
        SELECT *,
               RANK() OVER (PARTITION BY interval, label, site ORDER BY score DESC) AS rnk
        FROM (
          SELECT *,
                 strftime('{interval_format}', timestamp) AS interval
          FROM your_table
        )
      )
      WHERE rnk = 1
    );
  ")

  return(query)
}


intervalFormat <- function (interval_input) {

  # Determine the appropriate strftime format based on input$intervalInput
  switch(interval_input,
         "hour" = "%Y-%m-%d %H",
         "day" = "%Y-%m-%d",
         "week" = "%Y-%W",  # Week of year format
         "month" = "%Y-%m"  # Month format
  )

}


getCountsNoZerosSqlQuery <- function (input) {

  interval_format <- intervalFormat(input$intervalInput)

  select_query <- getSelectQuery(input)


  # Construct the SQL query using glue (or sprintf for more traditional string formatting)
  query <- glue::glue("
    SELECT *,
           COUNT(*) AS count,
           GROUP_CONCAT(row_id) AS row_ids
    FROM (
      SELECT *,
             strftime('{interval_format}', timestamp) AS interval
      FROM data
      WHERE {select_query}
    )
    GROUP BY interval, label
    ORDER BY interval, label
  ")

  return(query)

}
