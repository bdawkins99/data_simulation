# Create `datatable` HTML widget with preferred button options.

create_html_DT <- function(df, tab.codes = "Bfrtip", page.length = 10, page.width = 10, ...){
  DT::datatable(
    df,
    rownames     = FALSE,
    style        = "bootstrap",
    extensions   = "Buttons",
    options      = list(
      dom          = tab.codes,
      buttons      = c(I("colvis"), "copy", "csv", "pdf"),
      initComplete = DT::JS(
        "function(settings, json){", "$(this.api().table().header()).css({
        'background-color': '#4067E2', 'color': '#fff'});", "}"
      ),
      pageLength = page.length,
      scrollX = TRUE
    ),
    ...)
}