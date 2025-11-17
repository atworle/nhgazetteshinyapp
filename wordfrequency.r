# app.R ---------------------------------------------------------------
library(shiny)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(scales)

#--------------------- DATA PREP (GLOBAL) -----------------------------

# Load full data (keep all columns you need later)
df_raw <- read.csv("NHgazette1756-1783.csv", stringsAsFactors = FALSE)

# Keep relevant columns, extract year/month
df_raw <- df_raw %>%
    select(sequence, date, ocr_eng) %>%
    mutate(
        year  = substr(date, 1, 4),
        month = substr(date, 1, 6) # "YYYYMM"
    )

# Tokenize
word.tokens <- df_raw %>%
    unnest_tokens(word, ocr_eng)

# Remove stopwords and numbers
data("stop_words")
clean.tokens <- word.tokens %>%
    anti_join(stop_words, by = "word") %>%
    filter(!str_detect(word, "^[0-9]+$"))

# Year & month as proper types
clean.tokens <- clean.tokens %>%
    mutate(
        year_num   = as.integer(year),
        # convert "YYYYMM" to Date (1st of month)
        month_date = as.Date(paste0(month, "01"), format = "%Y%m%d")
    )

year_min <- min(clean.tokens$year_num, na.rm = TRUE)
year_max <- max(clean.tokens$year_num, na.rm = TRUE)
# Precompute counts for efficiency
counts_year_word <- clean.tokens %>%
    mutate(
        word_lower = tolower(word),
        year_date = as.Date(paste0(year_num, "0101"), "%Y%m%d")
    ) %>%
    count(year_date, word_lower, name = "n")
counts_month_word <- clean.tokens %>%
    mutate(
        word_lower = tolower(word)
    ) %>%
    filter(!is.na(month_date)) %>%
    count(month_date, word_lower, name = "n")
word_totals <- clean.tokens %>%
    mutate(word_lower = tolower(word)) %>%
    count(word_lower, name = "n_total")

#----------------------------- UI -------------------------------------

ui <- fluidPage(
    titlePanel("New-Hampshire Gazette Text Explorer (1756–1783)"),
    sidebarLayout(
        sidebarPanel(
            h4("Filters"),
            textInput("target_words","Words to track(comma-separated):", value = "tyranny, liberty"),
            radioButtons(
                "agg_level", "Aggregation level:",
                choices = c("Year" = "year", "Month" = "month"),
                inline = TRUE
            ),
            sliderInput(
                "year_range", "Year range:",
                min = year_min, max = year_max,
                value = c(year_min, year_max),
                sep = ""
            ),
            numericInput(
                "wc_n", "Top N words for wordcloud:",
                value = 100, min = 10, max = 500, step = 10
            )
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Frequency over time",
                    br(),
                    plotOutput("freq_plot", height = "350px"),
                    br(),
                    verbatimTextOutput("freq_summary")
                ),
                tabPanel(
                    "Wordcloud",
                    br(),
                    wordcloud2Output("wc_plot", height = "400px"),
                    br(),
                    tableOutput("top_words_table")
                ),
                tabPanel(
                    "Word vectors (placeholder)",
                    br(),
                    p("Example ideas:"),
                    tags$ul(
                        tags$li("Select a period and a target word, show its nearest neighbors."),
                        tags$li("Compare cosine similarity of a word across time periods."),
                        tags$li("2D projection (PCA/TSNE) of semantic neighborhoods.")
                    )
                )
            )
        )
    )
)

#---------------------------- SERVER ----------------------------------

server <- function(input, output, session) {
    #--------------------- Reactive filtering ---------------------------

    freq_over_time <- reactive({
        # parse input words
        words <- str_split(input$target_words, ",")[[1]] %>%
            str_trim() %>%
            tolower()
        words <- words[words != ""]
        req(length(words) > 0)

        if (input$agg_level == "year") {
            df <- counts_year_word %>%
                filter(
                    word_lower %in% words,
                    year(year_date) >= input$year_range[1],
                    year(year_date) <= input$year_range[2]
                ) %>%
                rename(word = word_lower)

            # fill missing years × words
            full_years <- seq(
                as.Date(paste0(input$year_range[1], "0101"), "%Y%m%d"),
                as.Date(paste0(input$year_range[2], "0101"), "%Y%m%d"),
                by = "year"
            )

            full_grid <- expand_grid(
                year_date = full_years,
                word = words
            )

            df <- full_grid %>%
                left_join(df, by = c("year_date", "word")) %>%
                mutate(n = replace_na(n, 0))

            return(df)
        } else {
            df <- counts_month_word %>%
                filter(
                    word_lower %in% words,
                    year(month_date) >= input$year_range[1],
                    year(month_date) <= input$year_range[2]
                ) %>%
                rename(word = word_lower)

            req(nrow(df) > 0)
            return(df)
        }
    })

    #------------------------- Plot -------------------------------------

    output$freq_plot <- renderPlot({
        df <- freq_over_time()

        if (input$agg_level == "year") {
            ggplot(df, aes(x = year_date, y = n, color = word)) +
                geom_line() +
                geom_point() +
                theme_minimal() +
                labs(
                    title = "Mentions over time (by year)",
                    x = "Year",
                    y = "Count",
                    color = "Word"
                ) +
                scale_x_date(
                    breaks = scales::pretty_breaks(n = 10),
                    date_labels = "%Y"
                )
        } else {
            ggplot(df, aes(x = month_date, y = n, color = word)) +
                geom_line() +
                theme_minimal() +
                labs(
                    title = "Mentions over time (by month)",
                    x = "Month",
                    y = "Count",
                    color = "Word"
                ) +
                scale_x_date(
                    breaks = scales::pretty_breaks(n = 10),
                    date_labels = "%Y"
                )
        }
    })

    #------------------------- Summary ----------------------------------

    output$freq_summary <- renderPrint({
        df <- freq_over_time()

        # Range text
        if (input$agg_level == "year") {
            range_text <- paste0(
                "Years: ",
                format(min(df$year_date), "%Y"), "–",
                format(max(df$year_date), "%Y")
            )
        } else {
            range_text <- paste0(
                "Months: ",
                format(min(df$month_date), "%Y-%m"), " – ",
                format(max(df$month_date), "%Y-%m")
            )
        }

        cat(range_text, "\n\n")

        # Totals per word
        totals <- df %>%
            group_by(word) %>%
            summarise(total_mentions = sum(n), .groups = "drop") %>%
            arrange(desc(total_mentions))

        print(totals)
    })



    #-------------------------- Wordcloud -------------------------------

output$wc_plot <- renderWordcloud2({
    # only filter, no recomputing
    df <- word_totals %>%
        rename(word = word_lower)

    wc_n <- input$wc_n
    df <- df %>% slice_max(order_by = n_total, n = wc_n)

    wordcloud2(df)
})

}
shinyApp(ui = ui, server = server)




