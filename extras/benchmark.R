library(tibble)
library(dplyr)
library(purrr)
library(ellmer)
library(ggplot2)

# from smallest/ cheapest to biggest/ most expensive
benchmark_models <- c(
  "google/gemma-3-12b-it",
  "google/gemma-4-26b-a4b-it",
  "google/gemma-4-31b-it",
  "meta-llama/llama-4-maverick",
  "z-ai/glm-5.3-flash",
  "openai/gpt-oss-120b",
  "openai/gpt-6-luna",
  "deepseek/deepseek-v4.1-flash-1017",
  "google/gemini-3.5-flash-lite",
  "anthropic/claude-haiku-4.5",
  "google/gemini-3.8-flash",
  "mistralai/mistral-large-4-0",
  "google/gemini-4-hydrogen",
  "anthropic/claude-sonnet-5.5",
  "openai/gpt-5.5",
  "openai/gpt-6.2-sol"
)



benchmark_summary <- list()
benchmark_results <- list()
for(i in seq_along(benchmark_models)){
  cli::cli_inform("Running evaluation for {benchmark_models[i]}")
  tictoc::tic()
  ch <- chat_openrouter(
    model = benchmark_models[i]
  )
  res <- evaluateDeviceClassifier(ch,
                                  data = device_test_set,
                                  batchSize = 25L)
  benchmark_summary[[i]] <- res$summary |>
    mutate(
      model = benchmark_models[i]
    )
  benchmark_results[[i]] <- res$results |>
    mutate(
      model = benchmark_models[i]
    )

  tictoc::toc()
}
benchmark_summary <- bind_rows(benchmark_summary)
benchmark_results <- bind_rows(benchmark_results)

readr::write_csv(benchmark_summary,
                 here::here("vignettes", "benchmark_summary.csv"))
readr::write_csv(benchmark_results,
                 here::here("vignettes", "benchmark_results.csv"))
