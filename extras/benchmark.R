library(tibble)
library(dplyr)
library(purrr)
library(ellmer)
library(ggplot2)

benchmark_models <- c(
  "google/gemma-3-12b-it",
  "google/gemma-3-27b-it",
  "qwen/qwen3-32b",
  "meta-llama/llama-3.3-70b-instruct",
  "google/gemini-4-hydrogen",
  "openai/gpt-5.5",
  "anthropic/claude-sonnet-4.6",
  "mistralai/mistral-large-4-0"
)

results <- list()
for(i in seq_along(benchmark_models)){
  cli::cli_inform("Running evaluation for {benchmark_models[i]}")
  ch <- chat_openrouter(
    model = benchmark_models[i]
  )
  res <- evaluateDeviceClassifier(ch,
                                  name = benchmark_models[i],
                                  data = device_test_set,
                                  batchSize = 5L)
  results[[i]] <- res$summary |>
    mutate(
      model = benchmark_models[i]
    )
}
results <- bind_rows(results)

readr::write_csv(results,
                 here::here("extras", "benchmark_results.csv"))


results |>
  mutate(model = factor(model,
                        levels = benchmark_models)) |>
  ggplot(aes(x = accuracy, y = column, colour = model)) +
  geom_linerange(
    aes(xmin = 0, xmax = accuracy),
    linewidth = 0.8
  ) +
  geom_point(size = 3.5) +
  facet_wrap(vars(model)) +
  scale_x_continuous(limits = c(0, 100),
                     expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = "Accuracy (%)",
    y = NULL,
    colour = "Model"
  ) +
  theme_minimal() +
  theme(
    strip.text.y = element_text(angle = 0, hjust = 0),
    legend.position = "none"
  )
