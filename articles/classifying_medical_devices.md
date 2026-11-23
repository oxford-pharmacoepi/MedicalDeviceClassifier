# Classifying medical devices using an LLM

``` r

library(MedicalDeviceClassifier)

chat <- ellmer::chat_ollama(model = "MedGemma:4b")
device_data <- dplyr::tibble(device_name = c("Dexcom's G7"))
chat |> 
  classifyMedicalDevices(device_data)
```
