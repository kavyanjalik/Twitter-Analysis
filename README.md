This R script allows users to analyse Twitter data. I developed this script for my PhD project (2020-2024). Researchers and data analysts can use this script to analyse Twitter data for various metrics, such as retweets, mentions, hashtags, and sentiment.

This R script performs comprehensive analyses on Twitter datasets, including:
- **Retweet network analysis**
- **Hashtag analysis**
- **Structural topic modelling (STM)**
- **Text and sentiment analysis**
- **User-follower networks**
- **User-hashtag networks**
- **Two-mode and co-occurrence networks**
- **Visualisation of above analysis**

**SET UP**

To start, you should have a Twitter data file downloaded in .csv. Ensure your dataset is formatted correctly with necessary columns such as author.username, text, retweet_count, and created_at etc.

Install required R packages: In R or RStudio, run the following commands to install all necessary libraries:
install.packages(c(
  "dplyr", "data.table", "tidyr", "readr", "purrr", "rtweet", "igraph",
  "quanteda", "stm", "ggraph", "ggplot2", "DT", "visNetwork", "cowplot",
  "scales", "RColorBrewer", "textnets", "sentimentr", "webshot"
))

Set the working directory: Modify the script to point to your working directory where the data files are located:
setwd("/path/to/your/directory")

**USAGE**

Ensure that your Twitter dataset (in CSV format) is correctly formatted with columns such as author.username, text, retweet_count, and created_at.

Open the twitter_analysis.R script in R or RStudio and replace the placeholder file path in the script with the path to your dataset.

You can execute the script line by line or run it entirely to perform:

Retweet network analysis
Topic modelling
Sentiment analysis
Hashtag frequency analysis
Network visualization (saved as PNG or GML files)
Modify parameters as needed: Adjust the script to your specific needs, such as filtering tweets by language, changing the number of top users, or modifying the STM parameters.

View outputs: The script will save visualizations like retweet networks, hashtag analysis graphs, and sentiment distribution plots in the working directory.

**CONTRIBUTIONS**

Contributions are welcome! Please follow these steps:

Fork the repository.
Create a new branch (git checkout -b feature-XYZ).
Commit your changes (git commit -m 'Add feature XYZ').
Push to the branch (git push origin feature-XYZ).
Open a pull request.

**Contact**
Kavyanjali (Kav)
Email: kavyanjali.kaushik@uc3m.es

