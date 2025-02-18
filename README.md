# 📊 Airbnb Booking Rate Prediction  

## 🚀 Project Overview  
This project predicts whether an Airbnb listing will have a **high booking rate** by leveraging **feature engineering, sentiment analysis, and predictive modeling**. The goal is to enhance model accuracy while keeping the **False Positive Rate (FPR) below 0.3**.  

## 🏗️ Key Features  
✔ **Data Pipeline Development:** Cleaned and transformed structured & unstructured Airbnb datasets using R (`tidyverse`, `dplyr`).  
✔ **Feature Engineering & Sentiment Analysis:** Applied **TF-IDF, tokenization, and text mining** on listing descriptions.  
✔ **Predictive Modeling:** Built **LASSO regression & Random Forest models**, achieving **85%+ accuracy**.  
✔ **Performance Optimization:** Tuned models to maintain an **FPR below 0.3** while maximizing precision.  

---

## 📂 Repository Structure  
📁 airbnb-booking-prediction
│── 📂 data/ # Sample datasets (public data only)
│── 📂 scripts/ # R scripts for data cleaning, feature engineering & modeling
│── 📂 notebooks/ # Exploratory data analysis & model evaluation
│── 📄 README.md # Project documentation
│── 📄 requirements.txt # List of required libraries


> **📢 Note:** Due to GitHub’s file size limitations, **large datasets and model files** are available in the **[GitHub Releases Section](https://github.com/Carter17401/Machine-Learning-Driven-Booking-Rate-Optimization-for-Airbnb-Listings/releases).**  

---

## 🛠️ Tech Stack  
- **Languages:** R  
- **Libraries:** `tidyverse`, `dplyr`, `text2vec`, `caret`, `ggplot2`, `tm`, `quanteda`  
- **Machine Learning:** LASSO Regression, Random Forest  
- **Text Mining:** Tokenization, TF-IDF, Sentiment Analysis  

---

## 📊 Results  
✅ Achieved **85%+ accuracy** with **optimized feature engineering**  
✅ Maintained **False Positive Rate (FPR) below 0.3**  
✅ Improved model performance by **20%** using advanced text analytics  

---

## 📌 How to Run the Project  
1. Clone the repository:  
   ```bash
   git clone https://github.com/yourusername/airbnb-booking-prediction.git

2. Download Large Files:
- Visit GitHub Releases to download large datasets and model files.
- Extract them into the `data/` folder in your cloned repository.

3. Install dependencies:
   ```r
   install.packages(c("tidyverse", "dplyr", "caret", "text2vec", "ggplot2", "tm", "quanteda"))

4. Run Data Processing & Model Training
- Execute `scripts/data_cleaning.R` to preprocess the dataset.
- Run `scripts/model_training.R` to train and evaluate the model.

---
## 📢 Future Improvements  

🔹 Incorporate **deep learning models** for improved accuracy  
🔹 Optimize feature selection using **SHAP values & XGBoost**  
🔹 Develop a **dashboard** for real-time booking rate predictions  

---

## 📬 Contact  

For any questions, feel free to reach out via [LinkedIn](https://www.linkedin.com/in/tanmaysakharkar) or open an issue!  

