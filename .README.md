# Project Overview: Customer Data Analysis

This R project aims to analyze customer behavior using data visualization techniques, data cleaning processes, clustering methods, and association rule mining. The project is built as a Shiny application, which allows for interactive data exploration and analysis.

## 1. Data Input

- **Functionality:** Users can upload a CSV file containing customer transaction data.
- **Input Format:** The expected input format is a CSV file that includes columns like `paymentType`, `total`, `city`, `age`, `rnd`, and `items`.

---

## 2. Data Cleaning

- **Purpose:** Ensure the data is in the right format and ready for analysis.
- **Steps:**
  - Validate the uploaded file to check if it contains the necessary columns and is not empty.
  - Handle missing values and erroneous entries (if applicable).

---

## 3. Data Visualization

The project includes several visualizations to help analyze customer behaviors:

### Pie Chart
- **Purpose:** Show the proportion of total spending between Cash and Credit transactions.
- **Implementation:** Uses `ggplot2` to create a polar plot based on total spending for each payment type.

### Bar Chart
- **Purpose:** Display total spending for each city.
- **Implementation:** Bar chart represents the total spending per city, sorted in descending order.

### Scatter Plot
- **Purpose:** Illustrate the total spending across different age groups.
- **Implementation:** Points are plotted to show the relationship between age and total spending.

### Box Plot
- **Purpose:** Visualize the distribution of total spending.
- **Implementation:** Box plot provides insights into spending distribution, highlighting outliers.

---

## 4. Clustering

- **Purpose:** Group customers based on their age and total spending to identify segments.
- **Implementation:**
  - Uses the K-means clustering algorithm.
  - Users can input the number of clusters (2 to 4).
  - A table displays the clustering results, showing which cluster each customer belongs to, along with their age and total spending.

---

## 5. Association Rules

- **Purpose:** Identify patterns and relationships between items purchased by customers.
- **Implementation:**
  - Uses the Apriori algorithm to generate association rules.
  - Users can input support and confidence thresholds.
  - Displays the rules in a table format, showing how items are associated based on the specified parameters.

---

## 6. User Interface

The Shiny app has a user-friendly interface with several tabs:

- **Data Input:** For uploading the CSV file.
- **Customer Behavior Analysis:** Contains the visualizations (pie, bar, scatter, and box plots).
- **Customer Clustering:** For performing K-means clustering and displaying results.
- **Itemsets Algorithm:** For generating and displaying association rules.

---

## Conclusion

This project combines various aspects of data science, including data visualization, clustering, and association rule mining, providing a comprehensive analysis of customer behavior. The use of Shiny allows users to interactively explore the data and gain insights into customer spending patterns.
