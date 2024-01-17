# Confusion Matrix and ROC Curve Visualization 

## Motivation

This GitHub repository provides an R Shiny app for visualizing the effects of confusion matrices on classification metrics, accompanied by the visualization of Receiver Operating Characteristic (ROC) curves. Understanding the performance of classification models is crucial, and these visualizations offer insights into the model's behavior. 

I always found it mysterious how each quantity influences the various metrics associated with classification. Use this app to untangle your doubts and fears surrounding these metrics.

## Features

- **Confusion Matrix Visualization:** Explore the confusion matrix to understand the distribution of true positives, true negatives, false positives, and false negatives. Gain insights into the model's performance on different classes.

- **Classification Metrics:** Calculate and visualize essential classification metrics such as accuracy, precision, recall, and F1 score. Understand how these metrics are influenced by the confusion matrix.

- **ROC Curve Analysis:** Visualize the ROC curve to assess the trade-off between sensitivity and specificity. Determine the model's ability to distinguish between classes at various threshold levels.

## Usage

1. **Clone the Repository:**
   ```bash
   git clone https://github.com/AlexanderHolmes0/Visualize_Metrics.git
   cd confusion-matrix-roc-visualization
   ```

2. **Install Required Packages:**
   Open R and install the required packages using:
   ```R
   install.packages(c("shiny", "pROC", "caret"))
   ```

3. **Run the R Script:**
   Open and run the `app.R` script in your preferred R environment.

4. **Enter Confusion Matrix Data:**
   Prepare your confusion matrix data in a format compatible with the script. Follow the provided example or adapt it to your dataset.

5. **Explore Visualizations:**
   Run the script to generate visualizations of the confusion matrix, classification metrics, and ROC curve. Customize parameters as needed.

## Example

Check out the [Helpers.R](Helperr.R) file for the function that generates the matrix. Use it to understand how the visualization toolkit works.

## Contributing

We welcome contributions! If you have ideas for improvements, feature requests, or bug reports, please open an issue or submit a pull request.

## License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## Acknowledgments

- Special thanks to contributors and open-source libraries that made this app possible. The confusion matrix plotting function was originally found [here](https://stackoverflow.com/questions/23891140/r-how-to-visualize-confusion-matrix-using-the-caret-package)

Happy visualizing metrics in R!
