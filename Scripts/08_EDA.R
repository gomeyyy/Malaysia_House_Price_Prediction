       library(ggplot2)
       library(scales)
      
       # median house distribution 
       ggplot(housing, aes(x = Median_Price)) +
         geom_histogram(bins = 30, fill = "steelblue", color = "black") +
         scale_x_continuous(
           limits = c(0, 4000000),
           labels = scales::dollar_format(prefix = "RM ", big.mark = ",")
         ) +
         labs(
           title = "Distribution of Median House Prices",
           x = "Median House Price (MYR)",
           y = "Frequency"
         )
       

      # median house price by state
       ggplot(housing,
              aes(x = reorder(State, Median_Price, FUN = mean),
                  y = Median_Price)) +
         stat_summary(fun = mean, geom = "bar", fill = "darkgreen") +
         coord_flip() +
         scale_y_continuous(labels = dollar_format(prefix = "RM ", big.mark = ",")) +
         labs(
           title = "Average Median House Price by State",
           x = "State",
           y = "Average House Price (MYR)"
         )
       
       # median_price vs median_psf
       ggplot(housing, aes(x = Median_PSF, y = Median_Price)) +
         geom_point(alpha = 0.5, color = "purple") +
         geom_smooth(method = "lm", color = "red") +
         scale_y_continuous(labels = dollar_format(prefix = "RM ", big.mark = ",")) +
         labs(
           title = "Median House Price vs Price per Square Foot",
           x = "Median Price per Square Foot (MYR)",
           y = "Median House Price (MYR)"
         )