# RShinyApp

link to shiny app: https://hsu230.shinyapps.io/MasterProgram/
link to poster that won 1st place in the OR/MS Mini-poster Competition: https://www.linkedin.com/feed/update/urn:li:activity:6741096909919916032/

This project was for my Using R for Analytics class. The goal was to create an R Shiny App that help the business and did two types of analytics (descriptive, predictive, prescriptive). Utilizing data on Analytics Masters Programs, we were tasked with taking a look at the course offerings and see the impact of course offerings on program outcomes like Starting Salary and Placement Rate. 

The inspiration and driver behind how we set up the shiny app was our own personal experience researching programs. Having a tool that would give you all the information you need at once depending on the different factors you were looking for in a program was the ultimate goal. Beyond that, we also wanted to see which courses were offered the most and how much can courses predict salary. We were able to achieve an R-squared of around 40% for the course offering in our predictive models of both Average Starting Salary and Placement Rate. 

Things I would do differently
1. Create better functionality for the "Find a Program" tab. I really wanted to include a checklist for courses to also have the csv filter by course, but I couldn't figure it out with the time given. 
2. Predict on more than just the courses offered. Since this project was really just looking at the impact of courses offered, we were only able to get an R-squared of 40% for our model. With complete control, I would add factors like tuition or location to the model to try to get a higher predictive accuracy. 
3. Figure out another way to handle the large amount of missing data, especially in our predictor variable. While we utilized mice to impute values, I believe just getting rid of the schools with lots of missing data might achieve an overall better look at the impact of specific variables and a better accuracy in the end. 
4. Add a prescriptive  element. Because we ran out of time, we were able to add this in, but in the future, adding a component that optimizes which classes should be offered based on program outcomes like salary. I would do this by creating a linear model with the variables and then using the coefficients in the setup of the optimization problem. 
