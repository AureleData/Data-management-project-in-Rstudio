# Project Description: Restructuring Configuration Table for Central Banks

## Objective
The objective of this project was to modify the configuration table to implement changes in the structure by adding rules instead of fixed inputs. This approach aimed to reduce the number of rows in the configuration table, making it easier to manage and modify.

## Background
This project was undertaken at the request of the Central Bank of Italy (Banca d'Italia) in collaboration with the Central Bank of Malta. The requested modifications were made to enhance the efficiency and manageability of the configuration table, particularly focusing on the table named "PI DI," which relates to input from Portfolio and Direct Investments.

## Tools Used
- **IDE**: RStudio
- **Packages**: Dplyr, TidyR

## Data Sources
The codes for confidential inputs were altered, while the codes from Eurostat sources remained public and can be referenced in the BPM6 vademecum for Balance of Payments.

## Steps Taken

1. **Subset Common Rows**: Gathered common rows together by creating subsets. For example, outdated filters like SMSEO were updated using the IN SQL operator instead of using the '|' operator.

2. **Apply Common Process**: For most tables, a similar process was applied. The primary method involved subseting by `INPUTCUBEID`, using regex to extract information between `"TYPINS IN ("` and `") AND"`, and then separating the extracted `TYPINS` codes. These codes were then used to create rules based on a predefined template, where `TYPINS` served as input elements with various output items.

3. **Rule Implementation**: Once the rules were formulated, the original table was amended. This involved grouping `TYPINS` based on columns with different inputs. The extracted `TYPINS` were then used to replace the content between `"TYPINS IN ()"`, effectively reducing the size of the table.

## Conclusion
By implementing these steps, we successfully restructured the configuration table, replacing fixed inputs with dynamic rules. This not only reduced the table's size but also enhanced its flexibility, making it easier to manage and modify in the future.
This script was then generalized and apply across all 24 configuration tables we were using. This process also allowed to us to find issues and the configuration tables in terms of consistency that we then corrected. 
