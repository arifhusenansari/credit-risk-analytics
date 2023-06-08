# Problem Statement
Build Pipeline to load data from three different csv files to SQL database. Prepage scalable data ingestion pipeline. Build Data Mapping and Analytical query to analyse the data. 
# Approach
Provided data will be processed and loaded to final layer. Data ingestion pipelien is build in python code. 
Data will be loaded from csv to landing table without any transformation. Data from landing will be loaded to correction table, with proper data type coversion and data cleaning. Based on data source final model will have three table. This model will be build in enrichement layer. Final model table and intermediate table will have primary key defined and proper merge logic between source and target. This will make sure we will have latest updated data in Target entity. 
1.  dimtenant
2.  facttenantcontract
3.  facttenantsales

# Architecture Details
For this problem statement and to build enterprise level scalable data ingestion process with enterprise standards.
Data ingestion pipelien is divided in different zones and layers.

-   raw_zone: This zone will contain initially processed data from souce. Source can be any type. E.g. (structured   file like csv, SQL Database Like MSSQL Server). Data will be stored for historical purpose as well.
    Below are layers in row_zone
    1.  langing_layer -> Data from souce in our case from csv will be loaded to respective lable in landing layer. 
    landing layer will have all the historical data. 
-   quality_zone: This zone will contain all tables with clean data. Also business logic will be implemented in different layer of quality_zone based on data model. quality_zone has three layer. 
    1.  correction_layer -> Data will be loaded from landing layer to correction layer table by table. Also data will be converted to propre data type and data cleaning will done while loading data from landing to correction.
    2.  integration_layer -> integration layer will have view definitions to load data in our file tables. This views will contain business logic and other transformations to load data in data model. This will load data in tables act like fact and have reference on Dimension. 
    3.  enrichment_layer -> enrichment layer will represent our final data model, with fact and dimenstions. But Analytical product or any data product will not connect direclty to enrichment layer. Specific consumption layer is build for data consumption. Dimension table in enrichment layer are loaded from correction layer and Fact tables are loaded using logic in integration layer views. 
-   curated_zone: Curate_zone will act as consumption layer. Tables in this layer will be loaded as it is from enrichment. 
    1.  discovry_layer -> Data will be directly loaded from enrichment layer to discovery layer.  
    However, any views or table like aggregated table or aggregated view will be created in this layer and loaded based on view implemented in enrichment layer. Like we have two views one for Yearly Review and Category Analysis
    2.  provision_layer -> There is also skop for provision layer. This is used where we do not need transormed data. Like Data Scientist or Data Analyst might need raw data. In that case raw data can be directly loaded to provision layer for their consumption. In our case looking at time and requirement it's not needed.  


# Data Ingestion Flow
    1.  source (csv) -> raw_zone (landing_layer): Data will be laoded from souce files to landing layer table withour any transformation.
    2.  raw_zone(landing_layer) -> quality_zone(correction_layer): Data will be loaded from landing layer to respective table in correction layer. Data will be coverted to proper data type based on data and cleaning will be done.
    3.  quality_zone(correction_layer) -> quality_zone(enrichment_layer): All the dimension tables in enrichment layer will be loaded first from correction layer. This will be based on customer logic defined in view in correction layer. 
    4.  quality_zone(integration_layer) -> quality_zone(enrichment_layer): All the fact table in enrichment layer will be loaded from integration layer. This will be custome logic defined in views in integration layer. 
    5.  quality_zone(enrichment_layer) -> curated_zone(discovery_layer): Data from enrichemtn to discovery layer will be loaded for final consumption for end users.

# Metadata Driven Approach
Whole pipeline is build based on metadata driven approach. Below are brief details.
1.  metadata.dbo.Object: Contains entity header details. It can be file or table.
2.  metadata.dbo.Task: Contains the the flow of data from source to target entity. Different entities are configured for different process load. E.g. sourcetolanding, landingtocorrection etc.
3.  metadata.dbo.TaskColumnMappping: This table contain source and target column mapping. With all details like
    columnname, columdatatype, columnlength. Table are automatically generated based on definition provided in this table. No need to run separate DDL script to create table. 
4.  metadata.dbo.ELTLog: Used to log elt logs. 

# Implementation
Data ingestion pipeline is build in python for orchestration and MS SQL server to store data. 
Separate python file is created for zones. EndToEndLoad.py is the main file that will be executed and it will orchestrate all the step one by one by executing python script for repective layer. 
Three database in MSSQL server are created to represent different zone. metadata database is created to hold metadata configuration details.
1.  raw_zone
2.  quality_zone
3.  curated_zone
4.  metadata

This whole data pipeline cab be built using ADF as orchestrator and DataBricks notebook/python file as data processing code and Azure data lake will act as Storage having three container for three different zone. 
Databricks with Delta lakehouse feature can provide lot's of benefits. 

# Run the project
Below are steps to Run project locally.
# Tools
- MS SQL Server
- python
- vscode (for better experience)

# Steps
- Run script Create Database.sql avaible in Solution\scripts folder. This will create necessary database, metadata table and do the cofiguration
- Install pyodbc, pandas packages.
- In VSCode open solution folder. 
- Open DBUtility.py file and provide server details. Better to use SQLAuthentication.
- Navigate to EndToEndLoad.py
- Execute the python script.

# Scalability
- Any new data souce can be easily added. by doing proper configuration. We can on board new csv file. 
- Data can be loaded to any further layes and Zone by doing configuration. 
- Business logic can be implemented in views and deployed to proper layer and by mapping data can be transformed and loaded with minimal or no changes in existing code. 
- All table has datalineage id and this will help to check the flow of data in different layer. 

# What else can be implemented
- there are several things we can implement. Due to lake of time. In this solution they are not implemented. 
- new metatable e.g. taskincrementalload -> This will have configuration for increamental load from source to target. Only incremental data will be loaded. etl_lastmodified_date can be used. 
- In Merge, I have not implemented logic to update only updated data. But can be implemented easly with the help of configuratino and SHA256 code.
