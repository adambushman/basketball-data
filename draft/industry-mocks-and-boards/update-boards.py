import duckdb

# Connect
duckdb.execute("ATTACH 'ducklake:draft_catalog.ducklake' AS nbadraft (DATA_PATH 'draft-ducklake');")
duckdb.execute("USE nbadraft;")


# Populate raw tables
for file, table in list(zip(
  ['2026_Industry_Boards.csv', '2026_Industry_Mocks.csv'],
  ['industry_boards_raw', 'industry_mocks_raw']
)):
  duckdb.execute(f"""
             CREATE OR REPLACE TABLE {table} AS
             FROM '{file}'
             """)


# Append to clean tables
for raw_table, clean_table in list(zip(
  ['industry_boards_raw', 'industry_mocks_raw'],
  ['industry_boards_clean', 'industry_mocks_clean']
)):
  duckdb.execute(f"""
              INSERT INTO {clean_table}
              WITH long AS (
                UNPIVOT {raw_table}
                ON COLUMNS(* EXCLUDE "Rank")
                INTO
                  NAME "source"
                  VALUE "prospect"
              )
              SELECT
              date_trunc('day', strptime(
                str_split(source, ' | ')[2]
                ,'%m/%d/%y'
              )) AS publish_date
              ,str_split(source, ' | ')[1] AS source
              ,prospect
              ,Rank AS rank
              FROM long
              """)