BEGIN
  FOR rec IN
    (
      SELECT
        table_name
      FROM
        all_tables
      WHERE
        table_name LIKE 'RESULTS%'
    )
  LOOP
    EXECUTE immediate 'DROP TABLE  '||rec.table_name || ' CASCADE CONSTRAINTS';
  END LOOP;
END;