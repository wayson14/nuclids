# Module in which are functions responsible for
# accessing data in databases and APIs.
# TODO: Write CSV writer module
# TODO: Write CSV loader module


# import csv

# csv.writer
import csv


# TODO: add exception control
def save_rows(table_name: str, table_dicts: [dict], fieldnames: [str]):
    with open("csv/" + table_name + ".csv", "w", newline="") as csvfile:
        writer = csv.DictWriter(
            csvfile,
            fieldnames=fieldnames,
            quoting=csv.QUOTE_NONNUMERIC,
        )
        writer.writeheader()
        for table_dict in table_dicts:
            writer.writerow(table_dict)


def load_rows(table_name: str) -> [dict]:
    with open("csv/" + table_name + ".csv", "r", newline="") as csvfile:
        reader = csv.DictReader(csvfile)
        table_dicts = []
        for table_dict in reader:
            table_dicts.append(table_dict)
        return table_dicts


# TODO: Add exceptions
def parse_rows_to_referenced_types(
    list_of_types: [str],
    rows: [dict],
    fieldnames: [str],
) -> [dict]:
    result_dicts = []
    if list_of_types is None:
        list_of_types = ["str" for i in range(len(fieldnames))]
    for row in rows:
        new_dict = {}
        for fieldname, fieldname_type in zip(fieldnames, list_of_types):
            if fieldname_type == "int":
                new_dict[fieldname] = int(row[fieldname])
            elif fieldname_type == "float":
                new_dict[fieldname] = float(row[fieldname])
            else:
                new_dict[fieldname] = str(row[fieldname])
        result_dicts.append(new_dict)
    return result_dicts
