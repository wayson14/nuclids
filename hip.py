import json
import csv
import periodictable as pt

"""Hivap Input Preparator:
Creates n hivap input tables based on input csv file, template of columns:
relcode | shortened_label       | energy (E_lab) [MeV] | max_channel   | ...(unnecessary)
(int)   | (a)Cr+(a)Pb -> (a)Sg  | 250                  | 1n2p1a        | ...(unnecessary)
"""


class Experiment:
    """Class with properities of an experiment, later written into hivap template file"""

    def __init__(self, data_dict):

        self.relccode = data_dict["relcode"]
        self.shortened_label: int = data_dict["shortened_label"]
        self.a_proj: int = data_dict["a_proj"]
        self.z_proj: int = data_dict["z_proj"]
        self.a_targ: int = data_dict["a_targ"]
        self.z_targ: int = data_dict["z_targ"]
        self.energy_data_series = [data_dict["energy"]]
        self.channel = self.determine_max_channel(data_dict["channel"])

        pass

    def parse_shortened_label(self) -> tuple[int, int, int, int]:
        pass

    def determine_max_channel(self, exp_channel_str: str) -> tuple[int, int, int]:
        "Returns a vector of of max n, p and alpha(a) values"
        pass

    def prepare_hivap_input_file(self):
        pass

    def append_energy_series(self, energy_value: int):
        self.energy_data_series.append(energy_value)


def load_rows(filename: str) -> list[dict]:
    with open(filename, "r", newline="\n") as csvfile:
        reader = csv.DictReader(csvfile)
        table_dicts = []
        for table_dict in reader:
            table_dicts.append(table_dict)
        return table_dicts


def parse_data(dict_table):
    pass


# def load_experiment_series_from_csv(relcode):
#     def load_rows(table_name: str) -> [dict]:
#     with open("csv/" + table_name + ".csv", "r", newline="") as csvfile:
#         reader = csv.DictReader(csvfile)
#         table_dicts = []
#         for table_dict in reader:
#             table_dicts.append(table_dict)
#         return table_dicts


def save_hivap_input_file(relcode):
    pass


def experiment_generator(csv_filename="results.csv"):
    """Function which:
    1. reads the content of the whole csv containing experiment series,
    2. creates Experiment objects of particular experiments (rows where
    laboratory energies combined, with determined max evaporation of n, p, a)
    3. and in specific folder (./hivap_input_files/) outputs
    Hivap input files (hivapein[n].dat) later used to perform simulations
    (method from Experiment class, so every Experiment creates its own file)"""

    data = load_rows(csv_filename)
    experiment_dict = {}
    relcodes_table = []
    # def check_for_relcode(relcode, exp):

    # going through rows
    for row in data:
        if row["relcode"] not in relcodes_table:
            experiment = Experiment(row)
            experiment_dict[row["relcode"]] = experiment
            relcodes_table.append(row["relcode"])
        else:
            experiment: Experiment = experiment_dict[row["relcode"]]
            experiment.append_energy_series(row["energy"])

    # now experiment_dict should be full of experiment data


if __name__ == "__main__":
    experiment_generator()
