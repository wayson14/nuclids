import requests
from bs4 import BeautifulSoup as bs
import json
import csv
import periodictable as pt

# TODO: (possibilities)
# -plotting charts, nice interface to interact
# -further data-mining, maybe some universalisation of the scraper
# -calculations, predictions
# -building proper database with easy development
# -connecting to some LLM


def get_reaction_data(id=1):
    reaction_id = id
    base_url = f"http://nrv.jinr.ru/nrv/webnrv/expdata/get_reaction.php?task=evaporation_residues&id={reaction_id}&p=&n=&a=&channel="
    response = requests.get(base_url)
    # soup = bs(response.content, 'html.parser')
    response.encoding = "utf-8-sig"
    valid_data = response.json()
    print(f"relcode: {id}")
    # print(valid_data["data"])
    return valid_data


class ExperimentalDataRow:
    def __init__(self, reaction_id: int, json_experiment_data: dict):
        for key, value in json_experiment_data.items():
            setattr(self, key, value)
        self.reaction_id = reaction_id


class Reaction:
    def __init__(self, json_source_data: dict):
        normalise_sigma = False
        normalise_E = False
        self.original_sigma_unit = ""
        for key, value in json_source_data.items():
            if key != "data":
                if key == "e_sys" and value == "cm":
                    # checks the energy frame and if it's cm, translates to lab
                    setattr(self, key, "non relativistically converted cm -> lab")
                    normalise_E = True
                    continue
                if key == "sigma_units" and value != "mb":
                    # checks the sigma unit, and if it is not mb, signals need of conversion
                    setattr(self, key, f"converted from {value} to mb")
                    normalise_sigma = True
                    continue

                # if none normalisation needed, just copy the value
                setattr(self, key, value)
            else:
                self.data = []
                self.parse_experimental_data(value, normalise_sigma, normalise_E)

    def get_element_symbol_by_z(self, z) -> str:
        return pt.elements[z].symbol

    def get_shortened_reaction_label(self) -> str:
        return f"({self.a_proj}){self.elem_proj}+({self.a_targ}){self.elem_targ} -> ({int(self.a_proj)+int(self.a_targ)}){self.get_element_symbol_by_z(int(self.z_proj)+int(self.z_targ))}"

    def E_cm_to_E_lab(self, e_cm_value: float, a_targ: int, a_proj: int) -> float:
        """This function converts center of mass energy (E_cm) into the energy of the projectile (E_lab) - it doesn't take into
        account relativistic effects, so the error might be as big as a few %"""
        return e_cm_value * ((a_proj + a_targ) / a_targ)

    def do_sigma_normalisation(
        self, sigma_value: float, original_sigma_unit: str
    ) -> float:
        """This function performs sigma normalisation from original unit to mb"""
        sigma_conversion_dict = {
            "mb": 1,
            "mcb": 1000,
            "nb": 1000000,
            "pb": 1000000000,
            "fb": 1000000000000,
        }
        return sigma_value * sigma_conversion_dict[original_sigma_unit]

    def parse_experimental_data(self, value, normalise_sigma=False, normalise_E=False):
        rows = value.split("\n")
        for row in rows:
            buffer = row.split()
            self.data.append(
                {
                    "energy": (
                        self.E_cm_to_E_lab(float(buffer[0]), self.a_targ, self.a_proj)
                        if normalise_E
                        else float(buffer[0])
                    ),
                    "cross_section": (
                        self.do_sigma_normalisation(
                            float(buffer[1]), self.original_sigma_unit
                        )
                        if normalise_sigma
                        else float(buffer[1])
                    ),
                    "error_plus": (
                        self.do_sigma_normalisation(
                            float(buffer[2]), self.original_sigma_unit
                        )
                        if normalise_sigma
                        else float(buffer[2])
                    ),
                    "error_minus": (
                        self.do_sigma_normalisation(
                            float(buffer[3]), self.original_sigma_unit
                        )
                        if normalise_sigma
                        else float(buffer[3])
                    ),
                    "channel": buffer[4],
                }
            )


class EvaporationResiduesReaction(Reaction):
    def __init__(self):
        super().__init__()


class ElasticScatteringReaction(Reaction):
    def __init__(self):
        super().__init__()


class FusionReaction(Reaction):
    def __init__(self):
        super().__init__()


class ReactionTable(list):
    def __init__(self, *args):
        super().__init__()

    def combine_experimental_data_rows_with_ids(self) -> list:
        buffer = []
        for item in self[:]:
            # item_relcode = item.relcode
            for data_row_dict in item.data:
                row = [item.relcode, item.get_shortened_reaction_label()]
                row += data_row_dict.values()
                buffer.append(row)
        return buffer

    def save_results_table_to_csv(self, filename: str = "results.csv"):
        items = self.combine_experimental_data_rows_with_ids()
        # keys = self[0].__dict__
        try:
            with open(filename, "w", newline="\n") as f:
                writer = csv.writer(f)
                writer.writerow(
                    [
                        "relcode",
                        "shortened_label",
                        "energy",
                        "cross_section",
                        "error_plus",
                        "error_minus",
                        "channel",
                    ]
                )
                for item in items:
                    writer.writerow(item)
        except BaseException as e:
            print(f"Exception:{e}", filename)
        else:
            print("Data has been loaded properly!")
        pass


def scrape_all_evaporation_data_to_csv(min_relcode=1, max_relcode=300):
    reaction_table = ReactionTable()
    for relcode in range(min_relcode, max_relcode + 1):
        try:
            reaction_data = get_reaction_data(relcode)
            reaction = Reaction(reaction_data)
            reaction_table.append(reaction)
        except BaseException:
            continue
    reaction_table.save_results_table_to_csv()


if __name__ == "__main__":
    # get_reaction_data()

    # EASY TESTING #1
    # sample_reaction_data = get_reaction_data(id=1)
    # r1 = Reaction(sample_reaction_data)
    # r2 = Reaction(sample_reaction_data)
    # r_table = ReactionTable()
    # r_table.append(r1)
    # r_table.append(r2)
    # to_csv = r_table.combine_experimental_data_rows_with_ids()
    # print(to_csv)
    # r_table.save_results_table_to_csv()

    # INTEGRATION TESTING
    # scrape_all_evaporation_data_to_csv(1, 305)
    scrape_all_evaporation_data_to_csv(1, 2)
