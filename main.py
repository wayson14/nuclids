import requests
from bs4 import BeautifulSoup as bs
import json

# Making a GET request
# r = requests.get('http://nrv.jinr.ru/nrv/webnrv/expdata/get_reaction.php?task=evaporation_residues&id=285&p=&n=&a=&channel=')

# Parsing the HTML
# soup = bs(r.content, 'html.parser')

# s = soup.find('textarea', id_='ID_DATA_EVR')
# content = soup.find_all('p')


def get_reaction_data(id=1):
    reaction_id = id
    base_url = f"http://nrv.jinr.ru/nrv/webnrv/expdata/get_reaction.php?task=evaporation_residues&id={reaction_id}&p=&n=&a=&channel="
    response = requests.get(base_url)
    # soup = bs(response.content, 'html.parser')
    response.encoding = "utf-8-sig"
    valid_data = response.json()
    print(valid_data["data"])
    return valid_data


# def create_reaction_object:


class ExperimentalDataRow:
    def __init__(self, reaction_id: int, json_experiment_data: dict):
        for key, value in json_experiment_data.items():
            setattr(self, key, value)
        self.reaction_id = reaction_id


class Reaction:
    def __init__(self, json_source_data: dict):
        for key, value in json_source_data.items():
            if key != "data":
                # self[key]
                # self[key] = value
                setattr(self, key, value)
            else:
                self.data = []
                self.parse_experimental_data(value)

    def parse_experimental_data(self, value):
        rows = value.split("\n")
        for row in rows:
            buffer = row.split()
            self.data.append(
                {
                    "energy": float(buffer[0]),
                    "cross_section": float(buffer[1]),
                    "error_plus": float(buffer[2]),
                    "error_minus": float(buffer[3]),
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
            item_relcode = item.relcode
            for data_row_dict in item.data:
                buffer.append(item_relcode)
                buffer += data_row_dict.values()
        return buffer

    def save_results_table_to_csv(self):
        pass


if __name__ == "__main__":
    # get_reaction_data()
    # EASY TESTING
    sample_reaction_data = get_reaction_data(id=1)
    r1 = Reaction(sample_reaction_data)
    r2 = Reaction(sample_reaction_data)
    r_table = ReactionTable()
    r_table.append(r1)
    r_table.append(r2)
    to_csv = r_table.combine_experimental_data_rows_with_ids()
    print(to_csv)
