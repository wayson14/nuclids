import pytest
from scraper import get_reaction_data, Reaction, ReactionTable

sample_reaction_data = get_reaction_data(id=1)


def test_scraper_reaction_creation():
    # print(sample_reaction_data)
    r1 = Reaction(sample_reaction_data)
    assert r1.elem_proj == "Cr"

    pass


# test written prior to normalisation efforts
# def test_main_reaction_experimental_data_parser():
#     r1 = Reaction(sample_reaction_data)
#     assert r1.data[0] == {
#         "energy": 198.74,
#         "cross_section": 595,
#         "error_plus": 63,
#         "error_minus": 63,
#         "channel": "1n",
#     }


def test_scraper_reaction_table():
    r1 = Reaction(sample_reaction_data)
    r2 = Reaction(sample_reaction_data)
    r_table = ReactionTable()
    r_table.append(r1)
    r_table.append(r2)
    assert len(r_table) == 2
    assert r_table[0].elem_proj == "Cr"


def test_scraper_sigma_normalisation():
    r1 = Reaction(sample_reaction_data)
    assert r1.do_sigma_normalisation(1000, "mcb") == 1


def test_scraper_E_normalisation():
    pass


# def test_main_get
