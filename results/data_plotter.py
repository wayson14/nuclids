import matplotlib.pyplot as plt
import periodictable as pt
import re
import numpy as np


# I'm not sure if it needs consideration between aphas and compond neutrons, so I have omitted alpha channel
def translate_residuals_into_channels(
    residual_dict: dict, n_sum: int, p_sum: int
) -> dict:
    channels_dict = {}
    i = 1
    for key in residual_dict.keys():
        # print(residual_dict)

        symbols_match = re.search("[A-Z]{1}[a-z]{1}|[A-Z]{1}", residual_dict[key])
        element_symbol = symbols_match.group()
        # element_symbol = residual_dict[key][-2:]

        numbers_match = re.search("[0-9]{3}|[0-9]{2}", residual_dict[key])
        neutrons = int(numbers_match.group())

        p_channel = p_sum - pt.elements.symbol(element_symbol).number
        n_channel = n_sum - neutrons
        channels_dict[i] = {"n": n_channel, "p": p_channel}
        i += 1
    return channels_dict


def extract_reaction_label_from_file(filename: str = "output_5.dat") -> str:
    with open(filename, "r") as f:
        words = f.readline().split()
        reaction_label = (
            f"({words[4]}){pt.elements[int(words[2])]}"
            + " + "
            + f"({words[8]}){pt.elements[int(words[6])]}"
            + " --> "
            + f"({int(words[4])+int(words[8])}){pt.elements[int(words[2])+int(words[6])]}"
        )
    return reaction_label


def extract_channels_energies_sigmas_from_file(filename: str = "output_5.dat"):
    with open(filename, "r") as f:
        line_number = 0
        headers = []
        energies = []
        n_sum = 0
        p_sum = 0
        final_table = []
        for line in f:
            line_number += 1
            # print(f"{line_number}: {line}")

            if line_number == 1:
                words = line.split()
                n_sum = int(words[4]) + int(words[8])
                p_sum = int(words[2]) + int(words[6])

            if line_number < 5:
                continue
            if line_number == 5:
                headers_table = line.split()[8:]
                residual_dict = {}
                for i in range(len(headers_table)):
                    index = i + 1
                    residual_dict[index] = headers_table[i]
                channels_dict = translate_residuals_into_channels(
                    residual_dict, n_sum, p_sum
                )
                # print(channels_dict)
                headers = ["ELAB"] + list(channels_dict.values())
                final_table.append(headers)
            if line_number > 5:
                energies.append(float(line.split()[0]))
                sigmas = line.split()[8:]
                row = [float(line.split()[0])] + [
                    float(x) for x in sigmas
                ]  # str to float
                final_table.append(row)
        return final_table


def plot_experiment(
    data_table, reaction_label: str = "AA + BB", output_filename: str = "test_plot.png"
):
    i = 0
    markers_table = ["o", "v", "^", "s", "P", "X", "D"]
    columns = zip(*data_table)
    columns = list(columns)
    # print(columns)

    fig, ax = plt.subplots()
    plt.yscale("log")
    x = columns[0][1:]  # energies
    plt.title(reaction_label)
    plt.xlabel("E_Lab (MeV)")
    plt.ylabel("Cross section for evaporation (mb)")
    for column in columns[1:]:  # first is the energy column
        # print(type(max(column[1:])))

        if max(column[1:]) == 0:
            print(column[0], "zeroes")
            continue

        channel_dict = column[0]
        y = np.array(column[1:])
        ax.plot(
            np.array(x),
            # np.array(column[1:]),
            np.where(y == 0, np.nan, y),
            # np.where(y == 0, np.nan, y),
            markers_table[
                channel_dict["p"]
            ],  # every residual element has a different marker
            markeredgewidth=2.0,
            linestyle="solid",
            label=str(channel_dict["n"]) + "n" + str(channel_dict["p"]) + "p",
        )
        i += 1

    plt.grid()
    plt.tight_layout()
    box = ax.get_position()
    ax.set_position([box.x0, box.y0, box.width * 0.8, box.height])
    ax.legend(loc="center left", bbox_to_anchor=(1.025, 0.5), title="EvR channels")

    plt.savefig(output_filename)
    plt.show()


if __name__ == "__main__":
    plot_data = extract_channels_energies_sigmas_from_file()
    reaction_label = extract_reaction_label_from_file()
    plot_experiment(plot_data, reaction_label)
