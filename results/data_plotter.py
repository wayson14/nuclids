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
        print(residual_dict)

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
            print(f"{line_number}: {line}")

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
                print(channels_dict)
                headers = ["ELAB"] + list(channels_dict.values())
                final_table.append(headers)
            if line_number > 5:
                energies.append(float(line.split()[0]))
                sigmas = line.split()[8:]
                row = [float(line.split()[0])] + sigmas
                final_table.append(row)
        return final_table


#     content = f.readline()
# #TODO: TU SKOŃCZYŁEM
#             content = content.format(
#                 shortened_label=self.shortened_label,
#                 a_proj=self.a_proj,
#                 z_proj=self.z_proj,
#                 a_targ=self.a_targ,
#                 z_targ=self.z_targ,
#                 channel_n=self.channel_n,
#                 channel_a=self.channel_a,
#                 channel_p=self.channel_p,
#                 energies_marker=energy_input_str,
#             )

#             self.input_file_string = content
#         return


def plot_experiment(data_table):
    energies = []
    data = []
    data2 = []
    headers = []
    s = 0
    data_series = [len(data_table[0])]

    # print(data_table)
    n = len(data_table)
    columns = zip(*data_table)
    columns = list(columns)
    print(columns)
    # for row in data_table:
    #     # for i in range(len(row)):

    #     if row[0] == "ELAB":
    #         headers = row
    #         continue
    #     else:
    #         for i in range(len(row)):
    #             if i == 0:
    #                 energies.append(row[i])
    #             else:
    #                 data[i].append(row[i])
    #         print(row)
    #         data.append(row[10])
    #         data2.append(row[11])

    # for i in range(len(data_table)):
    #     if i == 0:
    #         continue
    #     for j in range(len(data_table[i])):
    #         # data_series.
    #         if j == 0:
    #             continue
    #         data_series[j].append(data_table[i][j])
    # plt.style.use("_mpl-gallery")

    # make data
    x = energies
    # y = 4 + 1 * np.sin(2 * x)
    print(data)
    y = data
    # x2 = np.linspace(0, 10, 25)
    # y2 = 4 + 1 * np.sin(2 * x2)

    # plot
    fig, ax = plt.subplots()

    # ax.plot(x2, y2 + 2.5, "x", markeredgewidth=2)
    for serial in data_series:
        print(serial)
        ax.plot(x, serial, "x", markeredgewidth=2.0)
    # ax.plot(x, y, "x", markeredgewidth=2.0)
    # ax.plot(x, data2, "x", markeredgewidth=2.0)
    # ax.plot(x2, y2 - 2.5, "o-", linewidth=2)

    # ax.set(xlim=(0, 8), xticks=np.arange(1, 8), ylim=(0, 8), yticks=np.arange(1, 8))
    # plt.yscale("log")
    plt.show()


if __name__ == "__main__":
    plot_data = extract_channels_energies_sigmas_from_file()
    plot_experiment(plot_data)
