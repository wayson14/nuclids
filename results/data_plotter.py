import matplotlib
import periodictable as pt
import re


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
if __name__ == "__main__":
    print(extract_channels_energies_sigmas_from_file())
