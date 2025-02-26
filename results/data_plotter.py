import matplotlib

with open("output_5.dat", "r") as f:
    content = f.readline()
#TODO: TU SKOŃCZYŁEM
            content = content.format(
                shortened_label=self.shortened_label,
                a_proj=self.a_proj,
                z_proj=self.z_proj,
                a_targ=self.a_targ,
                z_targ=self.z_targ,
                channel_n=self.channel_n,
                channel_a=self.channel_a,
                channel_p=self.channel_p,
                energies_marker=energy_input_str,
            )

            self.input_file_string = content
        return