import pytest
from results.data_plotter import translate_residuals_into_channels


def test_translate_residuals_into_channels():
    residuals_dict = {1: "sig_244No", 2: "sig_245Md", 3: "sig_246Fm"}
    n_sum = 255
    p_sum = 102
    channels = translate_residuals_into_channels(residuals_dict, n_sum, p_sum)
    assert channels[1] == {"n": 11, "p": 0}
    assert channels[2] == {"n": 10, "p": 1}
    assert channels[3] == {"n": 9, "p": 2}
