import os
import sys

# ensure package root is on path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

import oferta_educativa_laboral.pipeline.pipeline_oferta_laboral as pol

def test_count_words_runs(tmp_path):
    text_file = tmp_path / "dummy.txt"
    text_file.write_text("hello world")

    out_file = text_file.with_suffix('.counts')
    pol.countWords(str(text_file), str(out_file))

    assert out_file.exists()
    lines = out_file.read_text().splitlines()
    assert lines[0].startswith("word")
    words = {l.split('\t')[0]: int(l.split('\t')[1]) for l in lines[1:]}
    assert words == {"hello": 1, "world": 1}
