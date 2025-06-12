from pathlib import Path


def test_pipeline_exists():
    """Ensure the example pipeline file is present."""
    base = Path(__file__).resolve().parents[1]
    path = base / "oferta_educativa_laboral" / "pipeline" / "pipeline_test.py"
    assert path.is_file()


def test_main_pipeline_has_convert():
    """Check convert_to_csv task is defined."""
    base = Path(__file__).resolve().parents[1]
    pipeline_path = (
        base / "oferta_educativa_laboral" / "pipeline" / "pipeline_oferta_laboral.py"
    )
    text = pipeline_path.read_text()
    assert "def convert_to_csv" in text
