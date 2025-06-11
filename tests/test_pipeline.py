from pathlib import Path


def test_pipeline_exists():
    """Ensure the example pipeline file is present."""
    base = Path(__file__).resolve().parents[1]
    path = base / "oferta_educativa_laboral" / "pipeline" / "pipeline_test.py"
    assert path.is_file()
