from pathlib import Path
import importlib
import inspect
import sys


def test_pipeline_exists():
    """Ensure the example pipeline file is present."""
    base = Path(__file__).resolve().parents[1]
    path = base / "oferta_educativa_laboral" / "pipeline" / "pipeline_test.py"
    assert path.is_file()


def test_pipeline_tasks_present():
    """Import the real pipeline and verify key tasks are defined."""
    base = Path(__file__).resolve().parents[1]
    sys.path.insert(0, str(base))
    module = importlib.import_module(
        "oferta_educativa_laboral.pipeline.pipeline_oferta_laboral"
    )
    functions = {
        name
        for name, obj in inspect.getmembers(module)
        if inspect.isfunction(obj)
    }
    expected = {"convert_to_csv", "run_1b_accdb_tables_check"}
    assert expected.issubset(functions)
